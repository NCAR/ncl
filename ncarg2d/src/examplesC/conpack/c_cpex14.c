/*
 * $Id: c_cpex14.c,v 1.1 1996-10-16 15:09:28 haley Exp $
 */
#include <stdio.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>
#include <math.h>

#define min(x,y)  ((x) < (y) ? (x) : (y))
#define max(x,y)  ((x) > (y) ? (x) : (y))

/*
 * Define the error file, the Fortran unit number, the workstation type,
 * and the workstation ID.
 */
#define lunit "gmeta"
#define iwtype SED_WSTYPE
#define iwkid 1

/*
 * This program attempts to present a solution to a problem sent to us
 * by a user: Imagine that we have three arrays, ICLR, XP, and YP, each
 * of which is dimensioned 129x97.  For each index pair "I,J" on a 129x97
 * grid, ICLR(I,J) is a color index associated with the point having user
 * coordinates XP(I,J) and YP(I,J); the color indices presumably imply a
 * mapping of a physical quantity into a palette of colors.  We want to
 * use the color indices to paint a picture of the grid in user space; we
 * do this in two different ways: 1) by using the GKS routine GFA to fill
 * areas; and 2) by using the GKS routine GCA to fill a cell array.  The
 * first of these is straightforward, but the second is not; to do it, we
 * have to program the inverse of the mapping from grid coordinates to
 * user coordinates that is defined by the contents of the arrays XP and
 * YP and this is difficult to do efficiently, because it is difficult,
 * given a point in user space, to determine which of the mapped grid
 * boxes contains that point.  The inverse mapping is done by the routine
 * CPMPXY, below.
 *
 * The key to doing the inverse mapping efficiently is to use an index
 * array, called INDX.  The following PARAMETER statement specifies two
 * of the dimensions of this array, MIND and NIND (the third dimension
 * is a 4).  Using larger values of MIND and/or NIND results in faster
 * execution and a larger INDX array, while using smaller values results
 * in slower execution and a smaller INDX array.  Note that, if this
 * PARAMETER statement is changed, another one in the mapping routine
 * CPMPXY must be changed to match it.
 */
#define MIND 25
#define NIND 25

/*
 * The parameters MCRA and NCRA declare the dimensions of the cell array,
 * ICRA, to be used.  Using larger values of MCRA and NCRA give a better
 * picture, but increase execution time and metafile size.  Using smaller
 * values give smaller execution times and smaller metafiles.  It is up
 * to the user to determine whether the picture given by a particular
 * pair of values is acceptable or not.
 *
 *    (MCRA=800,NCRA=640)  !  Fine (maybe too fine).
 *
 * Probably acceptable?
 */
#define NCRA 320 
#define MCRA 400


/*   (MCRA=200,NCRA=160)  !  Too coarse, I think.
 *
 * The parameters LRWK and LIWK define the lengths of the workspace
 * arrays to be passed to CONPACK.
 */
#define LRWK  5000
#define LIWK 1000

/*
 * Declare stuff that has to be passed to the mapping routine CPMPXY:
 * this includes the arrays XP and YP; XPMN, XPMX, YPMN, and YPMX, in
 * which are placed the min/max values of XP/YP; IBEG, IEND, JBEG, and
 * JEND, which specify the grid box in which CPMPXY determined that the
 * last point to be inverse-transformed lay; the index array INDX; and
 * a flag, ISOD, saying whether to use single or double precision
 * arithmetic.
 */

float xp[97][129], yp[97][129],xpmn,xpmx,ypmn,ypmx;
int ibeg,iend,jbeg,jend;
int isod, indx[MIND][NIND][4];

main()
{
    Gcolr_rep ctab;
    FILE *fpr;
    float xvpl,xvpr,yvpb,yvpt;
    float xwdl,xwdr,ywdb,ywdt;
    int lnlg;
    int i, j, l1, in, jn, icnt;
    extern void gettrn();
    extern void NGCALLF(cpmpxy,CPMPXY)();
    float xmin, xmax, ymin, ymax;
    float tst1, tst2, tst3;
    float xgrd, ygrd, xusr, yusr;
    float eavg, emax, eror;
    float xgbk, ygbk;
    float xdat, ydat;
    int inmn, inmx, jnmn, jnmx;
    float x, y;
    int imap;
    extern double nint();
/*
 * The array iclr holds the color indices described above.  The array
 * RCLR, which is the same size is of type REAL, will be used to produce
 * a fourth frame, showing contours.
 */
    int iclr[97][129];
    float rclr[97][129];
/*
 * The array ICRA is the cell array.
 */
    Gpat_rep icra;
    Grect rect;
/*
 * The arrays XTMP and YTMP are used to hold the coordinates defining
 * polygons in calls to GFA, below.
 */
    Gpoint_list area;
/*
 * The arrays RWRK and IWRK are workspace arrays for CONPACK.
 */
    float rwrk[LRWK];
    int iwrk[LIWK];
/*
 * INITIALIZE GKS AND SET INTERNAL PARAMETERS IN UTILITIES. -------------
 *
 * Open GKS.
 */
    gopen_gks ("stdout",0);
    gopen_ws (iwkid, lunit, iwtype);
    gactivate_ws(iwkid);
/*
 * Turn clipping by GKS off.
 */
    gset_clip_ind (GIND_NO_CLIP);
/*
 * Define the color red for use in a couple of frames below.
 */
    ctab.rgb.red = 1.; ctab.rgb.green = ctab.rgb.blue = .4;
    gset_colr_rep (iwkid,199,&ctab);
/*
 * Tell PLOTCHAR to use font number 25 (a filled font) and to outline
 * each character.
 */
    c_pcseti ("FN - FONT NUMBER",25);
    c_pcseti ("OF - OUTLINE FLAG",1);
/*
 * Tell PLOTCHAR to tell the Bezier package to reproduce the curves
 * outlining the characters with a little less fidelity.  This cuts
 * down on the size of the metafile.
 */
    c_pcsetr ("FB - FIDELITY OF BEZIER CURVES",.00015);
/*
 * READ INPUT FILE. -----------------------------------------------------
 *
 * Open an input file created by running the user's program, with some
 * added WRITE statements, on the Cray.
 */
    printf("\n Reading input data file...\n" );

    fpr = fopen("cpex14.dat","r");
/*
 * Read argument values and call SET to duplicate the user's mapping of
 * user coordinates into the plotter frame.
 */
    fscanf(fpr,"%g", &xvpl);
    fscanf(fpr,"%g", &xvpr);
    fscanf(fpr,"%g", &yvpb);
    fscanf(fpr,"%g", &yvpt);
    fscanf(fpr,"%g", &xwdl);
    fscanf(fpr,"%g", &xwdr);
    fscanf(fpr,"%g", &ywdb);
    fscanf(fpr,"%g", &ywdt);
    fscanf(fpr,"%d", &lnlg);
    c_set (xvpl,xvpr,yvpb,yvpt,xwdl,xwdr,ywdb,ywdt,lnlg);

    printf("  viewport left edge: %g\n",xvpl );
    printf("  viewport right edge: %g\n",xvpr);
    printf("  viewport bottom edge: %g\n",yvpb);
    printf("  viewport top edge: %g\n",yvpt);
    printf("  window left edge: %g\n",xwdl);
    printf("  window right edge: %g\n",xwdr);
    printf("  window bottom edge: %g\n",ywdb);
    printf("  window top edge: %g\n",ywdt);
/*
 * Read color-index information and do the calls necessary to duplicate
 * the user's palette of colors.
 */
    for( i = 1; i <= 128; i++ ) {
        fscanf(fpr,"%d %g %g %g",&icnt,&ctab.rgb.red,&ctab.rgb.green,&ctab.rgb.blue);
        gset_colr_rep (iwkid,icnt,&ctab);
    }
/*
 * Read the arrays XP, YP, and ICLR, as described above.
 */
    for( i = 0; i < 97; i++ ) {
        for( j = 0; j < 129; j++ ) {
            fscanf(fpr,"%g", &xp[i][j] );
        }
    }
    for( i = 0; i < 97; i++ ) {
        for( j = 0; j < 129; j++ ) {
            fscanf(fpr,"%g", &yp[i][j] );
        }
    }
    for( i = 0; i < 97; i++ ) {
        for( j = 0; j < 129; j++ ) {
            fscanf(fpr,"%d", &iclr[i][j] );
        }
    }
/*
 * Close the input unit.
 */
    fclose(fpr);
/*
 * INITIALIZE STUFF NEEDED BY CPMPXY. -----------------------------------
 *
 * First, find the minimum and maximum values in the XP and YP arrays.
 */
    xpmn=xp[0][0];
    xpmx=xp[0][0];
    ypmn=yp[0][0];
    ypmx=yp[0][0];

    for( i = 0; i < 129; i++ ) {
        for( j = 0; j < 97; j++ ) {
            xpmn=min(xpmn,xp[j][i]);
            xpmx=max(xpmx,xp[j][i]);
            ypmn=min(ypmn,yp[j][i]);
            ypmx=max(ypmx,yp[j][i]);
        }
    }

    printf("  minimum x coordinate: %g\n",xpmn);
    printf("  maximum x coordinate: %g\n",xpmx);
    printf("  minimum y coordinate: %g\n",ypmn);
    printf("  maximum y coordinate: %g\n",ypmx);
/*
 * Initialize the indices that cpmpxy uses to search for the box that a
 * point to be inverse-transformed lies in.
 */
    ibeg=1;
    iend=129;
    jbeg=1;
    jend=97;
/*
 * Fill the array INDX.  Each combination of indices "IN,JN", where IN
 * is between 1 and MIND and JN is between 1 and NIND, inclusive, is
 * associated with a particular rectangular portion of the user system,
 * (call it R(IN,JN) - itself a subrectangle of the rectangle bounded
 * by the lines X=XPMN, X=XPMX, Y=YPMN, and Y=YPMX); INDX(IN,JN,1) and
 * INDX(IN,JN,2) specify a range of values of IP and INDX(IN,JN,3) and
 * INDX(IN,JN,4) specify a range of values of JP such that, if a point
 * is in R(IN,JN), then it is either in the union of the mapped grid
 * boxes for values of IP and JP in those ranges or it is not in the
 * mapped grid at all.  Thus, to find the mapped grid box containing a
 * particular point, we first find out what R(IN,JN) it's in and then
 * examine all the grid boxes specified by the entries in INDX(IN,JN,.).
 *
 * The initial values are such as to specify an illegal portion of the
 * grid.  (The specified ranges of IP and JP are in the wrong order.)
 */
    for( i = 0; i < MIND; i++ ) {
        for( j = 0; j < NIND; j++ ) {
            indx[i][j][0] = 129;
            indx[i][j][1] = 1;
            indx[i][j][2] = 97;
            indx[i][j][3] = 1;
        }
    }
/*
 * Examine each mapped grid box and use its bounding box to update the
 * contents of INDX.
 */
    for( i = 0; i < 128; i++ ) {
        for( j = 0; j < 96; j++ ) {
            xmin=min(xp[j][i],min(xp[j+1][i],min(xp[j][i+1],xp[j+1][i+1])));
            xmax=max(xp[j][i],max(xp[j+1][i],max(xp[j][i+1],xp[j+1][i+1])));
            ymin=min(yp[j][i],min(yp[j+1][i],min(yp[j][i+1],yp[j+1][i+1])));
            ymax=max(yp[j][i],max(yp[j+1][i],max(yp[j][i+1],yp[j+1][i+1])));
            inmn=max(1,min(MIND,1+(int)(((xmin-xpmn)/(xpmx-xpmn))*(float)(MIND))));;
            inmx=max(1,min(MIND,1+(int)(((xmax-xpmn)/(xpmx-xpmn))*(float)(MIND))));;
            jnmn=max(1,min(NIND,1+(int)(((ymin-ypmn)/(ypmx-ypmn))*(float)(NIND))));;
            jnmx=max(1,min(NIND,1+(int)(((ymax-ypmn)/(ypmx-ypmn))*(float)(NIND))));;
            for( in = inmn; in <= inmx; in++ ) {
                for( jn = jnmn; jn <= jnmx; jn++ ) {
                    indx[in-1][jn-1][0]=min(indx[in-1][jn-1][0],i+1);
                    indx[in-1][jn-1][1]=max(indx[in-1][jn-1][1],i+2);
                    indx[in-1][jn-1][2]=min(indx[in-1][jn-1][2],j+1);
                    indx[in-1][jn-1][3]=max(indx[in-1][jn-1][3],j+2);
                }
            }
        }
    }
/*
 * Set the flag isod, which says whether to use single precision or
 * double precision arithmetic.  we do this by calling a subroutine to
 * generate and return the three real numbers 1, 1+1E-10, and 1+2E-10.
 * If all three of those numbers appear to be different, we can use
 * single precision arithmetic and we set ISOD = 0; otherwise, we should
 * use double precision arithmetic, so we set ISOD = 1.  (The subroutine
 * call is necessary to fool some compilers into storing TST1, TST2, and
 * TST3; otherwise, real precision may be used on machines on which
 * double precision is actually necessary.)
 */

    gettrn (&tst1,&tst2,&tst3);

    if (tst1 != tst2 && tst2 != tst3) {
        isod=0;
        printf("\nusing single precision arithmetic\n");
    }
    else {
        isod=1;
        printf("\nusing double precision arithmetic\n");
    }
/*
 * Test CPMPXY TO SEE IF IT'S WORKING ALL RIGHT. ------------------------
 *
 * First, just do a single point and print out all values involved.
 */
    printf("\nSIMPLE TEST:" );
    xgrd=13.56;
    ygrd=17.29;

    imap = 3;
    NGCALLF(cpmpxy,CPMPXY) (&imap,&xgrd,&ygrd,&xusr,&yusr);
    printf("  forward transform:\n");
    printf("    xgrd = %g\n",xgrd );
    printf("    ygrd = %g\n",ygrd);
    printf("    xusr = %g\n",xusr);
    printf("    yusr = %g\n",yusr);

    imap = -3;
    NGCALLF(cpmpxy,CPMPXY) (&imap,&xusr,&yusr,&xgrd,&ygrd);
    printf("  reverse transform:\n");
    printf("    xusr = %g\n",xusr);
    printf("    yusr = %g\n",yusr);
    printf("    xgrd = %g\n",xgrd);
    printf("    ygrd = %g\n",ygrd);
/*
 * next, do a bunch of points inside a single grid box and see what
 * the largest error looks like.
 */
    printf("\nmore complicated test:\n");

    eavg=0.;
    emax=0.;

    for( i=1; i <= 101; i++ ) {
        xgrd=13.+(float)(i-1)/100.;
        for( j=1; j <= 101; j++ ) {
            ygrd=17.+(float)(j-1)/100.;
            imap = 3;
            NGCALLF(cpmpxy,CPMPXY) (&imap,&xgrd,&ygrd,&xusr,&yusr);
            imap = -3;
            NGCALLF(cpmpxy,CPMPXY) (&imap,&xusr,&yusr,&xgbk,&ygbk);
            eror=sqrt(pow((xgbk-xgrd),2.)+pow((ygbk-ygrd),2.));
            eavg=eavg+eror;
            emax=max(emax,eror);
        }
    }

    eavg=eavg/10201.;

    printf("  eavg = %g\n",eavg);
    printf("  emax = %g\n",emax);
/*
 * Frame 1:  DRAW THE TRANSFORMED GRID. ---------------------------------
 *
 * Label the frame.
 */
    printf("\nGENERATING FRAME 1, SHOWING THE MAPPED GRID\n");

    c_plchhq (c_cfux(.50),c_cfuy(.98),"ARBITRARILY TRANSFORMED GRID",.015,0.,0.);
    c_plchhq (c_cfux(.50),c_cfuy(.950),"X and Y coordinate arrays, each dimensioned 129x97, specify the shape of this irregular grid.",.012,0.,0.);
    c_plchhq (c_cfux(.50),c_cfuy(.926),"In the next two frames, we will show two different ways to fill this grid with color.",.012,0.,0.);
    c_plchhq (c_cfux(.50),c_cfuy(.062),"For more information, see the comments in the code for this example.",.012,0.,0.);
/*
 * Draw the transformed vertical lines of the grid.
 */
    for( i = 0; i < 129; i++ ) {
        c_frstpt (xp[0][i],yp[0][i]);
        for( j = 1; j < 97; j++ ) {
            c_vector (xp[j][i],yp[j][i]);
        }
    }
/*
 * draw the transformed horizontal lines of the grid.
 */
        for( j = 0; j < 97; j++ ) {
            c_frstpt (xp[j][0],yp[j][0]);
            for( i = 1; i < 129; i++ ) {
                c_vector (xp[j][i],yp[j][i]);
            }
        }
/*
 * Dump out anything that's left in the buffer.
 */
    c_plotif (0.,0.,2);
/*
 * Advance the frame.
 */
    c_frame();
/*
 * FRAME 2:  CALL GFA TO DRAW A PICTURE OF THE GRID. --------------------
 *
 * Label the frame.
 */
    printf("\nGENERATING FRAME 2, USING FILL AREAS\n");

    c_plchhq (c_cfux(.50),c_cfuy(.98),"FILLED AREAS EXACTLY COVERING GRID",.015,0.,0.);

    c_plchhq (c_cfux(.50),c_cfuy(.950),"Here we use the routine GFA to fill the grid, using color indices from a 129x97 array.",.012,0.,0.);
    c_plchhq (c_cfux(.50),c_cfuy(.926),"Note that the total filled area (outlined in red) is exactly the area occupied by the grid.",.012,0.,0.);
    c_plchhq (c_cfux(.50),c_cfuy(.086),"This frame's metafile contains 375,840 bytes; a cell-array version (next) contains 306,720 bytes.",.012,0.,0.);
    c_plchhq (c_cfux(.50),c_cfuy(.062),"For more information, see the comments in the code for this example.",.012,0.,0.);
/*
 * Do the GFA calls.  Note that, because the color specified by ICLR(I,J)
 * is thought of as being associated with the point (XP(I,J),YP(I,J)), we
 * fill boxes centered at the grid points (except along the edges), which
 * is a little different from filling the grid boxes themselves.
 */
    area.num_points = 4;
    area.points = (Gpoint *) malloc(area.num_points*sizeof(Gpoint));
    if( !area.points ) {
        fprintf( stderr, "c_cpex14: Not enough memory to create fill area structure\n" );
        gemergency_close_gks();
        exit(1);
    }
    imap = 3;
    for( i = 1; i <= 129; i++ ) {
      for( j = 1; j <= 97; j++ ) {
        x = max(1.,min(129.,(float)(i)-.5));
        y = max(1.,min( 97.,(float)(j)-.5));
        NGCALLF(cpmpxy,CPMPXY) (&imap,&x,&y,&area.points[0].x,&area.points[0].y);
        x = max(1.,min(129.,(float)(i)+.5));
        y = max(1.,min( 97.,(float)(j)-.5));
        NGCALLF(cpmpxy,CPMPXY) (&imap,&x,&y,&area.points[1].x,&area.points[1].y);
        x = max(1.,min(129.,(float)(i)+.5));
        y = max(1.,min( 97.,(float)(j)+.5));
        NGCALLF(cpmpxy,CPMPXY) (&imap,&x,&y,&area.points[2].x,&area.points[2].y);
        x = max(1.,min(129.,(float)(i)-.5));
        y = max(1.,min( 97.,(float)(j)+.5));
        NGCALLF(cpmpxy,CPMPXY) (&imap,&x,&y,&area.points[3].x,&area.points[3].y);
        gset_fill_colr_ind (iclr[j-1][i-1]);
        gfill_area(&area);
      }
    }
    free(area.points);
/*
 * In red, outline the mapped grid.
 */
    c_plotif (0.,0.,2);
    gset_linewidth (2.);
    gset_line_colr_ind (199);
    c_frstpt (xp[0][0],yp[0][0]);

    for( i = 1; i < 129; i++ ) {
        c_vector (xp[0][i],yp[0][i]);
    }

    for( j = 1; j < 97; j++ ) {
        c_vector (xp[j][128],yp[j][128]);
    }

    for( i = 127; i >= 0; i-- ) {
        c_vector (xp[96][i],yp[96][i]);
    }

    for( j = 96; j >= 0; j-- ) {
        c_vector (xp[j][0],yp[j][0]);
    }

    c_plotif (0.,0.,2);
    gset_linewidth (1.);
    gset_line_colr_ind (1);
/*
 * Advance the frame.
 */
    c_frame();
/*
 * FRAME 3:  USE GCA TO DRAW A PICTURE OF THE GRID. ---------------------
 *
 * Label the frame.
 */
    printf("\nGENERATING FRAME 3, USING A CELL ARRAY\n");

    c_plchhq (c_cfux(.50),c_cfuy(.98),"CELL ARRAY FILLING BOUNDING BOX OF GRID",.015,0.,0.);

    c_plchhq (c_cfux(.50),c_cfuy(.950),"Here we use the routine GCA to fill the bounding box of the grid (the area outlined in red).",.012,0.,0.);
    c_plchhq (c_cfux(.50),c_cfuy(.926),"To do this, we need a routine that does the inverse mapping, from user coordinates to grid coordinates.",.012,0.,0.);
    c_plchhq (c_cfux(.50),c_cfuy(.902),"Such a routine must be constructed carefully in order to run in less than geologic time.",.012,0.,0.);
    c_plchhq (c_cfux(.50),c_cfuy(.110),"Note that the cell array used here could have been built using the CONPACK routine CPCICA.",.012,0.,0.);
    c_plchhq (c_cfux(.50),c_cfuy(.086),"This frame's metafile contains 306,720 bytes; the fill-area version (last) contained 375,840 bytes.",.012,0.,0.);
    c_plchhq (c_cfux(.50),c_cfuy(.062),"For more information, see the comments in the code for this example.",.012,0.,0.);
/*
 * We generate a cell array covering the area occupied by the bounding
 * box of the transformed grid and dump it out with a single call to GCA.
 * Note that we could do this using the CONPACK routine CPCICA; we do it
 * this way instead for the sake of clarity and because our data is
 * already an integer array of color indices.
 *
 * At each box in a rectangular grid of boxes, we must figure out where
 * that box is relative to the transformed data grid, determine a color
 * index for the box, and stuff it into the cell array.
 */
    icra.colr_array = (Gint *)malloc(MCRA*NCRA*sizeof(Gint));
    icra.dims.size_x = MCRA;
    icra.dims.size_y = NCRA;
    imap = -3;
    for( i = 1; i <= MCRA; i++ ) {
        ibeg=1;
        iend=129;
        jbeg=1;
        jend=97;
        xusr=xpmn+(xpmx-xpmn)*(((float)(i)-.5)/(float)(MCRA));
        for( j = 1; j <= NCRA; j++ ) {
            l1 = (i-1)*NCRA + j-1;
            yusr=ypmn+(ypmx-ypmn)*(((float)(j)-.5)/(float)(NCRA));
            NGCALLF(cpmpxy,CPMPXY) (&imap,&xusr,&yusr,&xdat,&ydat);
            if (xdat > .5 && xdat < 129.5 && ydat > .5 && ydat <  97.5) {
                icra.colr_array[l1]=iclr[(int)nint(ydat)-1][(int)nint(xdat)-1];
            }
            else {
                icra.colr_array[l1]=0;
            }
        }
    }
/*
 * Dump out the cell array.
 */
    rect.p.x = xpmn;
    rect.p.y = ypmn;
    rect.q.x = xpmx;
    rect.q.y = ypmx;
    gcell_array (&rect,&icra);
/*
 * In red, outline the bounding box.
 */
    c_plotif (0.,0.,2);
    gset_linewidth (2.);
    gset_line_colr_ind (199);
    c_frstpt (xpmn,ypmn);
    c_vector (xpmx,ypmn);
    c_vector (xpmx,ypmx);
    c_vector (xpmn,ypmx);
    c_vector (xpmn,ypmn);
    c_plotif (0.,0.,2);
    gset_linewidth (1.);
    gset_line_colr_ind (1);
/*
 * Advance the frame.
 */
    c_frame();
/*
 * FRAME 4:  USE CONPACK TO PUT CONTOURS OVER THE CELL ARRAY. -----------
 *
 * Label the frame.
 */
    printf("\nGENERATING FRAME 4, A CELL ARRAY PLUS CONTOURS\n");

    c_plchhq (c_cfux(.50),c_cfuy(.98),"CELL ARRAY IN BOUNDING BOX, PLUS CONTOURS",.015,0.,0.);

    c_plchhq (c_cfux(.50),c_cfuy(.950),"This frame is just like the last one, except that we use CONPACK to contour the data field.",.012,0.,0.);
    c_plchhq (c_cfux(.50),c_cfuy(.926),"The contours are drawn in black over the filled area produced by the call to GCA.",.012,0.,0.);
    c_plchhq (c_cfux(.50),c_cfuy(.902),"What is contoured is just the field of color indices, and the label values reflect this.",.012,0.,0.);
    c_plchhq (c_cfux(.50),c_cfuy(.062),"For more information, see the comments in the code for this example.",.012,0.,0.);
/*
 * Dump out the cell array again.
 */
    rect.p.x = xpmn;
    rect.p.y = ypmn;
    rect.q.x = xpmx;
    rect.q.y = ypmx;
    gcell_array (&rect,&icra);
/*
 * In red, outline the bounding box.
 */
    c_plotif (0.,0.,2);
    gset_linewidth (2.);
    gset_line_colr_ind (199);
    c_frstpt (xpmn,ypmn);
    c_vector (xpmx,ypmn);
    c_vector (xpmx,ypmx);
    c_vector (xpmn,ypmx);
    c_vector (xpmn,ypmn);
    c_plotif (0.,0.,2);
    gset_linewidth (1.);
    gset_line_colr_ind (1);
/*
 * Move the integer data from ICLR to the real array RCLR (for delivery
 * to CONPACK).  Note that the real data overlay the integer data in
 * memory, so that ICLR becomes undefined.
 */
    for( i = 0; i < 129; i++ ) {
        for( j = 0; j < 97; j++ ) {
            rclr[j][i]=(float)iclr[j][i];
        }
    }
/*
 * Tell conpack not to do its own set call.
 */
    c_cpseti ("SET - DO-SET-CALL FLAG",0);
/*
 * Tell CONPACK which mapping to use and what the out-of-range value
 * is for that mapping.
 */
    c_cpseti ("MAP - MAPPING FLAG",3);
    c_cpsetr ("ORV - OUT-OF-RANGE VALUE",1.e+12);
/*
 * Tell CONPACK to use DASHPACK.
 */
    c_cpseti ("DPU - DASH PATTERN USE FLAG",-3);
/*
 * Tell CONPACK to use twenty-five contour levels: 5, 10, 15, ... 125.
 */
    c_cpseti ("CLS - CONTOUR LEVEL SELECTION",0);
    c_cpseti ("NCL - NUMBER OF CONTOUR LEVELS",25);

    for( i = 1; i <= 25; i++ ) {
        c_cpseti ("PAI - PARAMETER ARRAY INDEX",i);
        c_cpseti ("CLV - CONTOUR LEVEL",5*i);
        if (i % 2 == 0) {
            c_cpseti ("CLU - CONTOUR LEVEL USE FLAG",3);
            c_cpseti ("CLL - CONTOUR LEVEL USE FLAG",2);
        }
        else {
            c_cpseti ("CLU - CONTOUR LEVEL USE FLAG",1);
            c_cpseti ("CLL - CONTOUR LEVEL USE FLAG",0);
        }
        c_cpseti ("CLC - CONTOUR LINE COLOR INDEX",0);
    }
/*
 * Initialize CONPACK, telling it where the data array and the two work
 * arrays are.
 */
    c_cprect (&rclr[0][0],129,129,97,rwrk,LRWK,iwrk,LIWK);
/*
 * Draw the contour lines; the calls to PCSETI make the labels black.
 */
    c_pcseti ("CC",0);
    c_pcseti ("OC",0);
    c_cpcldr ((float *)rclr,rwrk,iwrk);
    c_pcseti ("CC",-1);
    c_pcseti ("OC",-1);
/*
 * Advance the frame.
 */
    c_frame();
/*
 * DONE.  CLOSE GKS AND QUIT. -------------------------------------------
 *
 * Close GKS.
 */
    gdeactivate_ws(iwkid);
    gclose_ws(iwkid);
    gclose_gks();

    printf("\n");
/*
 * Done.
 */
}

void NGCALLF(cpmpxy,CPMPXY)(imap,xinp,yinp,xout,yout)
int *imap;
float *xinp, *yinp, *xout, *yout;
{
    int in, jn, igrd, jgrd;
    int itry, jtry, ntry;
    float ss, sdos, st, slst, sfos, ss1, ss2, sxtm, sytm;
    extern float sdir();
/*
 * This version of CPMPXY implements what has sometimes, in the past,
 * been called a "parameterized distortion".  If imap has the value 3,
 * then, for given input values of XINP and YINP, where XINP lies in
 * the interval (1.,129.) and YINP lies in the interval (1.,97.),
 * CPMPXY generates output values of XOUT and YOUT, representing the
 * transformed position of a point on a 129x97 grid of data.
 *
 * The arrays XP and YP are passed in from the main program.  Each
 * (XP(I,J),YP(I,J)) represents a position associated with the grid
 * point (I,J).  XPMN, XPMX, YPMN, and YPMX are the computed minimum
 * and maximum values in XP and YP.  The variables IBEG, IEND, JBEG,
 * and JEND keep track of what box the last point inverse-transformed
 * turned out to be in.  The array INDX is used to determine quickly
 * approximately what part of the mapped grid contains a given point.
 * ISOD is a flag that says whether to use single-precision or
 * double-precision arithmetic.  The calling program is responsible
 * for initializing all the values in this array properly.
 *
 * Declare some required double precision variables.
 */
    extern double ddir1();
    double ddos,deps,dfos,dlst,ds,ds1,ds2,dt,dx1,dx2,dx3,dxtm,dy1,dy2,dy3,dytm;
/*
 * set the value of "epsilons" used to test for convergence of a
 * Newton iteration and/or binary search loop below.
 */
    float seps;

    seps = 1.e-8;
    deps = 0.00000001;
/*
 * Do it.  If imap = 0, the caller wants information about the mapping
 * specified by INT(XINP).  Returning YINP = 3 says that both forward
 * and inverse mappings are implemented, while returning YINP = 0 says
 * that neither is.
 */
    if (*imap == 0) {
        if ((int)(*xinp) == 3) {
            *yinp=3.;
        }
        else {
            *yinp=0.;
        }
    }
/*
 * If imap = 3, a forward "parameterized distortion" mapping is
 * requested.  The input value XINP is expected to be in the range
 * [1.,129.] and the input value YINP is expected to be in the range
 * [1.,97.].  If XINP and YINP are integers I and J, respectively,
 * then XOUT and YOUT should be XP(I,J) and YP(I,J), respectively;
 * if XINP and YINP are not integers, linear interpolation is done
 * to get the appropriate values of XOUT and YOUT.
 */
    else if (*imap == 3) {
/*
 * IGRD and JGRD are the indices of the lower left corner of the grid
 * box in which the input point falls.
 */
        igrd=max(1,min(128,(int)*xinp));
        jgrd=max(1,min( 96,(int)*yinp));
/*
 * We use either single precision or double precision arithmetic, as
 * appropriate.
 */
        if (isod == 0) {

            *xout = xp[jgrd-1][igrd-1]+
                  (xp[jgrd-1][igrd]-xp[jgrd-1][igrd-1])*
                  (*xinp-(float)igrd)+
                  (xp[jgrd][igrd-1]-xp[jgrd-1][igrd-1])*
                  (*yinp-(float)jgrd)+
                  (xp[jgrd][igrd]-xp[jgrd][igrd-1]-
                   xp[jgrd-1][igrd]+xp[jgrd-1][igrd-1])*
                  (*xinp-(float)igrd)*
                  (*yinp-(float)jgrd);

            *yout = yp[jgrd-1][igrd-1]+
                  (yp[jgrd-1][igrd]-yp[jgrd-1][igrd-1])*
                  (*xinp-(float)igrd)+
                  (yp[jgrd][igrd-1]-yp[jgrd-1][igrd-1])*
                  (*yinp-(float)jgrd)+
                  (yp[jgrd][igrd]-yp[jgrd][igrd-1]-
                   yp[jgrd-1][igrd]+yp[jgrd-1][igrd-1])*
                  (*xinp-(float)igrd)*
                  (*yinp-(float)jgrd);
        }
        else {

            *xout=(float)((double)(xp[jgrd-1][igrd-1])+
                  (double)(xp[jgrd-1][igrd]-xp[jgrd-1][igrd-1])*
                  (double)(*xinp-(float)igrd)+
                  (double)(xp[jgrd][igrd-1]-xp[jgrd-1][igrd-1])*
                  (double)(*yinp-(float)jgrd)+
                  (double)(xp[jgrd][igrd]-xp[jgrd][igrd-1]-
                  xp[jgrd-1][igrd]+xp[jgrd-1][igrd-1])*
                  (double)(*xinp-(float)igrd)*
                  (double)(*yinp-(float)jgrd));

            *yout=(float)((double)(yp[jgrd-1][igrd-1])+
                  (double)(yp[jgrd-1][igrd]-yp[jgrd-1][igrd-1])*
                  (double)(*xinp-(float)igrd)+
                  (double)(yp[jgrd][igrd-1]-yp[jgrd-1][igrd-1])*
                  (double)(*yinp-(float)jgrd)+
                  (double)(yp[jgrd][igrd]-yp[jgrd][igrd-1]-
                  yp[jgrd-1][igrd]+yp[jgrd-1][igrd-1])*
                  (double)(*xinp-(float)igrd)*
                  (double)(*yinp-(float)jgrd));
        }
/*
 * If imap = -3, the inverse of the "parameterized distortion" is
 * requested.  This is somewhat difficult.  We first have to find out
 * what mapped grid box (XINP,YINP) is in.
 */
    }
    else if (*imap == -3) {
/*
 * Jump if (XINP,YINP) is in the same mapped grid box as the last point
 * was.
 */
        if (iend == ibeg+1 && jend == jbeg+1) {
            if (isinpo(*xinp,*yinp,ibeg,iend,jbeg,jend,isod) != 0) goto start2;
        }
/*
 * It isn't; retrieve starting values of IBEG, IEND, JBEG, and JEND from
 * the index array INDX.
 */
        in=max(1,min(MIND,1+(int)(((*xinp-xpmn)/(xpmx-xpmn))*(float)(MIND))));
        jn=max(1,min(NIND,1+(int)(((*yinp-ypmn)/(ypmx-ypmn))*(float)(NIND))));
        
        ibeg=indx[in-1][jn-1][0];
        iend=indx[in-1][jn-1][1];
        jbeg=indx[in-1][jn-1][2];
        jend=indx[in-1][jn-1][3];
/*
 * If the indices imply an illegal portion of the mapped grid, return
 * out-of-range values.
 */
        if (iend <= ibeg || jend <= jbeg) {
            *xout=1.e12;
            *yout=1.e12;
            return;
        }
/*
 * If the point is outside the specified portion of the mapped grid,
 * return out-of-range values.
 */
        if (isinpo(*xinp,*yinp,ibeg,iend,jbeg,jend,isod) == 0) {
            *xout=1.e12;
            *yout=1.e12;
            return;
        }
/*
 * Zoom in until we find a single mapped grid box containing (XINP,YINP).
 */
start1:
        if (iend != ibeg+1 || jend != jbeg+1) {
            if (iend-ibeg > jend-jbeg) {
                itry=(ibeg+iend)/2;
                if (isinpo(*xinp,*yinp,ibeg,itry,jbeg,jend,isod) == 0) {
                    ibeg=itry;
                }
                else {
                    iend=itry;
                }
            }
            else {
                jtry=(jbeg+jend)/2;
                if (isinpo(*xinp,*yinp,ibeg,iend,jbeg,jtry,isod) == 0) {
                    jbeg=jtry;
                }
                else {
                    jend=jtry;
                }
            }
            goto start1;
        }
/*
 * Compute fractional coordinates within the grid box.  This is done with
 * an iterative procedure, rather than an algebraic solution, because of
 * the surprising number of cases that the latter solution gives rise to,
 * many of them seemingly pathological.  We first attempt to use Newton's
 * rule to come up with a solution and, if that doesn't work, we use a
 * binary search technique.
 */
start2:
        if (isod == 0) {

            ss=.5;

            ntry=0;

start3:
            ntry=ntry+1;
            if (ntry > 10) goto start4;
            sfos=sdir((1.-ss)*xp[jbeg][ibeg-1]+ss*xp[jbeg][ibeg],
                      (1.-ss)*yp[jbeg][ibeg-1]+ss*yp[jbeg][ibeg],
                      (1.-ss)*xp[jbeg-1][ibeg-1]+ss*xp[jbeg-1][ibeg],
                      (1.-ss)*yp[jbeg-1][ibeg-1]+ss*yp[jbeg-1][ibeg],
                      *xinp                                        ,
                      *yinp                                        );
            sdos=2.*((xp[jbeg][ibeg]-xp[jbeg][ibeg-1])*
                     (yp[jbeg-1][ibeg-1]-yp[jbeg-1][ibeg])-
                     (xp[jbeg-1][ibeg]-xp[jbeg-1][ibeg-1])*
                     (yp[jbeg][ibeg-1]-yp[jbeg][ibeg]))*ss+
                     (xp[jbeg][ibeg-1]-*xinp             )*
                     (yp[jbeg-1][ibeg-1]-yp[jbeg-1][ibeg])+
                     (xp[jbeg][ibeg]-xp[jbeg][ibeg-1])*
                     (*yinp             -yp[jbeg-1][ibeg-1])-
                     (xp[jbeg-1][ibeg-1]-*xinp             )*
                     (yp[jbeg][ibeg-1]-yp[jbeg][ibeg])-
                     (xp[jbeg-1][ibeg]-xp[jbeg-1][ibeg-1])*
                     (*yinp             -yp[jbeg][ibeg-1]);
            if (sdos == 0.) goto start4;
            slst=ss;
            ss=ss-sfos/sdos;
            if (abs(ss-slst) < seps) {
                if (ss < -seps || ss > 1.+seps) goto start4;
                goto start6;
            }
            goto start3;
        
start4:
            ss1=0.;
            ss2=1.;

start5:
            ss=.5*(ss1+ss2);

            if (sdir((1.-ss)*xp[jbeg][ibeg-1]+ss*xp[jbeg][ibeg],
                     (1.-ss)*yp[jbeg][ibeg-1]+ss*yp[jbeg][ibeg],
                     (1.-ss)*xp[jbeg-1][ibeg-1]+ss*xp[jbeg-1][ibeg],
                     (1.-ss)*yp[jbeg-1][ibeg-1]+ss*yp[jbeg-1][ibeg],
                     *xinp                                        ,
                     *yinp                                        )
                < 0.) {
                ss1=ss;
            }
            else {
                ss2=ss;
            }

            if (abs(ss2-ss1) > seps) goto start5;

start6:
            sxtm=(1.-ss)*(xp[jbeg][ibeg-1]-xp[jbeg-1][ibeg-1])+
              ss *(xp[jbeg][ibeg]-xp[jbeg-1][ibeg]);
        
            sytm=(1.-ss)*(yp[jbeg][ibeg-1]-yp[jbeg-1][ibeg-1])+
              ss *(yp[jbeg][ibeg]-yp[jbeg-1][ibeg]);
        
            if (abs(sxtm) > abs(sytm)) {
                st=(*xinp-(1.-ss)*xp[jbeg-1][ibeg-1]-ss*xp[jbeg-1][ibeg])/sxtm;
            }
            else {
                st=(*yinp-(1.-ss)*yp[jbeg-1][ibeg-1]-ss*yp[jbeg-1][ibeg])/sytm;
            }
        }
        else {
        
            ds=.5;

            ntry=0;

start23:
            ntry=ntry+1;
            if (ntry > 10) goto start24;
            dfos=ddir1((1.0-ds)*(double)(xp[jbeg][ibeg-1])+
                       ds *(double)(xp[jbeg][ibeg]),
                       (1.0-ds)*(double)(yp[jbeg][ibeg-1])+
                       ds *(double)(yp[jbeg][ibeg]),
                       (1.0-ds)*(double)(xp[jbeg-1][ibeg-1])+
                       ds *(double)(xp[jbeg-1][ibeg]),
                       (1.0-ds)*(double)(yp[jbeg-1][ibeg-1])+
                       ds *(double)(yp[jbeg-1][ibeg]),
                       (double)(*xinp)             ,
                       (double)(*yinp)             );
            ddos=2.0*((double)(xp[jbeg][ibeg]-xp[jbeg][ibeg-1])*
                       (double)(yp[jbeg-1][ibeg-1]-yp[jbeg-1][ibeg])-
                       (double)(xp[jbeg-1][ibeg]-xp[jbeg-1][ibeg-1])*
                       (double)(yp[jbeg][ibeg-1]-yp[jbeg][ibeg]))*ds+
                       (double)(xp[jbeg][ibeg-1]-*xinp             )*
                       (double)(yp[jbeg-1][ibeg-1]-yp[jbeg-1][ibeg])+
                       (double)(xp[jbeg][ibeg]-xp[jbeg][ibeg-1])*
                       (double)(*yinp             -yp[jbeg-1][ibeg-1])-
                       (double)(xp[jbeg-1][ibeg-1]-*xinp             )*
                       (double)(yp[jbeg][ibeg-1]-yp[jbeg][ibeg])-
                       (double)(xp[jbeg-1][ibeg]-xp[jbeg-1][ibeg-1])*
                       (double)(*yinp             -yp[jbeg][ibeg-1]);
            if (ddos == 0.0) goto start24;
            dlst=ds;
            ds=ds-dfos/ddos;
            if (fabs(ds-dlst) < deps) {
                if (ds < -deps || ds > 1.0+deps) goto start24;
                goto start26;
            }
            goto start23;
              
start24:
            ds1=0.;
            ds2=1.;

start25:
            ds=.50*(ds1+ds2);

            if (ddir1((1.0-ds)*(double)(xp[jbeg][ibeg-1])+
                      ds *(double)(xp[jbeg][ibeg]),
                      (1.0-ds)*(double)(yp[jbeg][ibeg-1])+
                      ds *(double)(yp[jbeg][ibeg]),
                      (1.0-ds)*(double)(xp[jbeg-1][ibeg-1])+
                      ds *(double)(xp[jbeg-1][ibeg]),
                      (1.0-ds)*(double)(yp[jbeg-1][ibeg-1])+
                      ds *(double)(yp[jbeg-1][ibeg]),
                      (double)(*xinp)             ,
                      (double)(*yinp)             ) < 0.0) {
                ds1=ds;
            }
            else {
                ds2=ds;
            }

            if (abs(ds2-ds1) > deps) goto start25;

start26:
            dxtm=(1.0-ds)*(double)(xp[jbeg][ibeg-1]-xp[jbeg-1][ibeg-1])+
              ds *(double)(xp[jbeg][ibeg]-xp[jbeg-1][ibeg]);
        
            dytm=(1.0-ds)*(double)(yp[jbeg][ibeg-1]-yp[jbeg-1][ibeg-1])+
              ds *(double)(yp[jbeg][ibeg]-yp[jbeg-1][ibeg]);
        
            if (abs(dxtm) > abs(dytm)) {
                dt=((double)(*xinp)-(1.0-ds)*(double)(xp[jbeg-1][ibeg-1])-
                    ds* (double)(xp[jbeg-1][ibeg]))/dxtm;
            }
            else {
                dt=((double)(*yinp)-(1.0-ds)*(double)(yp[jbeg-1][ibeg-1])-
                    ds* (double)(yp[jbeg-1][ibeg]))/dytm;
            }

            ss=(float)(ds);
            st=(float)(dt);
        }
/*
 * output values of X and Y are the fractional values added to the
 * indices of the lower left corner of the grid box.
 */
        *xout=(float)(ibeg)+max(0.,min(1.,ss));
        *yout=(float)(jbeg)+max(0.,min(1.,st));
/*
 * If imap is anything else, log an error.  if error recovery is on,
 * control will return to caller; otherwise, SETER will clobber the run.
 */
    }
    else {
        c_seter ("CPMPXY - UNKNOWN MAPPING",5,1);
    }
/*
 * Done.
 */
    return;

}

int isinpo(xpnt,ypnt,ibeg,iend,jbeg,jend,isod)
float xpnt, ypnt;
int ibeg, iend, jbeg,jend,isod;
{
    int imin, jmin, ipmn, inxt, jnxt, ipnt,ilst,jlst;
    float sdmn, sdst, slst, sbnd, snxt;
    extern float sdir();
/*
 * The value of this function is intended to be greater than zero if and
 * only if the point (XPNT,YPNT) is inside the polygon defined by the
 * points (XP(I,J),YP(I,J)), for "I,J" = "IBEG,JBEG", "IBEG+1,JBEG",
 * ... "IEND,JBEG", "IEND,JBEG+1", ... "IEND,JEND", "IEND-1,JEND", ...
 * "IBEG,JEND", "IBEG,JEND-1", ... "IBEG,JBEG".  If the function value
 * is zero, the point is outside the polygon; if it is negative, some
 * error has occurred.
 *
 * It is assumed that, if we trace the polygon in a counterclockwise
 * direction in grid space, we also trace the transformed polygon in a
 * counterclockwise direction.
 *
 * We find the point of the polygon that (XPNT,YPNT) is closest to and
 * then analyze the situation relative to the two segments of the polygon
 * that join at that point.
 *
 * Declare some required double precision variables.
 */
    extern double ddir2();
    double ddst,ddmn,dbnd,dlst,dnxt;
/*
 * Initialize the scan of the points comprising the polygon.
 */
    ipmn=1;
    imin=ibeg;
    jmin=jbeg;

    if (isod == 0) {
        sdmn=pow((xpnt-xp[jbeg-1][ibeg-1]),2.)+pow((ypnt-yp[jbeg-1][ibeg-1]),2.);
    }
    else {
        ddmn=(double)pow((xpnt-xp[jbeg-1][ibeg-1]),2.)+(double)pow((ypnt-yp[jbeg-1][ibeg-1]),2.);
    }

    inxt=ibeg;
    jnxt=jbeg;
/*
 * scan the points of the polygon.
 */
    if (isod == 0) {
        for( ipnt=2; ipnt <= iend-ibeg+jend-jbeg+iend-ibeg+jend-jbeg; ipnt++ ) {
            if      (ipnt <= 1+iend-ibeg) {
                inxt=inxt+1;
            }
            else if (ipnt <= 1+iend-ibeg+jend-jbeg) {
                jnxt=jnxt+1;
            }
            else if (ipnt <= 1+iend-ibeg+jend-jbeg+iend-ibeg) {
                inxt=inxt-1;
            }
            else {
                jnxt=jnxt-1;
            }
            sdst=pow((xpnt-xp[jnxt-1][inxt-1]),2.)+pow((ypnt-yp[jnxt-1][inxt-1]),2.);
            if (sdst < sdmn) {
                ipmn=ipnt;
                imin=inxt;
                jmin=jnxt;
                sdmn=sdst;
            }
        }
    }
    else {
        for( ipnt=2; ipnt <= iend-ibeg+jend-jbeg+iend-ibeg+jend-jbeg; ipnt++ ) {
            if      (ipnt <= 1+iend-ibeg) {
                inxt=inxt+1;
            }
            else if (ipnt <= 1+iend-ibeg+jend-jbeg) {
                jnxt=jnxt+1;
            }
            else if (ipnt <= 1+iend-ibeg+jend-jbeg+iend-ibeg) {
                inxt=inxt-1;
            }
            else {
                jnxt=jnxt-1;
            }
            ddst=(double)pow((xpnt-xp[jnxt-1][inxt-1]),2.)+(double)pow((ypnt-yp[jnxt-1][inxt-1]),2.);
            if (ddst < ddmn) {
                ipmn=ipnt;
                imin=inxt;
                jmin=jnxt;
                ddmn=ddst;
            }
        }
    }
/*
 * Get the indices of the previous point and of the next point.
 */
    if      (ipmn == 1) {
        ilst=imin;
        jlst=jmin+1;
        inxt=imin+1;
        jnxt=jmin;
    }
    else if (ipmn < 1+iend-ibeg) {
        ilst=imin-1;
        jlst=jmin;
        inxt=imin+1;
        jnxt=jmin;
    }
    else if (ipmn == 1+iend-ibeg) {
        ilst=imin-1;
        jlst=jmin;
        inxt=imin;
        jnxt=jmin+1;
    }
    else if (ipmn < 1+iend-ibeg+jend-jbeg) {
        ilst=imin;
        jlst=jmin-1;
        inxt=imin;
        jnxt=jmin+1;
    }
    else if (ipmn == 1+iend-ibeg+jend-jbeg) {
        ilst=imin;
        jlst=jmin-1;
        inxt=imin-1;
        jnxt=jmin;
    }
    else if (ipmn < 1+iend-ibeg+jend-jbeg+iend-ibeg) {
        ilst=imin+1;
        jlst=jmin;
        inxt=imin-1;
        jnxt=jmin;
    }
    else if (ipmn == 1+iend-ibeg+jend-jbeg+iend-ibeg) {
        ilst=imin+1;
        jlst=jmin;
        inxt=imin;
        jnxt=jmin-1;
    }
    else {
        ilst=imin;
        jlst=jmin+1;
        inxt=imin;
        jnxt=jmin-1;
    }
/*
 * If (XPNT,YPNT) is to the left of both segments, it's inside the
 * polygon.  If it's to the right of both segments, it's outside the
 * polygon.  If it's to the left of one and to the right of the other,
 * it's outside the polygon if the polygon edge bends to the left
 * where the segments join and inside the polygon if the polygon's
 * edge bends to the right there.
 */
        if (isod == 0) {
            slst=sdir(xp[jlst-1][ilst-1],yp[jlst-1][ilst-1],xp[jmin-1][imin-1],yp[jmin-1][imin-1],xpnt,ypnt);

            snxt=sdir(xp[jmin-1][imin-1],yp[jmin-1][imin-1],xp[jnxt-1][inxt-1],yp[jnxt-1][inxt-1],xpnt,ypnt);
            
            if      (slst <= 0. && snxt <= 0.) {
                return(1);
            }
            else if (slst >= 0. && snxt >= 0.) {
                return(0);
            }
            else {
                sbnd=sdir(xp[jlst-1][ilst-1],yp[jlst-1][ilst-1],xp[jmin-1][imin-1],yp[jmin-1][imin-1],xp[jnxt-1][inxt-1],yp[jnxt-1][inxt-1]);
                if (sbnd <= 0.) {
                    return(0);
                }
                else {
                    return(1);
                }
            }
        }
        else {
            dlst=ddir2(xp[jlst-1][ilst-1],yp[jlst-1][ilst-1],xp[jmin-1][imin-1],yp[jmin-1][imin-1],xpnt,ypnt);
            dnxt=ddir2(xp[jmin-1][imin-1],yp[jmin-1][imin-1],xp[jnxt-1][inxt-1],yp[jnxt-1][inxt-1],xpnt,ypnt);

            if      (dlst <= 0.0 && dnxt <= 0.0) {
                return(1);
            }
            else if (dlst >= 0.0 && dnxt >= 0.0) {
                return(0);
            }
            else {
                dbnd=ddir2(xp[jlst-1][ilst-1],yp[jlst-1][ilst-1],xp[jmin-1][imin-1],yp[jmin-1][imin-1],xp[jnxt-1][inxt-1],yp[jnxt-1][inxt-1]);
                if (dbnd <= 0.0) {
                    return(0);
                }
                else {
                    return(1);
                }
            }
        }
/*
 * done.
 */
    return(0);

}

extern void gettrn (tst1,tst2,tst3)
float *tst1, *tst2, *tst3;
{
/*
 * This subroutine generates and returns the three real numbers 1,
 * 1+1e-10, and 1+2e-10.  On a machine with 64-bit arithmetic, all
 * three of these numbers will be different from each other, while,
 * on machines with 32-bit arithmetic, two or more of them will be
 * equal.  Thus, we can use these numbers to determine whether we
 * should use single precision or double precision arithmetic.
 */
    *tst1 = 1.;
    *tst2 = *tst1+1.e-10;
    *tst3 = *tst2+1.e-10;
    return;
}
/*
 * The following arithmetic statement functions have a negative value
 * if and only if the point (X3,Y3) is to the left of the line from
 * (X1,Y1) to (X2,Y2).
 */

float sdir(x1,y1,x2,y2,x3,y3)
float x1,y1,x2,y2,x3,y3;
{
    return((x1-x3)*(y3-y2)-(x2-x3)*(y3-y1));
}

double ddir1(dx1,dy1,dx2,dy2,dx3,dy3)
double dx1,dy1,dx2,dy2,dx3,dy3;
{
    return((dx1-dx3)*(dy3-dy2)-(dx2-dx3)*(dy3-dy1));
}

double ddir2(x1,y1,x2,y2,x3,y3)
float x1,y1,x2,y2,x3,y3;
{
    return((double)(x1-x3)*(double)(y3-y2)-(double)(x2-x3)*(double)(y3-y1));
}



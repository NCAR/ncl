/*
 *  $Id: c_cbex01.c,v 1.4 2004-08-01 17:12:45 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define pow2(x)   ((x)*(x))

int icll, iama[10000];

#define IWTYPE 1
#define WKID   1

main()
{
/*
 * LATEST REVISION        September, 1989
 *
 *
 * PURPOSE                To provide a simple demonstration of the
 *                        use of BIVAR and CONPACK together as a
 *                        temporary replacement for CONRAN.
 *
 * PRECISION              Single.
 *
 * REQUIRED LIBRARY       BIVAR, CONPACK.
 * FILES
 *
 *
 * LANGUAGE               FORTRAN.
 * HISTORY                Written September, 1989, by Dave Kennison.
 * ALGORITHM              At each of nine points scattered "at random"
 *                        in a portion of the x/y plane, a mathematical
 *                        function is evaluated to obtain a value of z.
 *                        The resulting triplets approximately describe
 *                        the surface which is defined exactly by the
 *                        function.  The routine BIVAR is then called to
 *                        obtain an array of interpolated values of z on
 *                        a regular grid, approximating the same surface,
 *                        and this data is used as input to CONPACK to
 *                        draw two different contour plots.
 *                        On the first plot, contours are shown in a
 *                        rectangular area containing the x/y positions
 *                        of the original nine data points and those
 *                        positions are marked on the plot.
 *                        On the second plot, capabilities of CONPACK
 *                        and AREAS are used to limit the drawing of
 *                        contours to the convex hull of the original
 *                        nine x/y positions.
 */

/*
 * Define arrays to be used below.  XRAN, YRAN, and ZRAN are used for
 * the "random data".  XCNV and YCNV are used to define the convex hull
 * of the x/y positions of this data.  XDAT, YDAT, and ZDAT are used for
 * the regular grid of data returned by BIVAR.  IWRK and RWRK are used
 * as integer and real workspace arrays, both in calls to BIVAR and in
 * calls to CONPACK.
 */
    float xran[9],yran[9],zran[9],xcnv[7],ycnv[7];
    float xdat[11],ydat[12],zdat[12][11],rwrk[1000];
    float dumi[1][1], dumi2[1][1], xval1, yval1, xval2, yval2;
    int iwrk[1000];
    int i,j;
    extern void dfclrs();
/*
 * Define a temporary character variable for use below.
 */
    char ichr[2];
/*
 * Declare the routine which will draw contour lines, avoiding labels.
 */
    extern int NGCALLF(cpdrpl,CPDRPL)(
#ifdef NeedFuncProto
        float *xcra,
        float *ycra,
        int *ncra,
        int *iaia,
        int *igia,
        int *nagi
#endif
);
/*
 * Specify the X and Y input data values.
 */
    xran[0] = 0.4;    yran[0] = 5.4;
    xran[1] = 2.0;    yran[1] = 1.1;
    xran[2] = 2.8;    yran[2] = 3.7;
    xran[3] = 5.0;    yran[3] = 6.8;
    xran[4] = 4.8;    yran[4] = 9.5;
    xran[5] = 8.8;    yran[5] = 9.8;
    xran[6] = 5.1;    yran[6] = 0.7;
    xran[7] = 6.2;    yran[7] = 2.7;
    xran[8] = 9.1;    yran[8] = 3.0;
/*
 * Specify the Z input data values.
 */
    for( i =0; i < 9; i++ ) {
        zran[i]=exp(-pow2(xran[i]-3.)/9.-pow2(yran[i]-5.)/25.)-
                exp(-pow2(xran[i]-6.)/9.-pow2(yran[i]-5.)/25.);
    }
/*
 * Specify the points defining the convex hull.
 */
    xcnv[0]=xran[0];  ycnv[0]=yran[0];
    xcnv[1]=xran[1];  ycnv[1]=yran[1];  
    xcnv[2]=xran[6];  ycnv[2]=yran[6];  
    xcnv[3]=xran[8];  ycnv[3]=yran[8];
    xcnv[4]=xran[5];  ycnv[4]=yran[5];  
    xcnv[5]=xran[4];  ycnv[5]=yran[4];
    xcnv[6]=xran[0];  ycnv[6]=yran[0];
/*
 * Specify the X and Y coordinates of the points on the regular grid.
 */
    for(  i = 0; i < 11; i++ ) {
        xdat[i]=(float)i;
    }

    for(  j = 0; j < 12; j++ ) {
        ydat[j]=(float)j;
    }
/*
 * Call IDSFFT to obtain a regular grid of values on the fitted surface.
 */
    c_idsfft(1,9,xran,yran,zran,11,12,11,xdat,ydat,(float *)zdat,iwrk,rwrk);
/*
 * Open GKS.
 */
    gopen_gks("stdout",0);
    gopen_ws(WKID, NULL, IWTYPE);
    gactivate_ws(WKID);
/*
 * Turn off clipping.
 */
    gset_clip_ind (GIND_NO_CLIP);
/*
 * Define a set of colors to use.
 */
    dfclrs();
/*
 * Tell CONPACK to position labels using the "regular" scheme.
 */
    c_cpseti ("LLP - LINE LABEL POSITIONING",2);
/*
 * Tell CONPACK to put the informational label in a different place.
 */
    c_cpseti ("ILP - INFORMATIONAL LABEL POSITIONING",-2);
    c_cpsetr ("ILX - INFORMATIONAL LABEL X COORDINATE",.98);
    c_cpsetr ("ILY - INFORMATIONAL LABEL Y COORDINATE",.02);
/*
 * Provide a little more room below the viewport; otherwise, the labels
 * on AUTOGRAPH"s X axis get squashed.
 */
    c_cpsetr ("VPB - VIEWPORT BOTTOM EDGE",.08);
/*
 * Tell CONPACK in what ranges to generate X and Y coordinates.
 */
    c_cpsetr ("XC1 - X COORDINATE AT I=1",0.);
    c_cpsetr ("XCM - X COORDINATE AT I=M",10.);
    c_cpsetr ("YC1 - Y COORDINATE AT J=1",0.);
    c_cpsetr ("YCN - Y COORDINATE AT J=N",11.);
/*
 * Turn off the culling of labels by CPCHHL and CPCHLL.
 */
    icll=0;
/*
 * Dump the polyline buffer and change the polyline color to white.
 * Change the text color to orange.  CONPACK will use these colors.
 */
    c_plotif (0.,0.,2);
    gset_line_colr_ind (1);
    gset_text_colr_ind (12);
/*
 * Initialize the drawing of the first contour plot.
 */
    c_cprect (&zdat[0][0],11,11,12,rwrk,1000,iwrk,1000);
/*
 * Initialize the area map which will be used to keep contour lines from
 * passing through labels.
 */
    c_arinam (iama,10000);
/*
 * Put label boxes in the area map.
 */
    c_cplbam (&zdat[0][0],rwrk,iwrk,iama);
/*
 * Draw the contour lines, masked by the area map.
 */
    c_cpcldm (&zdat[0][0],rwrk,iwrk,iama,NGCALLF(cpdrpl,CPDRPL));
/*
 * Draw all the labels.
 */
    c_cplbdr (&zdat[0][0],rwrk,iwrk);
/*
 * Dump the polyline buffer and change the polyline color to orange.
 * Change the text color to orange, too, so that the AUTOGRAPH background
 * will come out entirely in that color.
 */
    c_plotif (0.,0.,2);
    gset_line_colr_ind (12);
    gset_text_colr_ind (12);
/*
 * Use AUTOGRAPH to produce a background for the contour plot, forcing
 * it to pick up appropriate values from CONPACK"s SET call.
 */
    c_agseti ("SET.",4);
    c_agstup (&dumi[0][0],1,1,1,1,&dumi2[0][0],1,1,1,1);
    c_agback();
/*
 * Dump the polyline buffer and change the polyline color to green.
 */
    c_plotif (0.,0.,2);
    gset_line_colr_ind (9);
/*
 * Change the aspect ratio of the characters drawn by PLCHMQ to make
 * them approximately square.
 */
    c_pcsetr ("HW",1.);
/*
 * At each of the "random" data positions, put the index of the point
 * and a starburst to set it off.
 */
    for( i = 0; i < 9; i++ ) {
        sprintf( ichr, "%d", i+1 );
        c_plchmq (xran[i],yran[i],ichr,.0175,0.,0.);
        xval1 = c_cfux(c_cufx(xran[i])-.02);
        yval1 = c_cfuy(c_cufy(yran[i])-.02);
        xval2 = c_cfux(c_cufx(xran[i])-.01);
        yval2 = c_cfuy(c_cufy(yran[i])-.01);
        c_line (xval1,yval1,xval2,yval2);
        xval1 = c_cfux(c_cufx(xran[i])+.01);
        yval1 = c_cfuy(c_cufy(yran[i])+.01);
        xval2 = c_cfux(c_cufx(xran[i])+.02);
        yval2 = c_cfuy(c_cufy(yran[i])+.02);
        c_line (xval1,yval1,xval2,yval2);
        xval1 = c_cfux(c_cufx(xran[i])-.02);
        yval1 = c_cfuy(c_cufy(yran[i])+.02);
        xval2 = c_cfux(c_cufx(xran[i])-.01);
        yval2 = c_cfuy(c_cufy(yran[i])+.01);
        c_line (xval1,yval1,xval2,yval2);
        xval1 = c_cfux(c_cufx(xran[i])+.01);
        yval1 = c_cfuy(c_cufy(yran[i])-.01);
        xval2 = c_cfux(c_cufx(xran[i])+.02);
        yval2 = c_cfuy(c_cufy(yran[i])-.02);
        c_line (xval1,yval1,xval2,yval2);
        xval1 = c_cfux(c_cufx(xran[i])-.02828);
        yval1 = c_cfuy(c_cufy(yran[i]));
        xval2 = c_cfux(c_cufx(xran[i])-.01414);
        yval2 = c_cfuy(c_cufy(yran[i]));
        c_line (xval1,yval1,xval2,yval2);
        xval1 = c_cfux(c_cufx(xran[i])+.01414);
        yval1 = c_cfuy(c_cufy(yran[i]));
        xval2 = c_cfux(c_cufx(xran[i])+.02828);
        yval2 = c_cfuy(c_cufy(yran[i]));
        c_line (xval1,yval1,xval2,yval2);
        xval1 = c_cfux(c_cufx(xran[i]));
        yval1 = c_cfuy(c_cufy(yran[i])-.02828);
        xval2 = c_cfux(c_cufx(xran[i]));
        yval2 = c_cfuy(c_cufy(yran[i])-.01414);
        c_line (xval1,yval1,xval2,yval2);
        xval1 = c_cfux(c_cufx(xran[i]));
        yval1 = c_cfuy(c_cufy(yran[i])+.01414);
        xval2 = c_cfux(c_cufx(xran[i]));
        yval2 = c_cfuy(c_cufy(yran[i])+.02828);
        c_line (xval1,yval1,xval2,yval2);
    }
/*
 * Dump the polyline buffer and switch the polyline color to yellow.
 */
    c_plotif (0.,0.,2);
    gset_line_colr_ind (11);
/*
 * Put a label at the top of the plot.
 */
    c_set (0.,1.,0.,1.,0.,1.,0.,1.,1);
    c_plchhq (.5,.975,"DEMONSTRATING THE USE OF BIVAR AND CONPACK",.012,0.,0.);
/*
 * Advance the frame.
 */
    c_frame();
/*
 * Do another frame.  First, turn on the culling of labels by the
 * routines CPCHHL and CPCHLL (which see, below).
 */
    icll=1;
/*
 * Dump the polyline buffer and switch the polyline color back to white.
 * Force the text color index to orange.
 */
    c_plotif (0.,0.,2);
    gset_line_colr_ind (1);
    gset_text_colr_ind (12);
/*
 * Initialize the drawing of the second contour plot.
 */
    c_cprect (&zdat[0][0],11,11,12,rwrk,1000,iwrk,1000);
/*
 * Initialize the area map.
 */
    c_arinam (iama,10000);
/*
 * Put the convex hull of the "random" data in the area map, using group
 * identifier 4, an area identifier of 0 inside the hull, and an area
 * identifier of -1 outside the hull.
 */
    c_aredam (iama,xcnv,ycnv,7,4,0,-1);
/*
 * Put label boxes in the area map.
 */
    c_cplbam (&zdat[0][0],rwrk,iwrk,iama);
/*
 * Draw contour lines, masked by the area map.
 */
    c_cpcldm (&zdat[0][0],rwrk,iwrk,iama,NGCALLF(cpdrpl,CPDRPL));
/*
 * Draw labels.
 */
    c_cplbdr (&zdat[0][0],rwrk,iwrk);
/*
 * Dump the polyline buffer and switch the polyline color to orange.
 */
    c_plotif (0.,0.,2);
    gset_line_colr_ind (12);
/*
 * Use AUTOGRAPH to draw a background.
 */
    c_agseti ("SET.",4);
    c_agstup (&dumi[0][0],1,1,1,1,&dumi2[0][0],1,1,1,1);
    c_agback();
/*
 * Dump the polyline buffer and switch the polyline color to yellow.
 */
    c_plotif (0.,0.,2);
    gset_line_colr_ind (11);
/*
 * Draw a label at the top of the plot.
 */
    c_set (0.,1.,0.,1.,0.,1.,0.,1.,1);
    c_plchhq (.5,.975,"DEMONSTRATING THE USE OF BIVAR AND CONPACK",.012,0.,0.);
/*
 * Advance the frame.
 */
    c_frame();
/*
 * Close GKS.
 */
    gdeactivate_ws(WKID);
    gclose_ws(WKID);
    gclose_gks();
}

void dfclrs()
{
/*
 * Define a set of RGB color triples for colors 1 through 15 on
 * workstation 1.
 */
    Gcolr_rep rgbv[15];
    int i;
/*
 * Define the RGB color triples needed below.
 */
    rgbv[1].rgb.red = 0.70; rgbv[1].rgb.green = 0.70; rgbv[1].rgb.blue = 0.70;
    rgbv[2].rgb.red = 0.75; rgbv[2].rgb.green = 0.50; rgbv[2].rgb.blue = 1.00;
    rgbv[3].rgb.red = 0.50; rgbv[3].rgb.green = 0.00; rgbv[3].rgb.blue = 1.00;
    rgbv[4].rgb.red = 0.00; rgbv[4].rgb.green = 0.00; rgbv[4].rgb.blue = 1.00;
    rgbv[5].rgb.red = 0.00; rgbv[5].rgb.green = 0.50; rgbv[5].rgb.blue = 1.00;
    rgbv[6].rgb.red = 0.00; rgbv[6].rgb.green = 1.00; rgbv[6].rgb.blue = 1.00;
    rgbv[7].rgb.red = 0.00; rgbv[7].rgb.green = 1.00; rgbv[7].rgb.blue = 0.60;
    rgbv[8].rgb.red = 0.00; rgbv[8].rgb.green = 1.00; rgbv[8].rgb.blue = 0.00;
    rgbv[9].rgb.red = 0.70; rgbv[9].rgb.green = 1.00; rgbv[9].rgb.blue = 0.00;
    rgbv[10].rgb.red = 1.00; rgbv[10].rgb.green = 1.00; rgbv[10].rgb.blue = 0.00;
    rgbv[11].rgb.red = 1.00; rgbv[11].rgb.green = 0.75; rgbv[11].rgb.blue = 0.00;
    rgbv[12].rgb.red = 1.00; rgbv[12].rgb.green = 0.38; rgbv[12].rgb.blue = 0.38;
    rgbv[13].rgb.red = 1.00; rgbv[13].rgb.green = 0.00; rgbv[13].rgb.blue = 0.38;
    rgbv[14].rgb.red = 1.00; rgbv[14].rgb.green = 0.00; rgbv[14].rgb.blue = 0.00;
/*
 * Define 16 different color indices, for indices 0 through 15.  The
 * color corresponding to index 0 is black and the color corresponding
 * to index 1 is white.
 */
    rgbv[0].rgb.red = 0.0; rgbv[0].rgb.green = 0.0; rgbv[0].rgb.blue = 0.0;
    gset_colr_rep (WKID,0,&rgbv[0]);
    rgbv[0].rgb.red = 1.00; rgbv[0].rgb.green = 1.00; rgbv[0].rgb.blue = 1.00;

    for( i = 0; i < 15; i++ ) {
        gset_colr_rep (WKID,i+1,&rgbv[i]);
    }
}

struct common1 {
    int icll,iama[10000];
} NGCALLC(amapra,AMAPRA);

NGCALLF(cpchhl,CPCHHL)(iflg)
int *iflg;
{
/*
 * this version of cpchhl, if and only if icll is non-zero, examines a
 * high/low label which is about to be drawn.  if that label would fall
 * in an area outside the convex hull defined by edge group 4, the text
 * of the label is changed to a blank, so that the label is effectively
 * deleted.
 */
    int i,iaai[10],iagi[10],nair,ivis;
    float xpos, ypos;

    if (NGCALLC(amapra,AMAPRA).icll == 0) return(1);
    if ((*iflg >= 2 && *iflg <= 4) || (*iflg >= 6 && *iflg <= 8)) {
        c_cpgetr ("LBX",&xpos);
        c_cpgetr ("LBY",&ypos);
        c_argtai (NGCALLC(amapra,AMAPRA).iama,xpos,ypos,iaai,iagi,10,&nair,1);
        ivis=1;
        for( i=0; i < nair; i++ ) {
            if (iagi[i] == 4 && iaai[i] < 0) ivis=0;
        }
        if (ivis == 0) {
            c_cpsetc ("CTM"," ");
        }
    }
    return(1);
}

NGCALLF(cpchll,CPCHLL)(iflg)
int *iflg;
{
/*
 * this version of cpchll, if and only if icll is non-zero, examines a
 * contour-line label which is about to be drawn.  if that label would
 * fall in an area outside the convex hull defined by edge group 4, the
 * text of the label is changed to a blank, so that it is effectively
 * deleted.
 */
    int i,iaai[10],iagi[10],nair,ivis;
    float xpos, ypos;

    if (NGCALLC(amapra,AMAPRA).icll == 0) return(1);
    if (*iflg >= 2 && *iflg <= 4) {
        c_cpgetr ("LBX",&xpos);
        c_cpgetr ("LBY",&ypos);
        c_argtai (NGCALLC(amapra,AMAPRA).iama,xpos,ypos,iaai,iagi,10,&nair,1);
        ivis=1;
        for( i=0; i < nair; i++ ) {
            if (iagi[i] == 4 && iaai[i] < 0) ivis=0;
        }
        if (ivis == 0) {
            c_cpsetc ("CTM"," ");
        }
    }
    return(1);
}

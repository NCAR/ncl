/*
 *	$Id: c_mpex09.c,v 1.1 1994-10-31 03:29:14 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>

#define max(x,y)  ((x) > (y) ? (x) : (y) )
#define min(x,y)  ((x) < (y) ? (x) : (y) )

float usav[1000],vsav[1000];

struct common {
    int icsz,nsav;
    float ulew,urew,vbew,vtew,delu,delv;
} NGCALLF(limits,LIMITS);

char *plbl = "SEGMENT NUMBERS FOR OUTLINE DATASET CO";

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
/*
 * The program EXMPL9 produces a set of plots showing the numbers
 * of all the segments in a chosen EZMAP outline dataset.  Certain
 * variables must be set in data statements at the beginning of
 * the program.  In each of the seven places marked off by rows of
 * dashes, un-comment the first card to do outline dataset 'CO',
 * the second to do 'US', the third to do 'PO', and the fourth to
 * do 'PS'.
 * The common block LIMITS communicates values between TESTIT and
 * the routines MAPEOD and MOVEIT.
 */
/*
 * Define variables required.
 */
    float pl1[2],pl2[2],pl3[2],pl4[2],plat,plon,rota;
    float flem,frem,fbem,ftem,ulem,urem,vbem,vtem;
    int i, j, lnlg, ilim,jlim,ilem,irem,ibem,litem;
    int ilew,irew,ibew,itew;
    float flew, frew, fbew, ftew,vtew;
    extern void bndary();
/*
 * Select the appropriate values of PLAT, PLON, and ROTA.
 */
    plat = plon = rota = 0.0;
/*
 * Select the values to be put in the limits arrays.
 */
    pl1[0] = pl2[0] = pl3[0] = pl4[0] = 0.0;
    pl1[1] = pl2[1] = pl3[1] = pl4[1] = 0.0;
/*
 * Select values determining how the whole map is to be carved
 * up into little maps.  ILIM is the number of divisions of the
 * horizontal axis, JLIM the number of divisions of the vertical
 * axis.  (If all the labels are put on a single map, the result
 * is confusing.)
 */
    ilim = 2;
    jlim = 2;
/*
 * Open GKS.
 */
	gopen_gks("stdout",0);
	gopen_ws(WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Set the character size; 6 is about the smallest usable value.
 */
    NGCALLF(limits,LIMITS).icsz=6;
/*
 * Set the outline-dataset parameter.
 */
    c_mapstc ("OU","CO");
/*
 * Set the projection-type parameters.
 */
    c_maproj ("ME",plat,plon,rota);
/*
 * Set the limits parameters.
 */
    c_mapset ("MA",pl1,pl2,pl3,pl4);
/*
 * Initialize EZMAP.
 */
    c_mapint();
/*
 * Retrieve the values with which MAPINT called SET.
 */
    c_getset(&flem,&frem,&fbem,&ftem,&ulem,&urem,&vbem,&vtem,&lnlg);
    ilem=c_kfpx(flem);
    irem=c_kfpx(frem);
    ibem=c_kfpy(fbem);
    litem=c_kfpy(ftem);
/*
 * Now, plot a set of maps which are subsets of the whole map.
 */
    for( i = 1; i <= ilim; i++ ) {
        for( j = 1; j <= jlim; j++ ) {
            pl1[0] = ulem+(urem-ulem)*(float)(i-1)/(float)ilim;
            pl2[0] = ulem+(urem-ulem)*(float)i/(float)ilim;
            pl3[0] = vbem+(vtem-vbem)*(float)(j-1)/(float)jlim;
            pl4[0] = vbem+(vtem-vbem)*(float)j/(float)jlim;
            pl1[1] = pl2[1] = pl3[1] = pl4[1] = 0.;
            c_mapset ("LI",pl1,pl2,pl3,pl4);
/*
 * Re-initialize EZMAP with the new limits.
 */
            c_mapint();
/*
 * Retrieve the values with which MAPINT called SET.
 */
            c_getset(&flew,&frew,&fbew,&ftew,&NGCALLF(limits,LIMITS).ulew,&NGCALLF(limits,LIMITS).urew,&NGCALLF(limits,LIMITS).vbew,&NGCALLF(limits,LIMITS).vtew,&lnlg);
            ilew=c_kfpx(flew);
            irew=c_kfpx(frew);
            ibew=c_kfpy(fbew);
            itew=c_kfpy(ftew);
/*
 * Compute quantities required by MAPEOD and MOVEIT to position
 * labels.
 */
            NGCALLF(limits,LIMITS).delu=3.5*((float)NGCALLF(limits,LIMITS).icsz/(float)(irew-ilew))*(NGCALLF(limits,LIMITS).urew-NGCALLF(limits,LIMITS).ulew);
            NGCALLF(limits,LIMITS).delv=2.0*((float)NGCALLF(limits,LIMITS).icsz/(float)(itew-ibew))*(NGCALLF(limits,LIMITS).vtew-NGCALLF(limits,LIMITS).vbew);
            NGCALLF(limits,LIMITS).nsav=0;
/*
 * Draw the outlines.
 */
            c_maplot();
/*
 * Put a label at the top of the plot.
 */
            c_set (0.,1.,0.,1.,0.,1.,0.,1.,1);
            c_pwrit (.5,.975,plbl,38,2,0,0);
/*
 * Draw a boundary around the edge of the plotter frame.
 */
            bndary();
/*
 * Advance the frame.
 */
            c_frame();
        }
    }
/*
 * Close GKS.
 */
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
}

void bndary()
{
/*
 * Routine to draw the plotter-frame edge.
 */
    c_plotit(    0,    0,0);
    c_plotit(32767,    0,1);
    c_plotit(32767,32767,1);
    c_plotit(    0,32767,1);
    c_plotit(    0,    0,1);
}

void NGCALLF(mapeod,MAPEOD)(nout,nseg,idls,idrs,npts,pnts)
int *nout, *nseg, *idls, *idrs, *npts;
float *pnts;
{
/*
 * this version of mapeod marks each segment of the map with its
 * segment number.  the resulting map may be used to set up other
 * versions of mapeod which plot selected segments.
 *
 *
 * the common block limits communicates values between testit and
 * the routines mapeod and moveit.
 */
/*
 * define local variables to hold the character-string form of the
 * segment number.
 */
    char csg1[5], csg2[5];
    int mpts, nfch, nchs;
    float ucen, vcen, ucm1, vcm1, ucp1, vcp1, x, y, ulab, vlab;
/*
 * find out where on the map the center of the segment is.
 */
    mpts = *npts/2+1;
    c_maptrn (pnts[2*mpts-2],pnts[2*mpts-1],&ucen,&vcen);
/*
 * if the center is visible, label it.
 */
    if (ucen >= NGCALLF(limits,LIMITS).ulew && ucen <= NGCALLF(limits,LIMITS).urew && 
        vcen >= NGCALLF(limits,LIMITS).vbew && vcen <= NGCALLF(limits,LIMITS).vtew) {
/*
 * generate a character string representing the value of the
 * segment number.
 */
        sprintf( csg2, "%4d", *nseg );
        nfch=4-(int)(log10((float)(*nseg)+.5));
        nchs=5-nfch;
        strcpy( csg1, csg2+nfch-1 );
/*
 * find out where the two points on either side of the center are.
 */
        mpts=max(*npts/2,1);
        c_maptrn (pnts[2*mpts-2],pnts[2*mpts-1],&ucm1,&vcm1);

        mpts=min(*npts/2+2,*npts);
        c_maptrn (pnts[2*mpts-2],pnts[2*mpts-1],&ucp1,&vcp1);
/*
 * compute the preferred position of the label, with one corner
 * of its enclosing box at the center of the segment.
 */
        x = .1428*NGCALLF(limits,LIMITS).delu*(float)nchs;
        if( ucm1+ucp1-2*ucen < 0 ) x = -x;
        ulab=ucen-x;
        x = .3333*NGCALLF(limits,LIMITS).delv;
        if( vcm1+vcp1-2*vcen < 0 ) x = -x;
        vlab=vcen-x;
/*
 * move the label as necessary to avoid its being on top of any
 * previous label.
 */
        (void)moveit (&ulab,&vlab);
/*
 * write out the character string and connect it to the segment
 * with a straight line.
 */
        if ( ulab-ucen < 0 ) x = -(.1428*NGCALLF(limits,LIMITS).delu*(float)nchs);
        else                 x =   .1428*NGCALLF(limits,LIMITS).delu*(float)nchs;
        if ( vlab-vcen < 0 ) y = -(.3333*NGCALLF(limits,LIMITS).delv);
        else                 y =   .3333*NGCALLF(limits,LIMITS).delv;
        c_line (ucen,vcen,ulab-x,vlab-y);

        c_pwrit (ulab,vlab,csg1,nchs,NGCALLF(limits,LIMITS).icsz,0,0);
            
    }
}

moveit (ulab,vlab)
float *ulab, *vlab;
{
/*
 * The object of this routine is to avoid putting segment labels
 * on top of each other.  (ulab,vlab), on entry, is the most
 * desirable position for a segment label.  moveit modifies this
 * position as necessary to make sure the label will not be on top
 * of any previous label.
 *
 * the common block limits communicates values between testit and
 * the routines mapeod and moveit.
 *
 * ulew, urew, vbew, and vtew specify the u/v limits of the window
 * in which the map was drawn.  delu and delv are the minimum
 * allowable distances between segment-label centers in the u
 * and v directions, respectively.  nsav is the number of label
 * positions saved in the arrays usav and vsav.
 *
 * previous label positions are saved in the arrays usav and vsav.
 */
    int ign1, ign2, i;
    float x, y;
/*
 * zero the variables which control the generation of a spiral.
 */
    ign1=0;
    ign2=2;
/*
 * check for overlap at the current position.
 */
    i = 0;
    do {
        if (fabs(*ulab-usav[i]) < NGCALLF(limits,LIMITS).delu && fabs(*vlab-vsav[i]) < NGCALLF(limits,LIMITS).delv) {

/*
 * overlap.  try a new point.  the points tried form a spiral.
 */
            do {
                ign1=ign1+1;
                if (ign1 <= ign2/2) {
                    if ( -.5+(float)((ign2/2)%2) < 0 ) x = -NGCALLF(limits,LIMITS).delu;
                    else                               x =  NGCALLF(limits,LIMITS).delu;
                    *ulab = *ulab+x;
                }
                else {
                    if ( -.5+(float)((ign2/2)%2) < 0 ) y = -NGCALLF(limits,LIMITS).delv;
                    else                               y =  NGCALLF(limits,LIMITS).delv;
                    *vlab = *vlab+y;
                }
                if (ign1 == ign2) {
                    ign1=0;
                    ign2=ign2+2;
                }
            } while(*ulab < NGCALLF(limits,LIMITS).ulew || *ulab > NGCALLF(limits,LIMITS).urew || 
                    *vlab < NGCALLF(limits,LIMITS).vbew || *vlab > NGCALLF(limits,LIMITS).vtew);
            i = -1;
        }
    } while( ++i < NGCALLF(limits,LIMITS).nsav );
/*
 * no overlap.  save the new label position and return to caller.
 */
    NGCALLF(limits,LIMITS).nsav++;
    if (NGCALLF(limits,LIMITS).nsav > 1000) return(0);
    usav[NGCALLF(limits,LIMITS).nsav-1] = *ulab;
    vsav[NGCALLF(limits,LIMITS).nsav-1] = *vlab;
    return(1);
}

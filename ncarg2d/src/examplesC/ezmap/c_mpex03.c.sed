/*
 *	$Id: c_mpex03.c.sed,v 1.2 1994-06-21 15:00:07 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

char *plbl = "SIMPLIFIED CONTINENTS ON A MERCATOR PROJECTION";

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
/*
 * Produce a Mercator projection of the whole globe, using
 * simplified continental outlines.  See the routine MAPEOD,
 * below.
 *
 * Define the label for the top of the map.
 */
    float plm1[2], plm2[2], plm3[2], plm4[2];
    int ierr;
    extern void bndary();
/*
 * Open GKS.
 */
	gopen_gks("stdout",0);
	gopen_ws(WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Turn off the clipping indicator.
 */
    gset_clip_ind (GIND_NO_CLIP);
/*
 * Draw the map.
 */
    plm1[0] = plm2[0] = plm3[0] = plm4[0] = 0.;
    c_supmap (9,0.,0.,0.,plm1,plm2,plm3,plm4,1,15,2,0,&ierr);
/*
 * Put the label at the top of the plot.
 */
    c_set (0.,1.,0.,1.,0.,1.,0.,1.,1);
    c_pwrit (.5,.975,plbl,46,2,0,0);
/*
 * Draw a boundary around the edge of the plotter frame.
 */
    bndary();
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

mapeod_(nout,nseg,idls,idrs,npts,pnts)
int *nout, *nseg, *idls, *idrs, *npts;
float *pnts;
{
/*
 * this version of mapeod uses area identifiers for the outline
 * dataset 'co' to suppress all but the major global land masses.
 * in honor of Cicely Ridley, the British Isles are included.
 *
 * cull the segment if there's no ocean on either side of it ...
 */
    if ( *idls != 2 && *idrs != 2) *npts=0;
/*
 * or if it's not an edge of any of the desired areas.
 */
      if (*idls !=  1 && *idrs !=  1 && 
          *idls !=  3 && *idrs !=  3 && 
          *idls != 11 && *idrs != 11 && 
          *idls != 79 && *idrs != 79 && 
          *idls != 99 && *idrs != 99 && 
          *idls !=104 && *idrs !=104 && 
          *idls !=107 && *idrs !=107 && 
          *idls !=163 && *idrs !=163     ) *npts=0;
    return(1);
}

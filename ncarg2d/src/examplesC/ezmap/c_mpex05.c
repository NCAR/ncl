/*
 *  $Id: c_mpex05.c,v 1.3 1997-04-21 14:38:28 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <string.h>

/*
 * Include function prototypes
 */
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define IWTYPE 1
#define WKID   1

main()
{
    extern void bndary();
/*
 * This programproduces a single frame with maximal-area
 * views of all the EZMAP projections of the globe.
 */
/*
 * Define the label for the top of the map.
 */
    char plbl[38];

    strcpy( plbl, "MAXIMAL-AREA PROJECTIONS OF ALL TYPES" );
/*
 * open GKS.
 */
    gopen_gks("stdout",0);
    gopen_ws(WKID, NULL, IWTYPE);
    gactivate_ws(WKID);
/*
 * Turn off the clipping indicator.
 */
    gset_clip_ind (GIND_NO_CLIP);
/*
 * Set the outline-dataset parameter.
 */
    c_mapstc ("OU","CO");
/*
 * Put meridians and parallels every 15 degrees.
 */
    c_mapsti ("GR",15);
/*
 * Reduce the label size.
 */
    c_mapsti ("LS",0);
/*
 * Lambert conformal conic.
 */
    c_mappos (.025,.24375,.63125,.85);
    c_maproj ("LC",30.,0.,45.);
    c_mapdrw();
/*
 * Stereographic.
 */
    c_mappos (.26875,.4875,.63125,.85);
    c_maproj ("ST",0.,0.,0.);
    c_mapdrw();
/*
 * Orthographic.
 */
    c_mappos (.5125,.73125,.63125,.85);
    c_maproj ("OR",0.,0.,0.);
    c_mapdrw();
/*
 * Lambert equal-area.
 */
    c_mappos (.75625,.975,.63125,.85);
    c_maproj ("LE",0.,0.,0.);
    c_mapdrw();
/*
 * Gnomonic.
 */
    c_mappos (.025,.24375,.3875,.60625);
    c_maproj ("GN",0.,0.,0.);
    c_mapdrw();
/*
 * Azimuthal equidistant.
 */
    c_mappos (.26875,.4875,.3875,.60625);
    c_maproj ("AE",0.,0.,0.);
    c_mapdrw();
/*
 * Satellite-view.
 */
    c_mappos (.5125,.73125,.3875,.60625);
    c_maproj ("SV",0.,0.,0.);
    c_mapstr ("SA",2.);
    c_mapdrw();
/*
 * Mercator.
 */
    c_mappos (.75625,.975,.3875,.60625);
    c_maproj ("ME",0.,0.,0.);
    c_mapdrw();
/*
 * Cylindrical equidistant.
 */
    c_mappos (.025,.4875,.13125,.3625);
    c_maproj ("CE",0.,0.,0.);
    c_mapdrw();
/*
 * Mollweide type.
 */
    c_mappos (.5125,.975,.13125,.3625);
    c_maproj ("MO",0.,0.,0.);
    c_mapdrw();
/*
 * Put the label at the top of the plot ...
 */
    c_set (0.,1.,0.,1.,0.,1.,0.,1.,1);
    c_pwrit (.5,.925,plbl,37,2,0,0);
/*
 * and the labels under each sub-plot.
 */
    c_pwrit (.134375,.61875,"LAMBERT CONFORMAL CONIC",23,0,0,0);
    c_pwrit (.378125,.61875,"STEREOGRAPHIC",13,0,0,0);
    c_pwrit (.621875,.61875,"ORTHOGRAPHIC",12,0,0,0);
    c_pwrit (.865625,.61875,"LAMBERT EQUAL-AREA",18,0,0,0);

    c_pwrit (.134375,.375,"GNOMONIC",8,0,0,0);
    c_pwrit (.378125,.375,"AZIMUTHAL EQUIDISTANT",21,0,0,0);
    c_pwrit (.621875,.375,"SATELLITE-VIEW",14,0,0,0);
    c_pwrit (.865625,.375,"MERCATOR",8,0,0,0);
    c_pwrit (.25625,.11875,"CYLINDRICAL EQUIDISTANT",23,0,0,0);
    c_pwrit (.74375,.11875,"MOLLWEIDE TYPE",14,0,0,0);
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

NGCALLF(mapeod,MAPEOD)(nout,nseg,idls,idrs,npts,pnts)
int *nout, *nseg, *idls, *idrs, *npts;
float *pnts;
{
/*
 * this version of mapeod uses area identifiers for the outline
 * dataset 'co' to suppress all but the major global land masses.
 */
    if (*idls!=  2 && *idrs!=  2) *npts=0;

    if (*idls!=  1 && *idrs!=  1 && 
          *idls!=  3 && *idrs!=  3 && 
          *idls!= 11 && *idrs!= 11 && 
          *idls!= 79 && *idrs!= 79 && 
          *idls!= 99 && *idrs!= 99 && 
          *idls!=104 && *idrs!=104 && 
          *idls!=107 && *idrs!=107 && 
          *idls!=163 && *idrs!=163     ) *npts=0;

    return(1);
}

/*
 *  $Id: c_mpex06.c,v 1.2 1994-06-21 15:00:10 haley Exp $
 */
#include <stdio.h>
#include <math.h>

/*
 * Include function prototypes
 */
#include <ncarg/ncargC.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{

/*
 * This program produces a single frame showing satellite views
 * of the globe, each rotated by a different angle.
 *
 * Define the label for the top of the map.
 */
    char plbl[28];
    extern void bndary();

    strcpy( plbl,"THE EARTH IS SPINNING, TOTO" );
/*
 * Open GKS.
 */
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Set the outline-dataset parameter.
 */
    c_mapstc ("OU","PS");
/*
 * Use a satellite-view projection.
 */
    c_mapstr ("SA",1.25);
/*
 * Aim the camera 15 degrees away from straight down.
 */
    c_mapsti ("S1",15);
/*
 * Turn off the perimeter and reduce the number of grid lines.
 */
    c_mapsti ("PE",0);
    c_mapsti ("GR",15);
/*
 * Center the first map over Kansas.  Rotate by 0 degrees and look
 * to the upper left.
 */
    c_mappos (.05,.475,.525,.95);
    c_maproj ("SV",38.,-98.,0.);
    c_mapsti ("S2",135);
    c_mapdrw();
/*
 * Repeat, but rotate by 90 degrees and look to the upper right.
 */
    c_mappos (.525,.95,.525,.95);
    c_maproj ("SV",38.,-98.,90.);
    c_mapsti ("S2",45);
    c_mapdrw();
/*
 * Repeat, but rotate by 180 degrees and look to the lower left.
 */
    c_mappos (.05,.475,.05,.475);
    c_maproj ("SV",38.,-98.,180.);
    c_mapsti ("S2",-135);
    c_mapdrw();
/*
 * Repeat, but rotate by 270 degrees and look to the lower right.
 */
    c_mappos (.525,.95,.05,.475);
    c_maproj ("SV",38.,-98.,270.);
    c_mapsti ("S2",-45);
    c_mapdrw();
/*
 * Put the label at the top of the plot ...
 */
    c_set (0.,1.,0.,1.,0.,1.,0.,1.,1);
    c_pwrit (.5,.975,plbl,27,2,0,0);
/*
 * and the ones below each sub-plot.
 */
    c_pwrit (.2625,.5,"ROTA = 0",8,1,0,0);
    c_pwrit (.7375,.5,"ROTA = 90",9,1,0,0);
    c_pwrit (.2625,.025,"ROTA = 180",10,1,0,0);
    c_pwrit (.7375,.025,"ROTA = 270",10,1,0,0);
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

/*
 *	$Id: c_mpex02.c,v 1.2 1994-06-21 15:00:06 haley Exp $
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
 * This program produces a nice view of Africa, with an elliptical
 * perimeter.
 *
 * Define the label for the top of the map.
 */
    float p1[2],p2[2],p3[2],p4[2];
    char plbl[27];
    extern void bndary();

    strcpy( plbl, "CAN YOU NAME THE COUNTRIES" );
/*
 * open GKS.
 */
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Use an elliptical perimeter.
 */
    c_mapsti ("EL",1);
/*
 * Dot the outlines, using dots a quarter as far apart as the default.
 */
    c_mapsti ("DO",1);
    c_mapsti ("DD",3);
/*
 * Show continents and international boundaries.
 */
    c_mapstc ("OU","PO");
/*
 * Use a stereographic projection.
 */
    c_maproj ("ST",0.,0.,0.);
/*
 * Specify where two corners of the map are.
 */
    p1[0] = -38.;
    p2[0] = -28.;
    p3[0] = 40.;
    p4[0] = 62.;
    p1[1] = p2[1] = p3[1] = p4[1] = 0.;
    c_mapset ("CO",p1,p2,p3,p4);
/*
 * Draw the map.
 */
    c_mapdrw();
/*
 * Put the label at the top of the plot.
 */
    c_set (0.,1.,0.,1.,0.,1.,0.,1.,1);
    c_pwrit (.5,.975,plbl,26,2,0,0);
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

/*
 *	$Id: c_mpex04.c,v 1.2 1994-06-21 15:00:08 haley Exp $
 */
#include <stdio.h>
#include <math.h>

/*
 * Include function prototypes
 */
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
/*
 * This program produces a single frame with polar stereographic
 * views of the poles and a Mercator projection of the rest.
 *
 * Define the label for the top of the map.
 */
    char plbl[27];
    float p1[2],p2[2],p3[2],p4[2];
    extern void bndary();

    strcpy( plbl, "OMNIS TERRA IN PARTES TRES" );
/*
 * Open GKS.
 */
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Turn off the clipping indicator.
 */
    gset_clip_ind (GIND_NO_CLIP);
/*
 * Set the outline-dataset parameter.
 */
    c_mapstc ("OU","PO");
/*
 * Use dotted outlines and move the dots a little closer together
 * than normal.
 */
    c_mapsti ("DO",1);
    c_mapsti ("DD",3);
/*
 * Do the Mercator projection of the equatorial belt first.
 */
    p1[0] = -3.1416;
    p2[0] = 3.1416;
    p3[0] = -1.5708;
    p4[0] = 1.5708;
    p1[1] = p2[1] = p3[1] = p4[1] = 0.;
    c_mappos (.05,.95,.05,.5);
    c_maproj ("ME",0.,0.,0.);
    c_mapset ("LI",p1,p2,p3,p4);
    c_mapdrw();
/*
 * Switch to an elliptical (in this case, circular) boundary.
 */
    c_mapsti ("EL",1);
/*
 * Do a polar stereographic view of the North Pole ...
 */
    p1[0] = 30.;
    p2[0] = 30.;
    p3[0] = 30.;
    p4[0] = 30.;
    c_mappos (.07,.48,.52,.93);
    c_maproj ("ST",90.,0.,-90.);
    c_mapset ("AN",p1,p2,p3,p4);
    c_mapdrw();
/*
 * and then a similar view of the South Pole.
 */
    c_mappos (.52,.93,.52,.93);
    c_maproj ("ST",-90.,0.,-90.);
    c_mapset ("AN",p1,p2,p3,p4);
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

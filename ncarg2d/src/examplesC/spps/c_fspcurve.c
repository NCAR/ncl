/*
 * $Id: c_fspcurve.c,v 1.1 1994-07-27 22:54:04 haley Exp $
 */

#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

/*
 * Coordinates for line (used by both GPL and CURVE)
 */
float xcoord[10] = {1.,2.,3.,5.,7.,9.,13.,16.,19.,23.};
float ycoord[10] = {1.,3.,5.,6.,7.,10.,13.,16.,14.,17.};

main()
{
	Gpoint_list line;
	int i;
/*
 * This program demonstrates how to use the SPPS CURVE routine and
 * the GKS GPL routine.  Both routine are given the same set of
 * coordinates to draw, however, because of the SET call the x axis
 * is reversed.  GPL ignores this, and CURVE obeys it.
 */
	float x2[2], x3[2], y2[2], y3[2];
/*
 * Coordinates for plot key
 */
	x2[0] = .2; x2[1] = .3;
	y2[0] = .1; y2[1] = .1;
	x3[0] = .7; x3[1] = .8;
	y3[0] = .1; y3[1] = .1;
/*
 *  Open GKS, open and activate a workstation.
 */
	gopen_gks("stdout",0);
	gopen_ws(WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Map the plotter frame to user coordinates.  Notice that
 * we have reversed the X axis here.
 */
	c_set (0.,1., 0., 1., 25., 0., 0., 20., 1);
/*
 * Set the line type to dashed.
 */
	gset_linetype(2);
/*
 * Draw the line with GPL. It will ignore the axis reversal.
 */
	line.num_points = 10;
	line.points = (Gpoint *)malloc(line.num_points*sizeof(Gpoint));
	for( i = 0; i < 10; i++ ) {
		line.points[i].x = xcoord[i];
		line.points[i].y = ycoord[i];
	}
	gpolyline(&line);
/*
 * Set the line type to solid
 */
	gset_linetype(1);
/*
 * Draw the line with CURVE.  It will observe the axis reversal
 */
	c_curve(xcoord,ycoord,10);
/*
 * Reset the plotter to user coordinate mapping to make it easier
 * to plot the text and key.  (Axis reversal is turned off)
 */
	c_set (0.,1., 0., 1., 0., 1., 0., 1., 1);
/*
 * Draw the text
 */
	c_plchlq(.25, .15, "GKS GPL Routine", 15., 0., 0.);
/*
 * Set line type to dashed.
 */
	gset_linetype(2);
/*
 * Draw a dashed line under the previous text
 */
	line.num_points = 2;
	for( i = 0; i < 2; i++ ) {
		line.points[i].x = x2[i];
		line.points[i].y = y2[i];
	}
	gpolyline(&line);
/*
 * Draw more text
 */
	c_plchlq(.75, .15, "SPPS CURVE Routine", 15., 0., 0.);
/*
 * Set line type to solid.
 */
	gset_linetype(1);
/*
 * Draw a solid line under the previous text
 */
	line.num_points = 2;
	for( i = 0; i < 2; i++ ) {
		line.points[i].x = x3[i];
		line.points[i].y = y3[i];
	}
	gpolyline(&line);
/*
 * Draw a main title
 */
	c_plchlq(.5, .9, "Drawing lines with GPL and CURVE", 20.,  0., 0.);
/*
 * Draw a border around the plot
 */
	c_line(0.,0.,1.,0.);
	c_line(1.,0.,1.,1.);
	c_line(1.,1.,0.,1.);
	c_line(0.,1.,0.,0.);
/*
 * Advance the frame 
 */
	c_frame();
/*
 * Deactivate and close workstation, close GKS.
 */
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
}

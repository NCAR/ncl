/*
 *	$Id: c_fdlcurvd.c,v 1.3 1994-06-24 22:25:54 haley Exp $
 */

#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
	int i, j;
	float xcoord[120], ycoord[120];
	float xcoor2[120], ycoor2[120];
	float xcoor3[120], ycoor3[120];
	Gcolr_rep rgb;
	extern void gncrcl();
/*
 * OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
 */
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * PURPOSE                To provide a simple demonstration of the
 *                        CURVED line drawing techniques.
 *
 * USAGE                  CALL EXLINE 
 *
 * ARGUMENTS
 *
 * LANGUAGE               FORTRAN
 *
 * PORTABILITY            FORTRAN 77
 *
 * NOTE                   The call to GOPWK will have to be modified
 *                        when using a non-NCAR GKS package.  The third
 *                        argument must be the workstation type for WISS.
 *
 * Establish the viewport and window.
 */
	c_set(.01,.99,0.01,.99,0.,1.,0.,1.,1);
/*
 * Turn buffering off
 */
	c_setusv("PB",2);
/*
 * Set up a color table
 *
 * Black background
 */
	rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = 0.;
	gset_colr_rep (WKID,0,&rgb);
/*
 * White foreground
 */
	rgb.rgb.red = 1.; rgb.rgb.green = 1.; rgb.rgb.blue = 1.;
	gset_colr_rep (WKID,1,&rgb);
/*
 * Yellow
 */
	rgb.rgb.red = 1.; rgb.rgb.green = 1.; rgb.rgb.blue = 0.;
	gset_colr_rep (WKID,2,&rgb);
/*
 * Gray
 */
	rgb.rgb.red = 0.4; rgb.rgb.green = 0.4; rgb.rgb.blue = 0.4;
	gset_colr_rep (WKID,3,&rgb);
/*
 * Pink
 */
	rgb.rgb.red = 1.00; rgb.rgb.green = 0.00; rgb.rgb.blue = 0.5;
	gset_colr_rep (WKID,4,&rgb);
/*
 * Cyan
 */
	rgb.rgb.red = 0.00; rgb.rgb.green = 1.00; rgb.rgb.blue = 0.5;
	gset_colr_rep (WKID,5,&rgb);
/*
 * Set line color
 */
	gset_line_colr_ind(5);
/*
 * Set line width
 */
	gset_linewidth(2.);
/*
 * Draw a line of circles around the plotter frame
 */
	for( i = 0; i <= 23; i++ ) {
		gncrcl((float)i/25.+.025, 0.+.025, .025, 25, xcoord, ycoord);
		c_curved(xcoord,ycoord,25);
		gncrcl((float)i/25.+.025, 1.-.025, .025, 25, xcoord, ycoord);
		c_curved(xcoord,ycoord,25);
		gncrcl(0.+.025,(float)i/25.+.025,.025, 25, xcoord, ycoord);
		c_curved(xcoord,ycoord,25);
		gncrcl(1.-.025,(float)i/25.+.025,.025, 25, xcoord, ycoord);
		c_curved(xcoord,ycoord,25);
	}
	gncrcl(1.-.025,1.-.025,.025, 25, xcoord, ycoord);
	c_curved(xcoord,ycoord,25);
/*
 * Get the coordinates for a circle in the center of the frame
 */
	gncrcl(.5,.5,.333,30,xcoor2, ycoor2);
/*
 * Increase Line width
 */
	gset_linewidth(5.);
/*
 * Set the Line Color
 */
	gset_line_colr_ind(2);
/*
 * Draw it
 */  
	c_curved(xcoor2,ycoor2,30);
/*
 * Using these coordinates, plot 30 smaller circles on the circle
 *        Decrease Line Width
 */
	gset_linewidth(3.);
/*
 * Set the Line Color
 */
	gset_line_colr_ind(3);
	for( i = 0; i < 30; i++ ) {
		gncrcl(xcoor2[i],ycoor2[i],.07,30,xcoord, ycoord);
		c_curved(xcoord,ycoord,30);
/*
 * Using these coordinates, plot 30 smaller circles on the circle
 * Decrease Line Width
 */
		gset_linewidth(1.);
/*
 * Set the Line Color
 */
		gset_line_colr_ind(4);
		for( j = 0; j < 30; j++ ) {
			gncrcl(xcoord[j],ycoord[j],.01,30,xcoor3, ycoor3);
			c_curved(xcoor3,ycoor3,30);
		}
/*
 * Increase Line Width
 */
		gset_linewidth(3.);
/*
 * Set the Line Color
 */
		gset_line_colr_ind(3);
	}
/*
 * Draw a label in the center
 */
	c_plchlq(.5,.7,"Circles",.03,0.,0.);
	c_plchlq(.5,.6,"of",.03,0.,0.);
	c_plchlq(.5,.5,"Circles",.03,0.,0.);
	c_plchlq(.5,.4,"of",.03,0.,0.);
	c_plchlq(.5,.3,"Circles",.03,0.,0.);
/*
 * Advance the Frame
 */
	c_frame();
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
}

void gncrcl(xcntr, ycntr, rad, npts, xcoord, ycoord)
int npts;
float xcntr, ycntr, rad, *xcoord, *ycoord;
{
/*
 * This function generates the coordinates for a circle with
 * center at XCNTR, YCNTR, and a radius of RAD.  There are
 * NPTS in the circle and the coordinates are returned in the
 * arrays xcoord and ycoord.
 *
 * Compute number of radians per degree
 */
	int i;
	float radpdg, angle, delta;

	radpdg = 2.*3.14159/360.;
/*
 * Initialize the angle
 */
	angle = 0.;
/*
 * Calculate the change in angle (360./number of points in circle)
 */
	delta = 360./(npts-1);
/*
 * Convert to radians
 */
	delta = delta * radpdg;
/*
 * Calculate each coordinate
 */
	for( i = 0; i < npts; i++ ) {
		xcoord[i] = rad * (cos(angle)) + xcntr;
		ycoord[i] = rad * (sin(angle)) + ycntr;
		angle = angle + delta;
	}
	return;
}

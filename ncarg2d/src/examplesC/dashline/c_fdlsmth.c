/*
 *	$Id: c_fdlsmth.c,v 1.1 1994-07-18 18:01:26 haley Exp $
 */

#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
	extern void exline();
/*
 *  Open GKS, open and activate a workstation.
 */
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * INVOKE DEMO DRIVER
 */
	exline();
/*
 * Deactivate and close workstation, close GKS.
 */
	gdeactivate_ws (WKID);
	gclose_ws (WKID);
	gclose_gks();
}

void exline()
{
	Gcolr_rep rgb;
	int i;
	extern void gncrcl();
/*
 * PURPOSE                To provide a simple demonstration of the
 *                        smoothing and crowded line removal techniques.
 *
 * USAGE                  CALL EXLINE (IWKID)
 *
 * ARGUMENTS
 *
 * ON INPUT               IWKID
 *                          Workstation id
 *
 * LANGUAGE               FORTRAN
 *
 * PORTABILITY            FORTRAN 77
 *
 * NOTE                   The call to GOPWK will have to be modified
 *                        when using a non-NCAR GKS package.  The third
 *                        argument must be the workstation type for WISS.
 */
	float xcoord[10], ycoord[10];
/*
 * Establish the viewport and window.
 */
	c_set(.01,.99,0.01,.99,0.,1.,0.,1.,1);
/*
 * Initialize the crowded line removal buffer
 */
	c_reset();
/*
 * Set up a color table
 *
 *     White background
 */
	rgb.rgb.red = 1.; rgb.rgb.green = 1.; rgb.rgb.blue = 1.;
	gset_colr_rep (WKID,0,&rgb);
/*
 * Black foreground
 */
	rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = 0.;
	gset_colr_rep (WKID,1,&rgb);
/*
 * Red
 */
	rgb.rgb.red = 1.; rgb.rgb.green = 0.; rgb.rgb.blue = 0.;
	gset_colr_rep (WKID,2,&rgb);
/*
 * Green
 */
	rgb.rgb.red = 0.; rgb.rgb.green = 1.; rgb.rgb.blue = 0.;
	gset_colr_rep (WKID,3,&rgb);
/*
 * Blue
 */
	rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = 1.;
	gset_colr_rep (WKID,4,&rgb);
/*
 * Set line color
 */
	gset_line_colr_ind(4);
/*
 * Set line width
 */
	gset_linewidth(1.);
        
	for( i = 1; i <= 50; i++ ) {
/*
 * Get the coordinates for a circle in the center of the frame
 */
		gncrcl((float)i/50.,.5,.333,10,xcoord, ycoord);
/*
 * Draw it
 */
		c_curved(xcoord,ycoord,10);
	}
/*
 * Draw a label in the center
 */
	c_plchlq(.5,.95,"Smoothing and Crowded Line Removal",.02,0.,0.);
/*
 * Advance the Frame
 */
	c_frame();
}

void gncrcl(xcntr, ycntr, rad, npts, xcoord, ycoord)
int npts;
float xcntr, ycntr, rad, *xcoord, *ycoord;
{
/*
 * This function generates the coordinates for a circle with
 * center at XCNTR, YCNTR, and a radius of RAD.  There are
 * NPTS in the circle and the coordinates are returned in the
 * arrays XCOORD and YCOORD.
 *
 * Compute number of radians per degree
 */
	float radpdg, angle, delta;
	int i;

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

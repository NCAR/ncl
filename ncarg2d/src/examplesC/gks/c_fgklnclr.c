/*
 *	$Id: c_fgklnclr.c,v 1.1 1994-07-27 15:55:48 haley Exp $
 */
#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
	extern void lineex();
/*
 *  Open GKS, open and activate a workstation.
 */
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Invoke demo driver
 */
	lineex();
/*
 * Deactivate and close workstation, close GKS.
 */
	gdeactivate_ws (WKID);
	gclose_ws (WKID);
	gclose_gks();
}

void lineex()
{
	Gcolr_rep rgb;
/*
 * PURPOSE                To provide a simple demonstration of 
 *                        how to change line color..
 *
 * USAGE                  CALL LINEEX (IWKID)
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
 *
 *  Data for the graphical objects.
 */
	float twopi = 6.283185;
	float  x0p = .5, y0p = .5, rp = .4;
	int i, nptp = 16;
	float dtheta, ang, xc, yc;
/*
 * Declare the constant for converting from degrees to radians.
 */
	float dtr =  .017453292519943;
/*
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
 *     White background
 */
	rgb.rgb.red = 1.; rgb.rgb.green = 1.; rgb.rgb.blue = 1.;
	gset_colr_rep (WKID,0,&rgb);
/*
 *  Black foreground
 */
	rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = 0.;
	gset_colr_rep (WKID,1,&rgb);
/*
 *  Red
 */
	rgb.rgb.red = 1.; rgb.rgb.green = 0.; rgb.rgb.blue = 0.;
	gset_colr_rep (WKID,2,&rgb);
/*
 *  Green
 */
	rgb.rgb.red = 0.; rgb.rgb.green = 1.; rgb.rgb.blue = 0.;
	gset_colr_rep (WKID,3,&rgb);
/*
 *  Blue
 */
	rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = 1.;
	gset_colr_rep (WKID,4,&rgb);
/*
 *  Create a polygonal fan 
 */
	dtheta = twopi/nptp;
	for( i = 1; i <= 16; i++ ) {
		ang = dtheta*(float)i + .19625;
		xc = rp*cos(ang);
        yc = rp*sin(ang);
/*
 *
 * Set the line color
 */ 
		gset_line_colr_ind ((i%4)+1);
/*
 * Set line width 
 */ 
		gset_linewidth(6.);
/*
 * Draw a line
 */
		c_lined(x0p,y0p,x0p+xc,y0p+yc);
	}
      
	c_set(0.,1.,0.,1.,0.,1.,0.,1.,1);
/*
 *     Set the line color to black
 */
	gset_line_colr_ind (1);
/*
 *  Create a background perimeter 
 */
	c_frstpt( 0.0, 0.0);
	c_vector( 1.0, 0.0);
	c_vector( 1.0, 1.0);
	c_vector( 0.0, 1.0);
	c_vector( 0.0, 0.0);
/*
 *  Label the plot
 */
	c_plchlq(0.5,0.91,"Changing Line Color",25.,0.,0.);
/*
 * Advance the frame
 */
	c_frame();
}

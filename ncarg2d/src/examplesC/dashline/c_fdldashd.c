/*
 *	$Id: c_fdldashd.c,v 1.1 1994-07-18 18:01:25 haley Exp $
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
 * INVOKE DEMO DRIVER
 */
	lineex();
/*
 * DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
 */
	gdeactivate_ws (WKID);
	gclose_ws (WKID);
	gclose_gks();
}

void lineex()
{
	int i;
/*
 * PURPOSE                To provide a simple demonstration of 
 *                        how to set line dash patterns. 
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
 */
	Gcolr_rep rgb;
	char str[61];
	float xcoord[360], ycoord[360], y;
/*
 *  Data for the graphical objects.
 */
	float twopi = 6.283185;
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
 * White background
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
 * Create and Plot 2 Sine Curves
 *
 * Set the dash pattern
 */
	strcpy( str, "'$'$'Line'drawn'with'VECTD$'$'$'$'" );
	c_dashdc(str,15,20);
/*
 * Move plotter pen
 */
	c_frstd(0.,.60);
	for( i = 1; i <= 360; i++ ) {
		y = (sin((float)i*(twopi/360.)) * .25) + .60;
		c_vectd((float)i/360.,y);
	}
/*
 * Set the Dash pattern
 */
	strcpy( str, "$$$$$$Line'drawn'with'CURVED$$$$$$$$$$$$" );
	c_dashdc(str,15,20);
	gset_linewidth(3.);
	for( i = 1; i <= 360; i++ ) {
		xcoord[i-1] = (float)i/360.;
		ycoord[i-1] = (sin((float)i*(twopi/360.)) * .25) + .45;
	}
/*
 * Draw the second curve
 */
	c_curved(xcoord, ycoord, 360);
/*
 * Draw a straight line
 */
	gset_linewidth(4.);
/*
 * 1111100110011111 binary  = 63903 decimal
 */
	i = 63903;
	c_dashdb(&i);
	c_lined(0.1,.15, .9,.15);
/*
 * Label the line
 */
	c_plchlq(0.5,0.10,"Line drawn with LINED",20.,0.,0.);
/*
 *  Create a background perimeter 
 */
	gset_line_colr_ind(1);
	c_frstpt( 0.0, 0.0);
	c_vector( 1.0, 0.0);
	c_vector( 1.0, 1.0);
	c_vector( 0.0, 1.0);
	c_vector( 0.0, 0.0);
/*
 *  Label the plot
 */
	c_plchlq(0.7,0.90,"Setting Dash Patterns",25.,0.,0.);
	c_plchlq(0.7,0.81,"with DASHDB and DASHDC",25.,0.,0.);
	c_frame();
	return;
}

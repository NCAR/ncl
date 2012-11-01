/*
 *	$Id: c_fdldashc.c,v 1.2 1995-06-14 13:59:14 haley Exp $
 */

#include <stdio.h>
#include <math.h>
#include <string.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define IWTYPE 1
#define WKID   1

main()
{
	extern void lineex();
/*
 *  Open GKS, open and activate a workstation.
 */
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, IWTYPE);
	gactivate_ws(WKID);
/*
 * Invoke demo driver
 */
    lineex();
/*
 *  Deactivate and close workstation, close GKS.
 */
      gdeactivate_ws (WKID);
      gclose_ws (WKID);
      gclose_gks();
}

void lineex()
{
	Gcolr_rep rgb;
/*
 * PURPOSE                To provide a simple demonstration of the
 *                        different line drawing techniques.
 *
 * USAGE                  lineex
 *
 * ARGUMENTS
 *
 * LANGUAGE               FORTRAN
 *
 * HISTORY                Written  by members of the
 *                        Scientific Computing Division of NCAR,
 *                        Boulder Colorado
 *
 * PORTABILITY            FORTRAN 77
 *
 * NOTE                   The call to GOPWK will have to be modified
 *                        when using a non-NCAR GKS package.  The third
 *                        argument must be the workstation type for WISS.
 */
	char str[61];
/*
 *  Data for the graphical objects.
 */
	float twopi = 6.283185;
	float x0p = .5, y0p = .5, rp = .45;
	float ang, dtheta, xc, yc, y;
	float rad, xcd, ycd;
	int i, ing, nptp = 16;
/*
 * Declare the constant for converting from degrees to radians.
 */
	float dtr = .017453292519943;
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
 *  Create a polygonal fan 
 */
	dtheta = twopi/nptp;
	for( i =1; i <= 6; i++ ) {
		ang = dtheta*(float)i + .19625;
		xc = rp*cos(ang);
		yc = rp*sin(ang);
/*
 * Set the line color
 */
		gset_line_colr_ind ((i%4)+1);
/*
 * Set line width 
 */
		gset_linewidth(2.);

		if (i == 1) {
/*
 *  Solid Line (the default)
 */
            strcpy( str, "$$$$Solid$$$$$$$$$$$$$$$$$$$$$$$$$$$$$" );
            c_dashdc(str,20,20);
            gset_linetype(1);
            c_lined(x0p,y0p,x0p+xc,y0p+yc);
        }
        else if (i == 2) {
/*
 * Dashed line
 */
            strcpy( str, "$$$$Dashed$$$$$$$$$$$$$$$$$$$$$$$$$$$$$" );
            c_dashdc(str,20,20);
            gset_linetype(2);
            c_lined(x0p,y0p,x0p+xc,y0p+yc);
        }
        else if (i == 3) {
/*
 * Dotted line
 */
            strcpy( str, "$$$$Dotted$$$$$$$$$$$$$$$$$$$$$$$$$$$$$" );
            c_dashdc(str,20,20);
            gset_linetype(3);
            c_lined(x0p,y0p,x0p+xc,y0p+yc);
		}
		else if (i == 4) {
/*
 * Dashed dotted line
 */
            strcpy( str, "$$$$Dotted'Dashed$$$$$$$$$$$$$$$$$$$$$$$$$$$$$" );
            c_dashdc(str,20,20);
            gset_linetype(4);
            c_lined(x0p,y0p,x0p+xc,y0p+yc);
		}
		else if (i == 5) {
/*
 * Don't do anything different here
 * Color is changed at beginning of loop
 */
            strcpy( str, "$$$$Color'Any$$$$$$$$$$$$$$$$$$$$$$$$$$$$$" );
            c_dashdc(str,20,20);
            c_lined(x0p,y0p,x0p+xc,y0p+yc);
		}
		else if (i == 6) {
/*
 * Increase the line width 
 */
            strcpy( str, "$$$$Width'Any$$$$$$$$$$$$$$$$$$$$$$$$$$$$$" );
            c_dashdc(str,20,20);
            gset_linewidth(10.) ;
            c_lined(x0p,y0p,x0p+xc,y0p+yc);
		}
/*
 * Reset the line type to a thin black solid line
 */
        gset_linetype(1);
        gset_linewidth(2.);
        gset_line_colr_ind (1);
    }
/*
 * Create and Plot 2 Sine Curves
 *
 * Set line width
 */
    gset_linewidth(4.) ;
/*
 * Set the dash pattern
 */
    strcpy( str, "$'$Draw'Any'Curve$'$'$'$'$'$'$'" );
    c_dashdc(str,15,20);
/*
 * Move plotter pen
 */
    c_frstd(0.,.25);
/*
 * Compute the curve coordinates
 */
    for( i = 1; i <= 360; i++ ) {
        y = (sin((float)i*(twopi/360.)) * .25) + .25;
        c_vectd((float)i/360.,y);
    }
/*
 * Set line width
 */
    gset_linewidth(2.);
/*
 *  Set the dash pattern
 */
    strcpy( str, "$'$$'$$$'$$$$ Any'Pattern" );
    c_dashdc(str,20,20);
/*
 * Set the line color to green
 */
    gset_line_colr_ind (3);
/*
 * Move plotter pen
 */
    c_frstd(0.,.125);
/*
 * Compute the curve coordinates
 */
    for( i = 1; i <= 360; i++ ) {
        y = (sin((float)i*(twopi/360.)) * .125) + .125;
        c_vectd((float)i/720.,y);
    }
/*
 * Create and plot a spiral curve
 */
    c_set (.4,.9,.0,.55,-1.,1.,-1.,1.,1);
    strcpy( str, "$$$$$$$$$$$$$$$$$$$$$$$$$$Any'Shape");
    c_dashdc(str,80,20);
/*
 * Move plotter pen
 */
    rad=.001;
    xcd=.25+.5*rad*cos(0.);
    ycd=.25+.5*rad*sin(0.);
    c_frstd(xcd,ycd);
/*
 * Set the line color to red
 */
    gset_line_colr_ind (2);
    for( ing = 1; ing <= 1500; ing++ ) {
        rad=(float)(ing)/1000.;
        ang=dtr*(float)(ing-1);
        xcd=.25+.5*rad*cos(ang);
        ycd=.25+.5*rad*sin(ang);
        c_vectd(xcd, ycd);
    }
    c_set(0.,1.,0.,1.,0.,1.,0.,1.,1);
/*
 * Set the line color to black
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
    c_frame();
}

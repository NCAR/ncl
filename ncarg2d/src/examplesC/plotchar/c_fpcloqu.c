/*
 *  $Id: c_fpcloqu.c,v 1.1 1994-07-27 16:57:14 haley Exp $
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
	extern void expclq();
/*
 * Open GKS.
 */
	gopen_gks("stdout",0);
	gopen_ws(WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Invoke demo driver
 */
	expclq();
/*
 * Deactivate and close workstation, close GKS.
 */
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
}

void expclq()
{
	Gcolr_rep rgb;
	Gtext_font_prec text_font_prec;
	extern void gnsprl();
/*
 * PURPOSE                To provide a simple demonstration of the
 *                        PLCHLQ text drawing techniques.
 *
 * USAGE                  CALL EXPCLQ (IWKID)
 *
 * ARGUMENTS
 *
 * ON INPUT               IWKID
 *                          A workstation id
 *
 * LANGUAGE               FORTRAN
 *
 * PORTABILITY            FORTRAN 77
 *
 * NOTE                   The call to GOPWK will have to be modified
 *                        when using a non-NCAR GKS package.  The third
 *                        argument must be the workstation type for WISS.
 */
	float xcoor2[500], ycoor2[500];
/*
 * Turn buffering off
 */
	c_setusv("PB",2);
/*
 * Set up a color table
 *
 *  White background
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
 * Establish the viewport and window.
 */
	c_set(.0,1.,0.,1.,0.,1.,0.,1.,1);
/*
 * Get the coordinates for a Spiral in the center of the frame
 */
	gnsprl(.5,.5,.45,500,4,xcoor2, ycoor2);
/*
 *  Set line width
 */
	gset_linewidth(3.);
/*
 *  Set line color
 */
	gset_line_colr_ind(2);
/*
 *  Draw the spiral
 */
	c_curved(xcoor2,ycoor2,500);
/*
 * Draw labels
 *  Set text color
 */
	gset_text_colr_ind(4);
/*
 * Set the font
 */
	text_font_prec.font = -13;
	text_font_prec.prec = 2;
	gset_text_font_prec(&text_font_prec);
/*
 * Plot the strings
 */
	c_plchlq(.41,.58,"Use",.04,55.,0.);
	c_plchlq(.58,.62,"PLCHLQ",.03,-25.,0.);
/*
 * Set the font
 */
	text_font_prec.font = -6;
	text_font_prec.prec = 2;
	gset_text_font_prec(&text_font_prec);
/*
 * Plot the strings
 */
	c_plchlq(.66,.47,"to",.04,-90.,0.);
	c_plchlq(.57,.33,"access",.05,-145.,0.);
/*
 * Set the font
 */
	text_font_prec.font = -12;
	text_font_prec.prec = 2;
	gset_text_font_prec(&text_font_prec);
/*
 * Plot the strings
 */
	c_plchlq(.39,.32,"the",.05,150.,0.);
	c_plchlq(.28,.47,"GKS",.05,110.,0.);
/*
 * Set the font
 */
	text_font_prec.font = -16;
	text_font_prec.prec = 2;
	gset_text_font_prec(&text_font_prec);
/*
 * Plot the strings
 */
	c_plchlq(.34,.67,"fonts",.05,45.,0.);
	c_plchlq(.525,.75,"and",.05,0.,0.);
/*
 * Set the font
 */
	text_font_prec.font = -7;
	text_font_prec.prec = 2;
	gset_text_font_prec(&text_font_prec);
/*
 *  Plot the strings
 */
	c_plchlq(.74,.59,"position",.05,-65.,0.);
	c_plchlq(.73,.31,"text",.05,-130.,0.);
/*
 * Set the font
 */
	text_font_prec.font = 1;
	text_font_prec.prec = 2;
	gset_text_font_prec(&text_font_prec);
/*
 *  Plot the strings
 */
	c_plchlq(.57,.20,"at",.05,-165.,0.);
	c_plchlq(.40,.20,"any",.05,165.,0.);
/*
 * Set the font
 */
	text_font_prec.font = -9;
	text_font_prec.prec = 2;
	gset_text_font_prec(&text_font_prec);
/*
 * Plot the string
 */
	c_plchlq(.23,.32,"angle.",.05,130.,0.);
/*
 * Advance the Frame
 */
	c_frame();
}

void gnsprl(xcntr,ycntr,iradus,npts,loops,xcoord,ycoord)
float xcntr, ycntr, *xcoord, *ycoord;
int npts, loops;
float iradus;
{
	float radius, radpdg, angle, delta, drad;
	int i;
/*
 * This function generates the coordinates for a spiral with
 * center at XCNTR, YCNTR, and an initial radius of IRADUS. The spiral
 * will turn on itself LOOPS times.  There are
 * NPTS in the Spiral and the coordinates are returned in the
 * arrays XCOORD and YCOORD.
 */
	radius = iradus;
/*
 * Compute number of radians per degree
 */
	radpdg = 2.*3.14159/360.;
/*
 * Initialize the angle
 */
	angle = 0.;
/*
 * Calculate the change in angle (360./number of points in circle)
 */
	delta = (float)(loops) * 360./((float)(npts-1));
/*
 * Convert to radians
 */
	delta = delta * radpdg;
/*
 * Calculate the change in radius
 */
	drad = radius/((float)(npts - 1));
/*
 * Calculate each coordinate
 */
	for( i = 0; i < npts; i++ ) {
		xcoord[i] = radius * (cos(angle)) + xcntr;
		ycoord[i] = radius * (sin(angle)) + ycntr;
/*
 * Increase the angle
 */
		angle = angle + delta;
/*
 * Reduce the radius
 */
		radius = radius - drad;
	}
	return;
}

/*
 * $Id: c_splogy.c,v 1.2 1994-07-27 23:03:28 haley Exp $
 */

#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
	int i;
	Gcolr_rep rgb;
/*
 * This test program is an example of logarithmic axis scaling
 * in the NCAR Graphics User Coordinate System.
 * 
 * Dimension a line of 100 points
 */
	float x[100],y[100];
/*
 * Open GKS
 */
	gopen_gks("stdout",0);
	gopen_ws(WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Turn clipping off
 */
	gset_clip_ind(GIND_NO_CLIP);
/*
 * Generate a straight line of 100 points.
 */
	for( i = 0; i < 100; i++ ) {
		x[i] = (float)(i+1);
		y[i] = 10.*(i+1);
	}
/*
 * Select axes of linear in X, logarithmic in Y,
 * in NCAR Graphics user coordinates.
 */
	c_set(.05,.95,.20,.95,1.,100.,10.,1000.,2);
/*
 * Set attributes for output
 * Assign yellow to color index 2
 */
	rgb.rgb.red = 1.; rgb.rgb.green = 1.; rgb.rgb.blue = 0.;
	gset_colr_rep(WKID,2,&rgb);
/*
 * Generate output (GKS, SPPS, or NCAR utilities)
 *
 *  Set polyline color index to yellow
 */
	gset_line_colr_ind(2);
/*
 *  Initialize the AUTOGRAPH entry EZXY so that
 *  the frame is not advanced and the Y axis is logarithmic.
 */
	c_displa(2,0,2);
/*
 *  Output the polyline (X,Y) using EZXY.
 */
	c_ezxy(x,y,100," ");
/*
 * Add a yellow title.
 *
 * PLOTCHAR uses stroked characters; thus, the yellow polyline
 * color index previously set will still apply.
 *
 * Return the window to fractional coordinates for the title.
 * Also, return scaling to linear, linear.
 */
	c_set(0.,1.,0.,1.,0.,1.,0.,1.,1);
	c_plchhq(0.5,.05,"Example 5.1.  Log Scaling with SPPS",.019,0.,0.);
/*
 * Advance the frame to ensure all output is plotted
 */
	c_frame();
/*
 * Close GKS
 */
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
}

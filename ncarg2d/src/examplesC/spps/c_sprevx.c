/*
 * $Id: c_sprevx.c,v 1.1 1994-07-27 22:54:07 haley Exp $
 */

#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
	Gcolr_rep rgb;
	int i;
/*
 *  This test program is an example of X axis scale reversal
 *  in the NCAR Graphics User Coordinate System.
 *
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
		x[i] = (float)i;
		y[i] = 10.*i;
	}
/*
 *  Select the normalization transformation
 *  window and viewport.  Reverse X in the window.
 */
	c_set(.10,.95,.20,.95,100.,1.,10.,1000.,1);
/*
 *  Set attributes for output
 *  Assign yellow to color index 2
 */
	rgb.rgb.red = 1.; rgb.rgb.green = 1.; rgb.rgb.blue = 0.;
	gset_colr_rep(WKID,2,&rgb);
/*
 * Generate output (GKS, SPPS, or NCAR utilities)
 *
 * Set polyline color index to yellow
 */
	gset_line_colr_ind(2);
/*
 *  Initialize the AUTOGRAPH entry EZXY so that
 *  the frame is not advanced.
 */
	c_displa(2,0,1);
/*
 *  Tell EZXY that the SET ordering of the window
 *  is to be used (LSET 3 or 4).
 */
	c_anotat(" "," ",1,4,0,NULL);
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
 */
	c_set(0.,1.,0.,1.,0.,1.,0.,1.,1);
	c_plchhq(0.5,.05,"Example 5.2.  X Axis Reversal with SPPS",.019,0.,0.);
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

/*
 * $Id: c_fcoord.c,v 1.1 1994-07-27 22:54:02 haley Exp $
 */

#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
	int i, ils, imi, ierr;
	float theta, vpl, vpr, vpb, vpt;
/*
 * Define arrays to hold data defining a spiral in the user coordinate
 * system.
 */
	float x[476],y[476];
/*
 * Define arrays to hold the numbers defining the viewport and window,
 * as retrieved from GKS.
 */
	Gtran norm_tran;
/*
 * Define a character variable in which to construct labels.
 */
	char chrs[27];
/*
 * Open GKS, open and activate a workstation.
 */
	gopen_gks("stdout",0);
	gopen_ws(WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Turn off clipping at the edges of the viewport (which GKS does by
 * default).
 */
	gset_clip_ind (GIND_NO_CLIP);
/*
 * Define the X and Y coordinates of a spiral in the user coordinate
 * system.  It lies in a rectangular region bounded by the lines
 * "X=100", "X=1000", "Y=100", and "Y=1000".
 */
	for( i = 0; i < 476; i++ ) {
		theta=.031415926535898*(float)i;
		x[i] = 500.+.9*(float)i*cos(theta);
		y[i] = 500.+.9*(float)i*sin(theta);
	}
/*
 * Loop through the possible values of 'LS'.
 */
	for( ils = 1; ils <= 4; ils++ ) {
/*
 * Define the fractional coordinates of the left and right edges of the
 * viewport.
 */
		vpl=(float)(ils-1)/4.+.020;
		vpr=(float)(ils  )/4.-.020;
/*
 * For each of the possible values of 'LS', loop through the possible
 * values of 'MI'.
 */
		for( imi = 1; imi <=4; imi++ ) {
/*
 * Define the fractional coordinates of the bottom and top edges of the
 * viewport.
 */
            vpb=(float)(4-imi)/4.+.059;
            vpt=(float)(5-imi)/4.-.001;
/*
 * Outline the viewport.  PLOTIF expects fractional coordinates.
 */
            c_plotif (vpl,vpb,0);
            c_plotif (vpr,vpb,1);
            c_plotif (vpr,vpt,1);
            c_plotif (vpl,vpt,1);
            c_plotif (vpl,vpb,1);
/*
 * Call SET to define the mapping from the user system to the plotter
 * frame.  The SET call specifies 'MI' = 1 (since the value of argument
 * 5 is less than that of argument 6 and the value of argument 7 is less
 * that of argument 8).  The SETUSV call overrides this to obtain the
 * desired value.
 */
            c_set    (vpl,vpr,vpb,vpt,100.,1000.,100.,1000.,ils);
            c_setusv ("MI (mirror imaging flag)",imi);
/*
 * Call the routine curve to draw the spiral.
 */
			c_curve  (x,y,476);
/*
 * Label the curve.  First, write the values of "MI" and "LS".  Note
 * the use of CFUX and c_cfuy to map meaningful fractional coordinates
 * to the user coordinates required by PLCHMQ.
 */
			sprintf( chrs,"MI= %d LS= %d", imi, ils);
            c_plchmq (c_cfux(.5*(vpl+vpr)),c_cfuy(vpb-.0120),chrs,.012,0.,0.);
/*
 * Retrieve the values defining the window and viewport, using GKS
 * calls.
 */
            ginq_norm_tran (1,&ierr,&norm_tran);
/*
 * Write them out, too.
 */
            sprintf (chrs,"VP=%5.3f,%5.3f,%5.3f,%5.3f", norm_tran.vp.x_min, norm_tran.vp.x_max, norm_tran.vp.y_min, norm_tran.vp.y_max );
            chrs[3] = ' ';
            chrs[9] = ' ';
            chrs[15] = ' ';
            chrs[21] = ' ';
            c_plchmq (c_cfux(.5*(vpl+vpr)),c_cfuy(vpb-.0320), chrs,.008,0.,0.);
            sprintf(chrs,"WD=%5.0f, %5.0f, %5.0f, %5.0f",  norm_tran.win.x_min, norm_tran.win.x_max, norm_tran.win.y_min, norm_tran.win.y_max );
            c_plchmq (c_cfux(.5*(vpl+vpr)),c_cfuy(vpb-.0480), chrs,.008,0.,0.);
/*
 * End of loop through the values of 'MI'.
 */
		}
/*
 * End of loop through the values of 'LS'.
 */
	}
/*
 * Deactivate and close workstation, close GKS.
 */
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
}


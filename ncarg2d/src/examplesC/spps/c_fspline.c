/*
 * $Id: c_fspline.c,v 1.1 1994-07-27 22:54:05 haley Exp $
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
/*
 * This is an example program which demonstrates how to use
 * the NCAR Graphics LINE routine.  The program draws a
 * simple flow diagram and labels the diagram objects.
 *
 * Open GKS, open and activate a workstation.
 */
	gopen_gks("stdout",0);
	gopen_ws(WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Set up a color table
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
 * Set the line color
 */
	gset_line_colr_ind(2);
/*
 * Set the plotter mapping space
 */
	c_set (0.,1.,0.,1.,0.,20., 0.,20.,1);
/*
 * Set the line width
 */
	gset_linewidth(4.);
/*
 *
 * Draw a box
 */
	c_line(2.,8.,2.,10.);
	c_line(2.,10.,5.,10.);
	c_line(5.,10.,5.,8.);
	c_line(5.,8.,2.,8.);
/*
 * Add text
 */
	c_plchlq(3.5,9.,"Read I",15.,0.,0.);
/*
 * Draw a diamond
 */
	c_line(8.,9.,9.5,11.);
	c_line(9.5,11.,11.,9.);
	c_line(11.,9.,9.5,7.);
	c_line(9.5,7.,8.,9.);
/*
 * Add text in Diamond
 */
	c_plchlq(9.5,9.,"Is I<3?",15.,0.,0.);
	c_plchlq(10.,11.5,"yes",15.,0.,0.);
	c_plchlq(9.9,6.5,"no",15.,0.,0.);
/*
 * Draw a box
 */ 
	c_line(15.,13.,18.,13.);
	c_line(18.,13.,18.,11.);
	c_line(18.,11.,15.,11.);
	c_line(15.,11.,15.,13.);
/*
 * Add text in box
 */
	c_plchlq(16.5,12.,"I = I+1",15.,0.,0.);
/*
 * Draw a box
 */ 
	c_line(15.,7.,18.,7.);
	c_line(18.,7.,18.,5.);
	c_line(18.,5.,15.,5.);
	c_line(15.,5.,15.,7.);
/*
 * Add text in box
 */
	c_plchlq(16.5,6.,"I = I-1",15.,0.,0.);
/*
 * Set the line width
 */
	gset_linewidth(2.);
/*
 * Set the line color
 */
	gset_line_colr_ind(4);
/*
 * Connect the objects
 */
	c_line(5.,9.,8.,9.);
	c_line(9.5,11.,9.5,12.);
	c_line(9.5,12.,15.,12.);
	c_line(9.5,7.,9.5,6.);
	c_line(9.5,6.,15.,6.);
/*
 * Label top of plot
 */
	c_plchhq(10.,15.,"Decision Flow Chart",25.,0.,0.);
/*
 * Draw a boundary around plotter frame
 */
	c_line(0.,0.,20.,0.);
	c_line(20.,0.,20.,20.);
	c_line(20.,20.,0.,20.);
	c_line(0.,20.,0.,0.);

	c_frame();
/*
 * Deactivate and close workstation, close GKS.
 */
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
}

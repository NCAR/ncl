/*
 *	$Id: c_cpex11.c,v 1.2 1994-08-17 22:59:07 haley Exp $
 */

/*
 * This program demonstrates the use of the new internal parameter 'PIT',
 * which was installed in May of 1994.
 *
 */

#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
/*
 * Declare the data array and the real and integer workspace arrays.
 */
	float zdat[19][37],rwrk[10000];
	int i, j, iwrk[10000];
/*
 * Define the constant pi/180, used to convert angles from degrees to
 * radians.
 */
	float dtor = .017453292519943;
	float rlat, rlon;
/*
 * Define a global array of data using a simple function of longitude
 * (which is a linear function of the first subscript of the data array)
 * and latitude (which is a linear function of the second subscript).
 */
	for( i = 0; i < 37; i++ ) { 
		rlon = -180.+10.*(float)i;
		for( j = 0; j < 19; j++ ) { 
            rlat = -90.+10.*(float)j;
            zdat[j][i] = sin(dtor*rlon)*cos(dtor*rlat);
		}
	}
/*
 * Salt in a few special values.
 */
	zdat[ 7][ 0] = 1.e36;
	zdat[ 7][ 1] = 1.e36;
	zdat[ 7][ 2] = 1.e36;
	zdat[ 7][34] = 1.e36;
	zdat[ 7][35] = 1.e36;
	zdat[ 7][36] = 1.e36;
	zdat[ 8][18] = 1.e36;
	zdat[ 9][15] = 1.e36;
	zdat[ 9][16] = 1.e36;
	zdat[ 9][17] = 1.e36;
	zdat[ 9][18] = 1.e36;
	zdat[ 9][19] = 1.e36;
	zdat[ 9][20] = 1.e36;
	zdat[ 9][21] = 1.e36;
	zdat[10][18] = 1.e36;
	zdat[11][ 0] = 1.e36;
	zdat[11][ 1] = 1.e36;
	zdat[11][ 2] = 1.e36;
	zdat[11][34] = 1.e36;
	zdat[11][35] = 1.e36;
	zdat[11][36] = 1.e36;
/*
 * Open GKS.
 */
	gopen_gks ("stdout",0);
	gopen_ws (WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * Turn off clipping by GKS.
 */
	gset_clip_ind (GIND_NO_CLIP);
/*
 * Tell EZMAP what part of the plotter frame to use.
 */
	c_mappos (.05,.95,.01,.91);
/*
 * Tell EZMAP what projection to use.  This projection maps the entire
 * globe to the interior of a circle of radius 2.  Distortion near the
 * outer edge of the circle is very great; mapped points from the data
 * grid become so widely separated that contour lines wind up crossing
 * each other.
 */
	c_maproj ("LE",75.,99.,0.);
/*
 * Initialize EZMAP.
 */
	c_mapint();
/*
 * Tell CONPACK to map its output through CPMPXY, using that value which
 * causes the EZMAP routine MAPTRA to be called.
 */
	c_cpseti ("MAP - MAPPING FLAG",1);
/*
 * Tell CONPACK what value EZMAP returns for the out-of-range value.
 */
	c_cpsetr ("ORV - OUT-OF-RANGE VALUE",1.e12);
/*
 * Tell CONPACK not to do a SET call (because EZMAP has already done it).
 */
	c_cpseti ("SET - DO-SET-CALL FLAG",0);
/*
 * Tell CONPACK what the special value is.
 */
	c_cpsetr ("SPV - SPECIAL VALUE",1.e36);
/*
 * Tell CONPACK what values are to be associated with the extreme values
 * of each of the subscripts of the data array.
 */
	c_cpsetr ("XC1 - X COORDINATE AT I=1",-180.);
	c_cpsetr ("XCM - X COORDINATE AT I=M", 180.);
	c_cpsetr ("YC1 - Y COORDINATE AT J=1", -90.);
	c_cpsetr ("YCN - Y COORDINATE AT J=N",  90.);
/*
 * Tell CONPACK to draw the mapped boundary of the data grid.
 */
	c_cpseti ("PAI - PARAMETER ARRAY INDEX",-1);
	c_cpseti ("CLU - CONTOUR LEVEL USE FLAG",1);
/*
 * Tell CONPACK to draw the mapped boundary of the area filled with
 * special values.
 */
	c_cpseti ("PAI - PARAMETER ARRAY INDEX",-2);
	c_cpseti ("CLU - CONTOUR LEVEL USE FLAG",1);
/*
 * Tell CONPACK to draw a limb line (separating areas that are visible
 * under the projection from area that are not visible).
 */
	c_cpseti ("PAI - PARAMETER ARRAY INDEX",-3);
	c_cpseti ("CLU - CONTOUR LEVEL USE FLAG",1);
/*
 * Tell PLOTCHAR not to interpret colons as function-code signal
 * characters.
 */
	c_pcsetc ("FC","\0");
/*
 * Tell PLOTCHAR to use one of the filled fonts.
 */
	c_pcseti ("FN",25);
/*
 * Tell CONPACK what the dimensions of the data array and the workspace
 * arrays are and make it initialize itself.
 */
	c_cprect (&zdat[0][0],37,37,19,rwrk,10000,iwrk,10000);
/*
 * Draw the mapped contour lines, the mapped edges of the grid, the
 * mapped edges of the special value area, and the visible/invisible
 * boundary of the mapping.
 */
	c_cpcldr (&zdat[0][0],rwrk,iwrk);
/*
 * Put a label at the top of the first frame.
 */
	c_plchhq (c_cfux(.5),c_cfuy(.975),"Using certain mappings can cause a problem like this:  Grid",.018,0.,0.);
	c_plchhq (c_cfux(.5),c_cfuy(.939),"points are spread so far apart that contour lines cross.",.018,0.,0.);
/*
 * Advance the frame.
 */
	c_frame();
/*
 * Set the value of the parameters 'PIC' and 'PIE' to interpolate points
 * on all contour lines and all edge lines.
 */
	c_cpseti ("PIC - POINT INTERPOLATION ON CONTOURS   ",7);
	c_cpseti ("PIE - POINT INTERPOLATION ON OTHER EDGES",7);
/*
 * Again draw the mapped contour lines, the mapped edges of the grid,
 * the mapped edges of the special value area, and the visible/invisible
 * boundary of the mapping.
 */
	c_cpcldr (&zdat[0][0],rwrk,iwrk);
/*
 * Label the second frame.
 */
	c_plchhq (c_cfux(.5),c_cfuy(.975),"Using 'PIC' = 'PIE' = 7 solves the problem expensively by",.018,0.,0.);
	c_plchhq (c_cfux(.5),c_cfuy(.939),"interpolating seven points on every segment before mapping.",.018,0.,0.);
/*
 * Advance the frame.
 */
	c_frame();
/*
 * Turn off the interpolation requested by 'PIC' and 'PIE'.
 */
	c_cpseti ("PIC - POINT INTERPOLATION ON CONTOURS   ",0);
	c_cpseti ("PIE - POINT INTERPOLATION ON OTHER EDGES",0);
/*
 * Set the value of the parameter 'PIT' to .05 to turn on interpolation
 * of points only in problem areas (where the X/Y distance between points
 * in user space exceeds 5/100ths of the horizontal/vertical dimension
 * of the window.
 */
	c_cpsetr ("PIT - POINT INTERPOLATION THRESHOLD",.05);
/*
 * Again draw the mapped contour lines, the mapped edges of the grid,
 * the mapped edges of the special value area, and the visible/invisible
 * boundary of the mapping.
 */
	c_cpcldr (&zdat[0][0],rwrk,iwrk);
/*
 * Label the third frame.
 */
	c_plchhq (c_cfux(.5),c_cfuy(.975),"Using 'PIT' = .05 solves the problem less expensively (and",.018,0.,0.);
	c_plchhq (c_cfux(.5),c_cfuy(.939),"more reliably) by interpolating points only as needed.",.018,0.,0.);
/*
 * Advance the frame.
 */
	c_frame();
/*
 * Now turn smoothing on, using a value that says to smooth before
 * mapping.
 */
	c_cpsetr ("T2D - TENSION ON 2D SMOOTHER",-2.5);
/*
 * Again draw the mapped contour lines, the mapped edges of the grid,
 * the mapped edges of the special value area, and the visible/invisible
 * boundary of the mapping.
 */
	c_cpcldr (&zdat[0][0],rwrk,iwrk);
/*
 * Label the fourth frame.
 */
	c_plchhq (c_cfux(.5),c_cfuy(.975),"With 'PIT' on, you can still smooth: Here, 'T2D' = -2.5,",.018,0.,0.);
	c_plchhq (c_cfux(.5),c_cfuy(.939),"which causes the smoothing to be done before the mapping.", .018,0.,0.);
/*
 * Advance the frame.
 */
	c_frame();
/*
 * Now turn smoothing on, using a value that says to smooth after
 * mapping.
 */
	c_cpsetr ("T2D - TENSION ON 2D SMOOTHER",2.5);
/*
 * Again draw the mapped contour lines, the mapped edges of the grid,
 * the mapped edges of the special value area, and the visible/invisible
 * boundary of the mapping.
 */
	c_cpcldr (&zdat[0][0],rwrk,iwrk);
/*
 * Label the fifth frame.
 */
	c_plchhq (c_cfux(.5),c_cfuy(.975),"With 'PIT' on, you can still smooth: Here, 'T2D' = +2.5,", .018,0.,0.);
	c_plchhq (c_cfux(.5),c_cfuy(.939),"which causes the smoothing to be done after the mapping.", .018,0.,0.);
/*
 * Advance the frame.
 */
	c_frame();
/*
 * Close GKS.
 */
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
}

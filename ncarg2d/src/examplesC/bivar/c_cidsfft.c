/*
 *	$Id: c_cidsfft.c,v 1.1 1994-05-31 22:30:05 haley Exp $
 */
#include <stdio.h>
#include <math.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define NRAN    30
#define LRWK    3500
#define LIWK    4000
#define MREG    50
#define NREG    50

float xran[NRAN] = {12., 60., 14., 33.,  8., 12., 43., 57., 22., 15.,19.,
                    12., 64., 19., 15., 55., 31., 32., 33., 29., 18., 1.,
                    18., 42., 56.,  9.,  6., 12., 44., 19.};

float yran[NRAN] = { 1.,  2.,  3., 53.,  7., 11., 13., 17., 19., 49.,  1.,
                    31., 37.,  5.,  7., 47., 61., 17.,  5., 23., 29.,  3.,
                     5., 41., 43.,  9., 13., 59.,  1., 67.};

float zran[NRAN] = {1.0, 1.5, 1.7, 1.4, 1.9, 1.0, 1.5, 1.2, 1.8, 1.4, 1.8,
                    1.7, 1.9, 1.5, 1.2, 1.1, 1.3, 1.7, 1.2, 1.6, 1.9, 1.0,
                    1.6, 1.3, 1.4, 1.8, 1.7, 1.5, 1.1, 1.0};

main()
{
	float xreg[MREG], yreg[NREG], zreg[NREG][MREG], rwrk[LRWK];
	int i, iwrk[LIWK];
	float xmin, xmax, ymin, ymax;
	extern void mark();
/*
 * open gks
 */
	c_opngks();
/*
 *  find the min and max data values.
 */
	xmin = 0.0;
	xmax = 65.0;
	ymin =  0.0;
	ymax = 68.0;
/*
 * choose the x and y coordinates for interpolation points on the 
 * regular grid.
 */
	xreg[0] = xmin;
	for( i = 1; i < MREG; i++ ) {
        xreg[i] = xmin + (xmax - xmin)* (float)i/(MREG-1);
	}

	yreg[0] = ymin;
	for( i = 1; i < NREG; i++ ) {
        yreg[i] = ymin + (ymax - ymin)* (float)i/(NREG-1);
	}
/*
 * interpolate data onto a regular grid
 */
	c_idsfft (1,NRAN,xran,yran,zran,MREG,NREG,MREG,xreg,yreg,(float *)zreg,iwrk,rwrk);
/*
 * initialize conpack
 */
	c_cprect((float *)zreg, MREG, MREG, NREG, rwrk, LRWK, iwrk, LIWK);
/*
 * draw perimeter
 */
	c_cpback((float *)zreg, rwrk, iwrk);
/*
 * draw contours
 */
	c_cpcldr((float *)zreg,rwrk,iwrk);
/*
 * mark data points
 */
	mark ();
/*
 * close frame and close gks
 */
	c_frame();
	c_clsgks();

}

void mark()
{
	int i;
	float dum1, dum2, dum3, dum4;
	float xmin, xmax, ymin, ymax;
	int idum5;

	c_getset(&dum1,&dum2,&dum3,&dum4,&xmin,&xmax,&ymin,&ymax,&idum5);
	gset_marker_size(.5);

	for( i = 1; i < NRAN; i++ ) {
		c_points (&xran[i], &yran[i], 1, -4, 0);
	}
	return;
}


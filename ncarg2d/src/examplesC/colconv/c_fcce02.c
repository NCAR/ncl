/*
 *  $Id: c_fcce02.c,v 1.1 1994-07-18 16:20:44 haley Exp $
 */
#include <stdio.h>
#include <math.h>

/*
 * Include function prototypes
 */
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

main()
{
/*
 * PURPOSE                To provide a demonstration of the routines in
 *                        the package COLCONV and to test them.
 *
 * I/O                    If the test is successful, the message
 *
 *                        COLCONV TEST SUCCESSFUL
 *
 *                        is written on unit 6.
 *
 *                        Otherwise, the message
 *
 *                        COLCONV TEST SUCCESSFUL
 *
 *                        is written on unit 6.
 *
 * REQUIRED PACKAGES      COLCONV
 *
 * ALGORITHM              TCOLCV executes six calls to test each of
 *                        the color conversions:
 *
 *                              HLS to RGB
 *                              RGB to HLS
 *                              HSV to RGB
 *                              RGB to HSV
 *                              YIQ to RGB
 *                              RGB to YIQ
 *
 * ---------------------------------------------------------------------
 *
 *  Initialize the error flag.
 */
	int ierr;
	float eps, hue, rlight, satr, red, green, blue;
	float value, y, ri, q;
	extern double fabs();

	ierr = 0;
/*
 *  Set tolerance limit for HLS and HSV tests.
 */
	eps = 0.00001;
/*
 *  HLS to RGB.
 */
	hue    = 120.;
	rlight =  50.;
	satr   = 100.;
	c_hlsrgb(hue,rlight,satr,&red,&green,&blue);
	if ( (fabs(red-1.) > eps) || (fabs(green) > eps) || (fabs(blue) > eps) ) ierr = 1;
/*
 *  RGB to HLS.
 */
	red   = 1.;
	green = 0.;
	blue  = 0.;
	c_rgbhls(red,green,blue,&hue,&rlight,&satr);
	if ( (fabs(hue-120.) > eps) || (fabs(rlight-50.) > eps) || (fabs(satr-100.) > eps) ) ierr = 1;
/*
 *  HSV to RGB.
 */
	hue   = 120.;
	satr  = 1.;
	value = 1.;
	c_hsvrgb(hue,satr,value,&red,&green,&blue);
	if ( (fabs(red-0.) > eps) || (fabs(green-1.) > eps) || (fabs(blue-0.) > eps) ) ierr = 1;
/*
 *  RGB to HSV.
 */
	red   = 0.;
	green = 0.;
	blue  = 1.;
	c_rgbhsv(red,green,blue,&hue,&satr,&value);
	if ( (fabs(hue-240.) > eps) || (fabs(satr-1.) > eps) || (fabs(value-1.) > eps) ) ierr = 1;
/*
 *  Set tolerance limit for YIQ tests.
 */
	eps = 0.01;
/*
 *  YIQ to RGB.
 */
	y = 0.59;
	ri = -.28;
	q = -.52;
	c_yiqrgb(y,ri,q,&red,&green,&blue);
	if ( (fabs(red-0.) > eps) || (fabs(green-1.) > eps) || (fabs(blue-0.) > eps) ) ierr = 1;
/*
 *  RGB to YIQ.
 */
	red   = 1.0;
	green = 1.0;
	blue  = 1.0;
	c_rgbyiq(red,green,blue,&y,&ri,&q);
	if ( (fabs(y-1.) > eps) || (fabs(ri) > eps) || (fabs(q) > eps) ) ierr = 1;

	if (ierr  ==  0) {
        printf( "Colconv test successful\n" );
	}
	else {
        printf( "Colconv test unsuccessful\n" );
	}
}

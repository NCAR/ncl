/*
 *	$Id: c_tcolcv.c,v 1.1 1994-05-13 14:25:33 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>

main()
{
    int ierr;
/*
 * INVOKE DEMO DRIVER
 */
    tcolcv(&ierr);
}
tcolcv (ierr)
int *ierr;
{
/*
 * PURPOSE                To provide a demonstration of the routines in
 *                        the package COLCONV and to test them.
 *
 * USAGE                  CALL TCOLCV (IERR)
 *
 * ARGUMENTS
 *
 * ON OUTPUT              IERR
 *                          An integer variable
 *                          = 0, if the test is successful,
 *                          = 1, otherwise
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
 * PRECISION              Single
 *
 * REQUIRED PACKAGES      COLCONV
 *
 * REQUIRED GKS LEVEL     NONE
 *
 * LANGUAGE               FORTRAN
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
    float r, g, b, h, fl, s, v, eps, y, fi, q;
    *ierr = 0;
/*
 *  Set tolerance limit for HLS and HSV tests.
 */
    eps = 0.00001;
/*
 *  HLS to RGB.
 */
    c_hlsrgb(120.,50.,100.,&r,&g,&b);
    if ( (fabs(r-1.) > eps) || (fabs(g) > eps) || (fabs(b) > eps) ) *ierr = 1;
/*
 *  RGB to HLS.
 */
    c_rgbhls(1.,0.,0.,&h,&fl,&s);
    if ( (fabs(h-120.) > eps) || (fabs(fl-50.) > eps) ||(fabs(s-100.) > eps) ) *ierr = 1;
/*
 *  HSV to RGB.
 */
    c_hsvrgb(120.,1.,1.,&r,&g,&b);
    if ( (fabs(r-0.) > eps) || (fabs(g-1.) > eps) || (fabs(b-0.) > eps) ) *ierr = 1;
/*
 *  rgb to HSV.
 */
    c_rgbhsv(0.,0.,1.,&h,&s,&v);
    if ( (fabs(h-240.) > eps) || (fabs(s-1.) > eps) || (fabs(v-1.) > eps) ) *ierr = 1;
/*
 *  Set tolerance limit for YIQ tests.
 */
    eps = 0.01;
/*
 *  YIQ to RGB.
 */
    c_yiqrgb(.59,-.28,-.52,&r,&g,&b);
    if ( (fabs(r-0.) > eps) || (fabs(g-1.) > eps) || (fabs(b-0.) > eps) ) *ierr = 1;
/*
 *  RGB to YIQ.
 */
    c_rgbyiq(1.,1.,1.,&y,&fi,&q);
    if ( (fabs(y-1.) > eps) || (fabs(fi) > eps) || (fabs(q) > eps) ) *ierr = 1;

    if ( ! *ierr ) {
        printf(" COLCONV TEST SUCCESSFUL\n");
    }
    else {
        printf(" COLCONV TEST UNSUCCESSFUL\n");
    }
    return(1);
}

/*
 *  $Id: c_ftex07d.c,v 1.2 2003-08-07 20:06:08 haley Exp $
 */
#include <stdio.h>
#include <ncarg/ngmath.h>
#include <math.h>

/*
 * This example shows how to use the double precision C routines in
 * Fitgrid. In order to plot this double precision data using NCAR
 * Graphics, you must link with a double precision version of NCAR
 * Graphics.
 */

/*
 *  This program illustrates the use of c_ftcurvs1dp to
 *  interpolate a smoothing tension spline for data
 *  in the plane.
 */

/*
 *  Specify the number of original data points and number of points
 *  in the interpoaltion.
 */
#define IDIM   4
#define IOUT 101

main()
{
/*
 *  Declare arrays.
 */
  double xinc, xo[IOUT], yo[IOUT], xoo[IOUT], yoo[IOUT];
  double d, sigma;
  int   dflg, ier;
 
/*
 *  Specify the original data points in the plane.
 */
  double x[] = { 0.5, -1.5,  0.5,  1.5};
  double y[] = { 1.5,  0.0, -2.5, -1.0};

/*
 *  Specify a uniform observational weight.
 */
  dflg = 1;
  d    = 0.2;

/*
 *  Tension factor.
 */
  c_ftsetr("sigma",1.);

/*
 *  Smoothing factor (larger values result in smoother curves).
 */
  c_ftseti("sf2",1);   /*  Flags use of user-set smoothing and eps.  */
  c_ftsetr("smt",(double) IDIM);

/*
 *  Computational tolerance value.
 */
  c_ftsetr("eps",sqrt(2./(double)IDIM));

/*
 *  Compute a smoothing spline.
 */
  ier = c_ftcurvs1dp(IDIM, x, y, dflg, &d, IOUT, 0., 1., xo, yo);
  if (ier != 0) {
    printf("\nc_ftcurvs1dp - error %d in smoothing spline calculation",ier);
    exit(ier);
  }

/* 
 *  Now use c_ftcurvs1dp to compute an interpolating tension
 *  spline by setting the smoothing parameter to zero.
 */ 
  c_ftsetr("smt",0.);
  c_ftsetr("eps",0.);
  ier = c_ftcurvs1dp(IDIM, x, y, dflg, &d, IOUT, 0., 1., xoo, yoo);
  if (ier != 0) {
    printf("\nc_ftcurvs1dp - error %d in tension spline calculation",ier);
    exit(ier);
  }

}

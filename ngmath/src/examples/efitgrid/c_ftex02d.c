/*
 *  $Id: c_ftex02d.c,v 1.2 2003-08-07 20:06:07 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ngmath.h>

/*
 * This example shows how to use the double precision C routines in
 * Fitgrid. In order to plot this double precision data using NCAR
 * Graphics, you must link with a double precision version of NCAR
 * Graphics.
 */

/*
 *  Example of c_ftkurvpdp and c_ftkurvpidp.
 */

#define IDIM  10
#define IOUT 201

main()
{
  double x[] = { 0.000, 0.210, 0.360, 0.540, 1.000,
                1.500, 1.970, 2.300, 2.500, 2.700};
  double y[] = { 0.000, 2.600, 3.000, 2.500, 0.000,
               -1.000, 0.000, 0.800, 0.920, 0.700};
  double xinc, xo[IOUT], yo[IOUT], yi[IOUT];
  double period = 3., xr = 5., xl = -1.;
  int   i;

/*
 *  Create the output X coordinate array.
 */
  xinc =  (xr-xl)/(IOUT-1);
  for (i = 0; i < IOUT; i++) {
    xo[i] = xl + xinc*i;
  }

/*
 *  Calculate the interpolated values and the integral.
 */
  c_ftcurvpdp(IDIM, x, y, period, IOUT, xo, yo);
  for (i = 0; i < IOUT; i++) {
    c_ftcurvpidp(0., xo[i], period, IDIM, x, y, yi+i);
  }

}

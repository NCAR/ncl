/*
 *  $Id: c_ftex03d.c,v 1.2 2003-08-07 20:06:07 haley Exp $
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
 *  Example of c_ftcurvsdp and c_ftcurvpsdp.
 */

#define IDIM  10
#define IOUT 201

main()
{
  double x[] = { 0.000, 0.210, 0.360, 0.540, 1.000,
                1.500, 1.970, 2.300, 2.500, 2.700};
  double y[] = { 0.000, 2.600, 3.000, 2.500, 0.000,
               -1.000, 0.000, 0.800, 0.920, 0.700};
  double xinc, xo[IOUT], yos[IOUT], yosp[IOUT];
  double p = 3., d, xr = 5., xl = -1.;
  int   i;

/*
 *  Create the output X coordinate array.
 */
  xinc =  (xr-xl)/(IOUT-1);
  for (i = 0; i < IOUT; i++) {
    xo[i] = xl + xinc*i;
  }

/*
 *  Calculate the interpolated values.
 */
  d = 0.3;
  c_ftcurvsdp(IDIM, x, y, 1, &d, IOUT, xo, yos);
/*
 *  Calculate the interpolated values for a periodic function.
 */
  c_ftcurvpsdp(IDIM, x, y, p, 1, &d, IOUT, xo, yosp);

}

/*
 *  $Id: c_ftex01d.c,v 1.2 2003-08-07 20:06:07 haley Exp $
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
 *  Example of c_ftcurvddp, c_ftcurvddp, and c_ftcurvidp.
 */

#define IDIM  11
#define IOUT 201

main()
{
  double x[] = { 0.00,   2.00,   5.00,   8.00,  10.00,  13.00,
               15.00,  18.00,  21.00,  23.00,  30.00};
  double y[] = { 1.00,   0.81,   0.00,  -0.81,  -1.00,  -0.84,
               -0.56,   0.04,   0.73,   1.18,   2.00};
  double xinc, xo[IOUT], yo[IOUT], yd[IOUT], yi[IOUT];
  int   i;

/*
 *  Create the output X coordinate array.
 */
  xinc =  30./(IOUT-1);
  for (i = 0; i < IOUT; i++) {
    xo[i] = xinc * i;
  }

/*
 *  Require that the derivatives of the interpolated curve are
 *  zero at the end points.
 */
  c_ftseti("sf1",   0);
  c_ftsetr("sl1", 0.0);
  c_ftsetr("sln", 0.0);

/*
 *  Calculate the interpolated values, the derivative, and the integral.
 */
  c_ftcurvdp(IDIM, x, y, IOUT, xo, yo);
  c_ftcurvddp(IDIM, x, y, IOUT, xo, yd);
  for (i = 0; i < IOUT; i++) {
    c_ftcurvidp(0., xo[i], IDIM, x, y, yi+i);
  }
}

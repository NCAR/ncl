/*
 *  $Id: c_ftex04d.c,v 1.2 2003-08-07 20:06:08 haley Exp $
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
 *  Example of c_ftkurvdp and c_ftkurvddp.
 */

#define IDIM  11
#define IOUT 201

main()
{
  double x[] = {  3.0,  4.0,  9.0, 16.0, 21.0, 27.0, 
                34.0, 36.0, 34.0, 26.0, 18.0       };
  double y[] = {  2.4,  9.6, 14.4, 12.0,  9.6,  8.4,
                13.2, 21.6, 30.0, 37.2, 38.4       };
  double xinc, xo[IOUT], yo[IOUT], xs[IOUT], ys[IOUT],
              xd[IOUT], yd[IOUT], xdd[IOUT], ydd[IOUT],
              xp[IDIM], yp[IDIM], u[IOUT];
  double tinc;
  int   i;

/*
 *  Set up the array of parameter values where we want to obtain
 *  interpolated values.
 */
  tinc = 1./(IOUT-1.);
  for (i = 0; i < IOUT; i++) {
    u[i] = tinc * (double) i;
  }

/*
 *  Get the interpolated points.
 */
  c_ftkurvdp(IDIM, x, y, IOUT, u, xo, yo);

/*
 *  Get the derivatives (this returns the interpolsted values as well).
 */
  c_ftkurvddp(IDIM, x, y, IOUT, u, xs, ys, xd, yd, xdd, ydd);

}

/*
 *  $Id: c_ftex06d.c,v 1.2 2003-08-07 20:06:08 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ngmath.h>
 
/*
 *  Example of c_ftsurfdp.
 */
 
#define NXI   11
#define NYI   17
#define NXO   31
#define NYO   21
 
main()
{
  double x[NXI],y[NYI],z[NXI][NYI],xo[NXO],yo[NYO],zo[NXO][NYO];
  double *surface;
  int   i,j, ier;
 
/*
 *  Define the input surface.
 */
  for (i = 0; i < NXI; i++) {
    x[i] = (double) i / (double) (NXI-1);
    for (j = 0; j < NYI; j++) {
      y[j] = (double) j / (double) (NYI-1);
      z[i][j] = 0.5 + 0.25*sin(-7.*x[i]) + 0.25*cos(5.*y[j]);
    }
  }

/*
 *  Set up the output arrays.
 */
  for (i = 0; i < NXO; i++) {
    xo[i] = (double) i / (double) (NXO-1) ;
    for (j = 0; j < NYO; j++) {
      yo[j] = (double) j / (double) (NYO-1);
    }
  }

/*
 *  Get the interpolated surface.
 */
  surface = c_ftsurfdp(NXI, NYI, x, y, &z[0][0], NXO, NYO, xo, yo, &ier);
/*
 * This example shows how to use the double precision C routines in
 * Fitgrid. In order to plot this double precision data using NCAR
 * Graphics, you must link with a double precision version of NCAR
 * Graphics.
 */

}

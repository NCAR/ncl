/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU General Public License as published     *
* by the Free Software Foundation; either version 2 of the License, or  *
* (at your option) any later version.                                   *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* General Public License for more details.                              *
*                                                                       *
* You should have received a copy of the GNU General Public License     *
* along with this software; if not, write to the Free Software         *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

#include <ncarg/ngmath.h>
#include "dstypes.h"
#include "dsproto.h"
#include "dsuhead.h"

/*
 *  Interpolate randomly-spaced 2D input data to a regularly spaced grid.
 */
double *c_dsgrid2d(int n, double x[], double y[], double u[],
                  int nx, int ny, double xo[], double yo[], int *ier)
/*
 *    Arguments
 *    ---------
 *        n - The number of input data points.
 *        x - An array of X coordinate values of the input data points.
 *        y - An array of Y coordinate values of the input data points.
 *        u - The functional value at coordinate (x,y).
 *       nx - The dimension of the array xo containing the X coordinate 
 *            values for the output grid.
 *       ny - The dimension of the array yo containing the Y coordinate 
 *            values for the output grid.
 *       xo - The array containing the X coordinate values for the output 
 *            grid (must be monotone increasing, but need not be equally 
 *            spaced.
 *       yo - The array containing the Y coordinate values for the output 
 *            grid (must be monotone increasing, but need not be equally 
 *            spaced.
 *     *ier - An error return value ( = 0 is no error).
 *
 *   Return value
 *   ------------
 *      A pointer to the first element of a linear array that is
 *      laid out as a 2D array (i.e. the last subscript varies fastest)
 *      of dimension: nx by ny.
 */
{
  
  static double perror = 1.;
  double zo[1] = {0.}, *z, *retval;
  
  z = (double *) calloc(n, sizeof(double));
  if (z == NULL) {
    DSErrorHnd(13, "c_dsgrid2d", stderr, "\n");
    *ier = ds_error_status;
    return(&perror);
  }

  retval = c_dsgrid3d(n, x, y, z, u, nx, ny, 1, xo, yo, zo, ier);

  free(z);
  return(retval);
}

void c_dspnt2d(int n, double xi[], double yi[], double zi[],
             int m, double xo[], double yo[], double zo[], int *ier)
{
  int    i;
  double xt[1], yt[1];

  for (i = 0; i < m; i++) {
    xt[0] = xo[i];
    yt[0] = yo[i];
    zo[i] = *c_dsgrid2d(n, xi, yi, zi, 1, 1, xt, yt, ier);
  }
}

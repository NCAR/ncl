/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

#include <ncarg/ngmath.h>
#include "dstypes.h"
#include "dsproto.h"
#include "dsuhead.h"

/*
 *  Interpolate randomly-spaced 2D input data to a regularly spaced grid.
 *  Single precision version.
 */
float *c_dsgrid2s(int n, float x[], float y[], float u[],
                  int nx, int ny, float xo[], float yo[], int *ier)
{
  static float perror = 1.;
  float zo[1] = {0.}, *z, *retval;
  
  z = (float *) calloc(n, sizeof(float));
  if (z == NULL) {
    DSErrorHnd(13, "c_dsgrid2s", stderr, "\n");
    *ier = ds_error_status;
    return(&perror);
  }

  retval = c_dsgrid3s(n, x, y, z, u, nx, ny, 1, xo, yo, zo, ier);

  free(z);
  return(retval);
}
void c_dspnt2s(int n, float xi[], float yi[], float zi[],
               int m, float xo[], float yo[], float zo[], int *ier)
{
  int    i;
  float  xt[1], yt[1];

  for (i = 0; i < m; i++) {
    xt[0] = xo[i];
    yt[0] = yo[i];
    zo[i] = *c_dsgrid2s(n, xi, yi, zi, 1, 1, xt, yt, ier);
  }
}

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>
#include <ncarg/ngmath.h>

#define NUM 1000
#define NX    21
#define NY    21
#define NZ    21
#define IWTYPE 1
#define WKID   1

main()
{
  int  i, j, k, ier;
  double xi[NUM], yi[NUM], zi[NUM], u[NUM];
  double xo[NX], yo[NY], zo[NZ], *output;
  float  xp[NX], yp[NY], zp[NZ], outp[NZ][NY][NX];
  double xmin = -2.0, ymin = -2.0, zmin = -2.0;
  double xmax =  2.0, ymax =  2.0, zmax =  2.0;

/*
 *  Create random data in three space and define a function.
 */
  for (i = 0; i < NUM; i++) {
    xi[i] = xmin+(xmax-xmin)*((double) rand() / (double) RAND_MAX);
    yi[i] = ymin+(ymax-ymin)*((double) rand() / (double) RAND_MAX);
    zi[i] = zmin+(zmax-zmin)*((double) rand() / (double) RAND_MAX);
     u[i] = xi[i]*xi[i] + yi[i]*yi[i] + zi[i]*zi[i];
  }

/*
 *  Create the output grid.
 */
  for (i = 0; i < NX; i++) {
    xo[i] = xmin + ( (double) i / (double) (NX-1)) * (xmax-xmin);
  }
  for (j = 0; j < NY; j++) {
    yo[j] = ymin + ( (double) j / (double) (NY-1)) * (ymax-ymin);
  }
  for (k = 0; k < NZ; k++) {
    zo[k] = zmin + ( (double) k / (double) (NZ-1)) * (zmax-zmin);
  }

/*
 *  Interpolate.
 */
  output = c_dsgrid3d(NUM, xi, yi, zi, u, NX, NY, NZ, xo, yo, zo, &ier);
  if (ier != 0) {
    printf(" Error %d returned from c_dsgrid3s\n",ier);
    exit(1);
  }

/*
 *  Plot an isosurface, converting to single precision arrays first.
 *  Also, rearrange the output array, since dsgrid3d returns its
 *  array in column dominate order.
 */
  for (i = 0; i < NX; i++) {
    xp[i] = xo[i];
    for (j = 0; j < NY; j++) {
      yp[i] = yo[i];
      for (k = 0; k < NZ; k++) {
        zp[i] = zo[i];
        outp[k][j][i] = output[i*NY*NZ + j*NY + k];
      }
    }
  }

  gopen_gks ("stdout",0);
  gopen_ws (WKID, NULL, IWTYPE);
  gactivate_ws(WKID);

  c_tdez3d(NX, NY, NZ, xp, yp, zp, &outp[0][0][0], 3.0, 2., -35., 65., 6);

  gdeactivate_ws(WKID);
  gclose_ws(WKID);
  gclose_gks();
}

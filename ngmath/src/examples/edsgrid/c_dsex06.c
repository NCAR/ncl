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
  float xi[NUM], yi[NUM], zi[NUM], u[NUM];
  float xo[NX], yo[NY], zo[NZ], output[NX][NY][NZ], outr[NZ][NY][NX];
  float xmin = -2.0, ymin = -2.0, zmin = -2.0;
  float xmax =  2.0, ymax =  2.0, zmax =  2.0;

/*
 *  Create random data in three space and define a function.
 */
  for (i = 0; i < NUM; i++) {
    xi[i] = xmin+(xmax-xmin)*((float) rand() / (float) RAND_MAX);
    yi[i] = ymin+(ymax-ymin)*((float) rand() / (float) RAND_MAX);
    zi[i] = zmin+(zmax-zmin)*((float) rand() / (float) RAND_MAX);
     u[i] = xi[i]*xi[i] + yi[i]*yi[i] + zi[i]*zi[i];
  }

/*
 *  Create the output grid.
 */
  for (i = 0; i < NX; i++) {
    xo[i] = xmin + ( (float) i / (float) (NX-1)) * (xmax-xmin);
  }
  for (j = 0; j < NY; j++) {
    yo[j] = ymin + ( (float) j / (float) (NY-1)) * (ymax-ymin);
  }
  for (k = 0; k < NZ; k++) {
    zo[k] = zmin + ( (float) k / (float) (NZ-1)) * (zmax-zmin);
  }

/*
 *  Interpolate a single point at a time.
 */
  for (i = 0; i < NX; i++) {
    for (j = 0; j < NY; j++) {
      for (k = 0; k < NZ; k++) {
        c_dspnt3s(NUM,xi,yi,zi,u,1,&xo[i],&yo[j],&zo[k],
                  &output[i][j][k],&ier);
        if (ier != 0) {
          printf(" Error %d returned from c_dspnt3s\n",ier);
          exit(1);
        }
      }
    }
  }

/*
 *  Plot an isosurface.
 */
  gopen_gks ("stdout",0);
  gopen_ws (WKID, NULL, IWTYPE);
  gactivate_ws(WKID);

/*
 *  Reorder the array elements for c_tdez3d, since c_dsgrid3s
 *  returns an array in column dominate order.
 */
  for (i = 0; i < NX; i++) {
    for (j = 0; j < NY; j++) {
      for (k = 0; k < NZ; k++) {
        outr[k][j][i] = output[i][j][k];
      }
    }
  }
  c_tdez3d(NX, NY, NZ, xo, yo, zo, &outr[0][0][0], 3.0, 2., -35., 65., 6);
  c_frame();

  gdeactivate_ws(WKID);
  gclose_ws(WKID);
  gclose_gks();
}

/*
 *  $Id: c_ftex06.c,v 1.3 1998-06-24 23:40:20 fred Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>
#include <ncarg/ngmath.h>
 
/*
 *  Example of c_ftsurf.
 */
 
#define NXI   11
#define NYI   17
#define NXO   31
#define NYO   21
 
#define IWTYPE 1
#define WKID   1
 
main()
{
  float x[NXI],y[NYI],z[NXI][NYI],xo[NXO],yo[NYO],zo[NXO][NYO];
  float outr[NYO][NXO];
  float *surface;
  int   i,j, ier;
 
/*
 *  Define the input surface.
 */
  for (i = 0; i < NXI; i++) {
    x[i] = (float) i / (float) (NXI-1);
    for (j = 0; j < NYI; j++) {
      y[j] = (float) j / (float) (NYI-1);
      z[i][j] = 0.5 + 0.25*sin(-7.*x[i]) + 0.25*cos(5.*y[j]);
    }
  }

/*
 *  Set up the output arrays.
 */
  for (i = 0; i < NXO; i++) {
    xo[i] = (float) i / (float) (NXO-1) ;
    for (j = 0; j < NYO; j++) {
      yo[j] = (float) j / (float) (NYO-1);
    }
  }

/*
 *  Get the interpolated surface.
 */
  surface = c_ftsurf(NXI, NYI, x, y, &z[0][0], NXO, NYO, xo, yo, &ier);

/*
 *  Reverse the array indices for plotting with tdez2d, since
 *  c_dsgrid returns its array in column dominate order.
 */
  for (i = 0; i < NXO; i++) {
    for (j = 0; j < NYO; j++) {
      outr[j][i] = surface[i*NYO+j];
    }
  }

/*
 *  Draw plot.
 */
  gopen_gks ("stdout",0);
  gopen_ws (WKID, NULL, IWTYPE);
  gactivate_ws(WKID);

  c_tdez2d(NXO, NYO, xo, yo, &outr[0][0], 3., 36., 67., -6);
  c_frame();
 
  gdeactivate_ws(WKID);
  gclose_ws(WKID);
  gclose_gks();


}

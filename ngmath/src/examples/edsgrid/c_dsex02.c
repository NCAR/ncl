#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>
#include <ncarg/ngmath.h>

#define NUM    6
#define NX    61
#define NY    61
#define IWTYPE 1
#define WKID   1

main()
{
  int  i, j, k, ier;
  float xi[] = {0.00, 1.00, 0.00, 1.00, 0.40, 0.75};
  float yi[] = {0.00, 0.00, 1.00, 1.00, 0.20, 0.65};
  float zi[] = {0.00, 0.00, 0.00, 0.00, 1.25, 0.80};
  float xo[NX], yo[NY], xinc, yinc, *output, outr[NY][NX];
  float rho =  3., theta = -45., phi =  55.;

/*
 *  Create the output grid.
 */
  xinc = 1./ (float) (NX-1); 
  yinc = 1./ (float) (NY-1); 
  for (i = 0; i < NX; i++) {
    xo[i] = xinc * (float) i;
  }
  for (j = 0; j < NY; j++) {
    yo[j] = yinc * (float) j;
  }

/*
 *  Exponent equals 0.5
 */
  c_dssetr("exp", 0.5);
  output = c_dsgrid2s(NUM, xi, yi, zi, NX, NY, xo, yo, &ier);
  if (ier != 0) {
    printf(" Error %d returned from c_dsgrid2s\n",ier);
    exit(1);
  }
  gopen_gks ("stdout",0);
  gopen_ws (WKID, NULL, IWTYPE);
  gactivate_ws(WKID);
/*
 *  Reverse the array indices for plotting with tdez2d, since
 *  c_dsgrid returns its array in column dominate order.
 */
  for (i = 0; i < NX; i++) {
    for (j = 0; j < NY; j++) {
      outr[j][i] = output[i*NY+j];
    }
  }
  c_tdez2d(NX, NY, xo, yo, &outr[0][0], rho, theta, phi, 6);
  c_frame();

/*
 *  Exponent equals 1.0
 */
  c_dssetr("exp", 1.0);
  output = c_dsgrid2s(NUM, xi, yi, zi, NX, NY, xo, yo, &ier);
  if (ier != 0) {
    printf(" Error %d returned from c_dsgrid2s\n",ier);
    exit(1);
  }
/*
 *  Reverse the array indices for plotting with tdez2d, since
 *  c_dsgrid returns its array in column dominate order.
 */
  for (i = 0; i < NX; i++) {
    for (j = 0; j < NY; j++) {
      outr[j][i] = output[i*NY+j];
    }
  }
  c_tdez2d(NX, NY, xo, yo, &outr[0][0], rho, theta, phi, 6);
  c_frame();

/*
 *  Exponent equals 5.0
 */
  c_dssetr("exp", 5.0);
  output = c_dsgrid2s(NUM, xi, yi, zi, NX, NY, xo, yo, &ier);
  if (ier != 0) {
    printf(" Error %d returned from c_dsgrid2s\n",ier);
    exit(1);
  }
/*
 *  Reverse the array indices for plotting with tdez2d, since
 *  c_dsgrid returns its array in column dominate order.
 */
  for (i = 0; i < NX; i++) {
    for (j = 0; j < NY; j++) {
      outr[j][i] = output[i*NY+j];
    }
  }
  c_tdez2d(NX, NY, xo, yo, &outr[0][0], rho, theta, phi, 6);
  c_frame();

  gdeactivate_ws(WKID);
  gclose_ws(WKID);
  gclose_gks();
}

#include <stdio.h>
#include <stddef.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>
#include <ncarg/ngmath.h>

#define NX    21
#define NY    21
#define IWTYPE 1
#define WKID   1

main()
{
  int  i, j, k, ier;
  float xi[] = {0.00, 1.00, 0.00, 1.00, 0.30, 0.30, 0.30, 0.69,
                0.71, 0.71, 0.69, 0.70, 0.70, 0.70, 0.69, 0.71};
  float yi[] = {0.00, 0.00, 1.00, 1.00, 0.70, 0.30, 0.70, 0.69,
                0.71, 0.71, 0.69, 0.70, 0.70, 0.70, 0.69, 0.71};
  float zi[] = {0.00, 0.00, 0.00, 0.50, 0.50, 0.50, 0.50, 1.00,
                1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00};
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
 *  Set the shadowing flag.
 */
  c_dsseti("shd", 1);
  output = c_dsgrid2s(sizeof(xi)/sizeof(xi[0]), xi, yi, zi, 
                                            NX, NY, xo, yo, &ier);
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

  gdeactivate_ws(WKID);
  gclose_ws(WKID);
  gclose_gks();
}

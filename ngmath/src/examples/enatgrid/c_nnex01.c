#include <stdio.h>
#include <stdlib.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>
#include <ncarg/ngmath.h>

void drwsrfc (int, int, float *, float *, float *, float, float, float, int *);

#define NUMIN 6
#define NUMXOUT 21
#define NUMYOUT 21
#define IWTYPE 1
#define WKID   1

main()
{
  int i, j, ier, iert, *iwk;

  float x[] = {0.00, 1.00, 0.00, 1.00, 0.40, 0.75},
        y[] = {0.00, 0.00, 1.00, 1.00, 0.20, 0.65},
        z[] = {0.00, 0.00, 0.00, 0.00, 1.25, 0.80};
  float *out, xo[NUMXOUT], yo[NUMYOUT], xc, yc;

  Gcolr_rep rgb;

  iwk = (int *) calloc(2*NUMXOUT*NUMYOUT,sizeof(float));

  xc = 1./(NUMXOUT-1.);
  for (i = 0 ; i < NUMXOUT ; i++) {
    xo[i] = i * xc;
  }

  yc = 1./(NUMYOUT-1.);
  for (j = 0 ; j < NUMYOUT ; j++) {
    yo[j] = j * yc;
  }

  out = c_natgrids(NUMIN, x, y, z, NUMXOUT, NUMYOUT, xo, yo, &ier);
  if (ier != 0) {
     printf (" Error return from c_natgrids = %d\n",ier);
  }

/*
 *  Draw the surface plot.
 */

/*
 * open gks
 */
  gopen_gks ("stdout",0);
  gopen_ws (WKID, NULL, IWTYPE);
  gactivate_ws(WKID);

  rgb.rgb.red = rgb.rgb.green = rgb.rgb.blue = 1.;
  gset_colr_rep(WKID,0,&rgb);
  rgb.rgb.red = rgb.rgb.green = rgb.rgb.blue = 0.;
  gset_colr_rep(WKID,1,&rgb);

  drwsrfc (NUMXOUT, NUMYOUT, xo, yo, out, 15.,-25.,90., iwk);

  gdeactivate_ws(WKID);
  gclose_ws(WKID);
  gclose_gks();

}

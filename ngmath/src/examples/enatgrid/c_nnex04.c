#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>
#include <ncarg/ngmath.h>

void drwsrfc (int, int, float *, float *, float *, float, float, float, int *);
void drwvctc (int, int, float *, float*);
void drwconc (int, int, float *);

#define NUMIN 171
#define NUMXOUT 21
#define NUMYOUT 21
#define RAD2DEG 57.29578
#define IWTYPE 1
#define WKID   1

main()
{
  int i, j, ier, iert, *iwk;

/*
 *  Coordinate data are defined as random numbers between
 *  -0.2 and 1.2. and are explicitly defined here for uniformity
 *  across platforms.
 */
float x[] = {
             1.16,  0.47,  0.29,  0.72,  0.52,  1.12,  0.33,  0.20,  0.30,
             0.78,  0.92,  0.52,  0.44,  0.22, -0.10,  0.11,  0.59,  1.13,
             0.68,  1.11,  0.93,  0.29,  0.74,  0.43,  0.87,  0.87, -0.10,
             0.26,  0.85,  0.00, -0.02,  1.01, -0.12,  0.65,  0.39,  0.96,
             0.39,  0.38,  0.94, -0.03, -0.17,  0.00,  0.03,  0.67, -0.06,
             0.82, -0.03,  1.08,  0.37,  1.02, -0.11, -0.13,  1.03,  0.61,
             0.26,  0.18,  0.62,  0.42,  1.03,  0.72,  0.97,  0.08,  1.18,
             0.00,  0.69,  0.10,  0.80,  0.06,  0.82,  0.20,  0.46,  0.37,
             1.16,  0.93,  1.09,  0.96,  1.00,  0.80,  0.01,  0.12,  1.01,
             0.48,  0.79,  0.04,  0.42,  0.48, -0.18,  1.16,  0.85,  0.97,
             0.14,  0.40,  0.78,  1.12,  1.19,  0.68,  0.65,  0.41,  0.90,
             0.84, -0.11, -0.01, -0.02, -0.10,  1.04,  0.58,  0.61,  0.12,
            -0.02, -0.03,  0.27,  1.17,  1.02,  0.16, -0.17,  1.03,  0.13,
             0.04, -0.03,  0.15,  0.00, -0.01,  0.91,  1.20,  0.54, -0.14,
             1.03,  0.93,  0.42,  0.36, -0.10,  0.57,  0.22,  0.74,  1.15,
             0.40,  0.82,  0.96,  1.09,  0.42,  1.13,  0.24,  0.51,  0.60,
             0.06,  0.38,  0.15,  0.59,  0.76,  1.16,  0.02,  0.86,  1.14,
             0.37,  0.38,  0.26,  0.26,  0.07,  0.87,  0.90,  0.83,  0.09,
             0.03,  0.56, -0.19,  0.51,  1.07, -0.13,  0.99,  0.84,  0.22
            };
float y[] = {
            -0.11,  1.07,  1.11, -0.17,  0.08,  0.09,  0.91,  0.17, -0.02,
             0.83,  1.08,  0.87,  0.46,  0.66,  0.50, -0.14,  0.78,  1.08,
             0.65,  0.00,  1.03,  0.06,  0.69, -0.16,  0.02,  0.59,  0.19,
             0.54,  0.68,  0.95,  0.30,  0.77,  0.94,  0.76,  0.56,  0.12,
             0.05, -0.07,  1.01,  0.61,  1.04, -0.07,  0.46,  1.07,  0.87,
             0.11,  0.63,  0.06,  0.53,  0.95,  0.78,  0.48,  0.45,  0.77,
             0.78,  0.29,  0.38,  0.85, -0.10,  1.17,  0.35,  1.14, -0.04,
             0.34, -0.18,  0.78,  0.17,  0.63,  0.88, -0.12,  0.58, -0.12,
             1.00,  0.99,  0.45,  0.86, -0.15,  0.97,  0.99,  0.90,  0.42,
             0.61,  0.74,  0.41,  0.44,  1.08,  1.06,  1.18,  0.89,  0.74,
             0.74, -0.06,  0.00,  0.99,  0.03,  1.00, -0.04,  0.24,  0.65,
             0.12,  0.13, -0.09, -0.05,  1.03,  1.07, -0.02,  1.18,  0.19,
             0.03, -0.03,  0.86,  1.12,  0.38,  0.72, -0.20, -0.08, -0.18,
             0.32,  0.13, -0.19,  0.93,  0.81,  0.31,  1.09, -0.03,  1.01,
            -0.17,  0.84, -0.11,  0.45,  0.18,  0.23,  0.81,  0.39,  1.09,
            -0.05,  0.58,  0.53,  0.96,  0.43,  0.48,  0.96, -0.03,  1.13,
             1.16,  0.16,  1.15,  0.57,  0.13,  0.71,  0.35,  1.04,  0.62,
             1.03,  0.98,  0.31,  0.70,  0.97,  0.87,  1.14,  0.08,  1.19,
             0.88,  1.00,  0.51,  0.03,  0.17,  1.01,  0.44,  0.17, -0.11
            };

  float z[NUMIN];
  float *out, xo[NUMXOUT], yo[NUMYOUT], xc, yc;
  float u[NUMXOUT][NUMYOUT], v[NUMXOUT][NUMYOUT], uvtmp;
  float xmin,xmax,xinc,ymin,ymax,yinc;

  Gcolr_rep rgb;

  iwk = (int *) calloc(2*NUMXOUT*NUMYOUT,sizeof(int));

  for (i = 0 ; i < NUMIN ; i++) {
    z[i] = (x[i]-0.25)*(x[i]-0.25) + (y[i]-0.50)*(y[i]-0.50);
  }

  xmin = -0.3;
  xmax =  1.3;
  xinc = (xmax-xmin)/(NUMXOUT-1.);
  for (i = 0 ; i < NUMXOUT ; i++) {
    xo[i] = xmin+(float)i * xinc;
  }

  ymin = -0.3;
  ymax =  1.3;
  yinc = (ymax-ymin)/(NUMYOUT-1.);
  for (i = 0 ; i < NUMYOUT ; i++) {
    yo[i] = ymin+(float)i * yinc;
  }

/*
 *  Indicate that extrapolation is not to be done and specify
 *  the function value for points outside the convex hull.
 */
  c_nnseti("ext",0);
  c_nnsetr("nul",-1.);

/*
 *  Do the interpolation.
 */
  out = c_natgrids(NUMIN, x, y, z, NUMXOUT, NUMYOUT, xo, yo, &ier);
  if (ier != 0) {
     printf (" Error return from c_natgrids = %d\n",ier);
  }

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

/*
 *  Draw the surface plot.
 */
  drwsrfc (NUMXOUT, NUMYOUT, xo, yo, out, 15.,-25.,90., iwk);

  gdeactivate_ws(WKID);
  gclose_ws(WKID);
  gclose_gks();

}

#include "ftgvars.h"
#include "ftproto.h"
#include <math.h>

int c_ftcurv(int n, float xi[], float yi[], int m, float xo[], float yo[])
{
  float *yp, *temp;
  int   i;

  yp   = (float *) calloc(n, sizeof(float));
  temp = (float *) calloc(n, sizeof(float));
  
  NGCALLF(curv1,CURV1)(&n, xi, yi, &ft_slp1, &ft_slpn, &ft_islp, 
          yp, temp, &ft_sigma, &ft_err);
  NGCALLF(fcurv2,FCURV2)(&n, xi, yi, yp, &ft_sigma, &m, xo, yo);

  free(yp);
  free(temp);

  return(ft_err);
}

int c_ftcurvd(int n, float xi[], float yi[], int m, float xo[], float yo[])
{
  float *yp, *temp;
  int   i;

  yp   = (float *) calloc(n, sizeof(float));
  temp = (float *) calloc(n, sizeof(float));
  
  NGCALLF(curv1,CURV1)(&n, xi, yi, &ft_slp1, &ft_slpn, &ft_islp, 
          yp, temp, &ft_sigma, &ft_err);
  NGCALLF(fcurvd,FCURVD)(&n, xi, yi, yp, &ft_sigma, &m, xo, yo);

  free(yp);
  free(temp);

  return(ft_err);
}

int c_ftcurvi(float xl, float xr, int n, float xi[], float yi[], 
              float *integral)
{
  float *yp, *temp;
  int   i;

  yp   = (float *) calloc(n, sizeof(float));
  temp = (float *) calloc(n, sizeof(float));
  
  NGCALLF(curv1,CURV1)(&n, xi, yi, &ft_slp1, &ft_slpn, &ft_islp, 
          yp, temp, &ft_sigma, &ft_err);
  NGCALLF(fcurvi,FCURVI)(&xl, &xr, &n, xi, yi, yp, &ft_sigma, integral);

  free(yp);
  free(temp);

  return(ft_err);
}

int c_ftcurvp(int n, float xi[], float yi[], float p, 
              int m, float xo[], float yo[])
{
  float *yp, *temp;
  int   i;

  yp   = (float *) calloc(n, sizeof(float));
  temp = (float *) calloc(2*n, sizeof(float));
  
  NGCALLF(curvp1,CURVP1)(&n, xi, yi, &p, yp, temp, &ft_sigma, &ft_err);
  NGCALLF(fcurvp2,FCURVP2)(&n, xi, yi, yp, &p, &ft_sigma, &m, xo, yo);

  free(yp);
  free(temp);

  return(ft_err);
}

int c_ftcurvpi(float xl, float xr, float p, int n, float xi[], float yi[], 
              float *integral)
{
  float *yp, *temp;
  int   i;

  yp   = (float *) calloc(n, sizeof(float));
  temp = (float *) calloc(2*n, sizeof(float));
  
  NGCALLF(curvp1,CURVP1)(&n, xi, yi, &p, yp, temp, &ft_sigma, &ft_err);
  NGCALLF(fcurvpi,FCURVPI)(&xl, &xr, &p, &n, xi, yi, yp, &ft_sigma, integral);

  free(yp);
  free(temp);

  return(ft_err);
}

int c_ftcurvs(int n, float xi[], float yi[], int dflg, float d[],
              int m, float xo[], float yo[])
{
  float *ys, *ysp, *temp, smths, smeps;
  int   i;

/*
 *  Establish the values for the parameters S, EPS, and D.
 */

  if (ft_sms == 0) {
    smths = (float) n;
    smeps = (float) sqrt( (float) (2./smths) );
  }
  else if (ft_sms == 1) {
    smths = ft_s;
    smeps = ft_eps;
  }
  else if (ft_sms == 2) {
    smths = ft_s;
    smeps = (float) sqrt( (float) (2./((float) n)) );
  }
  else if (ft_sms == 3) {
    smths = (float) n;
    smeps = ft_eps;
  }

  ys   = (float *) calloc(n, sizeof(float));
  ysp  = (float *) calloc(n, sizeof(float));
  temp = (float *) calloc(9*n, sizeof(float));
  
  NGCALLF(curvs,CURVS)(&n, xi, yi, d, &dflg, &smths, &smeps, ys, ysp,
                       &ft_sigma, temp, &ft_err);
  NGCALLF(fcurv2,FCURV2)(&n, xi, ys, ysp, &ft_sigma, &m, xo, yo);

  free(ys);
  free(ysp);
  free(temp);

  return(ft_err);
}

int c_ftcurvps(int n, float xi[], float yi[], float p, int dflg, float d[],
               int m, float xo[], float yo[])
{
  float *ys, *ysp, *temp, smths, smeps;
  int   i;

/*
 *  Establish the values for the parameters S, EPS, and D.
 */

  if (ft_sms == 0) {
    smths = (float) n;
    smeps = (float) sqrt( (float) (2./smths) );
  }
  else if (ft_sms == 1) {
    smths = ft_s;
    smeps = ft_eps;
  }
  else if (ft_sms == 2) {
    smths = ft_s;
    smeps = (float) sqrt( (float) (2./((float) n)) );
  }
  else if (ft_sms == 3) {
    smths = (float) n;
    smeps = ft_eps;
  }

  ys   = (float *) calloc(n, sizeof(float));
  ysp  = (float *) calloc(n, sizeof(float));
  temp = (float *) calloc(11*n, sizeof(float));
  
  NGCALLF(curvps,CURVPS)(&n, xi, yi, &p, d, &dflg, &smths, &smeps, ys, ysp,
                       &ft_sigma, temp, &ft_err);
  NGCALLF(fcurvp2,FCURVP2)(&n, xi, ys, ysp, &p, &ft_sigma, &m, xo, yo);

  free(ys);
  free(ysp);
  free(temp);

  return(ft_err);
}

int c_ftkurv(int n, float xi[], float yi[], int m, float t[], 
             float xo[], float yo[])
{
  float *s, *xp, *yp, *temp;

  s    = (float *) calloc(n, sizeof(float));
  xp   = (float *) calloc(n, sizeof(float));
  yp   = (float *) calloc(n, sizeof(float));
  temp = (float *) calloc(n, sizeof(float));
  
  NGCALLF(kurv1,KURV1)(&n, xi, yi, &ft_slp1, &ft_slpn, &ft_islp,
                       xp, yp, temp, s, &ft_sigma, &ft_err);
  NGCALLF(fkurv2,FKURV2)(&n, xi, yi, &m, t, xo, yo, xp, yp, s, &ft_sigma);

  free(s);
  free(xp);
  free(yp);
  free(temp);

  return(ft_err);
}

int c_ftkurvp(int n, float xi[], float yi[], int m, float t[], 
             float xo[], float yo[])
{
  float *s, *xp, *yp, *temp;

  s    = (float *) calloc(n, sizeof(float));
  xp   = (float *) calloc(n, sizeof(float));
  yp   = (float *) calloc(n, sizeof(float));
  temp = (float *) calloc(2*n, sizeof(float));
  
  NGCALLF(kurvp1,KURVP1)(&n, xi, yi, xp, yp, temp, s, &ft_sigma, &ft_err);
  NGCALLF(fkurvp2,FKURVP2)(&n, xi, yi, &m, t, xo, yo, xp, yp, s, &ft_sigma);

  free(s);
  free(xp);
  free(yp);
  free(temp);

  return(ft_err);
}

int c_ftkurvd(int n, float xi[], float yi[], int m, float t[], 
             float xo[], float yo[], float xd[], float yd[],
             float xdd[], float ydd[])
{
  float *s, *xp, *yp, *temp;

  s    = (float *) calloc(n, sizeof(float));
  xp   = (float *) calloc(n, sizeof(float));
  yp   = (float *) calloc(n, sizeof(float));
  temp = (float *) calloc(n, sizeof(float));
  
  NGCALLF(kurv1,KURV1)(&n, xi, yi, &ft_slp1, &ft_slpn, &ft_islp,
                       xp, yp, temp, s, &ft_sigma, &ft_err);
  NGCALLF(fkurvd,FKURVD)(&n, xi, yi, &m, t, xo, yo, xd, yd, xdd, ydd,
                         xp, yp, s, &ft_sigma);

  free(s);
  free(xp);
  free(yp);
  free(temp);

  return(ft_err);
}

int c_ftkurvpd(int n, float xi[], float yi[], int m, float t[], 
              float xo[], float yo[], float xd[], float yd[],
              float xdd[], float ydd[])
{
  float *s, *xp, *yp, *temp;

  s    = (float *) calloc(n, sizeof(float));
  xp   = (float *) calloc(n, sizeof(float));
  yp   = (float *) calloc(n, sizeof(float));
  temp = (float *) calloc(2*n, sizeof(float));
  
  NGCALLF(kurvp1,KURVP1)(&n, xi, yi, xp, yp, temp, s, &ft_sigma, &ft_err);
  NGCALLF(fkurvpd,FKURVPD)(&n, xi, yi, &m, t, xo, yo, xd, yd, xdd, ydd,
                           xp, yp, s, &ft_sigma);

  free(s);
  free(xp);
  free(yp);
  free(temp);

  return(ft_err);
}

float *c_ftsurf(int mi, int ni, float *xi, float *yi, float *zi,
              int mo, int no, float *xo, float *yo, int *ier)
{
  int   i, j, sflag;
  float *zp, *temp, *fz, *output;

  fz   = (float *) calloc(mi*ni, sizeof(float));
  zp   = (float *) calloc(3*mi*ni, sizeof(float));
  temp = (float *) calloc(2*ni+mi, sizeof(float));

/*
 *  Rearrange the zi array, since surf1 expects an array ordered as 
 *  per Fortran.
 */
  for (i = 0; i < mi; i++) {
    for (j = 0; j < ni; j++) {
      fz[j*mi+i] = zi[i*ni+j];
    }
  }
  sflag = 0;
  if (ft_df1 != 0) {
    ft_zx1.size = ni;
    ft_zx1.data = (float *) calloc(ni, sizeof(float));
    sflag = sflag + 1;
  }
  if (ft_df2 != 0) {
    ft_zxm.size = ni;
    ft_zxm.data = (float *) calloc(ni, sizeof(float));
    sflag = sflag + 2;
  }
  if (ft_df3 != 0) {
    ft_zy1.size = mi;
    ft_zy1.data = (float *) calloc(mi, sizeof(float));
    sflag = sflag + 4;
  }
  if (ft_df4 != 0) {
    ft_zyn.size = mi;
    ft_zyn.data = (float *) calloc(mi, sizeof(float));
    sflag = sflag + 8;
  }
  if (ft_df5 != 0) {
    sflag = sflag + 16;
  }
  if (ft_df6 != 0) {
    sflag = sflag + 32;
  }
  if (ft_df7 != 0) {
    sflag = sflag + 64;
  }
  if (ft_df8 != 0) {
    sflag = sflag + 128;
  }
  
  NGCALLF(surf1,SURF1)(&mi, &ni, xi, yi, fz, &mi, 
          ft_zx1.data, ft_zxm.data, ft_zy1.data, ft_zyn.data,
          &ft_z11, &ft_zm1, &ft_z1n, &ft_zmn, &sflag,
          zp, temp, &ft_sigma, &ft_err);

/*
 *  Call surf2, reversing the ordering of the output array.
 */
  output = (float *) calloc(mo*no, sizeof(float));
  for (i = 0; i < mo; i++) {
    for (j = 0; j < no; j++) {
      NGCALLF(fsurf2,FSURF2)(output + i*no + j, xo+i, yo+j, 
                             &mi, &ni, xi, yi, fz, &mi, zp, &ft_sigma);
    }
  }

  free(fz);
  free(zp);
  free(temp);
  if (ft_df1 != 0) {
    ft_zx1.size = 0;
    free(ft_zx1.data);
  }
  if (ft_df2 != 0) {
    ft_zxm.size = ni;
    free(ft_zxm.data);
  }
  if (ft_df3 != 0) {
    ft_zy1.size = mi;
    free(ft_zy1.data);
  }
  if (ft_df4 != 0) {
    ft_zyn.size = mi;
    free(ft_zyn.data);
  }
  return(output);
}

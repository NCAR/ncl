/*
 * $Id: c_ftcurv.c,v 1.10 2008-07-27 03:10:10 haley Exp $
 */
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
#include "ftgvars.h"
#include "ftproto.h"

int c_ftcurv(int n, float xi[], float yi[], int m, float xo[], float yo[])
{
  float *yp, *temp;
  float ft_slp1_tmp, ft_slpn_tmp, ft_sigma_tmp;

  yp   = (float *) calloc(n, sizeof(float));
  temp = (float *) calloc(n, sizeof(float));
  
/*
 * Note: these next three parameters are supposedly unaltered by
 * the Fortran routines, so we don't need to save their values when
 * we return from the Fortran routines.
 */
  ft_slp1_tmp  = (float) ft_slp1;
  ft_slpn_tmp  = (float) ft_slpn;
  ft_sigma_tmp = (float) ft_sigma;

  NGCALLF(curv1,CURV1)(&n, xi, yi, &ft_slp1_tmp, &ft_slpn_tmp, &ft_islp, 
                       yp, temp, &ft_sigma_tmp, &ft_err);
  NGCALLF(fcurv2,FCURV2)(&n, xi, yi, yp, &ft_sigma_tmp, &m, xo, yo);

  free(yp);
  free(temp);

  return(ft_err);
}

int c_ftcurvd(int n, float xi[], float yi[], int m, float xo[], float yo[])
{
  float *yp, *temp;
  float ft_slp1_tmp, ft_slpn_tmp, ft_sigma_tmp;

  yp   = (float *) calloc(n, sizeof(float));
  temp = (float *) calloc(n, sizeof(float));
  
/*
 * Note: these next three parameters are supposedly unaltered by
 * the Fortran routines, so we don't need to save their values when
 * we return from the Fortran routines.
 */
  ft_slp1_tmp  = (float) ft_slp1;
  ft_slpn_tmp  = (float) ft_slpn;
  ft_sigma_tmp = (float) ft_sigma;

  NGCALLF(curv1,CURV1)(&n, xi, yi, &ft_slp1_tmp, &ft_slpn_tmp, &ft_islp, 
                       yp, temp, &ft_sigma_tmp, &ft_err);
  NGCALLF(fcurvd,FCURVD)(&n, xi, yi, yp, &ft_sigma_tmp, &m, xo, yo);

  free(yp);
  free(temp);

  return(ft_err);
}

int c_ftcurvi(float xl, float xr, int n, float xi[], float yi[], 
              float *integral)
{
  float *yp, *temp;
  float ft_slp1_tmp, ft_slpn_tmp, ft_sigma_tmp;

  yp   = (float *) calloc(n, sizeof(float));
  temp = (float *) calloc(n, sizeof(float));
  
/*
 * Note: these next three parameters are supposedly unaltered by
 * the Fortran routines, so we don't need to save their values when
 * we return from the Fortran routines.
 */
  ft_slp1_tmp  = (float) ft_slp1;
  ft_slpn_tmp  = (float) ft_slpn;
  ft_sigma_tmp = (float) ft_sigma;

  NGCALLF(curv1,CURV1)(&n, xi, yi, &ft_slp1_tmp, &ft_slpn_tmp, &ft_islp, 
                       yp, temp, &ft_sigma_tmp, &ft_err);
  NGCALLF(fcurvi,FCURVI)(&xl, &xr, &n, xi, yi, yp, &ft_sigma_tmp, integral);

  free(yp);
  free(temp);

  return(ft_err);
}

int c_ftcurvp(int n, float xi[], float yi[], float p, 
              int m, float xo[], float yo[])
{
  float *yp, *temp;
  float ft_sigma_tmp;

  yp   = (float *) calloc(n, sizeof(float));
  temp = (float *) calloc(2*n, sizeof(float));
  
/*
 * Note: this parameter is supposedly unaltered by the
 * Fortran routines, so we don't need to save its value
 * when we return from the Fortran routines.
 */
  ft_sigma_tmp = (float) ft_sigma;

  NGCALLF(curvp1,CURVP1)(&n, xi, yi, &p, yp, temp, &ft_sigma_tmp, &ft_err);
  NGCALLF(fcurvp2,FCURVP2)(&n, xi, yi, yp, &p, &ft_sigma_tmp, &m, xo, yo);

  free(yp);
  free(temp);

  return(ft_err);
}

int c_ftcurvpi(float xl, float xr, float p, int n, float xi[], float yi[], 
              float *integral)
{
  float *yp, *temp;
  float ft_sigma_tmp;

  yp   = (float *) calloc(n, sizeof(float));
  temp = (float *) calloc(2*n, sizeof(float));
  
/*
 * Note: this parameter is supposedly unaltered by the
 * Fortran routines, so we don't need to save its value
 * when we return from the Fortran routines.
 */
  ft_sigma_tmp = (float) ft_sigma;

  NGCALLF(curvp1,CURVP1)(&n, xi, yi, &p, yp, temp, &ft_sigma_tmp, &ft_err);
  NGCALLF(fcurvpi,FCURVPI)(&xl, &xr, &p, &n, xi, yi, yp, &ft_sigma_tmp,
                           integral);

  free(yp);
  free(temp);

  return(ft_err);
}

int c_ftcurvs(int n, float xi[], float yi[], int dflg, float d[],
              int m, float xo[], float yo[])
{
  float *ys, *ysp, *temp, smths, smeps;
  float ft_sigma_tmp;

/*
 *  Establish the values for the parameters S, EPS, and D.
 */

  if (ft_sms == 0) {
    smths = (float) n;
    smeps = (float) sqrt( (float) (2./smths) );
  }
  else if (ft_sms == 1) {
    smths = (float) ft_s;
    smeps = (float) ft_eps;
  }
  else if (ft_sms == 2) {
    smths = (float) ft_s;
    smeps = (float) sqrt( (float) (2./((float) n)) );
  }
  else if (ft_sms == 3) {
    smths = (float) n;
    smeps = (float) ft_eps;
  }

  ys   = (float *) calloc(n, sizeof(float));
  ysp  = (float *) calloc(n, sizeof(float));
  temp = (float *) calloc(9*n, sizeof(float));
  
/*
 * Note: this parameter is supposedly unaltered by the
 * Fortran routines, so we don't need to save its value
 * when we return from the Fortran routines.
 */
  ft_sigma_tmp = (float) ft_sigma;

  NGCALLF(curvs,CURVS)(&n, xi, yi, d, &dflg, &smths, &smeps, ys, ysp,
                       &ft_sigma_tmp, temp, &ft_err);
  NGCALLF(fcurv2,FCURV2)(&n, xi, ys, ysp, &ft_sigma_tmp, &m, xo, yo);

  free(ys);
  free(ysp);
  free(temp);

  return(ft_err);
}

int c_ftcurvps(int n, float xi[], float yi[], float p, int dflg, float d[],
               int m, float xo[], float yo[])
{
  float *ys, *ysp, *temp, smths, smeps;
  float ft_sigma_tmp;

/*
 *  Establish the values for the parameters S, EPS, and D.
 */

  if (ft_sms == 0) {
    smths = (float) n;
    smeps = (float) sqrt( (float) (2./smths) );
  }
  else if (ft_sms == 1) {
    smths = (float) ft_s;
    smeps = (float) ft_eps;
  }
  else if (ft_sms == 2) {
    smths = (float) ft_s;
    smeps = (float) sqrt( (float) (2./((float) n)) );
  }
  else if (ft_sms == 3) {
    smths = (float) n;
    smeps = (float) ft_eps;
  }

  ys   = (float *) calloc(n, sizeof(float));
  ysp  = (float *) calloc(n, sizeof(float));
  temp = (float *) calloc(11*n, sizeof(float));
  
/*
 * Note: this parameter is supposedly unaltered by the
 * Fortran routines, so we don't need to save its value
 * when we return from the Fortran routines.
 */
  ft_sigma_tmp = (float) ft_sigma;

  NGCALLF(curvps,CURVPS)(&n, xi, yi, &p, d, &dflg, &smths, &smeps, ys, ysp,
                         &ft_sigma_tmp, temp, &ft_err);
  NGCALLF(fcurvp2,FCURVP2)(&n, xi, ys, ysp, &p, &ft_sigma_tmp, &m, xo, yo);

  free(ys);
  free(ysp);
  free(temp);

  return(ft_err);
}

int c_ftkurv(int n, float xi[], float yi[], int m, float t[], 
             float xo[], float yo[])
{
  float *s, *xp, *yp, *temp;
  float ft_slp1_tmp, ft_slpn_tmp, ft_sigma_tmp;

  s    = (float *) calloc(n, sizeof(float));
  xp   = (float *) calloc(n, sizeof(float));
  yp   = (float *) calloc(n, sizeof(float));
  temp = (float *) calloc(n, sizeof(float));
  
/*
 * Note: this parameter is supposedly unaltered by the
 * Fortran routines, so we don't need to save its value
 * when we return from the Fortran routines.
 */
  ft_slp1_tmp  = (float) ft_slp1;
  ft_slpn_tmp  = (float) ft_slpn;
  ft_sigma_tmp = (float) ft_sigma;

  NGCALLF(kurv1,KURV1)(&n, xi, yi, &ft_slp1_tmp, &ft_slpn_tmp, &ft_islp,
                       xp, yp, temp, s, &ft_sigma_tmp, &ft_err);
  NGCALLF(fkurv2,FKURV2)(&n, xi, yi, &m, t, xo, yo, xp, yp, s, &ft_sigma_tmp);

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
  float ft_sigma_tmp;

  s    = (float *) calloc(n, sizeof(float));
  xp   = (float *) calloc(n, sizeof(float));
  yp   = (float *) calloc(n, sizeof(float));
  temp = (float *) calloc(2*n, sizeof(float));
  
/*
 * Note: this parameter is supposedly unaltered by the
 * Fortran routines, so we don't need to save its value
 * when we return from the Fortran routines.
 */
  ft_sigma_tmp = (float) ft_sigma;

  NGCALLF(kurvp1,KURVP1)(&n, xi, yi, xp, yp, temp, s, &ft_sigma_tmp, &ft_err);
  NGCALLF(fkurvp2,FKURVP2)(&n, xi, yi, &m, t, xo, yo, xp, yp, s, &ft_sigma_tmp);

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
  float ft_slp1_tmp, ft_slpn_tmp, ft_sigma_tmp;

  s    = (float *) calloc(n, sizeof(float));
  xp   = (float *) calloc(n, sizeof(float));
  yp   = (float *) calloc(n, sizeof(float));
  temp = (float *) calloc(n, sizeof(float));
  
/*
 * Note: this parameter is supposedly unaltered by the
 * Fortran routines, so we don't need to save its value
 * when we return from the Fortran routines.
 */
  ft_slp1_tmp  = (float) ft_slp1;
  ft_slpn_tmp  = (float) ft_slpn;
  ft_sigma_tmp = (float) ft_sigma;

  NGCALLF(kurv1,KURV1)(&n, xi, yi, &ft_slp1_tmp, &ft_slpn_tmp, &ft_islp,
                       xp, yp, temp, s, &ft_sigma_tmp, &ft_err);
  NGCALLF(fkurvd,FKURVD)(&n, xi, yi, &m, t, xo, yo, xd, yd, xdd, ydd,
                         xp, yp, s, &ft_sigma_tmp);

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
  float ft_sigma_tmp;

  s    = (float *) calloc(n, sizeof(float));
  xp   = (float *) calloc(n, sizeof(float));
  yp   = (float *) calloc(n, sizeof(float));
  temp = (float *) calloc(2*n, sizeof(float));
  
/*
 * Note: this parameter is supposedly unaltered by the
 * Fortran routines, so we don't need to save its value
 * when we return from the Fortran routines.
 */
  ft_sigma_tmp = (float) ft_sigma;

  NGCALLF(kurvp1,KURVP1)(&n, xi, yi, xp, yp, temp, s, &ft_sigma_tmp, &ft_err);
  NGCALLF(fkurvpd,FKURVPD)(&n, xi, yi, &m, t, xo, yo, xd, yd, xdd, ydd,
                           xp, yp, s, &ft_sigma_tmp);

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
  float ft_sigma_tmp, ft_z11_tmp, ft_zm1_tmp, ft_z1n_tmp, ft_zmn_tmp;
  float *ft_zx1_tmp, *ft_zxm_tmp, *ft_zy1_tmp, *ft_zyn_tmp;

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
/*
 * zx1, zxn, zy1, and zym are parameter arrays that can either be
 * float or double. If the user set them as doubles, then we have to
 * coerce to float since the Fortran routines below expect floats.
 */
  sflag = 0;
  if (ft_df1 != 0) {
    ft_zx1.size = ni;
    ft_zx1_tmp = (float *) calloc(ni, sizeof(float));
    sflag = sflag + 1;
  }
  else {
    if(ft_zx1.type == ft_double) {
      ft_zx1_tmp = (float *)copy_dtof(ft_zx1.data,ft_zx1.size);
    }
    else {
      ft_zx1_tmp = (float *)ft_zx1.data;
    }
  }
  if (ft_df2 != 0) {
    ft_zxm.size = ni;
    ft_zxm_tmp  = (float *) calloc(ni, sizeof(float));
    sflag = sflag + 2;
  }
  else {
    if(ft_zxm.type == ft_double) {
      ft_zxm_tmp = (float *)copy_dtof(ft_zxm.data,ft_zxm.size);
    }
    else {
      ft_zxm_tmp = (float *)ft_zxm.data;
    }
  }
  if (ft_df3 != 0) {
    ft_zy1.size = mi;
    ft_zy1_tmp  = (float *) calloc(mi, sizeof(float));
    sflag = sflag + 4;
  }
  else {
    if(ft_zy1.type == ft_double) {
      ft_zy1_tmp = (float *)copy_dtof(ft_zy1.data,ft_zy1.size);
    }
    else {
      ft_zy1_tmp = (float *)ft_zy1.data;
    }
  }
  if (ft_df4 != 0) {
    ft_zyn.size = mi;
    ft_zyn_tmp  = (float *) calloc(mi, sizeof(float));
    sflag = sflag + 8;
  }
  else {
    if(ft_zyn.type == ft_double) {
      ft_zyn_tmp = (float *)copy_dtof(ft_zyn.data,ft_zyn.size);
    }
    else {
      ft_zyn_tmp = (float *)ft_zyn.data;
    }
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
  
/*
 * Note: these five parameter are supposedly unaltered by the
 * Fortran routine, so we don't need to save their values
 * when we return.
 */
  ft_sigma_tmp = (float) ft_sigma;
  ft_z11_tmp   = (float) ft_z11;
  ft_zm1_tmp   = (float) ft_zm1;
  ft_z1n_tmp   = (float) ft_z1n;
  ft_zmn_tmp   = (float) ft_zmn;

  NGCALLF(surf1,SURF1)(&mi, &ni, xi, yi, fz, &mi, 
          ft_zx1_tmp, ft_zxm_tmp, ft_zy1_tmp, ft_zyn_tmp,
          &ft_z11_tmp, &ft_zm1_tmp, &ft_z1n_tmp, &ft_zmn_tmp, &sflag,
          zp, temp, &ft_sigma_tmp, &ft_err);

/*
 *  Call surf2, reversing the ordering of the output array.
 */
  output = (float *) calloc(mo*no, sizeof(float));
  for (i = 0; i < mo; i++) {
    for (j = 0; j < no; j++) {
      NGCALLF(fsurf2,FSURF2)(output + i*no + j, xo+i, yo+j, 
                             &mi, &ni, xi, yi, fz, &mi, zp, &ft_sigma_tmp);
    }
  }

  free(fz);
  free(zp);
  free(temp);
  if (ft_df1 != 0) {
    ft_zx1.size = 0;
    free(ft_zx1_tmp);
  }
  if (ft_df2 != 0) {
    ft_zxm.size = ni;
    free(ft_zxm_tmp);
  }
  if (ft_df3 != 0) {
    ft_zy1.size = mi;
    free(ft_zy1_tmp);
  }
  if (ft_df4 != 0) {
    ft_zyn.size = mi;
    free(ft_zyn_tmp);
  }
  return(output);
}

int c_ftcurvs1(int n, float xi[], float yi[], int dflg, float d[],
               int m, float xl, float xr, float xo[], float yo[])
{
  float *param, *xs, *xsp, *ys, *ysp, *temp, smths, smeps;
  float ft_sigma_tmp;
  int   i;

/*
 *  Establish the values for the parameters S and EPS.
 */

  if (ft_sms == 0) {
    smths = (float) n;
    smeps = (float) sqrt( (float) (2./smths) );
  }
  else if (ft_sms == 1) {
    smths = (float) ft_s;
    smeps = (float) ft_eps;
  }
  else if (ft_sms == 2) {
    smths = (float) ft_s;
    smeps = (float) sqrt( (float) (2./((float) n)) );
  }
  else if (ft_sms == 3) {
    smths = (float) n;
    smeps = (float) ft_eps;
  }

  ys    = (float *) calloc(n, sizeof(float));
  ysp   = (float *) calloc(n, sizeof(float));
  xs    = (float *) calloc(n, sizeof(float));
  xsp   = (float *) calloc(n, sizeof(float));
  param = (float *) calloc(n, sizeof(float));
  temp  = (float *) calloc(19*n, sizeof(float));
  
/*
 * Note: this parameter is supposedly unaltered by the
 * Fortran routines, so we don't need to save its value
 * when we return from the Fortran routines.
 */
  ft_sigma_tmp = (float) ft_sigma;

  NGCALLF(curvs1,CURVS1)(&n, xi, yi, d, &dflg, &smths, &smeps, param,
                         xs, ys, xsp, ysp, &ft_sigma_tmp, temp, &ft_err);
  NGCALLF(fcurvs2,FCURVS2)(&n, param, xi, yi, xs, xsp, ys, ysp, 
                           &ft_sigma_tmp, &m, &xl, &xr, xo, yo);

  free(xs);
  free(xsp);
  free(ys);
  free(ysp);
  free(param);
  free(temp);

  return(ft_err);
}

int c_ftcurvdp(int n, double xi[], double yi[], int m, double xo[], double yo[])
{
  double *yp, *temp;

  yp   = (double *) calloc(n, sizeof(double));
  temp = (double *) calloc(n, sizeof(double));

  NGCALLF(curv1dp,CURV1DP)(&n, xi, yi, &ft_slp1, &ft_slpn, &ft_islp,
                           yp, temp, &ft_sigma, &ft_err);
  NGCALLF(fcurv2dp,FCURV2DP)(&n, xi, yi, yp, &ft_sigma, &m, xo, yo);

  free(yp);
  free(temp);

  return(ft_err);
}

int c_ftcurvddp(int n, double xi[], double yi[], int m, double xo[], double yo[])
{
  double *yp, *temp;

  yp   = (double *) calloc(n, sizeof(double));
  temp = (double *) calloc(n, sizeof(double));
  
  NGCALLF(curv1dp,CURV1DP)(&n, xi, yi, &ft_slp1, &ft_slpn, &ft_islp, 
                           yp, temp, &ft_sigma, &ft_err);
  NGCALLF(fcurvddp,FCURVDDP)(&n, xi, yi, yp, &ft_sigma, &m, xo, yo);

  free(yp);
  free(temp);

  return(ft_err);
}

int c_ftcurvidp(double xl, double xr, int n, double xi[], double yi[], 
              double *integral)
{
  double *yp, *temp;

  yp   = (double *) calloc(n, sizeof(double));
  temp = (double *) calloc(n, sizeof(double));
  
  NGCALLF(curv1dp,CURV1DP)(&n, xi, yi, &ft_slp1, &ft_slpn, &ft_islp, 
                           yp, temp, &ft_sigma, &ft_err);
  NGCALLF(fcurvidp,FCURVIDP)(&xl, &xr, &n, xi, yi, yp, &ft_sigma, integral);

  free(yp);
  free(temp);

  return(ft_err);
}

int c_ftcurvpdp(int n, double xi[], double yi[], double p, 
              int m, double xo[], double yo[])
{
  double *yp, *temp;

  yp   = (double *) calloc(n, sizeof(double));
  temp = (double *) calloc(2*n, sizeof(double));
  
  NGCALLF(curvp1dp,CURVP1DP)(&n, xi, yi, &p, yp, temp, &ft_sigma, &ft_err);
  NGCALLF(fcurvp2dp,FCURVP2DP)(&n, xi, yi, yp, &p, &ft_sigma, &m, xo, yo);

  free(yp);
  free(temp);

  return(ft_err);
}

int c_ftcurvpidp(double xl, double xr, double p, int n, double xi[], double yi[], 
              double *integral)
{
  double *yp, *temp;

  yp   = (double *) calloc(n, sizeof(double));
  temp = (double *) calloc(2*n, sizeof(double));
  
  NGCALLF(curvp1dp,CURVP1DP)(&n, xi, yi, &p, yp, temp, &ft_sigma, &ft_err);
  NGCALLF(fcurvpidp,FCURVPIDP)(&xl, &xr, &p, &n, xi, yi, yp, &ft_sigma,
                               integral);

  free(yp);
  free(temp);

  return(ft_err);
}

int c_ftcurvsdp(int n, double xi[], double yi[], int dflg, double d[],
              int m, double xo[], double yo[])
{
  double *ys, *ysp, *temp, smths, smeps;

/*
 *  Establish the values for the parameters S, EPS, and D.
 */

  if (ft_sms == 0) {
    smths = (double) n;
    smeps = (double) sqrt( (double) (2./smths) );
  }
  else if (ft_sms == 1) {
    smths = ft_s;
    smeps = ft_eps;
  }
  else if (ft_sms == 2) {
    smths = ft_s;
    smeps = (double) sqrt( (double) (2./((double) n)) );
  }
  else if (ft_sms == 3) {
    smths = (double) n;
    smeps = ft_eps;
  }

  ys   = (double *) calloc(n, sizeof(double));
  ysp  = (double *) calloc(n, sizeof(double));
  temp = (double *) calloc(9*n, sizeof(double));
  
  NGCALLF(curvsdp,CURVSDP)(&n, xi, yi, d, &dflg, &smths, &smeps, ys, ysp,
                           &ft_sigma, temp, &ft_err);
  NGCALLF(fcurv2dp,FCURV2DP)(&n, xi, ys, ysp, &ft_sigma, &m, xo, yo);

  free(ys);
  free(ysp);
  free(temp);

  return(ft_err);
}

int c_ftcurvpsdp(int n, double xi[], double yi[], double p, int dflg,
                 double d[], int m, double xo[], double yo[])
{
  double *ys, *ysp, *temp, smths, smeps;

/*
 *  Establish the values for the parameters S, EPS, and D.
 */

  if (ft_sms == 0) {
    smths = (double) n;
    smeps = (double) sqrt( (double) (2./smths) );
  }
  else if (ft_sms == 1) {
    smths = ft_s;
    smeps = ft_eps;
  }
  else if (ft_sms == 2) {
    smths = ft_s;
    smeps = (double) sqrt( (double) (2./((double) n)) );
  }
  else if (ft_sms == 3) {
    smths = (double) n;
    smeps = ft_eps;
  }

  ys   = (double *) calloc(n, sizeof(double));
  ysp  = (double *) calloc(n, sizeof(double));
  temp = (double *) calloc(11*n, sizeof(double));
  
  NGCALLF(curvpsdp,CURVPSDP)(&n, xi, yi, &p, d, &dflg, &smths, &smeps, ys,
                             ysp, &ft_sigma, temp, &ft_err);
  NGCALLF(fcurvp2dp,FCURVP2DP)(&n, xi, ys, ysp, &p, &ft_sigma, &m, xo, yo);

  free(ys);
  free(ysp);
  free(temp);

  return(ft_err);
}

int c_ftkurvdp(int n, double xi[], double yi[], int m, double t[], 
             double xo[], double yo[])
{
  double *s, *xp, *yp, *temp;

  s    = (double *) calloc(n, sizeof(double));
  xp   = (double *) calloc(n, sizeof(double));
  yp   = (double *) calloc(n, sizeof(double));
  temp = (double *) calloc(n, sizeof(double));
  
  NGCALLF(kurv1dp,KURV1DP)(&n, xi, yi, &ft_slp1, &ft_slpn, &ft_islp,
                           xp, yp, temp, s, &ft_sigma, &ft_err);
  NGCALLF(fkurv2dp,FKURV2DP)(&n, xi, yi, &m, t, xo, yo, xp, yp, s,
                             &ft_sigma);
  free(s);
  free(xp);
  free(yp);
  free(temp);

  return(ft_err);
}

int c_ftkurvpdp(int n, double xi[], double yi[], int m, double t[], 
             double xo[], double yo[])
{
  double *s, *xp, *yp, *temp;

  s    = (double *) calloc(n, sizeof(double));
  xp   = (double *) calloc(n, sizeof(double));
  yp   = (double *) calloc(n, sizeof(double));
  temp = (double *) calloc(2*n, sizeof(double));
  
  NGCALLF(kurvp1dp,KURVP1DP)(&n, xi, yi, xp, yp, temp, s, &ft_sigma, 
                             &ft_err);
  NGCALLF(fkurvp2dp,FKURVP2DP)(&n, xi, yi, &m, t, xo, yo, xp, yp, s, 
                               &ft_sigma);

  free(s);
  free(xp);
  free(yp);
  free(temp);

  return(ft_err);
}

int c_ftkurvddp(int n, double xi[], double yi[], int m, double t[], 
             double xo[], double yo[], double xd[], double yd[],
             double xdd[], double ydd[])
{
  double *s, *xp, *yp, *temp;

  s    = (double *) calloc(n, sizeof(double));
  xp   = (double *) calloc(n, sizeof(double));
  yp   = (double *) calloc(n, sizeof(double));
  temp = (double *) calloc(n, sizeof(double));
  
  NGCALLF(kurv1dp,KURV1DP)(&n, xi, yi, &ft_slp1, &ft_slpn, &ft_islp,
                           xp, yp, temp, s, &ft_sigma, &ft_err);
  NGCALLF(fkurvddp,FKURVDDP)(&n, xi, yi, &m, t, xo, yo, xd, yd, xdd, ydd,
                             xp, yp, s, &ft_sigma);

  free(s);
  free(xp);
  free(yp);
  free(temp);

  return(ft_err);
}

int c_ftkurvpddp(int n, double xi[], double yi[], int m, double t[], 
              double xo[], double yo[], double xd[], double yd[],
              double xdd[], double ydd[])
{
  double *s, *xp, *yp, *temp;

  s    = (double *) calloc(n, sizeof(double));
  xp   = (double *) calloc(n, sizeof(double));
  yp   = (double *) calloc(n, sizeof(double));
  temp = (double *) calloc(2*n, sizeof(double));
  
  NGCALLF(kurvp1dp,KURVP1DP)(&n, xi, yi, xp, yp, temp, s, &ft_sigma,
                             &ft_err);
  NGCALLF(fkurvpddp,FKURVPDDP)(&n, xi, yi, &m, t, xo, yo, xd, yd, xdd, ydd,
                               xp, yp, s, &ft_sigma);

  free(s);
  free(xp);
  free(yp);
  free(temp);

  return(ft_err);
}

double *c_ftsurfdp(int mi, int ni, double *xi, double *yi, double *zi,
                   int mo, int no, double *xo, double *yo, int *ier)
{
  int   i, j, sflag;
  double *zp, *temp, *fz, *output;
  double *ft_zx1_tmp, *ft_zxm_tmp, *ft_zy1_tmp, *ft_zyn_tmp;

  fz   = (double *) calloc(mi*ni, sizeof(double));
  zp   = (double *) calloc(3*mi*ni, sizeof(double));
  temp = (double *) calloc(2*ni+mi, sizeof(double));

/*
 *  Rearrange the zi array, since surf1 expects an array ordered as 
 *  per Fortran.
 */
  for (i = 0; i < mi; i++) {
    for (j = 0; j < ni; j++) {
      fz[j*mi+i] = zi[i*ni+j];
    }
  }
/*
 * zx1, zxn, zy1, and zym are parameter arrays that can either be
 * float or double. If the user set them as floats, then we have to
 * coerce to double since the Fortran routines below expect doubles
 */
  sflag = 0;
  if (ft_df1 != 0) {
    ft_zx1.size = ni;
    ft_zx1_tmp  = (double *) calloc(ni, sizeof(double));
    sflag = sflag + 1;
  }
  else {
    if(ft_zx1.type == ft_float) {
      ft_zx1_tmp = (double *)copy_ftod(ft_zx1.data,ft_zx1.size);
    }
    else {
      ft_zx1_tmp = (double *)ft_zx1.data;
    }
  }
  if (ft_df2 != 0) {
    ft_zxm.size = ni;
    ft_zxm_tmp  = (double *) calloc(ni, sizeof(double));
    sflag = sflag + 2;
  }
  else {
    if(ft_zxm.type == ft_float) {
      ft_zxm_tmp = (double *)copy_ftod(ft_zxm.data,ft_zxm.size);
    }
    else {
      ft_zxm_tmp = (double *)ft_zxm.data;
    }
  }
  if (ft_df3 != 0) {
    ft_zy1.size = mi;
    ft_zy1_tmp  = (double *) calloc(mi, sizeof(double));
    sflag = sflag + 4;
  }
  else {
    if(ft_zy1.type == ft_float) {
      ft_zy1_tmp = (double *)copy_ftod(ft_zy1.data,ft_zy1.size);
    }
    else {
      ft_zy1_tmp = (double *)ft_zy1.data;
    }
  }
  if (ft_df4 != 0) {
    ft_zyn.size = mi;
    ft_zyn_tmp  = (double *) calloc(mi, sizeof(double));
    sflag = sflag + 8;
  }
  else {
    if(ft_zyn.type == ft_float) {
      ft_zyn_tmp = (double *)copy_ftod(ft_zyn.data,ft_zyn.size);
    }
    else {
      ft_zyn_tmp = (double *)ft_zyn.data;
    }
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
  
  NGCALLF(surf1dp,SURF1DP)(&mi, &ni, xi, yi, fz, &mi, 
          ft_zx1_tmp, ft_zxm_tmp, ft_zy1_tmp, ft_zyn_tmp,
          &ft_z11, &ft_zm1, &ft_z1n, &ft_zmn, &sflag,
          zp, temp, &ft_sigma, &ft_err);

/*
 *  Call surf2, reversing the ordering of the output array.
 */
  output = (double *) calloc(mo*no, sizeof(double));
  for (i = 0; i < mo; i++) {
    for (j = 0; j < no; j++) {
      NGCALLF(fsurf2dp,FSURF2DP)(output + i*no + j, xo+i, yo+j, &mi, &ni,
                                 xi, yi, fz, &mi, zp, &ft_sigma);
    }
  }

  free(fz);
  free(zp);
  free(temp);
  if (ft_df1 != 0) {
    ft_zx1.size = 0;
    free(ft_zx1_tmp );
  }
  if (ft_df2 != 0) {
    ft_zxm.size = ni;
    free(ft_zxm_tmp );
  }
  if (ft_df3 != 0) {
    ft_zy1.size = mi;
    free(ft_zy1_tmp );
  }
  if (ft_df4 != 0) {
    ft_zyn.size = mi;
    free(ft_zyn_tmp );
  }
  return(output);
}

int c_ftcurvs1dp(int n, double xi[], double yi[], int dflg, double d[],
                 int m, double xl, double xr, double xo[], double yo[])
{
  double *param, *xs, *xsp, *ys, *ysp, *temp, smths, smeps;
  int   i;

/*
 *  Establish the values for the parameters S and EPS.
 */

  if (ft_sms == 0) {
    smths = (double) n;
    smeps = (double) sqrt( (double) (2./smths) );
  }
  else if (ft_sms == 1) {
    smths = ft_s;
    smeps = ft_eps;
  }
  else if (ft_sms == 2) {
    smths = ft_s;
    smeps = (double) sqrt( (double) (2./((double) n)) );
  }
  else if (ft_sms == 3) {
    smths = (double) n;
    smeps = ft_eps;
  }

  ys    = (double *) calloc(n, sizeof(double));
  ysp   = (double *) calloc(n, sizeof(double));
  xs    = (double *) calloc(n, sizeof(double));
  xsp   = (double *) calloc(n, sizeof(double));
  param = (double *) calloc(n, sizeof(double));
  temp  = (double *) calloc(19*n, sizeof(double));
  
  NGCALLF(curvs1dp,CURVS1DP)(&n, xi, yi, d, &dflg, &smths, &smeps,
          param, xs, ys, xsp, ysp, &ft_sigma, temp, &ft_err);
  NGCALLF(fcurvs2dp,FCURVS2DP)(&n, param, xi, yi, xs, xsp, ys, ysp, 
          &ft_sigma, &m, &xl, &xr, xo, yo);

  free(xs);
  free(xsp);
  free(ys);
  free(ysp);
  free(param);
  free(temp);

  return(ft_err);
}


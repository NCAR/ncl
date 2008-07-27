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

float *c_csa1s(int n, float xi[], float yi[], int knots, 
            int m, float xo[], int *ier)
{
  float *work, *yo;
  int   nwrk;

  nwrk = knots*(knots+3);
  work = (float *) calloc(nwrk, sizeof(float));
  if (work == NULL) {
    printf("Unable to allocate work space in c_csa1s\n");    
    *ier = 300;
    return( (float *) NULL);
  }

  yo   = (float *) calloc(m, sizeof(float));
  if (yo == NULL) {
    printf("Unable to allocate space for output in c_csa1s\n");
    *ier = 300;
    return((float *)NULL);
  }

  NGCALLF(csa1s,CSA1S)(&n, xi, yi, &knots, &m, xo, yo, &nwrk, work, ier);

  free(work);

  return(yo);
}

float *c_csa1xs(int n, float xi[], float yi[], float wts[], int knots, 
             float ssmth, int deriv, int m, float xo[], int *ier)
{
  float *work, *yo;
  int   nwrk;

  nwrk = knots*(knots+3);
  work = (float *) calloc(nwrk, sizeof(float));
  if (work == NULL) {
    printf("Unable to allocate work space in c_csa1xs\n");    
    *ier = 300;
    return( (float *)NULL);
  }
  
  yo   = (float *) calloc(m, sizeof(float));
  if (yo == NULL) {
    printf("Unable to allocate space for output in c_csa1s\n");
    *ier = 300;
    return((float *)NULL);
  }

  NGCALLF(csa1xs,CSA1XS)(&n, xi, yi, wts, &knots, &ssmth, &deriv, &m, xo, yo, 
                         &nwrk, work, ier);

  free(work);

  return(yo);
}

float *c_csa2xs(int ni, float xi[], float yi[], float zi[], 
                float wts[], int knots[2], float ssmth, int nderiv[2],
                int mo, int no, float xo[], float yo[], int *ier)
{
  int   i, j, ncf, nwrk;
  float *zt, *zo, *zor, *work;
 
  zt   = (float *) calloc(2*ni, sizeof(float));
  if (zt == NULL) {
    printf("Unable to allocate temp space in c_csa2xs\n");
    *ier = 300;
    return((float *)NULL);
  }
  zo   = (float *) calloc(mo*no, sizeof(float));
  if (zo == NULL) {
    printf("Unable to allocate space for output in c_csa2xs\n");
    *ier = 300;
    return((float *)NULL);
  }
  ncf = knots[0]*knots[1];
  nwrk = ncf*(ncf+3);
  work = (float *) calloc(nwrk, sizeof(float));
  if (work == NULL) {
    printf("Unable to allocate work space in c_csa2xs\n");
    *ier = 300;
    return((float *)NULL);
  }

/*
 *  Build the input array for the Fortran call.
 */
  for (i = 0; i < ni; i++) {
    zt[2*i  ] = xi[i];
    zt[2*i+1] = yi[i];
  }

/*
 *  Make the Fortran call.
 */
  NGCALLF(csa2xs,CSA2XS)(&ni, zt, zi, wts, knots, &ssmth,
          nderiv, &mo, &no, xo, yo, zo, &nwrk, work, ier);

  free(work);
  free(zt);

  if (*ier != 0) {
    return((float *)NULL);
  }

/*
 *  Rearrange the zo array to be row dominant.
 */
  zor = (float *) calloc(mo*no, sizeof(float));
  if (zor == NULL) {
    printf("Unable to allocate temp space in c_csa2xs\n");
    *ier = 300;
    return((float *)NULL);
  }

  for (i = 0; i < mo; i++) {
    for (j = 0; j < no; j++) {
      zor[i*no+j] = zo[j*mo+i];
    }
  }
  
  free(zo);

  return(zor);
}

float *c_csa2s(int ni, float xi[], float yi[], float zi[], int knots[2], 
               int mo, int no, float xo[], float yo[], int *ier)
{
  float wts[]={-1.}, ssmth=0.0;
  int   nderiv[]={0,0};
 
  return(c_csa2xs(ni,xi,yi,zi,wts,knots,ssmth,nderiv,mo,no,xo,yo,ier));

}

float *c_csa2lxs(int ni, float xi[], float yi[], float zi[], 
                 float wts[], int knots[2], float ssmth, int nderiv[2],
                 int no, float xo[], float yo[], int *ier)
{
  int   i, j, ncf, nwrk;
  float *zt, *zo, *zor, *work;
 
  zt   = (float *) calloc(2*ni, sizeof(float));
  if (zt == NULL) {
    printf("Unable to allocate temp space in c_csa2lxs\n");
    *ier = 300;
    return((float *)NULL);
  }
  zo   = (float *) calloc(no, sizeof(float));
  if (zo == NULL) {
    printf("Unable to allocate space for output in c_csa2lxs\n");
    *ier = 300;
    return((float *)NULL);
  }
  ncf = knots[0]*knots[1];
  nwrk = ncf*(ncf+3);
  work = (float *) calloc(nwrk, sizeof(float));
  if (work == NULL) {
    printf("Unable to allocate work space in c_csa2lxs\n");
    *ier = 300;
    return((float *)NULL);
  }

/*
 *  Build the input array for the Fortran call.
 */
  for (i = 0; i < ni; i++) {
    zt[2*i  ] = xi[i];
    zt[2*i+1] = yi[i];
  }

/*
 *  Make the Fortran call.
 */
  NGCALLF(csa2lxs,CSA2LXS)(&ni, zt, zi, wts, knots, &ssmth,
          nderiv, &no, xo, yo, zo, &nwrk, work, ier);

  free(work);
  free(zt);

  return(zo);
}

float *c_csa2ls(int ni, float xi[], float yi[], float zi[], int knots[2], 
                int no, float xo[], float yo[], int *ier)
{
  float wts[]={-1.}, ssmth=0.0;
  int   nderiv[]={0,0};
 
  return(c_csa2lxs(ni,xi,yi,zi,wts,knots,ssmth,nderiv,no,xo,yo,ier));
}

float *c_csa3s(int ni, float xi[], float yi[], float zi[], float ui[],
               int knots[3], int nxo, int nyo, int nzo, float xo[], 
               float yo[], float zo[], int *ier)
{
  float wts[]={-1.}, ssmth=0.0;
  int   nderiv[]={0,0,0};

  return(c_csa3xs(ni,xi,yi,zi,ui,wts,knots,ssmth,nderiv,nxo,nyo,nzo,
                  xo,yo,zo,ier));
}

float *c_csa3xs(int ni, float xi[], float yi[], float zi[], float ui[],
                float wts[], int knots[3], float ssmth, int nderiv[3],
                int nxo, int nyo, int nzo, float xo[], float yo[], 
                float zo[], int *ier)
{
  int   i, j, k, ncf, nwrk;
  float *zt, *uo, *zor, *work;
 
  zt   = (float *) calloc(3*ni, sizeof(float));
  if (zt == NULL) {
    printf("Unable to allocate temp space in c_csa3xs\n");
    *ier = 300;
    return((float *)NULL);
  }
  uo   = (float *) calloc(nxo*nyo*nzo, sizeof(float));
  if (uo == NULL) {
    printf("Unable to allocate space for output in c_csa3xs\n");
    *ier = 300;
    return((float *)NULL);
  }
  ncf = knots[0]*knots[1]*knots[2];
  nwrk = ncf*(ncf+3);
  work = (float *) calloc(nwrk, sizeof(float));
  if (work == NULL) {
    printf("Unable to allocate work space in c_csa3xs\n");
    *ier = 300;
    return((float *)NULL);
  }

/*
 *  Build the input array for the Fortran call.
 */
  for (i = 0; i < ni; i++) {
    zt[3*i  ] = xi[i];
    zt[3*i+1] = yi[i];
    zt[3*i+2] = zi[i];
  }

/*
 *  Make the Fortran call.
 */
  NGCALLF(csa3xs,CSA3XS)(&ni, zt, ui, wts, knots, &ssmth,
          nderiv, &nxo, &nyo, &nzo, xo, yo, zo, uo, &nwrk, work, ier);

  free(work);
  free(zt);

/*
 *  Rearrange the zo array to be row dominant.
 */
  zor = (float *) calloc(nxo*nyo*nzo, sizeof(float));
  if (zor == NULL) {
    printf("Unable to allocate temp space in c_csa3xs\n");
    *ier = 300;
    return((float *)NULL);
  }

  for (i = 0; i < nxo; i++) {
    for (j = 0; j < nyo; j++) {
      for (k = 0; k < nzo; k++) {
        zor[i*nzo*nyo + j*nzo + k] = uo[k*nxo*nyo + j*nxo + i];
      }
    }
  }
  
  free(uo);

  return(zor);
}

float *c_csa3lxs(int ni, float xi[], float yi[], float zi[], float ui[],
                 float wts[], int knots[3], float ssmth, int nderiv[3],
                 int no, float xo[], float yo[], float zo[], int *ier)
{
  int   i, j, ncf, nwrk;
  float *zt, *uo, *zor, *work;
 
  zt   = (float *) calloc(3*ni, sizeof(float));
  if (zt == NULL) {
    printf("Unable to allocate temp space in c_csa3lxs\n");
    *ier = 300;
    return((float *)NULL);
  }
  uo   = (float *) calloc(no, sizeof(float));
  if (uo == NULL) {
    printf("Unable to allocate space for output in c_csa3lxs\n");
    *ier = 300;
    return((float *)NULL);
  }
  ncf = knots[0]*knots[1]*knots[2];
  nwrk = ncf*(ncf+3);
  work = (float *) calloc(nwrk, sizeof(float));
  if (work == NULL) {
    printf("Unable to allocate work space in c_csa3lxs\n");
    *ier = 300;
    return((float *)NULL);
  }

/*
 *  Build the input array for the Fortran call.
 */
  for (i = 0; i < ni; i++) {
    zt[3*i  ] = xi[i];
    zt[3*i+1] = yi[i];
    zt[3*i+2] = zi[i];
  }

/*
 *  Make the Fortran call.
 */
  NGCALLF(csa3lxs,CSA3LXS)(&ni, zt, ui, wts, knots, &ssmth,
          nderiv, &no, xo, yo, zo, uo, &nwrk, work, ier);

  free(work);
  free(zt);

  return(uo);
}

float *c_csa3ls(int ni, float xi[], float yi[], float zi[], float ui[],
                int knots[2], int no, float xo[], float yo[], float zo[], 
                int *ier)
{
  float wts[]={-1.}, ssmth=0.0;
  int   nderiv[]={0,0,0};
 
  return(c_csa3lxs(ni,xi,yi,zi,ui,wts,knots,ssmth,nderiv,no,xo,yo,zo,ier));
}

double *c_csa1d(int n, double xi[], double yi[], int knots, 
            int m, double xo[], int *ier)
{
  double *work, *yo;
  int   nwrk;

  nwrk = knots*(knots+3);
  work = (double *) calloc(nwrk, sizeof(double));
  if (work == NULL) {
    printf("Unable to allocate work space in c_csa1d\n");
    *ier = 300;
    return( (double *) NULL);
  }

  yo   = (double *) calloc(m, sizeof(double));
  if (yo == NULL) {
    printf("Unable to allocate space for output in c_csa1d\n");
    *ier = 300;
    return((double *)NULL);
  }

  NGCALLF(csa1d,CSA1D)(&n, xi, yi, &knots, &m, xo, yo, &nwrk, work, ier);

  free(work);

  return(yo);
}

double *c_csa1xd(int n, double xi[], double yi[], double wts[], int knots, 
             double ssmth, int deriv, int m, double xo[], int *ier)
{
  double *work, *yo;
  int   nwrk;

  nwrk = knots*(knots+3);
  work = (double *) calloc(nwrk, sizeof(double));
  if (work == NULL) {
    printf("Unable to allocate work space in c_csa1xd\n");    
    *ier = 300;
    return( (double *)NULL);
  }
  
  yo   = (double *) calloc(m, sizeof(double));
  if (yo == NULL) {
    printf("Unable to allocate space for output in c_csa1d\n");
    *ier = 300;
    return((double *)NULL);
  }

  NGCALLF(csa1xd,CSA1XD)(&n, xi, yi, wts, &knots, &ssmth, &deriv, &m, xo, yo, 
                         &nwrk, work, ier);

  free(work);

  return(yo);
}

double *c_csa2xd(int ni, double xi[], double yi[], double zi[], 
                double wts[], int knots[2], double ssmth, int nderiv[2],
                int mo, int no, double xo[], double yo[], int *ier)
{
  int   i, j, ncf, nwrk;
  double *zt, *zo, *zor, *work;
 
  zt   = (double *) calloc(2*ni, sizeof(double));
  if (zt == NULL) {
    printf("Unable to allocate temp space in c_csa2xd\n");
    *ier = 300;
    return((double *)NULL);
  }
  zo   = (double *) calloc(mo*no, sizeof(double));
  if (zo == NULL) {
    printf("Unable to allocate space for output in c_csa2xd\n");
    *ier = 300;
    return((double *)NULL);
  }
  ncf = knots[0]*knots[1];
  nwrk = ncf*(ncf+3);
  work = (double *) calloc(nwrk, sizeof(double));
  if (work == NULL) {
    printf("Unable to allocate work space in c_csa2xd\n");
    *ier = 300;
    return((double *)NULL);
  }

/*
 *  Build the input array for the Fortran call.
 */
  for (i = 0; i < ni; i++) {
    zt[2*i  ] = xi[i];
    zt[2*i+1] = yi[i];
  }

/*
 *  Make the Fortran call.
 */
  NGCALLF(csa2xd,CSA2XD)(&ni, zt, zi, wts, knots, &ssmth,
          nderiv, &mo, &no, xo, yo, zo, &nwrk, work, ier);

  free(work);
  free(zt);

  if (*ier != 0) {
    return((double *)NULL);
  }

/*
 *  Rearrange the zo array to be row dominant.
 */
  zor = (double *) calloc(mo*no, sizeof(double));
  if (zor == NULL) {
    printf("Unable to allocate temp space in c_csa2xd\n");
    *ier = 300;
    return((double *)NULL);
  }

  for (i = 0; i < mo; i++) {
    for (j = 0; j < no; j++) {
      zor[i*no+j] = zo[j*mo+i];
    }
  }
  
  free(zo);

  return(zor);
}

double *c_csa2d(int ni, double xi[], double yi[], double zi[], int knots[2], 
               int mo, int no, double xo[], double yo[], int *ier)
{
  double wts[]={-1.}, ssmth=0.0;
  int   nderiv[]={0,0};
 
  return(c_csa2xd(ni,xi,yi,zi,wts,knots,ssmth,nderiv,mo,no,xo,yo,ier));

}

double *c_csa2lxd(int ni, double xi[], double yi[], double zi[], 
                 double wts[], int knots[2], double ssmth, int nderiv[2],
                 int no, double xo[], double yo[], int *ier)
{
  int   i, j, ncf, nwrk;
  double *zt, *zo, *zor, *work;
 
  zt   = (double *) calloc(2*ni, sizeof(double));
  if (zt == NULL) {
    printf("Unable to allocate temp space in c_csa2lxd\n");
    *ier = 300;
    return((double *)NULL);
  }
  zo   = (double *) calloc(no, sizeof(double));
  if (zo == NULL) {
    printf("Unable to allocate space for output in c_csa2lxd\n");
    *ier = 300;
    return((double *)NULL);
  }
  ncf = knots[0]*knots[1];
  nwrk = ncf*(ncf+3);
  work = (double *) calloc(nwrk, sizeof(double));
  if (work == NULL) {
    printf("Unable to allocate work space in c_csa2lxd\n");
    *ier = 300;
    return((double *)NULL);
  }

/*
 *  Build the input array for the Fortran call.
 */
  for (i = 0; i < ni; i++) {
    zt[2*i  ] = xi[i];
    zt[2*i+1] = yi[i];
  }

/*
 *  Make the Fortran call.
 */
  NGCALLF(csa2lxd,CSA2LXD)(&ni, zt, zi, wts, knots, &ssmth,
          nderiv, &no, xo, yo, zo, &nwrk, work, ier);

  free(work);
  free(zt);

  return(zo);
}

double *c_csa2ld(int ni, double xi[], double yi[], double zi[], int knots[2], 
                int no, double xo[], double yo[], int *ier)
{
  double wts[]={-1.}, ssmth=0.0;
  int   nderiv[]={0,0};
 
  return(c_csa2lxd(ni,xi,yi,zi,wts,knots,ssmth,nderiv,no,xo,yo,ier));
}

double *c_csa3d(int ni, double xi[], double yi[], double zi[], double ui[],
               int knots[3], int nxo, int nyo, int nzo, double xo[], 
               double yo[], double zo[], int *ier)
{
  double wts[]={-1.}, ssmth=0.0;
  int   nderiv[]={0,0,0};

  return(c_csa3xd(ni,xi,yi,zi,ui,wts,knots,ssmth,nderiv,nxo,nyo,nzo,
                  xo,yo,zo,ier));
}

double *c_csa3xd(int ni, double xi[], double yi[], double zi[], double ui[],
                double wts[], int knots[3], double ssmth, int nderiv[3],
                int nxo, int nyo, int nzo, double xo[], double yo[], 
                double zo[], int *ier)
{
  int   i, j, k, ncf, nwrk;
  double *zt, *uo, *zor, *work;
 
  zt   = (double *) calloc(3*ni, sizeof(double));
  if (zt == NULL) {
    printf("Unable to allocate temp space in c_csa3xd\n");
    *ier = 300;
    return((double *)NULL);
  }
  uo   = (double *) calloc(nxo*nyo*nzo, sizeof(double));
  if (uo == NULL) {
    printf("Unable to allocate space for output in c_csa3xd\n");
    *ier = 300;
    return((double *)NULL);
  }
  ncf = knots[0]*knots[1]*knots[2];
  nwrk = ncf*(ncf+3);
  work = (double *) calloc(nwrk, sizeof(double));
  if (work == NULL) {
    printf("Unable to allocate work space in c_csa3xd\n");
    *ier = 300;
    return((double *)NULL);
  }

/*
 *  Build the input array for the Fortran call.
 */
  for (i = 0; i < ni; i++) {
    zt[3*i  ] = xi[i];
    zt[3*i+1] = yi[i];
    zt[3*i+2] = zi[i];
  }

/*
 *  Make the Fortran call.
 */
  NGCALLF(csa3xd,CSA3XD)(&ni, zt, ui, wts, knots, &ssmth,
          nderiv, &nxo, &nyo, &nzo, xo, yo, zo, uo, &nwrk, work, ier);

  free(work);
  free(zt);

/*
 *  Rearrange the zo array to be row dominant.
 */
  zor = (double *) calloc(nxo*nyo*nzo, sizeof(double));
  if (zor == NULL) {
    printf("Unable to allocate temp space in c_csa3xd\n");
    *ier = 300;
    return((double *)NULL);
  }

  for (i = 0; i < nxo; i++) {
    for (j = 0; j < nyo; j++) {
      for (k = 0; k < nzo; k++) {
        zor[i*nzo*nyo + j*nzo + k] = uo[k*nxo*nyo + j*nxo + i];
      }
    }
  }
  
  free(uo);

  return(zor);
}

double *c_csa3lxd(int ni, double xi[], double yi[], double zi[], double ui[],
                 double wts[], int knots[3], double ssmth, int nderiv[3],
                 int no, double xo[], double yo[], double zo[], int *ier)
{
  int   i, j, ncf, nwrk;
  double *zt, *uo, *zor, *work;
 
  zt   = (double *) calloc(3*ni, sizeof(double));
  if (zt == NULL) {
    printf("Unable to allocate temp space in c_csa3lxd\n");
    *ier = 300;
    return((double *)NULL);
  }
  uo   = (double *) calloc(no, sizeof(double));
  if (uo == NULL) {
    printf("Unable to allocate space for output in c_csa3lxd\n");
    *ier = 300;
    return((double *)NULL);
  }
  ncf = knots[0]*knots[1]*knots[2];
  nwrk = ncf*(ncf+3);
  work = (double *) calloc(nwrk, sizeof(double));
  if (work == NULL) {
    printf("Unable to allocate work space in c_csa3lxd\n");
    *ier = 300;
    return((double *)NULL);
  }

/*
 *  Build the input array for the Fortran call.
 */
  for (i = 0; i < ni; i++) {
    zt[3*i  ] = xi[i];
    zt[3*i+1] = yi[i];
    zt[3*i+2] = zi[i];
  }

/*
 *  Make the Fortran call.
 */
  NGCALLF(csa3lxd,CSA3LXD)(&ni, zt, ui, wts, knots, &ssmth,
          nderiv, &no, xo, yo, zo, uo, &nwrk, work, ier);

  free(work);
  free(zt);

  return(uo);
}

double *c_csa3ld(int ni, double xi[], double yi[], double zi[], double ui[],
                int knots[2], int no, double xo[], double yo[], double zo[], 
                int *ier)
{
  double wts[]={-1.}, ssmth=0.0;
  int   nderiv[]={0,0,0};
 
  return(c_csa3lxd(ni,xi,yi,zi,ui,wts,knots,ssmth,nderiv,no,xo,yo,zo,ier));
}

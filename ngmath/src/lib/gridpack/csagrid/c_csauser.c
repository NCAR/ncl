#include <stdio.h>
#include <math.h>

#include "csaproto.h"

int c_csa1s(int n, float xi[], float yi[], int knots, 
            int m, float xo[], float yo[])
{
  float *work;
  int   nwrk, ier;

  nwrk = knots*(knots+3);
  work = (float *) calloc(nwrk, sizeof(float));
  if (work == NULL) {
    printf("Unable to allocate work space in c_csa1s\n");    
    ier = 300;
    return(ier);
  }
  
  NGCALLF(csa1s,CSA1S)(&n, xi, yi, &knots, &m, xo, yo, &nwrk, work, &ier);

  free(work);

  return(ier);
}

int c_csa1xs(int n, float xi[], float yi[], float wts[], int knots, 
             float ssmth, int deriv, int m, float xo[], float yo[])
{
  float *work;
  int   nwrk, ier;

  nwrk = knots*(knots+3);
  work = (float *) calloc(nwrk, sizeof(float));
  if (work == NULL) {
    printf("Unable to allocate work space in c_csa1xs\n");    
    ier = 300;
    return(ier);
  }
  
  NGCALLF(csa1xs,CSA1XS)(&n, xi, yi, wts, &knots, &ssmth, &deriv, &m, xo, yo, 
                         &nwrk, work, &ier);

  free(work);

  return(ier);
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
          nderiv, &mo, &no, xo, yo, zo, &nwrk, work, &ier);

  free(work);
  free(zt);

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
          nderiv, &no, xo, yo, zo, &nwrk, work, &ier);

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
          nderiv, &nxo, &nyo, &nzo, xo, yo, zo, uo, &nwrk, work, &ier);

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
          nderiv, &no, xo, yo, zo, uo, &nwrk, work, &ier);

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

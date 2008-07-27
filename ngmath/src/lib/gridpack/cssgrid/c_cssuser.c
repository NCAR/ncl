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

int *c_csstri(int n, float rlat[], float rlon[], int *nt, int *ier)
{
  int nit,nwrk,num_tri,*trlist,*iwork,i;
  double *rwork;

  nit = n;

/*
 *  Allocate space for returned output array.
 */
  trlist = (int *) calloc(6*nit, sizeof(int));
  if (trlist == NULL) {
    printf("Unable to allocate space for output in c_csstri\n");
    *ier = 300;
    return((int *)NULL);
  }

/*
 *  Allocate workspace arrays.
 */
  nwrk = 27*nit;
  iwork = (int *) calloc(nwrk, sizeof(int));
  if (iwork == NULL) {
    printf("Unable to allocate work space in c_csstri\n");
    *ier = 300;
    return((int *)NULL);
  }
  rwork = (double *) calloc(13*nit, sizeof(double));
  if (rwork == NULL) {
    printf("Unable to allocate work space in c_csstri\n");
    *ier = 300;
    return((int *)NULL);
  }

/*
 *  Make the Fortran call.
 */
  NGCALLF(csstri,CSSTRI)(&nit, rlat, rlon, &num_tri, trlist, 
          iwork, rwork, ier);
  if (*ier != 0) {
    printf("Error number %d returned from c_csstri\n",*ier);
    return((int *)NULL);
  }
  *nt = num_tri;

/*
 *  Allocate the exact amount of space for the triangle list.
 */
  trlist = (int *) realloc(trlist, 3*num_tri*sizeof(int));
  if (rwork == NULL) {
    printf("Unable to reallocate space for triangle list in c_csstri\n");
    *ier = 300;
    return((int *)NULL);
  }

/*
 *  Reduce the tiangle indices by 1 for compatibility with C.
 */
  for (i = 0; i < 3*num_tri; i++) {
    (*(trlist+i))--;
  }

  free(iwork);
  free(rwork);

  return(trlist);
}

void c_cstrans(int n, float *rlat, float *rlon,
                      float *x, float *y, float *z)
{
  int nn,i;
  float phi,theta,cosphi;

  nn = n;
  for (i = 0; i < nn; i++) {
    phi = rlat[i];
    theta = rlon[i];
    cosphi = cos(phi);
    x[i] = cosphi*cos(theta);
    y[i] = cosphi*sin(theta);
    z[i] = sin(phi);
  }
}

float *c_cssgrid(int ni, float rlat[], float rlon[], float f[],
                 int nlat, int nlon, float plat[], float plon[], int *ier)
{
  int   i, j, nit, nwrk, *iwork;
  float *ff, *zor;
  double *rwork;
 
  nit = ni;
/*
 *  Allocate space for returned output array.
 */
  ff = (float *) calloc(nlat*nlon, sizeof(float));
  if (ff == NULL) {
    printf("Unable to allocate space for output in c_cssgrid\n");
    *ier = 300;
    return((float *)NULL);
  }

/*
 *  Allocate workspace arrays.
 */
  nwrk = 27*nit;
  iwork = (int *) calloc(nwrk, sizeof(int));
  if (iwork == NULL) {
    printf("Unable to allocate work space in c_cssgrid\n");
    *ier = 300;
    return((float *)NULL);
  }
  rwork = (double *) calloc(13*nit, sizeof(double));
  if (rwork == NULL) {
    printf("Unable to allocate work space in c_cssgrid\n");
    *ier = 300;
    return((float *)NULL);
  }

/*
 *  Make the Fortran call.
 */
  NGCALLF(cssgrid,CSSGRID)(&nit, rlat, rlon, f, &nlat, &nlon, 
          plat, plon, ff, iwork, rwork, ier);
  if (*ier != 0) {
    printf("Error number %d returned from c_cssgrid\n",*ier);
    return((float *)NULL);
  }

  free(iwork);
  free(rwork);

/*
 *  Rearrange the ff array to be row dominant.
 */
  zor = (float *) calloc(nlat*nlon, sizeof(float));
  if (zor == NULL) {
    printf("Unable to allocate temp space in c_cssgrid\n");
    *ier = 300;
    return((float *)NULL);
  }

  for (i = 0; i < nlat; i++) {
    for (j = 0; j < nlon; j++) {
      zor[i*nlon+j] = ff[j*nlat+i];
    }
  }
  free(ff);

  return(zor);
}

void  c_csscoord(float px, float py, float pz, 
               float *plat, float *plon, float *pnrm){

  NGCALLF(csscoord,CSSCOORD)(&px, &py, &pz, plat, plon, pnrm);

}

void c_csvoro(int npts, float rlat[], float rlon[],
             int ni, int nf, float plat[], float plon[],
             float rc[], int *nca, int *numv, int nv[], int *ier)
{
  int i,nj,nit,nwrk,nc;

  static int *iwork;
  static double *rwork;
  static int ifirst = 0;

/*
 *  Free the previously created work arrays if this in not the
 *  first call to this function, but it is the first call to 
 *  this function for a new dataset.
 */
  if ((ifirst != 0) && (nf != 0)) {
    free(iwork);
    free(rwork);
  }
  ifirst++;
    
  nc = 2*npts;
  nj = ni+1;
  nit = npts;
  nwrk = 28*nit;
/*
 *  Allocate workspace if this is the first call to find
 *  Voronoi polygons for the given input dataset.
 */
  if (nf != 0) {
    iwork = (int *) calloc(nwrk, sizeof(int));
    if (iwork == NULL) {
      printf("Unable to allocate work space in c_csvoro\n");
      *ier = 300;
      return;
    }
    rwork = (double *) calloc(13*nit, sizeof(double));
    if (rwork == NULL) {
      printf("Unable to allocate work space in c_csvoro\n");
      *ier = 300;
      return;
    }
  }

/*
 *  Make the Fortran call.
 */
  NGCALLF(csvoro,CSVORO)(&nit, rlat, rlon, &nj, &nf, iwork, rwork, &nc,
                         plat, plon, rc, nca, numv, nv, ier);

  if (*ier != 0) {
    printf("Error number %d returned from c_csvoro\n",*ier);
    return;
  }
/*
 *  Adjust the indices in nv to be compatible with C.
 */
  for (i = 0; i < *numv; i++) {
    nv[i]--;
  }
}

void c_cssetr (char *cnp, float rvp)
{
    float rvp2;
    NGstring cnp2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !cnp ) {
        fprintf( stderr, "c_cssetr:  illegal parameter name (NULL)\n" );
        return;
    }

    rvp2 = rvp;

    len = NGSTRLEN(cnp);
    cnp2 = NGCstrToFstr(cnp,len);
    NGCALLF(cssetr,CSSETR)(cnp2,&rvp2,len);
}

void c_csgetr ( char *cnp, float *rvp)
{
    NGstring cnp2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !cnp ) {
        fprintf( stderr, "c_csgetr:  illegal parameter name (NULL)\n" );
        return;
    }

    len = NGSTRLEN(cnp);
    cnp2 = NGCstrToFstr(cnp,len);
    NGCALLF(csgetr,CSGETR)(cnp2,rvp,len);
}

void c_csseti (char *cnp, int ivp)
{
    NGstring cnp2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if ( !cnp ) {
        fprintf( stderr, "c_csseti:  illegal parameter name (NULL)\n");
        return;
    }
    len = NGSTRLEN(cnp);
    cnp2 = NGCstrToFstr(cnp,len);
    NGCALLF(csseti,CSSETI)(cnp2,&ivp,len);
}

void c_csgeti ( char *cnp, int *ivp)
{
    NGstring cnp2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if ( !cnp ) {
        fprintf( stderr, "c_csgeti:  illegal parameter name (NULL)\n");
        return;
    }
    len = NGSTRLEN(cnp);
    cnp2 = NGCstrToFstr(cnp,len);
    NGCALLF(csgeti,CSSETI)(cnp2,ivp,len);
}

int *c_csstrid(int n, double rlat[], double rlon[], int *nt, int *ier)
{
  int nit,nwrk,num_tri,*trlist,*iwork,i;
  double *rwork;

  nit = n;

/*
 *  Allocate space for returned output array.
 */
  trlist = (int *) calloc(6*nit, sizeof(int));
  if (trlist == NULL) {
    printf("Unable to allocate space for output in c_csstrid\n");
    *ier = 300;
    return((int *)NULL);
  }

/*
 *  Allocate workspace arrays.
 */
  nwrk = 27*nit;
  iwork = (int *) calloc(nwrk, sizeof(int));
  if (iwork == NULL) {
    printf("Unable to allocate work space in c_csstrid\n");
    *ier = 300;
    return((int *)NULL);
  }
  rwork = (double *) calloc(13*nit, sizeof(double));
  if (rwork == NULL) {
    printf("Unable to allocate work space in c_csstrid\n");
    *ier = 300;
    return((int *)NULL);
  }

/*
 *  Make the Fortran call.
 */
  NGCALLF(csstrid,CSSTRID)(&nit, rlat, rlon, &num_tri, trlist, 
          iwork, rwork, ier);
  if (*ier != 0) {
    printf("Error number %d returned from c_csstrid\n",*ier);
    return((int *)NULL);
  }
  *nt = num_tri;

/*
 *  Allocate the exact amount of space for the triangle list.
 */
  trlist = (int *) realloc(trlist, 3*num_tri*sizeof(int));
  if (rwork == NULL) {
    printf("Unable to reallocate space for triangle list in c_csstrid\n");
    *ier = 300;
    return((int *)NULL);
  }

/*
 *  Reduce the tiangle indices by 1 for compatibility with C.
 */
  for (i = 0; i < 3*num_tri; i++) {
    (*(trlist+i))--;
  }

  free(iwork);
  free(rwork);

  return(trlist);
}

void c_cstransd(int n, double *rlat, double *rlon,
                       double *x, double *y, double *z)
{
  int nn,i;
  double phi,theta,cosphi;

  nn = n;
  for (i = 0; i < nn; i++) {
    phi = rlat[i];
    theta = rlon[i];
    cosphi = cos(phi);
    x[i] = cosphi*cos(theta);
    y[i] = cosphi*sin(theta);
    z[i] = sin(phi);
  }
}

double *c_cssgridd(int ni, double rlat[], double rlon[], double f[],
                  int nlat, int nlon, double plat[], double plon[], int *ier)
{
  int   i, j, nit, nwrk, *iwork;
  double *ff, *zor, *rwork;
 
  nit = ni;
/*
 *  Allocate space for returned output array.
 */
  ff = (double *) calloc(nlat*nlon, sizeof(double));
  if (ff == NULL) {
    printf("Unable to allocate space for output in c_cssgridd\n");
    *ier = 300;
    return((double *)NULL);
  }

/*
 *  Allocate workspace arrays.
 */
  nwrk = 27*nit;
  iwork = (int *) calloc(nwrk, sizeof(int));
  if (iwork == NULL) {
    printf("Unable to allocate work space in c_cssgridd\n");
    *ier = 300;
    return((double *)NULL);
  }
  rwork = (double *) calloc(13*nit, sizeof(double));
  if (rwork == NULL) {
    printf("Unable to allocate work space in c_cssgridd\n");
    *ier = 300;
    return((double *)NULL);
  }

/*
 *  Make the Fortran call.
 */
  NGCALLF(cssgridd,CSSGRIDD)(&nit, rlat, rlon, f, &nlat, &nlon, 
          plat, plon, ff, iwork, rwork, ier);
  if (*ier != 0) {
    printf("Error number %d returned from c_cssgridd\n",*ier);
    return((double *)NULL);
  }

  free(iwork);
  free(rwork);

/*
 *  Rearrange the ff array to be row dominant.
 */
  zor = (double *) calloc(nlat*nlon, sizeof(double));
  if (zor == NULL) {
    printf("Unable to allocate temp space in c_cssgridd\n");
    *ier = 300;
    return((double *)NULL);
  }

  for (i = 0; i < nlat; i++) {
    for (j = 0; j < nlon; j++) {
      zor[i*nlon+j] = ff[j*nlat+i];
    }
  }
  free(ff);

  return(zor);
}

void  c_csscoordd(double px, double py, double pz, 
                  double *plat, double *plon, double *pnrm){

  NGCALLF(csscoordd,CSSCOORDD)(&px, &py, &pz, plat, plon, pnrm);

}

void c_csvorod(int npts, double rlat[], double rlon[],
             int ni, int nf, double plat[], double plon[],
             double rc[], int *nca, int *numv, int nv[], int *ier)
{
  int i,nj,nit,nwrk,nc;

  static int *iwork;
  static double *rwork;
  static int ifirst = 0;

/*
 *  Free the previously created work arrays if this in not the
 *  first call to this function, but it is the first call to 
 *  this function for a new dataset.
 */
  if ((ifirst != 0) && (nf != 0)) {
    free(iwork);
    free(rwork);
  }
  ifirst++;
    
  nc = 2*npts;
  nj = ni+1;
  nit = npts;
  nwrk = 28*nit;
/*
 *  Allocate workspace if this is the first call to find
 *  Voronoi polygons for the given input dataset.
 */
  if (nf != 0) {
    iwork = (int *) calloc(nwrk, sizeof(int));
    if (iwork == NULL) {
      printf("Unable to allocate work space in c_csvorod\n");
      *ier = 300;
      return;
    }
    rwork = (double *) calloc(13*nit, sizeof(double));
    if (rwork == NULL) {
      printf("Unable to allocate work space in c_csvorod\n");
      *ier = 300;
      return;
    }
  }

/*
 *  Make the Fortran call.
 */
  NGCALLF(csvorod,CSVOROD)(&nit, rlat, rlon, &nj, &nf, iwork, rwork, &nc,
                           plat, plon, rc, nca, numv, nv, ier);

  if (*ier != 0) {
    printf("Error number %d returned from c_csvorod\n",*ier);
    return;
  }
/*
 *  Adjust the indices in nv to be compatible with C.
 */
  for (i = 0; i < *numv; i++) {
    nv[i]--;
  }
}

void c_cssetd (char *cnp, double rvp)
{
    double rvp2;
    NGstring cnp2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !cnp ) {
        fprintf( stderr, "c_cssetd:  illegal parameter name (NULL)\n" );
        return;
    }

    rvp2 = rvp;

    len = NGSTRLEN(cnp);
    cnp2 = NGCstrToFstr(cnp,len);
    NGCALLF(cssetd,CSSETD)(cnp2,&rvp2,len);
}

void c_csgetd ( char *cnp, double *rvp)
{
    NGstring cnp2;
    int len;
/*
 * Make sure parameter name is not NULL
 */
    if( !cnp ) {
        fprintf( stderr, "c_csgetd:  illegal parameter name (NULL)\n" );
        return;
    }

    len = NGSTRLEN(cnp);
    cnp2 = NGCstrToFstr(cnp,len);
    NGCALLF(csgetd,CSGETD)(cnp2,rvp,len);
}

void  c_css2c(int n, float *rlat, float *rlon, float *x, 
              float *y, float *z) {

  NGCALLF(css2c,CSS2C)(&n, rlat, rlon, x, y, z);

}
void  c_css2cd(int n, double *rlat, double *rlon, double *x, 
              double *y, double *z) {

  NGCALLF(css2cd,CSS2CD)(&n, rlat, rlon, x, y, z);

}
void  c_csc2s(int n, float *x, float *y, float *z, 
              float *rlat, float *rlon) {

  NGCALLF(csc2s,CSC2S)(&n, x, y, z, rlat, rlon);

}
void  c_csc2sd(int n, double *x, double *y, double *z, 
              double *rlat, double *rlon) {

  NGCALLF(csc2sd,CSC2SD)(&n, x, y, z, rlat, rlon);

}

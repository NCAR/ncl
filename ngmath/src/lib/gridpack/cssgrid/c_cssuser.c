#include <stdio.h>
#include <math.h>

#include "cssproto.h"

int *c_csstri(int n, float xi[], float yi[], float zi[], 
              int *nt, int *ier)
{
  int nit,nwrk,num_tri,*trlist,*iwork,i;
  float *rwork;

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
  rwork = (float *) calloc(nit, sizeof(float));
  if (rwork == NULL) {
    printf("Unable to allocate work space in c_csstri\n");
    *ier = 300;
    return((int *)NULL);
  }

/*
 *  Make the Fortran call.
 */
  NGCALLF(csstri,CSSTRI)(&nit, xi, yi, zi, &num_tri, trlist, 
          iwork, rwork, ier);
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

float *c_cssgrid(int ni, float xi[], float yi[], float zi[], float f[],
                 int nlat, int nlon, float plat[], float plon[], int *ier)
{
  int   i, j, nit, nwrk, *iwork;
  float *ff, *zor, *rwork;
 
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
  nwrk = 15*nit;
  iwork = (int *) calloc(nwrk, sizeof(int));
  if (iwork == NULL) {
    printf("Unable to allocate work space in c_cssgrid\n");
    *ier = 300;
    return((float *)NULL);
  }
  rwork = (float *) calloc(nit, sizeof(float));
  if (rwork == NULL) {
    printf("Unable to allocate work space in c_cssgrid\n");
    *ier = 300;
    return((float *)NULL);
  }

/*
 *  Make the Fortran call.
 */
  NGCALLF(cssgrid,CSSGRID)(&nit, xi, yi, zi, f, &nlat, &nlon, 
          plat, plon, ff, iwork, rwork, ier);

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

void c_csvoro(int npts, float x[], float y[], float z[],
             int ni, int nf, float xc[], float yc[], float zc[],
             float rc[], int *nca, int *numv, int nv[], int *ier)
{
  int i,nj,nit,nwrk,nc;

  static int *iwork;
  static float *rwork;
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
    rwork = (float *) calloc(9*nit, sizeof(float));
    if (rwork == NULL) {
      printf("Unable to allocate work space in c_csvoro\n");
      *ier = 300;
      return;
    }
  }

/*
 *  Make the Fortran call.
 */
  NGCALLF(csvoro,CSVORO)(&nit, x, y, z, &nj, &nf, iwork, rwork, &nc,
                         xc, yc, zc, rc, nca, numv, nv, ier);

/*
 *  Adjust the indices in nv to be compatible with C.
 */
  for (i = 0; i < *numv; i++) {
    nv[i]--;
  }
}

#include <stdio.h>
/*
* The following are the required NCAR Graphics include files.
* They should be located in ${NCARG_ROOT}/include
*/
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/ncl/defs.h>
#include <ncarg/ncl/NclDataDefs.h>
#include <ncarg/ncl/NclBuiltInSupport.h>
#include <ncarg/gks.h>


#define min(x,y)   ((x) < (y) ? (x) : (y))
#define max(x,y)   ((x) > (y) ? (x) : (y))

extern float powf(float,float);

NhlErrorTypes dv2uvf_W( void )
{
/*
 * Input array variables
 */
  float *dv;
  int ndims_dv, dsizes_dv[NCL_MAX_DIMENSIONS], nt, nlat, nlon;
  NclScalar missing_dv;
  int has_missing_dv, found_missing=0;
/*
 * Output array variables
 */
  float *ud, *vd;
  int dsizes_ud[NCL_MAX_DIMENSIONS], dsizes_vd[NCL_MAX_DIMENSIONS];
  int ndims_ud, ndims_vd;
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  float scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lshaec, lvhsec;
  float *work, *wshaec, *wvhsec, *pertrb, *a, *b;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  dv = (float*)NclGetArgValue(
           0,
           3,
           &ndims_dv, 
           dsizes_dv,
           &missing_dv,
           &has_missing_dv,
           NULL,
           2);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_dv < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvf: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }
  nlat = dsizes_dv[ndims_dv-2];
  nlon = dsizes_dv[ndims_dv-1];
/*
 * Compute the total number of elements in our array.
 */
  nt = 1;
  for(i = 0; i < ndims_dv-2; i++) {
    nt *= dsizes_dv[i];
  }
/*
 * Get output arrays.
 */
  ud = (float*)NclGetArgValue(
           1,
           3,
           &ndims_ud, 
           dsizes_ud,
           NULL,
           NULL,
           NULL,
           1);
  vd = (float*)NclGetArgValue(
           2,
           3,
           &ndims_vd, 
           dsizes_vd,
           NULL,
           NULL,
           NULL,
           1);
/*
 * The output arrays must also be 2 or 3-dimensional.
 */
  if( ndims_ud != ndims_dv || ndims_vd != ndims_dv ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvf: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_dv; i++ ) {
    if( dsizes_ud[i] != dsizes_dv[i] || dsizes_vd[i] != dsizes_dv[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvf: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Check for missing values.
 */
  if(has_missing_dv) {
    l = 0;
    while( l < nt*nlat*nlon && !found_missing ) {
      if(dv[l++] == missing_dv.floatval) found_missing = 1;
    }
  }
  if(found_missing) {
    for(i = 0; i < nt*nlat*nlon; i++) {
      ud[i] = vd[i] = missing_dv.floatval;
    }
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvf: The input array cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlon * nlat;
  work = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(geomat,GEOMAT)(&nlon,&nlat,&dv[j],work);
    j += nlat*nlon;
  }
  free(work);

/*
 * shaec performs the spherical harmonic analysis on a (scalar) gaussian
 * grid(s) and returns the coefficients in array(s) a,b.
 * Here the scalar grid is "dv" (divergence) 
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(2*(nlat+1),nlat*(nt*nlon+max(3*l2,nlon)));
  ldwork = nlat+1;
  lshaec = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;
  a =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  b =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  wshaec = (float*)calloc(        lshaec*sizeof(float),1);
  work   = (float*)calloc(         lwork*sizeof(float),1);
  dwork  = (double*)calloc(        ldwork*sizeof(double),1);

  if( a == NULL || b == NULL || wshaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(shaeci,SHAECI)(&nlat,&nlon,wshaec,&lshaec,dwork,&ldwork,&jer);
  NGCALLF(shaec,SHAEC)(&nlat,&nlon,&isym,&nt,&dv[0],&idvw,&jdvw,
		       a,b,&mdab,&ndab,wshaec,&lshaec,work,&lwork,&ker);
  free(wshaec);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("dv2uvf","shaec",&ier,&jer,&ker,&mer,8,5);
/* 
 * Reconstruct the divergent (irrotational) wind components.
 * Note the argument order idivec(...,vd,ud,...)
 */
  lwork  = max(4*(nlat+1),nlat*(2*nt*nlon+max(6*l2,nlon)+2*nt*l1+1));
  lvhsec = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15;
  ldwork = 2*(nlat+2);

  wvhsec = (float*)calloc(lvhsec*sizeof(float),1);
  work   = (float*)calloc( lwork*sizeof(float),1);
  dwork = (double*)calloc( ldwork*sizeof(double),1);
  pertrb = (float*)calloc(    nt*sizeof(float),1);
  if( pertrb == NULL || wvhsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(vhseci,VHSECI)(&nlat,&nlon,wvhsec,&lvhsec,dwork,&ldwork,&jer);
  NGCALLF(idivec,IDIVEC)(&nlat,&nlon,&isym,&nt,&vd[0],&ud[0],
			 &idvw,&jdvw,a,b,&mdab,&ndab,wvhsec,&lvhsec,
			 work,&lwork,pertrb,&ker);

  free(a);
  free(b);
  free(wvhsec);
  free(work);
  free(dwork);
  free(pertrb);
  NGCALLF(chkerr,CHKERR)("dv2uvf","vhseci,divec",&ier,&jer,&ker,&mer,6,12);

/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
 
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&dv[j],work);
    NGCALLF(matgeov,MATGEOV)(&nlat,&nlon,&ud[j],&vd[j],work);
    j += nlat*nlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 6.37122e+6;         /* radius of earth */

  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&ud[0],&scale,&ner);
  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&vd[0],&scale,&ner);
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return
 */
  return(NhlNOERROR);
}


NhlErrorTypes dv2uvg_W( void )
{
/*
 * Input array variables
 */
  float *dv;
  int ndims_dv, dsizes_dv[NCL_MAX_DIMENSIONS], nt, nlat, nlon;
  NclScalar missing_dv;
  int has_missing_dv, found_missing=0;
/*
 * Output array variables
 */
  float *ud, *vd;
  int dsizes_ud[NCL_MAX_DIMENSIONS], dsizes_vd[NCL_MAX_DIMENSIONS];
  int ndims_ud, ndims_vd;
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  float scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lshagc, lvhsgc;
  float *work, *wshagc, *wvhsgc, *pertrb, *a, *b;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  dv = (float*)NclGetArgValue(
           0,
           3,
           &ndims_dv, 
           dsizes_dv,
           &missing_dv,
           &has_missing_dv,
           NULL,
           2);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_dv < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvg: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }
  nlat = dsizes_dv[ndims_dv-2];
  nlon = dsizes_dv[ndims_dv-1];
/*
 * Compute the total number of elements in our array.
 */
  nt = 1;
  for(i = 0; i < ndims_dv-2; i++) {
    nt *= dsizes_dv[i];
  }
/*
 * Get output arrays.
 */
  ud = (float*)NclGetArgValue(
           1,
           3,
           &ndims_ud, 
           dsizes_ud,
           NULL,
           NULL,
           NULL,
           1);
  vd = (float*)NclGetArgValue(
           2,
           3,
           &ndims_vd, 
           dsizes_vd,
           NULL,
           NULL,
           NULL,
           1);
/*
 * The output arrays must also be 2 or 3-dimensional.
 */
  if( ndims_ud != ndims_dv || ndims_vd != ndims_dv ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvg: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_dv; i++ ) {
    if( dsizes_ud[i] != dsizes_dv[i] || dsizes_vd[i] != dsizes_dv[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvg: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Check for missing values.
 */
  if(has_missing_dv) {
    l = 0;
    while( l < nt*nlat*nlon && !found_missing ) {
      if(dv[l++] == missing_dv.floatval) found_missing = 1;
    }
  }
  if(found_missing) {
    for(i = 0; i < nt*nlat*nlon; i++) {
      ud[i] = vd[i] = missing_dv.floatval;
    }
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvg: The input array cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlon * nlat;
  work = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++) {
    NGCALLF(geomat,GEOMAT)(&nlon,&nlat,&dv[j],work);
    j += nlat*nlon;
  }
  free(work);
/*
 * shagc performs the spherical harmonic analysis on a (scalar) gaussian 
 * grid(s) and returns the coefficients in array(s) a,b.
 * Here the scalar grid is "dv" (divergence) 
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*nlat*(nlat+2)+2,nlat*(nlon*nt+max(3*l2,nlon)));
  lshagc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;
  lwork *= 10;
  ldwork = nlat*(nlat+4);

  a =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  b =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  wshagc = (float*)calloc(        lshagc*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  dwork =  (double*)calloc(       ldwork*sizeof(double),1);

  if( a == NULL || b == NULL || wshagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(shagci,SHAGCI)(&nlat,&nlon,wshagc,&lshagc,dwork,&ldwork,&jer);
  NGCALLF(shagc,SHAGC)(&nlat,&nlon,&isym,&nt,&dv[0],&idvw,&jdvw,
		       a,b,&mdab,&ndab,wshagc,&lshagc,work,&lwork,&ker);
  free(wshagc);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("dv2uvg","shagc",&ier,&jer,&ker,&mer,8,5);
/* 
 * reconstruct the divergent (irrotational) wind components.
 * note the argument order idivgc(...,vd,ud,...)
 */
  lwork  = max(4*nlat*(nlat+1)+2,nlat*(2*nt*nlon+max(6*l2,nlon)+2*nt*l1+1));
  lvhsgc = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15;
  ldwork = 2*nlat*(nlat+1)+1;

  wvhsgc = (float*)calloc(lvhsgc*sizeof(float),1);
  work   = (float*)calloc( lwork*sizeof(float),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);

  pertrb = (float*)calloc(    nt*sizeof(float),1);
  if( pertrb == NULL || wvhsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(vhsgci,VHSGCI)(&nlat,&nlon,wvhsgc,&lvhsgc,dwork,&ldwork,&jer);
  NGCALLF(idivgc,IDIVGC)(&nlat,&nlon,&isym,&nt,&vd[0],&ud[0],
			 &idvw,&jdvw,a,b,&mdab,&ndab,wvhsgc,&lvhsgc,
			 work,&lwork,pertrb,&ker);

  free(a);
  free(b);
  free(wvhsgc);
  free(work);
  free(dwork);
  free(pertrb);

  NGCALLF(chkerr,CHKERR)("dv2uvg","vhsgci,divgc",&ier,&jer,&ker,&mer,6,12);
/* 
 * transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
 
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&dv[j],work);
    NGCALLF(matgeov,MATGEOV)(&nlat,&nlon,&ud[j],&vd[j],work);
    j += nlat*nlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 6.37122e+6;         /* radius of earth */

  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&ud[0],&scale,&ner);
  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&vd[0],&scale,&ner);
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return
 */
  return(NhlNOERROR);
}


NhlErrorTypes gradsf_W( void )
{
/*
 * Input array variables
 */
  float *z;
  int ndims_z, dsizes_z[NCL_MAX_DIMENSIONS], nt, nlat, nlon;
  NclScalar missing_z;
  int has_missing_z, found_missing=0;
/*
 * Output array variables
 */
  float *gzx, *gzy;
  int dsizes_gzx[NCL_MAX_DIMENSIONS], dsizes_gzy[NCL_MAX_DIMENSIONS];
  int ndims_gzx, ndims_gzy;
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  float scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lshaec, lvhsec;
  float *work, *wshaec, *wvhsec, *a, *b;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  z = (float*)NclGetArgValue(
           0,
           3,
           &ndims_z, 
           dsizes_z,
           &missing_z,
           &has_missing_z,
           NULL,
           2);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_z < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsf: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }
  nlat = dsizes_z[ndims_z-2];
  nlon = dsizes_z[ndims_z-1];
/*
 * Compute the total number of elements in our array.
 */
  nt = 1;
  for(i = 0; i < ndims_z-2; i++) {
    nt *= dsizes_z[i];
  }
/*
 * Get output arrays.
 */
  gzx = (float*)NclGetArgValue(
           1,
           3,
           &ndims_gzx, 
           dsizes_gzx,
           NULL,
           NULL,
           NULL,
           1);
  gzy = (float*)NclGetArgValue(
           2,
           3,
           &ndims_gzy, 
           dsizes_gzy,
           NULL,
           NULL,
           NULL,
           1);
/*
 * The output arrays must also be at least 2-dimensional.
 */
  if( ndims_gzx != ndims_z || ndims_gzy != ndims_z ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsf: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_z; i++ ) {
    if( dsizes_gzx[i] != dsizes_z[i] || dsizes_gzy[i] != dsizes_z[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsf: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Check for missing values.
 */
  if(has_missing_z) {
    l = 0;
    while( l < nt*nlat*nlon && !found_missing ) {
      if(z[l++] == missing_z.floatval) found_missing = 1;
    }
  }
  if(found_missing) {
    for(i = 0; i < nt*nlat*nlon; i++) {
      gzx[i] = gzy[i] = missing_z.floatval;
    }
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsf: The input array cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlon * nlat;
  work = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(geomat,GEOMAT)(&nlon,&nlat,&z[j],work);
    j += nlat*nlon;
  }
  free(work);
/*
 * shaec performs the spherical harmonic analysis on a (scalar) gaussian 
 * grid(s) and returns the coefficients in array(s) a,b.
 * Here the scalar grid is "z" (a scalar divergence)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(2*(nlat+1),nlat*(nt*nlon+max(3*l2,nlon)));
  ldwork = nlat+1;
  lshaec = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;

  a =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  b =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  wshaec = (float*)calloc(        lshaec*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  dwork  = (double*)calloc(        ldwork*sizeof(double),1);

  if( a == NULL || b == NULL || wshaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(shaeci,SHAECI)(&nlat,&nlon,wshaec,&lshaec,dwork,&ldwork,&jer);
  NGCALLF(shaec,SHAEC)(&nlat,&nlon,&isym,&nt,&z[0],&idvw,&jdvw,
		       a,b,&mdab,&ndab,wshaec,&lshaec,work,&lwork,&ker);
  free(wshaec);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("gradsf","shaec",&ier,&jer,&ker,&mer,8,5);
/*
 * compute the gradient.
 * note the argument order (...,gzy,gzx,...)
 */ 
  lwork  = max(4*nlat*(nlat+1)+2,nlat*(2*nt*nlon+max(6*l2,nlon))+nlat*(2*l1*nt+1));
  lvhsec = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15;
  ldwork = 2*(nlat+2);

  lwork *= 10;

  work   = (float*)calloc( lwork*sizeof(float),1);
  dwork = (double*)calloc( ldwork*sizeof(double),1);
  wvhsec = (float*)calloc(lvhsec*sizeof(float),1);
  if( wvhsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(vhseci,VHSECI)(&nlat,&nlon,wvhsec,&lvhsec,dwork,&ldwork,&jer);
  NGCALLF(gradec,GRADEC)(&nlat,&nlon,&isym,&nt,&gzy[0],&gzx[0],
			 &idvw,&jdvw,a,b,&mdab,&ndab,wvhsec,&lvhsec,
			 work,&lwork,&ker);

  free(a);
  free(b);
  free(wvhsec);
  free(work);
  free(dwork);

  NGCALLF(chkerr,CHKERR)("gradsf","vhseci+gradec",&ier,&jer,&ker,&mer,8,13);
/* 
 * transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
 
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&z[j],work);
    NGCALLF(matgeov,MATGEOV)(&nlat,&nlon,&gzx[j],&gzy[j],work);
    j += nlat*nlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 1./6.37122e+6;       /* 1/(radius of earth) */

  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&gzx[0],&scale,&ner);
  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&gzy[0],&scale,&ner);
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return
 */
  return(NhlNOERROR);
}


NhlErrorTypes gradsg_W( void )
{
/*
 * Input array variables
 */
  float *z;
  int ndims_z, dsizes_z[NCL_MAX_DIMENSIONS], nt, nlat, nlon;
  NclScalar missing_z;
  int has_missing_z, found_missing=0;
/*
 * Output array variables
 */
  float *gzx, *gzy;
  int dsizes_gzx[NCL_MAX_DIMENSIONS], dsizes_gzy[NCL_MAX_DIMENSIONS];
  int ndims_gzx, ndims_gzy;
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  float scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lshagc, lvhsgc;
  float *work, *wshagc, *wvhsgc, *a, *b;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  z = (float*)NclGetArgValue(
           0,
           3,
           &ndims_z, 
           dsizes_z,
           &missing_z,
           &has_missing_z,
           NULL,
           2);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_z < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsg: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }
  nlat = dsizes_z[ndims_z-2];
  nlon = dsizes_z[ndims_z-1];
/*
 * Compute the total number of elements in our array.
 */
  nt = 1;
  for(i = 0; i < ndims_z-2; i++) {
    nt *= dsizes_z[i];
  }
/*
 * Get output arrays.
 */
  gzx = (float*)NclGetArgValue(
           1,
           3,
           &ndims_gzx, 
           dsizes_gzx,
           NULL,
           NULL,
           NULL,
           1);
  gzy = (float*)NclGetArgValue(
           2,
           3,
           &ndims_gzy, 
           dsizes_gzy,
           NULL,
           NULL,
           NULL,
           1);
/*
 * The output arrays must also be at least 2-dimensional.
 */
  if( ndims_gzx != ndims_z || ndims_gzy != ndims_z ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsg: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_z; i++ ) {
    if( dsizes_gzx[i] != dsizes_z[i] || dsizes_gzy[i] != dsizes_z[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsg: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Check for missing values.
 */
  if(has_missing_z) {
    l = 0;
    while( l < nt*nlat*nlon && !found_missing ) {
      if(z[l++] == missing_z.floatval) found_missing = 1;
    }
  }
  if(found_missing) {
    for(i = 0; i < nt*nlat*nlon; i++) {
      gzx[i] = gzy[i] = missing_z.floatval;
    }
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsg: The input array cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlon * nlat;
  work = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(geomat,GEOMAT)(&nlon,&nlat,&z[j],work);
    j += nlat*nlon;
  }
  free(work);
/*
 * shagc performs the spherical harmonic analysis on a (scalar) gaussian 
 * grid(s) and returns the coefficients in array(s) a,b.
 * Here the scalar grid is "z" (a scalar divergence)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*nlat*(nlat+2)+2,nlat*(nlon*nt+max(3*l2,nlon)));
  lshagc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;
  ldwork = nlat*(nlat+4);

  a =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  b =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  wshagc = (float*)calloc(        lshagc*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  dwork =  (double*)calloc(       ldwork*sizeof(double),1);

  if( a == NULL || b == NULL || wshagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(shagci,SHAGCI)(&nlat,&nlon,wshagc,&lshagc,dwork,&ldwork,&jer);
  NGCALLF(shagc,SHAGC)(&nlat,&nlon,&isym,&nt,&z[0],&idvw,&jdvw,
		       a,b,&mdab,&ndab,wshagc,&lshagc,work,&lwork,&ker);
  
  free(wshagc);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("gradsg","shagc",&ier,&jer,&ker,&mer,8,5);
/*
 * compute the gradient.
 */ 
  lwork  = max(4*nlat*(nlat+1)+2,nlat*(2*nt*nlon+max(6*l2,nlon))+nlat*(2*l1*nt+1));
  lvhsgc = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15;

  lwork *= 10;
  lvhsgc *= 10;
  ldwork = 2*nlat*(nlat+1)+1;

  work   = (float*)calloc( lwork*sizeof(float),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);
  wvhsgc = (float*)calloc(lvhsgc*sizeof(float),1);
  if( wvhsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(vhsgci,VHSGCI)(&nlat,&nlon,wvhsgc,&lvhsgc,dwork,&ldwork,&jer);
  NGCALLF(gradgc,GRADGC)(&nlat,&nlon,&isym,&nt,&gzy[0],&gzx[0],
			 &idvw,&jdvw,a,b,&mdab,&ndab,wvhsgc,&lvhsgc,
			 work,&lwork,&ker);

  free(a);
  free(b);
  free(wvhsgc);
  free(work);
  free(dwork);

  NGCALLF(chkerr,CHKERR)("gradsg","vhsgci+gradgc",&ier,&jer,&ker,&mer,8,13);
/* 
 * transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
 
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&z[j],work);
    NGCALLF(matgeov,MATGEOV)(&nlat,&nlon,&gzx[j],&gzy[j],work);
    j += nlat*nlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 1./6.37122e+6;       /* 1/(radius of earth) */

  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&gzx[0],&scale,&ner);
  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&gzy[0],&scale,&ner);
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return
 */
  return(NhlNOERROR);
}


NhlErrorTypes igradsf_W( void )
{
/*
 * Input array variables
 */
  float *gzx, *gzy;
  int dsizes_gzx[NCL_MAX_DIMENSIONS], dsizes_gzy[NCL_MAX_DIMENSIONS];
  int ndims_gzx, ndims_gzy;
  NclScalar missing_gzx, missing_gzy;
  float missing;
  int has_missing_gzx, has_missing_gzy, found_missing=0;
  int nt, nlat, nlon;
/*
 * Output array variables
 */
  float *z;
  int dsizes_z[NCL_MAX_DIMENSIONS];
  int ndims_z;
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2, l3;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  float scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhaec, lshsec;
  float *work, *wvhaec, *wshsec, *br, *bi, *cr, *ci;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  gzx = (float*)NclGetArgValue(
           0,
           3,
           &ndims_gzx, 
           dsizes_gzx,
           &missing_gzx,
           &has_missing_gzx,
           NULL,
           2);
  gzy = (float*)NclGetArgValue(
           1,
           3,
           &ndims_gzy, 
           dsizes_gzy,
           &missing_gzy,
           &has_missing_gzy,
           NULL,
           2);
/*
 * The grids coming in must be at least 2-dimensional.
 */
  if( ndims_gzx != ndims_gzy || ndims_gzx < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsf: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_gzx; i++ ) {
    if( dsizes_gzx[i] != dsizes_gzy[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsf: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  nlat = dsizes_gzx[ndims_gzx-2];
  nlon = dsizes_gzx[ndims_gzx-1];
/*
 * Compute the total number of elements in our array.
 */
  nt = 1;
  for(i = 0; i < ndims_gzx-2; i++) {
    nt *= dsizes_gzx[i];
  }
/*
 * Get output array.
 */
  z = (float*)NclGetArgValue(
           2,
           3,
           &ndims_z, 
           dsizes_z,
           NULL,
           NULL,
           NULL,
           1);
/*
 * The output array must also be at least 2-dimensional.
 */
  if( ndims_z != ndims_gzx  ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsf: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_gzx; i++ ) {
    if( dsizes_z[i] != dsizes_gzx[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsf: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Check for missing values.
 */
  if(has_missing_gzx || has_missing_gzy) {
    l = 0;
    while( l < nt*nlat*nlon && !found_missing ) {
      if(gzx[l] == missing_gzx.floatval || gzy[l] == missing_gzy.floatval) {
	found_missing = 1;
      }
      l++;
    }
  }
  if(found_missing) {
    if(has_missing_gzx) missing = missing_gzx.floatval;
    else                missing = missing_gzy.floatval;
    for(i = 0; i < nt*nlat*nlon; i++) {
      z[i] = missing;
    }
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsf: The input arrays cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlon * nlat;
  work = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(geomatv,GEOMATV)(&nlon,&nlat,&gzx[j],&gzy[j],work);
    j += nlat*nlon;
  }
  free(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 * dynamically allocate various temporary space.
 * Note the order "vhaec(...,v,u,....)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*(nlat+1),nlat*(2*nlon*nt+max(6*l2,nlon)));
  ldwork = 2*(nlat+2);
  lvhaec = 4*nlat*l2+3*max(l1-2,0)*(nlat+nlat-l1-1)+nlon+15;

  lwork  *= 10;
  lvhaec *= 10;

  br =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  bi =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  cr =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  ci =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  wvhaec = (float*)calloc(        lvhaec*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  dwork = (double*)calloc(        ldwork*sizeof(double),1);

  if( br == NULL || bi == NULL || cr == NULL || ci == NULL ||
      wvhaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(vhaeci,VHAECI)(&nlat,&nlon,wvhaec,&lvhaec,dwork,&ldwork,&jer);
  NGCALLF(vhaec,VHAEC)(&nlat,&nlon,&isym,&nt,&gzy[0],&gzx[0],
		       &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,wvhaec,&lvhaec,
		       work,&lwork,&ker);
  free(cr);
  free(ci);
  free(wvhaec);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("igradsf","vhaec",&ier,&jer,&ker,&mer,9,5);
/*
 * compute the scalar function given the input vector
 */
  l3     = max(nlat,(nlon+2)/2);
  lwork  = max(nlat+1,nlat*(nt*nlon+max(3*l2,nlon)+2*nt*l3+1));
  ldwork = nlat+1;
  lshsec = 2*nlat*l2+3*(max(l1-2,0)*(2*nlat-l1-1))/2+nlon+15;

  lwork  *= 10;
  lshsec *= 10;
  work   = (float*)calloc( lwork*sizeof(float),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);
  wshsec = (float*)calloc(lshsec*sizeof(float),1);
  if( wshsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(shseci,SHSECI)(&nlat,&nlon,wshsec,&lshsec,dwork,&ldwork,&jer);
  NGCALLF(igradec,IGRADEC)(&nlat,&nlon,&isym,&nt,&z[0],&idvw,&jdvw,br,bi,
			   &mdab,&ndab,wshsec,&lshsec,work,&lwork,&ker);
  if (ker) {
    printf("shseci+igradec: lwork,lshsec= %i7 %i7\n",lwork,lshsec);
  }

  free(br);
  free(bi);
  free(wshsec);
  free(work);
  free(dwork);
/* 
 * transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
 
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&z[j],work);
    NGCALLF(matgeov,MATGEOV)(&nlat,&nlon,&gzx[j],&gzy[j],work);
    j += nlat*nlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 6.37122e+6;         /* radius of earth */

  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&z[0],&scale,&ner);
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return array.
 */
  return(NhlNOERROR);
}


NhlErrorTypes igradsF_W( void )
{
/*
 * Input array variables
 */
  float *gzx, *gzy;
  int dsizes_gzx[NCL_MAX_DIMENSIONS], dsizes_gzy[NCL_MAX_DIMENSIONS];
  int ndims_gzx, ndims_gzy;
  NclScalar missing_gzx, missing_gzy;
  float missing;
  int has_missing_gzx, has_missing_gzy, found_missing=0;
  int nt, nlat, nlon;
/*
 * Output array variables
 */
  float *z;
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2, l3;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  float scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhaec, lshsec;
  float *work, *wvhaec, *wshsec, *br, *bi, *cr, *ci;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  gzx = (float*)NclGetArgValue(
           0,
           2,
           &ndims_gzx, 
           dsizes_gzx,
           &missing_gzx,
           &has_missing_gzx,
           NULL,
           2);
  gzy = (float*)NclGetArgValue(
           1,
           2,
           &ndims_gzy, 
           dsizes_gzy,
           &missing_gzy,
           &has_missing_gzy,
           NULL,
           2);
/*
 * The grids coming in must be at least 2-dimensional.
 */
  if( ndims_gzx != ndims_gzy || ndims_gzx < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsF: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_gzx; i++ ) {
    if( dsizes_gzx[i] != dsizes_gzy[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsF: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  nlat = dsizes_gzx[ndims_gzx-2];
  nlon = dsizes_gzx[ndims_gzx-1];
/*
 * Compute the total number of elements in our array.
 */
  nt = 1;
  for(i = 0; i < ndims_gzx-2; i++) {
    nt *= dsizes_gzx[i];
  }
/*
 * Allocate space for output array.
 */
  z = (float*)calloc(nt*nlat*nlon*sizeof(float),1);
  if( z == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsF: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Check for missing values.
 */
  if(has_missing_gzx || has_missing_gzy) {
    l = 0;
    while( l < nt*nlat*nlon && !found_missing ) {
      if(gzx[l] == missing_gzx.floatval || gzy[l] == missing_gzy.floatval) {
	found_missing = 1;
      }
      l++;
    }
  }
  if(found_missing) {
    if(has_missing_gzx) missing = missing_gzx.floatval;
    else                missing = missing_gzy.floatval;
    for(i = 0; i < nt*nlat*nlon; i++) {
      z[i] = missing;
    }
    NhlPError(NhlWARNING,NhlEUNKNOWN,"igradsF: The input arrays cannot contain any missing values");
    return(NclReturnValue((void*)z,ndims_gzx,dsizes_gzx,NULL,NCL_float,0));
  }
/*
 * Determine the workspace size.
 */
  lwork = nlon * nlat;
  work = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsF: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(geomatv,GEOMATV)(&nlon,&nlat,&gzx[j],&gzy[j],work);
    j += nlat*nlon;
  }
  free(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 * dynamically allocate various temporary space.
 * Note the order "vhaec(...,v,u,....)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*(nlat+1),nlat*(2*nlon*nt+max(6*l2,nlon)));
  ldwork = 2*(nlat+2);
  lvhaec = 4*nlat*l2+3*max(l1-2,0)*(nlat+nlat-l1-1)+nlon+15;

  lwork  *= 10;
  lvhaec *= 10;

  br =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  bi =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  cr =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  ci =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  wvhaec = (float*)calloc(        lvhaec*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  dwork = (double*)calloc(        ldwork*sizeof(double),1);

  if( br == NULL || bi == NULL || cr == NULL || ci == NULL ||
      wvhaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsF: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(vhaeci,VHAECI)(&nlat,&nlon,wvhaec,&lvhaec,dwork,&ldwork,&jer);
  NGCALLF(vhaec,VHAEC)(&nlat,&nlon,&isym,&nt,&gzy[0],&gzx[0],
		       &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,wvhaec,&lvhaec,
		       work,&lwork,&ker);
  free(cr);
  free(ci);
  free(wvhaec);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("igradsF","vhaec",&ier,&jer,&ker,&mer,9,5);
/*
 * compute the scalar function given the input vector
 */
  l3     = max(nlat,(nlon+2)/2);
  lwork  = max(nlat+1,nlat*(nt*nlon+max(3*l2,nlon)+2*nt*l3+1));
  ldwork  = nlat+1;
  lshsec = 2*nlat*l2+3*(max(l1-2,0)*(2*nlat-l1-1))/2+nlon+15;

  lwork  *= 10;
  lshsec *= 10;
  work   = (float*)calloc( lwork*sizeof(float),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);
  wshsec = (float*)calloc(lshsec*sizeof(float),1);
  if( wshsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsF: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(shseci,SHSECI)(&nlat,&nlon,wshsec,&lshsec,dwork,&ldwork,&jer);
  NGCALLF(igradec,IGRADEC)(&nlat,&nlon,&isym,&nt,&z[0],&idvw,&jdvw,br,bi,
			   &mdab,&ndab,wshsec,&lshsec,work,&lwork,&ker);
  if (ker) {
    printf("shseci+igradec: lwork,lshsec= %i7 %i7\n",lwork,lshsec);
  }

  free(br);
  free(bi);
  free(wshsec);
  free(work);
  free(dwork);
/* 
 * transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
 
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsF: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&z[j],work);
    NGCALLF(matgeov,MATGEOV)(&nlat,&nlon,&gzx[j],&gzy[j],work);
    j += nlat*nlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 6.37122e+6;         /* radius of earth */

  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&z[0],&scale,&ner);
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return array.
 */
  return(NclReturnValue((void*)z,ndims_gzx,dsizes_gzx,NULL,NCL_float,0));
}



NhlErrorTypes igradsg_W( void )
{
/*
 * Input array variables
 */
  float *gzx, *gzy;
  int dsizes_gzx[NCL_MAX_DIMENSIONS], dsizes_gzy[NCL_MAX_DIMENSIONS];
  int ndims_gzx, ndims_gzy;
  NclScalar missing_gzx, missing_gzy;
  float missing;
  int has_missing_gzx, has_missing_gzy, found_missing=0;
  int nt, nlat, nlon;
/*
 * Output array variables
 */
  float *z;
  int dsizes_z[NCL_MAX_DIMENSIONS];
  int ndims_z;
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2, l3;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  float scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhagc, lshsgc;
  float *work, *wvhagc, *wshsgc, *br, *bi, *cr, *ci;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  gzx = (float*)NclGetArgValue(
           0,
           3,
           &ndims_gzx, 
           dsizes_gzx,
           &missing_gzx,
           &has_missing_gzx,
           NULL,
           2);
  gzy = (float*)NclGetArgValue(
           1,
           3,
           &ndims_gzy, 
           dsizes_gzy,
           &missing_gzy,
           &has_missing_gzy,
           NULL,
           2);
/*
 * The grids coming in must be at least 2-dimensional.
 */
  if( ndims_gzx != ndims_gzy || ndims_gzx < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsg: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_gzx; i++ ) {
    if( dsizes_gzx[i] != dsizes_gzy[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsg: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  nlat = dsizes_gzx[ndims_gzx-2];
  nlon = dsizes_gzx[ndims_gzx-1];
/*
 * Compute the total number of elements in our array.
 */
  nt = 1;
  for(i = 0; i < ndims_gzx-2; i++) {
    nt *= dsizes_gzx[i];
  }
/*
 * Get output array.
 */
  z = (float*)NclGetArgValue(
           2,
           3,
           &ndims_z, 
           dsizes_z,
           NULL,
           NULL,
           NULL,
           1);
/*
 * The output array must also be at least 2-dimensional.
 */
  if( ndims_z != ndims_gzx  ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsg: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_gzx; i++ ) {
    if( dsizes_z[i] != dsizes_gzx[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsg: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Check for missing values.
 */
  if(has_missing_gzx || has_missing_gzy) {
    l = 0;
    while( l < nt*nlat*nlon && !found_missing ) {
      if(gzx[l] == missing_gzx.floatval || gzy[l] == missing_gzy.floatval) {
	found_missing = 1;
      }
      l++;
    }
  }
  if(found_missing) {
    if(has_missing_gzx) missing = missing_gzx.floatval;
    else                missing = missing_gzy.floatval;
    for(i = 0; i < nt*nlat*nlon; i++) {
      z[i] = missing;
    }
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsg: The input arrays cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlon * nlat;
  work = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(geomatv,GEOMATV)(&nlon,&nlat,&gzx[j],&gzy[j],work);
    j += nlat*nlon;
  }
  free(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 * dynamically allocate various temporary space.
 * Note the order "vhagc(...,v,u,....)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*nlat*(nlat+1)+2,2*nlat*(2*nlon*nt+3*l2));
  lvhagc = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+l2+15;
  ldwork = 2*nlat*(nlat+1)+1;

  br =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  bi =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  cr =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  ci =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  wvhagc = (float*)calloc(        lvhagc*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  dwork = (double*)calloc(  ldwork*sizeof(double),1);

  if( br == NULL || bi == NULL || cr == NULL || ci == NULL ||
      wvhagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(vhagci,VHAGCI)(&nlat,&nlon,wvhagc,&lvhagc,dwork,&ldwork,&jer);
  NGCALLF(vhagc,VHAGC)(&nlat,&nlon,&isym,&nt,&gzy[0],&gzx[0],
		       &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,wvhagc,&lvhagc,
		       work,&lwork,&ker);
  free(cr);
  free(ci);
  free(wvhagc);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("igradsg","vhagc",&ier,&jer,&ker,&mer,9,5);
/*
 * compute the scalar function given the input vector
 */
  l3     = max(nlat,(nlon+2)/2);
  lwork  = max(4*nlat*(nlat+2)+2,nlat*(nlon*nt+max(3*l2,nlon)+2*nt*l3+1));
  ldwork = nlat*(nlat+4);
  lshsgc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;
  work   = (float*)calloc( lwork*sizeof(float),1);
  wshsgc = (float*)calloc(lshsgc*sizeof(float),1);
  dwork = (double*)calloc(ldwork*sizeof(double),1);
  if( wshsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(shsgci,SHSGCI)(&nlat,&nlon,wshsgc,&lshsgc,dwork,&ldwork,&jer);
  NGCALLF(igradgc,IGRADGC)(&nlat,&nlon,&isym,&nt,&z[0],&idvw,&jdvw,br,bi,
			   &mdab,&ndab,wshsgc,&lshsgc,work,&lwork,&ker);
  NGCALLF(chkerr,CHKERR)("igradsg","shsgci+igradgc",&ier,&jer,&ker,&mer,9,14);

  free(br);
  free(bi);
  free(wshsgc);
  free(work);
  free(dwork);
/* 
 * transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
 
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&z[j],work);
    NGCALLF(matgeov,MATGEOV)(&nlat,&nlon,&gzx[j],&gzy[j],work);
    j += nlat*nlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 6.37122e+6;         /* radius of earth */

  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&z[0],&scale,&ner);
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return array.
 */
  return(NhlNOERROR);
}


NhlErrorTypes igradsG_W( void )
{
/*
 * Input array variables
 */
  float *gzx, *gzy;
  int dsizes_gzx[NCL_MAX_DIMENSIONS], dsizes_gzy[NCL_MAX_DIMENSIONS];
  int ndims_gzx, ndims_gzy;
  NclScalar missing_gzx, missing_gzy;
  float missing;
  int has_missing_gzx, has_missing_gzy, found_missing=0;
  int nt, nlat, nlon;
/*
 * Output array variables
 */
  float *z;
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2, l3;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  float scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhagc, lshsgc;
  float *work, *wvhagc, *wshsgc, *br, *bi, *cr, *ci;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  gzx = (float*)NclGetArgValue(
           0,
           2,
           &ndims_gzx, 
           dsizes_gzx,
           &missing_gzx,
           &has_missing_gzx,
           NULL,
           2);
  gzy = (float*)NclGetArgValue(
           1,
           2,
           &ndims_gzy, 
           dsizes_gzy,
           &missing_gzy,
           &has_missing_gzy,
           NULL,
           2);
/*
 * The grids coming in must be at least 2-dimensional.
 */
  if( ndims_gzx != ndims_gzy || ndims_gzx < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsG: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_gzx; i++ ) {
    if( dsizes_gzx[i] != dsizes_gzy[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsG: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  nlat = dsizes_gzx[ndims_gzx-2];
  nlon = dsizes_gzx[ndims_gzx-1];
/*
 * Compute the total number of elements in our array.
 */
  nt = 1;
  for(i = 0; i < ndims_gzx-2; i++) {
    nt *= dsizes_gzx[i];
  }
/*
 * Allocate space for output array.
 */
  z = (float*)calloc(nt*nlat*nlon*sizeof(float),1);
  if( z == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsG: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Check for missing values.
 */
  if(has_missing_gzx || has_missing_gzy) {
    l = 0;
    while( l < nt*nlat*nlon && !found_missing ) {
      if(gzx[l] == missing_gzx.floatval || gzy[l] == missing_gzy.floatval) {
	found_missing = 1;
      }
      l++;
    }
  }
  if(found_missing) {
    if(has_missing_gzx) missing = missing_gzx.floatval;
    else                missing = missing_gzy.floatval;
    for(i = 0; i < nt*nlat*nlon; i++) {
      z[i] = missing;
    }
    NhlPError(NhlWARNING,NhlEUNKNOWN,"igradsG: The input arrays cannot contain any missing values");
    return(NclReturnValue((void*)z,ndims_gzx,dsizes_gzx,NULL,NCL_float,0));
  }
/*
 * Determine the workspace size.
 */
  lwork = nlon * nlat;
  work = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsG: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(geomatv,GEOMATV)(&nlon,&nlat,&gzx[j],&gzy[j],work);
    j += nlat*nlon;
  }
  free(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 * dynamically allocate various temporary space.
 * Note the order "vhagc(...,v,u,....)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*nlat*(nlat+1)+2,2*nlat*(2*nlon*nt+3*l2));
  lvhagc = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+l2+15;
  ldwork = 2*nlat*(nlat+1)+1;

  br =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  bi =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  cr =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  ci =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  wvhagc = (float*)calloc(        lvhagc*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  dwork = (double*)calloc(  ldwork*sizeof(double),1);

  if( br == NULL || bi == NULL || cr == NULL || ci == NULL ||
      wvhagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsG: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(vhagci,VHAGCI)(&nlat,&nlon,wvhagc,&lvhagc,dwork,&ldwork,&jer);
  NGCALLF(vhagc,VHAGC)(&nlat,&nlon,&isym,&nt,&gzy[0],&gzx[0],
		       &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,wvhagc,&lvhagc,
		       work,&lwork,&ker);
  free(cr);
  free(ci);
  free(wvhagc);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("igradsG","vhagc",&ier,&jer,&ker,&mer,9,5);
/*
 * compute the scalar function given the input vector
 */
  l3     = max(nlat,(nlon+2)/2);
  lwork  = max(4*nlat*(nlat+2)+2,nlat*(nlon*nt+max(3*l2,nlon)+2*nt*l3+1));
  ldwork = nlat*(nlat+4);
  lshsgc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;
  work   = (float*)calloc( lwork*sizeof(float),1);
  wshsgc = (float*)calloc(lshsgc*sizeof(float),1);
  dwork = (double*)calloc(ldwork*sizeof(double),1);
  if( wshsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsG: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(shsgci,SHSGCI)(&nlat,&nlon,wshsgc,&lshsgc,dwork,&ldwork,&jer);
  NGCALLF(igradgc,IGRADGC)(&nlat,&nlon,&isym,&nt,&z[0],&idvw,&jdvw,br,bi,
			   &mdab,&ndab,wshsgc,&lshsgc,work,&lwork,&ker);
  NGCALLF(chkerr,CHKERR)("igradsG","shsgci+igradgc",&ier,&jer,&ker,&mer,9,14);

  free(br);
  free(bi);
  free(wshsgc);
  free(work);
  free(dwork);
/* 
 * transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
 
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsG: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&z[j],work);
    NGCALLF(matgeov,MATGEOV)(&nlat,&nlon,&gzx[j],&gzy[j],work);
    j += nlat*nlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 6.37122e+6;         /* radius of earth */

  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&z[0],&scale,&ner);
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return array.
 */
  return(NclReturnValue((void*)z,ndims_gzx,dsizes_gzx,NULL,NCL_float,0));
}



NhlErrorTypes ilapsf_W( void )
{
/*
 * Input array variables
 */
  float *zlap, *zlmbda;
  int dsizes_zlap[NCL_MAX_DIMENSIONS], dsizes_zlmbda[NCL_MAX_DIMENSIONS];
  int ndims_zlap, ndims_zlmbda;
  NclScalar missing_zlap, missing_zlmbda;
  float missing;
  int has_missing_zlap, has_missing_zlmbda, found_missing=0;
  int nt, nlat, nlon;
/*
 * Output array variables
 */
  float *z;
  int ndims_z, dsizes_z[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  float scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lshaec, lshsec;
  float *work, *wshaec, *wshsec, *pertrb, *a, *b;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  zlap = (float*)NclGetArgValue(
           0,
           3,
           &ndims_zlap, 
           dsizes_zlap,
           &missing_zlap,
           &has_missing_zlap,
           NULL,
           2);
  zlmbda = (float*)NclGetArgValue(
           1,
           3,
           &ndims_zlmbda, 
           dsizes_zlmbda,
           &missing_zlmbda,
           &has_missing_zlmbda,
           NULL,
           2);
/*
 * The grids coming in must be 1 and 3-dimensional.
 */
  if( ndims_zlap < 2 || ndims_zlmbda < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsf: The first input array must be at least 2-dimensional and the second input array must be at least 1-dimensional");
    return(NhlFATAL);
  }
  nlat = dsizes_zlap[ndims_zlap-2];
  nlon = dsizes_zlap[ndims_zlap-1];
  if( ndims_zlap == 2 ) {
    if( ndims_zlmbda != 1 || dsizes_zlmbda[0] != 1 ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsf: If the first input array is two dimensional, then the second argument must be a constant");
      return(NhlFATAL);
    }
  }
  else {
    for( i = 0; i < ndims_zlmbda; i++ ) {
      if( dsizes_zlmbda[i] != dsizes_zlap[i] ) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsf: If the first input array has more than two dimensions, then the length of the second input array should be equal to the third to the last dimension size of the first array, otherwise the length should be 1");
	return(NhlFATAL);
      }
    }
  }
/*
 * Compute the total number of elements in our array.
 */
  nt = 1;
  for(i = 0; i < ndims_zlap-2; i++) {
    nt *= dsizes_zlap[i];
  }
/*
 * Get output array.
 */
  z = (float*)NclGetArgValue(
           2,
           3,
           &ndims_z, 
           dsizes_z,
           NULL,
           NULL,
           NULL,
           1);
/*
 * The output array must also be at least 2-dimensional.
 */
  if( ndims_z != ndims_zlap  ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsf: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_zlap; i++ ) {
    if( dsizes_z[i] != dsizes_zlap[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsf: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Check for missing values.
 */
  if(has_missing_zlmbda) {
    l = 0;
    while( l < nt && !found_missing ) {
      if(zlmbda[l++] == missing_zlmbda.floatval) found_missing = 1;
    }
    missing = missing_zlmbda.floatval;
  }
  if(has_missing_zlap && !found_missing) {
    l = 0;
    while( l < nt*nlat*nlon && !found_missing) {
      if(zlap[l++] == missing_zlap.floatval) found_missing = 1;
    }
    missing = missing_zlap.floatval;
  }
  if(found_missing) {
    for(i = 0; i < nt*nlat*nlon; i++) {
      z[i] = missing;
    }
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsf: The input arrays cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlon * nlat;
  work = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(geomat,GEOMAT)(&nlon,&nlat,&zlap[j],work);
    j += nlat*nlon;
  }
  free(work);
/* 
 * shaec performs the spherical harmonic analysis on a (scalar) gaussian 
 * grid(s) and returns the coefficients in array(s) a,b
 * Here the scalar grid is "z" (a scalar function)
 */ 
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(2*(nlat+1),nlat*(nlon*nt+max(3*l2,nlon)));
  lshaec = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;
  ldwork = nlat+1;
  a =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  b =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  wshaec = (float*)calloc(        lshaec*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  dwork =  (double*)calloc(        ldwork*sizeof(double),1);

  if( a == NULL || b == NULL || wshaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(shaeci,SHAECI)(&nlat,&nlon,wshaec,&lshaec,dwork,&ldwork,&jer);
  NGCALLF(shaec,SHAEC)(&nlat,&nlon,&isym,&nt,&z[0],&idvw,&jdvw,a,b,
		       &mdab,&ndab,wshaec,&lshaec,work,&lwork,&ker);
  free(wshaec);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("ilapsf","shaec",&ier,&jer,&ker,&mer,8,5);
/* 
 * invert the laplacian
 */ 
  l1     = min(nlat,(nlon+1)/2);
  /*  lwork  = max(nlat+1,nlat*(2*nt*nlon+max(6*l2,nlon)+2*l3+1));*/
  lwork = nlat*(2*nt*nlon+max(6*l2,nlon)+2*nt*l1+1);
  ldwork = nlat+1;
  lshsec = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;

  wshsec = (float*)calloc(lshsec*sizeof(float),1);
  work =   (float*)calloc( lwork*sizeof(float),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);
  pertrb = (float*)calloc(     nt*sizeof(float),1);

  if( pertrb == NULL || wshsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(shseci,SHSECI)(&nlat,&nlon,wshsec,&lshsec,dwork,&ldwork,&jer);
  NGCALLF(islapec,ISLAPEC)(&nlat,&nlon,&isym,&nt,&zlmbda[0],&zlap[0],
			   &idvw,&jdvw,a,b,&mdab,&ndab,wshsec,&lshsec,
			   work,&lwork,pertrb,&ker);
  free(a);
  free(b);
  free(wshsec);
  free(work);
  free(dwork);
  free(pertrb);
  NGCALLF(chkerr,CHKERR)("ilapsf","shseci+islapec",&ier,&jer,&ker,&mer,7,13);
/*
 * transform from math coordinates to geophysical coordinates
 *  (math) nlat is the first dim
 */ 
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&z[j],work);
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&zlap[j],work);
    j += nlat*nlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = powf(6.37122e+6,2.);         /* radius of earth**2 */

  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&z[0],&scale,&ner);
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return array.
 */
  return(NhlNOERROR);
}


NhlErrorTypes ilapsF_W( void )
{
/*
 * Input array variables
 */
  float *zlap, *zlmbda;
  int dsizes_zlap[NCL_MAX_DIMENSIONS], dsizes_zlmbda[NCL_MAX_DIMENSIONS];
  int ndims_zlap, ndims_zlmbda;
  NclScalar missing_zlap, missing_zlmbda;
  float missing;
  int has_missing_zlap, has_missing_zlmbda, found_missing=0;
  int nt, nlat, nlon;
/*
 * Output array variables
 */
  float *z;
  int ndims_z, dsizes_z[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  float scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lshaec, lshsec;
  float *work, *wshaec, *wshsec, *pertrb, *a, *b;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  zlap = (float*)NclGetArgValue(
           0,
           2,
           &ndims_zlap, 
           dsizes_zlap,
           &missing_zlap,
           &has_missing_zlap,
           NULL,
           2);
  zlmbda = (float*)NclGetArgValue(
           1,
           2,
           &ndims_zlmbda, 
           dsizes_zlmbda,
           &missing_zlmbda,
           &has_missing_zlmbda,
           NULL,
           2);
/*
 * The grids coming in must be 1 and 3-dimensional.
 */
  if( ndims_zlap < 2 || ndims_zlmbda < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsF: The first input array must be at least 2-dimensional and the second input array must be at least 1-dimensional");
    return(NhlFATAL);
  }
  nlat = dsizes_zlap[ndims_zlap-2];
  nlon = dsizes_zlap[ndims_zlap-1];
  if( ndims_zlap == 2 ) {
    if( ndims_zlmbda != 1 || dsizes_zlmbda[0] != 1 ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsF: If the first input array is two dimensional, then the second argument must be a constant");
      return(NhlFATAL);
    }
  }
  else {
    for( i = 0; i < ndims_zlmbda; i++ ) {
      if( dsizes_zlmbda[i] != dsizes_zlap[i] ) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsF: If the first input array has more than two dimensions, then the length of the second input array should be equal to the third to the last dimension size of the first array, otherwise the length should be 1");
	return(NhlFATAL);
      }
    }
  }
/*
 * Compute the total number of elements in our array.
 */
  nt = 1;
  for(i = 0; i < ndims_zlap-2; i++) {
    nt *= dsizes_zlap[i];
  }
/*
 * Allocate space for output array
 */
  z = (float*)calloc(nt*nlat*nlon*sizeof(float),1);
  if( z == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsF: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Check for missing values.
 */
  if(has_missing_zlmbda) {
    l = 0;
    while( l < nt && !found_missing ) {
      if(zlmbda[l++] == missing_zlmbda.floatval) found_missing = 1;
    }
    missing = missing_zlmbda.floatval;
  }
  if(has_missing_zlap && !found_missing) {
    l = 0;
    while( l < nt*nlat*nlon && !found_missing) {
      if(zlap[l++] == missing_zlap.floatval) found_missing = 1;
    }
    missing = missing_zlap.floatval;
  }
  if(found_missing) {
    for(i = 0; i < nt*nlat*nlon; i++) {
      z[i] = missing;
    }
    NhlPError(NhlWARNING,NhlEUNKNOWN,"ilapsF: The input arrays cannot contain any missing values");
    return(NclReturnValue((void*)z,ndims_zlap,dsizes_zlap,NULL,NCL_float,0));
  }
/*
 * Determine the workspace size.
 */
  lwork = nlon * nlat;
  work = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsF: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(geomat,GEOMAT)(&nlon,&nlat,&zlap[j],work);
    j += nlat*nlon;
  }
  free(work);
/* 
 * shaec performs the spherical harmonic analysis on a (scalar) gaussian 
 * grid(s) and returns the coefficients in array(s) a,b
 * Here the scalar grid is "z" (a scalar function)
 */ 
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(2*(nlat+1),nlat*(nlon*nt+max(3*l2,nlon)));
  lshaec = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;
  ldwork = nlat+1;
  a =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  b =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  wshaec = (float*)calloc(        lshaec*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  dwork =  (double*)calloc(        ldwork*sizeof(double),1);

  if( a == NULL || b == NULL || wshaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsF: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(shaeci,SHAECI)(&nlat,&nlon,wshaec,&lshaec,dwork,&ldwork,&jer);
  NGCALLF(shaec,SHAEC)(&nlat,&nlon,&isym,&nt,&z[0],&idvw,&jdvw,a,b,
		       &mdab,&ndab,wshaec,&lshaec,work,&lwork,&ker);
  free(wshaec);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("ilapsF","shaec",&ier,&jer,&ker,&mer,8,5);
/* 
 * invert the laplacian
 */ 
  l1     = min(nlat,(nlon+1)/2);
  /*  lwork  = max(nlat+1,nlat*(2*nt*nlon+max(6*l2,nlon)+2*l3+1)); */
  lwork = nlat*(2*nt*nlon+max(6*l2,nlon)+2*nt*l1+1);
  ldwork = nlat+1;
  lshsec = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;

  wshsec = (float*)calloc(lshsec*sizeof(float),1);
  work =   (float*)calloc( lwork*sizeof(float),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);
  pertrb = (float*)calloc(     nt*sizeof(float),1);

  if( pertrb == NULL || wshsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsF: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(shseci,SHSECI)(&nlat,&nlon,wshsec,&lshsec,dwork,&ldwork,&jer);
  NGCALLF(islapec,ISLAPEC)(&nlat,&nlon,&isym,&nt,&zlmbda[0],&zlap[0],
			   &idvw,&jdvw,a,b,&mdab,&ndab,wshsec,&lshsec,
			   work,&lwork,pertrb,&ker);
  free(a);
  free(b);
  free(wshsec);
  free(work);
  free(dwork);
  free(pertrb);
  NGCALLF(chkerr,CHKERR)("ilapsF","shseci+islapec",&ier,&jer,&ker,&mer,7,13);
/*
 * transform from math coordinates to geophysical coordinates
 *  (math) nlat is the first dim
 */ 
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsF: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&z[j],work);
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&zlap[j],work);
    j += nlat*nlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = powf(6.37122e+6,2.);         /* radius of earth**2 */

  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&z[0],&scale,&ner);
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return array.
 */
  return(NclReturnValue((void*)z,ndims_zlap,dsizes_zlap,NULL,NCL_float,0));
}



NhlErrorTypes ilapsg_W( void )
{
/*
 * Input array variables
 */
  float *zlap, *zlmbda;
  int dsizes_zlap[NCL_MAX_DIMENSIONS], dsizes_zlmbda[NCL_MAX_DIMENSIONS];
  int ndims_zlap, ndims_zlmbda;
  NclScalar missing_zlap, missing_zlmbda;
  float missing;
  int has_missing_zlap, has_missing_zlmbda, found_missing=0;
  int nt, nlat, nlon;
/*
 * Output array variables
 */
  float *z;
  int ndims_z, dsizes_z[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  float scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lshagc, lshsgc;
  float *work, *wshagc, *wshsgc, *pertrb, *a, *b;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  zlap = (float*)NclGetArgValue(
           0,
           3,
           &ndims_zlap, 
           dsizes_zlap,
           &missing_zlap,
           &has_missing_zlap,
           NULL,
           2);
  zlmbda = (float*)NclGetArgValue(
           1,
           3,
           &ndims_zlmbda, 
           dsizes_zlmbda,
           &missing_zlmbda,
           &has_missing_zlmbda,
           NULL,
           2);
/*
 * The grids coming in must be 1 and 3-dimensional.
 */
  if( ndims_zlap < 2 || ndims_zlmbda < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsg: The first input array must be at least 2-dimensional and the second input array must be at least 1-dimensional");
    return(NhlFATAL);
  }
  nlat = dsizes_zlap[ndims_zlap-2];
  nlon = dsizes_zlap[ndims_zlap-1];
  if( ndims_zlap == 2 ) {
    if( ndims_zlmbda != 1 || dsizes_zlmbda[0] != 1 ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsg: If the first input array is two dimensional, then the second argument must be a constant");
      return(NhlFATAL);
    }
  }
  else {
    for( i = 0; i < ndims_zlmbda; i++ ) {
      if( dsizes_zlmbda[i] != dsizes_zlap[i] ) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsg: If the first input array has more than two dimensions, then the length of the second input array should be equal to the third to the last dimension size of the first array, otherwise the length should be 1");
	return(NhlFATAL);
      }
    }
  }
/*
 * Compute the total number of elements in our array.
 */
  nt = 1;
  for(i = 0; i < ndims_zlap-2; i++) {
    nt *= dsizes_zlap[i];
  }
/*
 * Get output array.
 */
  z = (float*)NclGetArgValue(
           2,
           3,
           &ndims_z, 
           dsizes_z,
           NULL,
           NULL,
           NULL,
           2);
/*
 * The output array must also be at least 2-dimensional.
 */
  if( ndims_z != ndims_zlap  ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsg: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_zlap; i++ ) {
    if( dsizes_z[i] != dsizes_zlap[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsg: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Check for missing values.
 */
  if(has_missing_zlmbda) {
    l = 0;
    while( l < nt && !found_missing ) {
      if(zlmbda[l++] == missing_zlmbda.floatval) found_missing = 1;
    }
    missing = missing_zlmbda.floatval;
  }
  if(has_missing_zlap && !found_missing) {
    i = 0;
    while( i < nt*nlat*nlon && !found_missing) {
      if(zlap[i++] == missing_zlap.floatval) found_missing = 1;
    }
    missing = missing_zlap.floatval;
  }
  if(found_missing) {
    for(i = 0; i < nt*nlat*nlon; i++) {
      z[i] = missing;
    }
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsg: The input arrays cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlon * nlat;
  work = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(geomat,GEOMAT)(&nlon,&nlat,&zlap[j],work);
    j += nlat*nlon;
  }
  free(work);
/* 
 * shagc performs the spherical harmonic analysis on a (scalar) gaussian 
 * grid(s) and returns the coefficients in array(s) a,b
 * Here the scalar grid is "z" (a scalar function)
 */ 
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*nlat*(nlat+2)+2,nlat*(nlon*nt+max(3*l2,nlon)));
  lshagc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;
  ldwork = nlat*(nlat+4);

  a =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  b =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  wshagc = (float*)calloc(        lshagc*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  dwork =  (double*)calloc(       ldwork*sizeof(double),1);

  if( a == NULL || b == NULL || wshagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(shagci,SHAGCI)(&nlat,&nlon,wshagc,&lshagc,dwork,&ldwork,&jer);
  NGCALLF(shagc,SHAGC)(&nlat,&nlon,&isym,&nt,&zlap[0],&idvw,&jdvw,a,b,
		       &mdab,&ndab,wshagc,&lshagc,work,&lwork,&ker);
  free(wshagc);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("ilapsg","shagc",&ier,&jer,&ker,&mer,8,5);
/* 
 * invert the laplacian
 */ 
  lwork = nlat*(2*nt*nlon+max(6*l2,nlon)+2*nt*l1+1);
  ldwork = nlat*(nlat+4);
  lshsgc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;


  wshsgc = (float*)calloc(lshsgc*sizeof(float),1);
  work =   (float*)calloc( lwork*sizeof(float),1);
  pertrb = (float*)calloc(     nt*sizeof(float),1);

  dwork = (double*)calloc(ldwork*sizeof(double),1);
  if( pertrb == NULL || wshsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(shsgci,SHSGCI)(&nlat,&nlon,wshsgc,&lshsgc,dwork,&ldwork,&jer);
  NGCALLF(islapgc,ISLAPGC)(&nlat,&nlon,&isym,&nt,&zlmbda[0],&z[0],
			   &idvw,&jdvw,a,b,&mdab,&ndab,wshsgc,&lshsgc,
			   work,&lwork,pertrb,&ker);
  free(a);
  free(b);
  free(wshsgc);
  free(work);
  free(dwork);
  free(pertrb);
  NGCALLF(chkerr,CHKERR)("ilapsg","shsgci+islapgc",&ier,&jer,&ker,&mer,7,13);
/*
 * transform from math coordinates to geophysical coordinates
 *  (math) nlat is the first dim
 */ 
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&z[j],work);
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&zlap[j],work);
    j += nlat*nlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = powf(6.37122e+6,2.);         /* radius of earth**2 */

  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&z[0],&scale,&ner);
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return array.
 */
  return(NhlNOERROR);
}


NhlErrorTypes ilapsG_W( void )
{
/*
 * Input array variables
 */
  float *zlap, *zlmbda;
  int dsizes_zlap[NCL_MAX_DIMENSIONS], dsizes_zlmbda[NCL_MAX_DIMENSIONS];
  int ndims_zlap, ndims_zlmbda;
  NclScalar missing_zlap, missing_zlmbda;
  float missing;
  int has_missing_zlap, has_missing_zlmbda, found_missing=0;
  int nt, nlat, nlon;
/*
 * Output array variables
 */
  float *z;
  int ndims_z, dsizes_z[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  float scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lshagc, lshsgc;
  float *work, *wshagc, *wshsgc, *pertrb, *a, *b;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  zlap = (float*)NclGetArgValue(
           0,
           2,
           &ndims_zlap, 
           dsizes_zlap,
           &missing_zlap,
           &has_missing_zlap,
           NULL,
           2);
  zlmbda = (float*)NclGetArgValue(
           1,
           2,
           &ndims_zlmbda, 
           dsizes_zlmbda,
           &missing_zlmbda,
           &has_missing_zlmbda,
           NULL,
           2);
/*
 * The grids coming in must be 1 and 3-dimensional.
 */
  if( ndims_zlap < 2 || ndims_zlmbda < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsG: The first input array must be at least 2-dimensional and the second input array must be at least 1-dimensional");
    return(NhlFATAL);
  }
  nlat = dsizes_zlap[ndims_zlap-2];
  nlon = dsizes_zlap[ndims_zlap-1];
  if( ndims_zlap == 2 ) {
    if( ndims_zlmbda != 1 || dsizes_zlmbda[0] != 1 ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsG: If the first input array is two dimensional, then the second argument must be a constant");
      return(NhlFATAL);
    }
  }
  else {
    for( i = 0; i < ndims_zlmbda; i++ ) {
      if( dsizes_zlmbda[i] != dsizes_zlap[i] ) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsG: If the first input array has more than two dimensions, then the length of the second input array should be equal to the third to the last dimension size of the first array, otherwise the length should be 1");
	return(NhlFATAL);
      }
    }
  }
/*
 * Compute the total number of elements in our array.
 */
  nt = 1;
  for(i = 0; i < ndims_zlap-2; i++) {
    nt *= dsizes_zlap[i];
  }
/*
 * Allocate space for output array
 */
  z = (float*)calloc(nt*nlat*nlon*sizeof(float),1);
  if( z == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsG: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Check for missing values.
 */
  if(has_missing_zlmbda) {
    l = 0;
    while( l < nt && !found_missing ) {
      if(zlmbda[l++] == missing_zlmbda.floatval) found_missing = 1;
    }
    missing = missing_zlmbda.floatval;
  }
  if(has_missing_zlap && !found_missing) {
    i = 0;
    while( i < nt*nlat*nlon && !found_missing) {
      if(zlap[i++] == missing_zlap.floatval) found_missing = 1;
    }
    missing = missing_zlap.floatval;
  }
  if(found_missing) {
    for(i = 0; i < nt*nlat*nlon; i++) {
      z[i] = missing;
    }
    NhlPError(NhlWARNING,NhlEUNKNOWN,"ilapsG: The input arrays cannot contain any missing values");
    return(NclReturnValue((void*)z,ndims_zlap,dsizes_zlap,NULL,NCL_float,0));
  }
/*
 * Determine the workspace size.
 */
  lwork = nlon * nlat;
  work = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsG: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(geomat,GEOMAT)(&nlon,&nlat,&zlap[j],work);
    j += nlat*nlon;
  }
  free(work);
/* 
 * shagc performs the spherical harmonic analysis on a (scalar) gaussian 
 * grid(s) and returns the coefficients in array(s) a,b
 * Here the scalar grid is "z" (a scalar function)
 */ 
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*nlat*(nlat+2)+2,nlat*(nlon*nt+max(3*l2,nlon)));
  lshagc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;
  ldwork = nlat*(nlat+4);

  a =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  b =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  wshagc = (float*)calloc(        lshagc*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  dwork =  (double*)calloc(       ldwork*sizeof(double),1);

  if( a == NULL || b == NULL || wshagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsG: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(shagci,SHAGCI)(&nlat,&nlon,wshagc,&lshagc,dwork,&ldwork,&jer);
  NGCALLF(shagc,SHAGC)(&nlat,&nlon,&isym,&nt,&zlap[0],&idvw,&jdvw,a,b,
		       &mdab,&ndab,wshagc,&lshagc,work,&lwork,&ker);
  free(wshagc);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("ilapsG","shagc",&ier,&jer,&ker,&mer,8,5);
/* 
 * invert the laplacian
 */ 
  lwork  = nlat*(2*nt*nlon+max(6*l2,nlon)+2*nt*l1+1);
  ldwork = nlat*(nlat+4);
  lshsgc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;


  wshsgc = (float*)calloc(lshsgc*sizeof(float),1);
  work =   (float*)calloc( lwork*sizeof(float),1);
  pertrb = (float*)calloc(     nt*sizeof(float),1);

  dwork = (double*)calloc(ldwork*sizeof(double),1);
  if( pertrb == NULL || wshsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsG: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(shsgci,SHSGCI)(&nlat,&nlon,wshsgc,&lshsgc,dwork,&ldwork,&jer);
  NGCALLF(islapgc,ISLAPGC)(&nlat,&nlon,&isym,&nt,&zlmbda[0],&z[0],
			   &idvw,&jdvw,a,b,&mdab,&ndab,wshsgc,&lshsgc,
			   work,&lwork,pertrb,&ker);
  free(a);
  free(b);
  free(wshsgc);
  free(work);
  free(dwork);
  free(pertrb);
  NGCALLF(chkerr,CHKERR)("ilapsG","shsgci+islapgc",&ier,&jer,&ker,&mer,7,13);
/*
 * transform from math coordinates to geophysical coordinates
 *  (math) nlat is the first dim
 */ 
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsG: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&z[j],work);
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&zlap[j],work);
    j += nlat*nlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = powf(6.37122e+6,2.);         /* radius of earth**2 */

  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&z[0],&scale,&ner);
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return array.
 */
  return(NclReturnValue((void*)z,ndims_zlap,dsizes_zlap,NULL,NCL_float,0));
}


NhlErrorTypes ilapvf_W( void )
{
/*
 * Input array variables
 */
  float *ulap, *vlap;
  int ndims_ulap, dsizes_ulap[NCL_MAX_DIMENSIONS], nt, nlat, nlon;
  int ndims_vlap, dsizes_vlap[NCL_MAX_DIMENSIONS];
  NclScalar missing_ulap, missing_vlap;
  float missing;
  int has_missing_ulap, has_missing_vlap, found_missing=0;
/*
 * Output array variables
 */
  float *u, *v;
  int dsizes_u[NCL_MAX_DIMENSIONS], dsizes_v[NCL_MAX_DIMENSIONS];
  int ndims_u, ndims_v;
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  float scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhaec, lvhsec;
  float *work, *wvhaec, *wvhsec, *br, *bi, *cr, *ci;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  ulap = (float*)NclGetArgValue(
           0,
           4,
           &ndims_ulap, 
           dsizes_ulap,
           &missing_ulap,
           &has_missing_ulap,
           NULL,
           2);
  vlap = (float*)NclGetArgValue(
           1,
           4,
           &ndims_vlap, 
           dsizes_vlap,
           &missing_vlap,
           &has_missing_vlap,
           NULL,
           2);
/*
 * The grids coming in must be at least 2-dimensional and the same size.
 */
  if( ndims_ulap != ndims_vlap || ndims_ulap < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvf: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_ulap; i++ ) {
    if( dsizes_ulap[i] != dsizes_vlap[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvf: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  nlat = dsizes_ulap[ndims_ulap-2];
  nlon = dsizes_ulap[ndims_ulap-1];
/*
 * Compute the total number of elements in our array.
 */
  nt = 1;
  for(i = 0; i < ndims_ulap-2; i++) {
    nt *= dsizes_ulap[i];
  }
/*
 * Get output arrays.
 */
  u = (float*)NclGetArgValue(
           2,
           4,
           &ndims_u, 
           dsizes_u,
           NULL,
           NULL,
           NULL,
           1);
  v = (float*)NclGetArgValue(
           3,
           4,
           &ndims_v, 
           dsizes_v,
           NULL,
           NULL,
           NULL,
           1);
/*
 * The output arrays must also be at least 2-dimensional and the same sizes.
 */
  if( ndims_u != ndims_ulap || ndims_v != ndims_ulap ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvf: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_ulap; i++ ) {
    if( dsizes_u[i] != dsizes_ulap[i] || dsizes_v[i] != dsizes_ulap[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvf: The input/output arrays must have the same  dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Check for missing values.
 */
  if(has_missing_ulap || has_missing_vlap) {
    l = 0;
    while( l < nt*nlat*nlon && !found_missing ) {
      if(ulap[l]==missing_ulap.floatval || vlap[l]==missing_vlap.floatval) {
	found_missing = 1;
      }
      l++;
    }
  }
  if(found_missing) {
    if(has_missing_ulap) missing = missing_ulap.floatval;
    else                 missing = missing_vlap.floatval;
    for(i = 0; i < nt*nlat*nlon; i++) {
      u[i] = v[i] = missing;
    }
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvf: The input array cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlon * nlat;
  work = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(geomatv,GEOMATV)(&nlon,&nlat,&ulap[j],&vlap[j],work);
    j += nlat*nlon;
  }
  free(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 * dynamically allocate various temporary space *
 * Note the order "vhaec(...,v,u,....)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*(nlat+1),nlat*(2*nt*nlon+max(6*l2,nlon)));
  ldwork = 2*(nlat+2);
  lvhaec = 4*nlat*l2+3*max(l1-2,0)*(nlat+nlat-l1-1)+nlon+15;

  br =     (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  bi =     (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  cr =     (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  ci =     (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  wvhaec = (float*)calloc(        lvhaec*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  dwork = (double*)calloc(        ldwork*sizeof(double),1);

  if( br == NULL || bi == NULL || cr == NULL || ci == NULL ||
      wvhaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(vhaeci,VHAECI)(&nlat,&nlon,wvhaec,&lvhaec,dwork,&ldwork,&jer);
  NGCALLF(vhaec,VHAEC)(&nlat,&nlon,&isym,&nt,&vlap[0],&ulap[0],
		       &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,wvhaec,&lvhaec,
		       work,&lwork,&ker);
  free(wvhaec);
  free(work);
  NGCALLF(chkerr,CHKERR)("ilapvf","vhaec",&ier,&jer,&ker,&mer,8,5);
/* 
 * compute the vector laplacian using the vector spherical harmonic 
 */ 
  l1     = min(nlat,nlon/2);
  /*  lwork  = max(4*(nlat+1),4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15);*/
  lwork = nlat*(2*nt*nlon+max(6*l2,nlon))+nlat*(4*nt*l1+1);
  lvhsec = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15;
  ldwork = 2*(nlat+2);

  wvhsec = (float*)calloc(lvhsec*sizeof(float),1);
  work   = (float*)calloc( lwork*sizeof(float),1);
  dwork = (double*)calloc( ldwork*sizeof(double),1);

  if( wvhsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(vhseci,VHSECI)(&nlat,&nlon,wvhsec,&lvhsec,dwork,&ldwork,&jer);
  NGCALLF(ivlapec,IVLAPEC)(&nlat,&nlon,&isym,&nt,&v[0],&u[0],
			   &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
			   wvhsec,&lvhsec,work,&lwork,&ker);

  free(br);
  free(bi);
  free(cr);
  free(ci);
  free(wvhsec);
  free(work);
  free(dwork);

  NGCALLF(chkerr,CHKERR)("ilapvf","vhseci,ivlapec",&ier,&jer,&ker,&mer,8,14);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
 
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeov,MATGEOV)(&nlat,&nlon,&u[j],&v[j],work);
    NGCALLF(matgeov,MATGEOV)(&nlat,&nlon,&ulap[j],&vlap[j],work);
    j += nlat*nlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = powf(6.37122e+6,2.);         /* radius of earth**2 */
  
  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&u[0],&scale,&ner);
  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&v[0],&scale,&ner);
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return
 */
  return(NhlNOERROR);
}


NhlErrorTypes ilapvg_W( void )
{
/*
 * Input array variables
 */
  float *ulap, *vlap;
  int ndims_ulap, dsizes_ulap[NCL_MAX_DIMENSIONS], nt, nlat, nlon;
  int ndims_vlap, dsizes_vlap[NCL_MAX_DIMENSIONS];
  NclScalar missing_ulap, missing_vlap;
  float missing;
  int has_missing_ulap, has_missing_vlap, found_missing=0;
/*
 * Output array variables
 */
  float *u, *v;
  int dsizes_u[NCL_MAX_DIMENSIONS], dsizes_v[NCL_MAX_DIMENSIONS];
  int ndims_u, ndims_v;
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  float scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhagc, lvhsgc;
  float *work, *wvhagc, *wvhsgc, *br, *bi, *cr, *ci;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  ulap = (float*)NclGetArgValue(
           0,
           4,
           &ndims_ulap, 
           dsizes_ulap,
           &missing_ulap,
           &has_missing_ulap,
           NULL,
           2);
  vlap = (float*)NclGetArgValue(
           1,
           4,
           &ndims_vlap, 
           dsizes_vlap,
           &missing_vlap,
           &has_missing_vlap,
           NULL,
           2);
/*
 * The grids coming in must be at least 2-dimensional and the same size.
 */
  if( ndims_ulap != ndims_vlap || ndims_ulap < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvg: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_ulap; i++ ) {
    if( dsizes_ulap[i] != dsizes_vlap[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvg: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  nlat = dsizes_ulap[ndims_ulap-2];
  nlon = dsizes_ulap[ndims_ulap-1];
/*
 * Compute the total number of elements in our array.
 */
  nt = 1;
  for(i = 0; i < ndims_ulap-2; i++) {
    nt *= dsizes_ulap[i];
  }
/*
 * Get output arrays.
 */
  u = (float*)NclGetArgValue(
           2,
           4,
           &ndims_u, 
           dsizes_u,
           NULL,
           NULL,
           NULL,
           1);
  v = (float*)NclGetArgValue(
           3,
           4,
           &ndims_v, 
           dsizes_v,
           NULL,
           NULL,
           NULL,
           1);
/*
 * The output arrays must also be at least 2-dimensional and the same sizes.
 */
  if( ndims_u != ndims_ulap || ndims_v != ndims_ulap ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvg: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_ulap[i] || dsizes_v[i] != dsizes_ulap[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvg: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Check for missing values.
 */
  if(has_missing_ulap || has_missing_vlap) {
    l = 0;
    while( l < nt*nlat*nlon && !found_missing ) {
      if(ulap[l]==missing_ulap.floatval || vlap[l]==missing_vlap.floatval) {
	found_missing = 1;
      }
      l++;
    }
  }
  if(found_missing) {
    if(has_missing_ulap) missing = missing_ulap.floatval;
    else                 missing = missing_vlap.floatval;
    for(i = 0; i < nt*nlat*nlon; i++) {
      u[i] = v[i] = missing;
    }
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvg: The input array cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlon * nlat;
  work = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(geomatv,GEOMATV)(&nlon,&nlat,&ulap[j],&vlap[j],work);
    j += nlat*nlon;
  }
  free(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 * dynamically allocate various temporary space *
 * Note the order "vhagc(...,v,u,....)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*nlat*(nlat+1)+2,2*nlat*(2*nlon*nt+3*l2 ));
  lvhagc = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+l2+15;
  ldwork = 2*nlat*(nlat+1)+1;

  br =     (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  bi =     (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  cr =     (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  ci =     (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  wvhagc = (float*)calloc(        lvhagc*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  dwork = (double*)calloc(  ldwork*sizeof(double),1);

  if( br == NULL || bi == NULL || cr == NULL || ci == NULL ||
      wvhagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(vhagci,VHAGCI)(&nlat,&nlon,wvhagc,&lvhagc,dwork,&ldwork,&jer);
  NGCALLF(vhagc,VHAGC)(&nlat,&nlon,&isym,&nt,&vlap[0],&ulap[0],
		       &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
		       wvhagc,&lvhagc,work,&lwork,&ker);
  free(wvhagc);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("ilapvg","vhagc",&ier,&jer,&ker,&mer,8,5);
/* 
 * compute the vector laplacian using the vector spherical harmonic 
 */ 
  /*  lwork  = max(4*nlat*(nlat+1)+2,4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15);*/
  l1     = min(nlat,nlon/2);
  lwork = nlat*(2*nt*nlon+max(6*l2,nlon))+nlat*(4*nt*l1+1);
  /*  lvhsgc = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15;*/
  lvhsgc = l1*l2*(2*nlat-l1+1)+nlon+15;

  ldwork = 2*nlat*(nlat+1)+1;

  wvhsgc = (float*)calloc(lvhsgc*sizeof(float),1);
  work   = (float*)calloc( lwork*sizeof(float),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);

  if( wvhsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(vhsgci,VHSGCI)(&nlat,&nlon,wvhsgc,&lvhsgc,dwork,&ldwork,&jer);
  NGCALLF(ivlapgc,IVLAPGC)(&nlat,&nlon,&isym,&nt,&v[0],&u[0],
			   &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
			   wvhsgc,&lvhsgc,work,&lwork,&ker);
  free(br);
  free(bi);
  free(cr);
  free(ci);
  free(wvhsgc);
  free(work);
  free(dwork);

  NGCALLF(chkerr,CHKERR)("ilapvg","vhsgci,ivlapgc",&ier,&jer,&ker,&mer,8,14);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
 
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeov,MATGEOV)(&nlat,&nlon,&u[j],&v[j],work);
    NGCALLF(matgeov,MATGEOV)(&nlat,&nlon,&ulap[j],&vlap[j],work);
    j += nlat*nlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = powf(6.37122e+6,2.);         /* radius of earth**2 */
  
  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&u[0],&scale,&ner);
  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&v[0],&scale,&ner);
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return
 */
  return(NhlNOERROR);
}


NhlErrorTypes lapsf_W( void )
{
/*
 * Input array variables
 */
  float *z;
  int dsizes_z[NCL_MAX_DIMENSIONS], ndims_z;
  NclScalar missing_z;
  int has_missing_z, found_missing=0;
  int nt, nlat, nlon;
/*
 * Output array variables
 */
  float *zlap;
  int ndims_zlap, dsizes_zlap[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  float scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lshaec, lshsec;
  float *work, *wshaec, *wshsec, *a, *b;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  z = (float*)NclGetArgValue(
           0,
           2,
           &ndims_z, 
           dsizes_z,
           &missing_z,
           &has_missing_z,
           NULL,
           2);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_z < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsf: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }
  nlat = dsizes_z[ndims_z-2];
  nlon = dsizes_z[ndims_z-1];
/*
 * Compute the total number of elements in our array.
 */
  nt = 1;
  for(i = 0; i < ndims_z-2; i++) {
    nt *= dsizes_z[i];
  }
/*
 * Get output array.
 */
  zlap = (float*)NclGetArgValue(
           1,
           2,
           &ndims_zlap, 
           dsizes_zlap,
           NULL,
           NULL,
           NULL,
           1);
/*
 * The output array must also be at least 2-dimensional.
 */
  if( ndims_z != ndims_zlap  ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsf: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_z; i++ ) {
    if( dsizes_z[i] != dsizes_zlap[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsf: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Check for missing values.
 */
  if(has_missing_z) {
    i = 0;
    while( i < nt*nlat*nlon && !found_missing ) {
      if(z[i++] == missing_z.floatval) found_missing = 1;
    }
  }
  if(found_missing) {
    for(i = 0; i < nt*nlat*nlon; i++) {
      zlap[i] = missing_z.floatval;
    }
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsf: The input arrays cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlon * nlat;
  work = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(geomat,GEOMAT)(&nlon,&nlat,&z[j],work);
    j += nlat*nlon;
  }
  free(work);
/* 
 * shaec performs the spherical harmonic analysis on a (scalar) gaussian 
 * grid(s) and returns the coefficients in array(s) a,b
 * Here the scalar grid is "z" (a scalar function)
 */ 
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(2*(nlat+1),nlat*(nlon*nt+max(3*l2,nlon)));
  lshaec = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;
  ldwork = nlat+1;

  a =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  b =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  wshaec = (float*)calloc(        lshaec*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  dwork =  (double*)calloc(        ldwork*sizeof(double),1);

  if( a == NULL || b == NULL || wshaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(shaeci,SHAECI)(&nlat,&nlon,wshaec,&lshaec,dwork,&ldwork,&jer);
  NGCALLF(shaec,SHAEC)(&nlat,&nlon,&isym,&nt,&z[0],&idvw,&jdvw,a,b,
		       &mdab,&ndab,wshaec,&lshaec,work,&lwork,&ker);
  free(wshaec);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("lapsf","shaec",&ier,&jer,&ker,&mer,7,5);
/* 
 * compute the laplacian
 */ 
  lwork = nlat*(2*nt*nlon+max(6*l2,nlon)+2*nt*l1+1);
  ldwork = nlat+1;
  lshsec = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;

  wshsec = (float*)calloc(lshsec*sizeof(float),1);
  work =   (float*)calloc( lwork*sizeof(float),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);

  if( wshsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(shseci,SHSECI)(&nlat,&nlon,wshsec,&lshsec,dwork,&ldwork,&jer);
  NGCALLF(slapec,SLAPEC)(&nlat,&nlon,&isym,&nt,&zlap[0],&idvw,&jdvw,a,b,
			 &mdab,&ndab,wshsec,&lshsec,work,&lwork,&ker);
  free(a);
  free(b);
  free(wshsec);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("lapsf","vhseci+slapec",&ier,&jer,&ker,&mer,7,13);
/*
 * transform from math coordinates to geophysical coordinates
 *  (math) nlat is the first dim
 */ 
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&z[j],work);
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&zlap[j],work);
    j += nlat*nlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = powf(1./6.37122e+6,2.);       /* (1/(radius of earth))**2 */
 
  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&zlap[0],&scale,&ner);
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return array.
 */
  return(NhlNOERROR);
}


NhlErrorTypes lapsF_W( void )
{
/*
 * Input array variables
 */
  float *z;
  int dsizes_z[NCL_MAX_DIMENSIONS], ndims_z;
  NclScalar missing_z;
  int has_missing_z, found_missing=0;
  int nt, nlat, nlon;
/*
 * Output array variables
 */
  float *zlap;
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  float scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lshaec, lshsec;
  float *work, *wshaec, *wshsec, *a, *b;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  z = (float*)NclGetArgValue(
           0,
           1,
           &ndims_z, 
           dsizes_z,
           &missing_z,
           &has_missing_z,
           NULL,
           2);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_z < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsF: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }
  nlat = dsizes_z[ndims_z-2];
  nlon = dsizes_z[ndims_z-1];
/*
 * Compute the total number of elements in our array.
 */
  nt = 1;
  for(i = 0; i < ndims_z-2; i++) {
    nt *= dsizes_z[i];
  }
/*
 * Allocate space for output array
 */
  zlap = (float*)calloc(nt*nlat*nlon*sizeof(float),1);
  if( zlap == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsF: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Check for missing values.
 */
  if(has_missing_z) {
    i = 0;
    while( i < nt*nlat*nlon && !found_missing ) {
      if(z[i++] == missing_z.floatval) found_missing = 1;
    }
  }
  if(found_missing) {
    for(i = 0; i < nt*nlat*nlon; i++) {
      zlap[i] = missing_z.floatval;
    }
    NhlPError(NhlWARNING,NhlEUNKNOWN,"lapsF: The input arrays cannot contain any missing values");
    return(NclReturnValue((void*)zlap,ndims_z,dsizes_z,NULL,NCL_float,0));
  }
/*
 * Determine the workspace size.
 */
  lwork = nlon * nlat;
  work = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsF: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(geomat,GEOMAT)(&nlon,&nlat,&z[j],work);
    j += nlat*nlon;
  }
  free(work);
/* 
 * shaec performs the spherical harmonic analysis on a (scalar) gaussian 
 * grid(s) and returns the coefficients in array(s) a,b
 * Here the scalar grid is "z" (a scalar function)
 */ 
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(2*(nlat+1),nlat*(nlon*nt+max(3*l2,nlon)));
  lshaec = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;
  ldwork = nlat+1;

  a =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  b =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  wshaec = (float*)calloc(        lshaec*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  dwork =  (double*)calloc(        ldwork*sizeof(double),1);

  if( a == NULL || b == NULL || wshaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsF: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(shaeci,SHAECI)(&nlat,&nlon,wshaec,&lshaec,dwork,&ldwork,&jer);
  NGCALLF(shaec,SHAEC)(&nlat,&nlon,&isym,&nt,&z[0],&idvw,&jdvw,a,b,
		       &mdab,&ndab,wshaec,&lshaec,work,&lwork,&ker);
  free(wshaec);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("lapsF","shaec",&ier,&jer,&ker,&mer,7,5);
/* 
 * compute the laplacian
 */ 
  /*  lwork  = max(nlat+1,nlat*(2*nt*nlon+max(6*l2,nlon)+2*l3+1)); */
  lwork = nlat*(2*nt*nlon+max(6*l2,nlon)+2*nt*l1+1);
  ldwork = nlat+1;
  l1     = min(nlat,(nlon+1)/2);
  lshsec = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;

  wshsec = (float*)calloc(lshsec*sizeof(float),1);
  work =   (float*)calloc( lwork*sizeof(float),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);

  if( wshsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsF: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(shseci,SHSECI)(&nlat,&nlon,wshsec,&lshsec,dwork,&ldwork,&jer);
  NGCALLF(slapec,SLAPEC)(&nlat,&nlon,&isym,&nt,&zlap[0],&idvw,&jdvw,a,b,
			 &mdab,&ndab,wshsec,&lshsec,work,&lwork,&ker);
  free(a);
  free(b);
  free(wshsec);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("lapsF","vhseci+slapec",&ier,&jer,&ker,&mer,7,13);
/*
 * transform from math coordinates to geophysical coordinates
 *  (math) nlat is the first dim
 */ 
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsF: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&z[j],work);
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&zlap[j],work);
    j += nlat*nlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = powf(1./6.37122e+6,2.);       /* (1/(radius of earth))**2 */
 
  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&zlap[0],&scale,&ner);
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return array.
 */
  return(NclReturnValue((void*)zlap,ndims_z,dsizes_z,NULL,NCL_float,0));
}


NhlErrorTypes lapsg_W( void )
{
/*
 * Input array variables
 */
  float *z;
  int dsizes_z[NCL_MAX_DIMENSIONS], ndims_z;
  NclScalar missing_z;
  int has_missing_z, found_missing=0;
  int nt, nlat, nlon;
/*
 * Output array variables
 */
  float *zlap;
  int ndims_zlap, dsizes_zlap[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  float scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lshagc, lshsgc;
  float *work, *wshagc, *wshsgc, *a, *b;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  z = (float*)NclGetArgValue(
           0,
           2,
           &ndims_z, 
           dsizes_z,
           &missing_z,
           &has_missing_z,
           NULL,
           2);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_z < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsg: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }
  nlat = dsizes_z[ndims_z-2];
  nlon = dsizes_z[ndims_z-1];
/*
 * Compute the total number of elements in our array.
 */
  nt = 1;
  for(i = 0; i < ndims_z-2; i++) {
    nt *= dsizes_z[i];
  }
/*
 * Get output array.
 */
  zlap = (float*)NclGetArgValue(
           1,
           2,
           &ndims_zlap, 
           dsizes_zlap,
           NULL,
           NULL,
           NULL,
           1);
/*
 * The output array must also be at least 2-dimensional.
 */
  if( ndims_z != ndims_zlap  ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsg: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_z; i++ ) {
    if( dsizes_z[i] != dsizes_zlap[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsg The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Check for missing values.
 */
  if(has_missing_z) {
    i = 0;
    while( i < nt*nlat*nlon && !found_missing ) {
      if(z[i++] == missing_z.floatval) found_missing = 1;
    }
  }
  if(found_missing) {
    for(i = 0; i < nt*nlat*nlon; i++) {
      zlap[i] = missing_z.floatval;
    }
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsg: The input arrays cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlon * nlat;
  work = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(geomat,GEOMAT)(&nlon,&nlat,&z[j],work);
    j += nlat*nlon;
  }
  free(work);
/* 
 * shagc performs the spherical harmonic analysis on a (scalar) gaussian 
 * grid(s) and returns the coefficients in array(s) a,b
 * Here the scalar grid is "z" (a scalar function)
 */ 
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*nlat*(nlat+2)+2,nlat*(nlon*nt+max(3*l2,nlon)));
  lshagc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;
  ldwork = nlat*(nlat+4);

  a =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  b =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  wshagc = (float*)calloc(        lshagc*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  dwork =  (double*)calloc(       ldwork*sizeof(double),1);

  if( a == NULL || b == NULL || wshagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(shagci,SHAGCI)(&nlat,&nlon,wshagc,&lshagc,dwork,&ldwork,&jer);
  NGCALLF(shagc,SHAGC)(&nlat,&nlon,&isym,&nt,&z[0],&idvw,&jdvw,a,b,
		       &mdab,&ndab,wshagc,&lshagc,work,&lwork,&ker);
  free(wshagc);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("lapsg","shagc",&ier,&jer,&ker,&mer,7,5);
/* 
 * compute the laplacian
 */ 
  /*  lwork  = max(4*nlat*(nlat+2)+2,nlat*(2*nt*nlon+max(6*l2,nlon)+2*l3+1));*/
  lwork = nlat*(2*nt*nlon+max(6*l2,nlon)+2*nt*l1+1);
  ldwork = nlat*(nlat+4);
  lshsgc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;

  wshsgc = (float*)calloc(lshsgc*sizeof(float),1);
  work =   (float*)calloc( lwork*sizeof(float),1);

  dwork = (double*)calloc(ldwork*sizeof(double),1);
  if( wshsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(shsgci,SHSGCI)(&nlat,&nlon,wshsgc,&lshsgc,dwork,&ldwork,&jer);
  NGCALLF(slapgc,SLAPGC)(&nlat,&nlon,&isym,&nt,&zlap[0],&idvw,&jdvw,a,b,
			 &mdab,&ndab,wshsgc,&lshsgc,work,&lwork,&ker);
  free(a);
  free(b);
  free(wshsgc);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("lapsg","vhsgci+slapgc",&ier,&jer,&ker,&mer,7,13);
/*
 * transform from math coordinates to geophysical coordinates
 *  (math) nlat is the first dim
 */ 
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&z[j],work);
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&zlap[j],work);
    j += nlat*nlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = powf(1./6.37122e+6,2.);       /* (1/(radius of earth))**2 */
  
  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&zlap[0],&scale,&ner);
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return array.
 */
  return(NhlNOERROR);
}


NhlErrorTypes lapsG_W( void )
{
/*
 * Input array variables
 */
  float *z;
  int dsizes_z[NCL_MAX_DIMENSIONS], ndims_z;
  NclScalar missing_z;
  int has_missing_z, found_missing=0;
  int nt, nlat, nlon;
/*
 * Output array variables
 */
  float *zlap;
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  float scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lshagc, lshsgc;
  float *work, *wshagc, *wshsgc, *a, *b;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  z = (float*)NclGetArgValue(
           0,
           1,
           &ndims_z, 
           dsizes_z,
           &missing_z,
           &has_missing_z,
           NULL,
           2);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_z < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsG: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }
  nlat = dsizes_z[ndims_z-2];
  nlon = dsizes_z[ndims_z-1];
/*
 * Compute the total number of elements in our array.
 */
  nt = 1;
  for(i = 0; i < ndims_z-2; i++) {
    nt *= dsizes_z[i];
  }
/*
 * Allocate space for output array
 */
  zlap = (float*)calloc(nt*nlat*nlon*sizeof(float),1);
  if( zlap == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsG: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Check for missing values.
 */
  if(has_missing_z) {
    i = 0;
    while( i < nt*nlat*nlon && !found_missing ) {
      if(z[i++] == missing_z.floatval) found_missing = 1;
    }
  }
  if(found_missing) {
    for(i = 0; i < nt*nlat*nlon; i++) {
      zlap[i] = missing_z.floatval;
    }
    NhlPError(NhlWARNING,NhlEUNKNOWN,"lapsG: The input arrays cannot contain any missing values");
    return(NclReturnValue((void*)zlap,ndims_z,dsizes_z,NULL,NCL_float,0));
  }
/*
 * Determine the workspace size.
 */
  lwork = nlon * nlat;
  work = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsG: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(geomat,GEOMAT)(&nlon,&nlat,&z[j],work);
    j += nlat*nlon;
  }
  free(work);
/* 
 * shagc performs the spherical harmonic analysis on a (scalar) gaussian 
 * grid(s) and returns the coefficients in array(s) a,b
 * Here the scalar grid is "z" (a scalar function)
 */ 
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*nlat*(nlat+2)+2,nlat*(nlon*nt+max(3*l2,nlon)));
  lshagc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;
  ldwork = nlat*(nlat+4);

  a =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  b =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  wshagc = (float*)calloc(        lshagc*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  dwork =  (double*)calloc(       ldwork*sizeof(double),1);

  if( a == NULL || b == NULL || wshagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsG: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(shagci,SHAGCI)(&nlat,&nlon,wshagc,&lshagc,dwork,&ldwork,&jer);
  NGCALLF(shagc,SHAGC)(&nlat,&nlon,&isym,&nt,&z[0],&idvw,&jdvw,a,b,
		       &mdab,&ndab,wshagc,&lshagc,work,&lwork,&ker);
  free(wshagc);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("lapsG","shagc",&ier,&jer,&ker,&mer,7,5);
/* 
 * compute the laplacian
 */ 
  /*  lwork  = max(4*nlat*(nlat+2)+2,nlat*(2*nt*nlon+max(6*l2,nlon)+2*l3+1));*/
  lwork = nlat*(2*nt*nlon+max(6*l2,nlon)+2*nt*l1+1);
  ldwork = nlat*(nlat+4);
  lshsgc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;

  wshsgc = (float*)calloc(lshsgc*sizeof(float),1);
  work =   (float*)calloc( lwork*sizeof(float),1);

  dwork = (double*)calloc(ldwork*sizeof(double),1);
  if( wshsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsG: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(shsgci,SHSGCI)(&nlat,&nlon,wshsgc,&lshsgc,dwork,&ldwork,&jer);
  NGCALLF(slapgc,SLAPGC)(&nlat,&nlon,&isym,&nt,&zlap[0],&idvw,&jdvw,a,b,
			 &mdab,&ndab,wshsgc,&lshsgc,work,&lwork,&ker);
  free(a);
  free(b);
  free(wshsgc);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("lapsG","vhsgci+slapgc",&ier,&jer,&ker,&mer,7,13);
/*
 * transform from math coordinates to geophysical coordinates
 *  (math) nlat is the first dim
 */ 
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsG: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&z[j],work);
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&zlap[j],work);
    j += nlat*nlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = powf(1./6.37122e+6,2.);       /* (1/(radius of earth))**2 */
  
  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&zlap[0],&scale,&ner);
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return array.
 */
  return(NclReturnValue((void*)zlap,ndims_z,dsizes_z,NULL,NCL_float,0));
}


NhlErrorTypes lapvf_W( void )
{
/*
 * Input array variables
 */
  float *u, *v;
  int ndims_u, dsizes_u[NCL_MAX_DIMENSIONS], nt, nlat, nlon;
  int ndims_v, dsizes_v[NCL_MAX_DIMENSIONS];
  NclScalar missing_u, missing_v;
  float missing;
  int has_missing_u, has_missing_v, found_missing=0;
/*
 * Output array variables
 */
  float *ulap, *vlap;
  int dsizes_ulap[NCL_MAX_DIMENSIONS], dsizes_vlap[NCL_MAX_DIMENSIONS];
  int ndims_ulap, ndims_vlap;
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  float scale;
/*
 * Workspace variables
 */
  int lwork, ldwork,lvhaec, lvhsec;
  float *work, *wvhaec, *wvhsec, *br, *bi, *cr, *ci;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  u = (float*)NclGetArgValue(
           0,
           4,
           &ndims_u, 
           dsizes_u,
           &missing_u,
           &has_missing_u,
           NULL,
           2);
  v = (float*)NclGetArgValue(
           1,
           4,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           NULL,
           2);
/*
 * The grids coming in must be at least 2-dimensional and the same size.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvf: The input arrays must be at least 2-dimensional and have the same dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_v[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvf: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  nlat = dsizes_u[ndims_u-2];
  nlon = dsizes_u[ndims_u-1];
/*
 * Compute the total number of elements in our array.
 */
  nt = 1;
  for(i = 0; i < ndims_u-2; i++) {
    nt *= dsizes_u[i];
  }
/*
 * Get output arrays.
 */
  ulap = (float*)NclGetArgValue(
           2,
           4,
           &ndims_ulap, 
           dsizes_ulap,
           NULL,
           NULL,
           NULL,
           1);
  vlap = (float*)NclGetArgValue(
           3,
           4,
           &ndims_vlap, 
           dsizes_vlap,
           NULL,
           NULL,
           NULL,
           1);
/*
 * The output arrays must also be at least 2-dimensional and the same sizes.
 */
  if( ndims_u != ndims_ulap || ndims_v != ndims_ulap ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvf: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_ulap[i] != dsizes_u[i] || dsizes_vlap[i] != dsizes_u[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvf: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Check for missing values.
 */
  if(has_missing_u || has_missing_v) {
    l = 0;
    while( l < nt*nlat*nlon && !found_missing ) {
      if(u[l]==missing_u.floatval || v[l]==missing_v.floatval) {
	found_missing = 1;
      }
      l++;
    }
  }
  if(found_missing) {
    if(has_missing_u) missing = missing_u.floatval;
    else              missing = missing_v.floatval;
    for(i = 0; i < nt*nlat*nlon; i++) {
      ulap[i] = vlap[i] = missing;
    }
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvf: The input array cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlon * nlat;
  work = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(geomatv,GEOMATV)(&nlon,&nlat,&u[j],&v[j],work);
    j += nlat*nlon;
  }
  free(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 * dynamically allocate various temporary space *
 * Note the order "vhaec(...,v,u,....)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*(nlat+1),nlat*(2*nt*nlon+max(6*l2,nlon)));
  ldwork = 2*(nlat+2);
  lvhaec = 4*nlat*l2+3*max(l1-2,0)*(nlat+nlat-l1-1)+nlon+15;

  br =     (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  bi =     (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  cr =     (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  ci =     (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  wvhaec = (float*)calloc(        lvhaec*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  dwork = (double*)calloc(        ldwork*sizeof(double),1);

  if( br == NULL || bi == NULL || cr == NULL || ci == NULL ||
      wvhaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(vhaeci,VHAECI)(&nlat,&nlon,wvhaec,&lvhaec,dwork,&ldwork,&jer);
  NGCALLF(vhaec,VHAEC)(&nlat,&nlon,&isym,&nt,&v[0],&u[0],
		       &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
		       wvhaec,&lvhaec,work,&lwork,&ker);
  free(wvhaec);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("lapvf","vhaec",&ier,&jer,&ker,&mer,7,5);
/* 
 * compute the vector laplacian using the vector spherical harmonic 
 */ 
  l1     = min(nlat,(nlon+1)/2);
  /*  lwork  = max(4*(nlat+1),4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15);*/
  lwork = nlat*(2*nt*nlon+max(6*l2,nlon)+1)+4*(l1*nlat*nt);
  lvhsec = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15;
  ldwork = 2*(nlat+2);

  wvhsec = (float*)calloc(lvhsec*sizeof(float),1);
  work   = (float*)calloc( lwork*sizeof(float),1);
  dwork = (double*)calloc( ldwork*sizeof(double),1);

  if( wvhsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(vhseci,VHSECI)(&nlat,&nlon,wvhsec,&lvhsec,dwork,&ldwork,&jer);
  NGCALLF(vlapec,VLAPEC)(&nlat,&nlon,&isym,&nt,&vlap[0],&ulap[0],
			 &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,wvhsec,&lvhsec,
			 work,&lwork,&ker);

  free(br);
  free(bi);
  free(cr);
  free(ci);
  free(wvhsec);
  free(work);
  free(dwork);

  NGCALLF(chkerr,CHKERR)("lapvf","vhseci,vlapec",&ier,&jer,&ker,&mer,7,13);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
 
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeov,MATGEOV)(&nlat,&nlon,&u[j],&v[j],work);
    NGCALLF(matgeov,MATGEOV)(&nlat,&nlon,&ulap[j],&vlap[j],work);
    j += nlat*nlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
   scale = powf(1./6.37122e+6,2.);       /* (1/(radius of earth))**2 */
  
   NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&ulap[0],&scale,&ner);
   NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&vlap[0],&scale,&ner);
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return
 */
  return(NhlNOERROR);
}


NhlErrorTypes lapvg_W( void )
{
/*
 * Input array variables
 */
  float *u, *v;
  int ndims_u, dsizes_u[NCL_MAX_DIMENSIONS], nt, nlat, nlon;
  int ndims_v, dsizes_v[NCL_MAX_DIMENSIONS];
  NclScalar missing_u, missing_v;
  float missing;
  int has_missing_u, has_missing_v, found_missing=0;
/*
 * Output array variables
 */
  float *ulap, *vlap;
  int dsizes_ulap[NCL_MAX_DIMENSIONS], dsizes_vlap[NCL_MAX_DIMENSIONS];
  int ndims_ulap, ndims_vlap;
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  float scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhagc, lvhsgc;
  float *work, *wvhagc, *wvhsgc, *br, *bi, *cr, *ci;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  u = (float*)NclGetArgValue(
           0,
           4,
           &ndims_u, 
           dsizes_u,
           &missing_u,
           &has_missing_u,
           NULL,
           2);
  v = (float*)NclGetArgValue(
           1,
           4,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           NULL,
           2);
/*
 * The grids coming in must be at least 2-dimensional and the same size.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvg: The input arrays must be at least 2-dimensional and have the same dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_v[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvg: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  nlat = dsizes_u[ndims_u-2];
  nlon = dsizes_u[ndims_u-1];
/*
 * Compute the total number of elements in our array.
 */
  nt = 1;
  for(i = 0; i < ndims_u-2; i++) {
    nt *= dsizes_u[i];
  }
/*
 * Get output arrays.
 */
  ulap = (float*)NclGetArgValue(
           2,
           4,
           &ndims_ulap, 
           dsizes_ulap,
           NULL,
           NULL,
           NULL,
           1);
  vlap = (float*)NclGetArgValue(
           3,
           4,
           &ndims_vlap, 
           dsizes_vlap,
           NULL,
           NULL,
           NULL,
           1);
/*
 * The output arrays must also be at least 2-dimensional and the same sizes.
 */
  if( ndims_u != ndims_ulap || ndims_v != ndims_ulap ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvg: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_ulap[i] != dsizes_u[i] || dsizes_vlap[i] != dsizes_u[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvg: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Check for missing values.
 */
  if(has_missing_u || has_missing_v) {
    l = 0;
    while( l < nt*nlat*nlon && !found_missing ) {
      if(u[l]==missing_u.floatval || v[l]==missing_v.floatval) {
	found_missing = 1;
      }
      l++;
    }
  }
  if(found_missing) {
    if(has_missing_u) missing = missing_u.floatval;
    else              missing = missing_v.floatval;
    for(i = 0; i < nt*nlat*nlon; i++) {
      ulap[i] = vlap[i] = missing;
    }
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvg: The input array cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlon * nlat;
  work = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(geomatv,GEOMATV)(&nlon,&nlat,&u[j],&v[j],work);
    j += nlat*nlon;
  }
  free(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 * dynamically allocate various temporary space *
 * Note the order "vhagc(...,v,u,....)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*nlat*(nlat+1)+2,2*nlat*(2*nlon*nt+3*l2 ));
  lvhagc = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+l2+15;
  ldwork = 2*nlat*(nlat+1)+1;

  br =     (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  bi =     (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  cr =     (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  ci =     (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  wvhagc = (float*)calloc(        lvhagc*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  dwork = (double*)calloc(  ldwork*sizeof(double),1);

  if( br == NULL || bi == NULL || cr == NULL || ci == NULL ||
      wvhagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(vhagci,VHAGCI)(&nlat,&nlon,wvhagc,&lvhagc,dwork,&ldwork,&jer);
  NGCALLF(vhagc,VHAGC)(&nlat,&nlon,&isym,&nt,&v[0],&u[0],
		       &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
		       wvhagc,&lvhagc,work,&lwork,&ker);

  free(wvhagc);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("lapvg","vhagc",&ier,&jer,&ker,&mer,7,5);
/* 
 * compute the vector laplacian using the vector spherical harmonic 
 */ 
  /*  lwork  = max(4*nlat*(nlat+1)+2,4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15);*/
  lwork = nlat*(2*nt*nlon+max(6*l2,nlon)+1)+4*(l1*nlat*nt);
  lvhsgc = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15;
  ldwork = 2*nlat*(nlat+1)+1;

  wvhsgc = (float*)calloc(lvhsgc*sizeof(float),1);
  work   = (float*)calloc( lwork*sizeof(float),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);

  if( wvhsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(vhsgci,VHSGCI)(&nlat,&nlon,wvhsgc,&lvhsgc,dwork,&ldwork,&jer);
  NGCALLF(vlapgc,VLAPGC)(&nlat,&nlon,&isym,&nt,&vlap[0],&ulap[0],
			 &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
			 wvhsgc,&lvhsgc,work,&lwork,&ker);

  free(br);
  free(bi);
  free(cr);
  free(ci);
  free(wvhsgc);
  free(work);
  free(dwork);

  NGCALLF(chkerr,CHKERR)("lapvg","vhsgci,vlapgc",&ier,&jer,&ker,&mer,7,13);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
 
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeov,MATGEOV)(&nlat,&nlon,&u[j],&v[j],work);
    NGCALLF(matgeov,MATGEOV)(&nlat,&nlon,&ulap[j],&vlap[j],work);
    j += nlat*nlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = powf(1./6.37122e+6,2.);       /* (1/(radius of earth))**2 */
  
  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&ulap[0],&scale,&ner);
  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&vlap[0],&scale,&ner);
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return
 */
  return(NhlNOERROR);
}


NhlErrorTypes uv2sfvpf_W( void )
{
/*
 * Input array variables
 */
  float *u, *v;
  int ndims_u, dsizes_u[NCL_MAX_DIMENSIONS], nt, nlat, nlon;
  int ndims_v, dsizes_v[NCL_MAX_DIMENSIONS];
  NclScalar missing_u, missing_v;
  float missing;
  int has_missing_u, has_missing_v, found_missing=0;
/*
 * Output array variables
 */
  float *sf, *vp;
  int dsizes_sf[NCL_MAX_DIMENSIONS], dsizes_vp[NCL_MAX_DIMENSIONS];
  int ndims_sf, ndims_vp;
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  float scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhaec, lshsec;
  float *work, *wvhaec, *wshsec, *br, *bi, *cr, *ci;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  u = (float*)NclGetArgValue(
           0,
           4,
           &ndims_u, 
           dsizes_u,
           &missing_u,
           &has_missing_u,
           NULL,
           2);
  v = (float*)NclGetArgValue(
           1,
           4,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           NULL,
           2);
/*
 * The grids coming in must be at least 2-dimensional and the same size.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpf: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_v[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpf: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  nlat = dsizes_u[ndims_u-2];
  nlon = dsizes_u[ndims_u-1];
/*
 * Compute the total number of elements in our array.
 */
  nt = 1;
  for(i = 0; i < ndims_u-2; i++) {
    nt *= dsizes_u[i];
  }
/*
 * Get output arrays.
 */
  sf = (float*)NclGetArgValue(
           2,
           4,
           &ndims_sf, 
           dsizes_sf,
           NULL,
           NULL,
           NULL,
           1);
  vp = (float*)NclGetArgValue(
           3,
           4,
           &ndims_vp, 
           dsizes_vp,
           NULL,
           NULL,
           NULL,
           1);
/*
 * The output arrays must have the same dimensions as the input arrays.
 */
  if( ndims_sf != ndims_u || ndims_vp != ndims_u ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpf: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_sf[i] || dsizes_u[i] != dsizes_vp[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpf: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Check for missing values.
 */
  if(has_missing_u || has_missing_v) {
    l = 0;
    while( l < nt*nlat*nlon && !found_missing ) {
      if(u[l]==missing_u.floatval || v[l]==missing_v.floatval) {
	found_missing = 1;
      }
      l++;
    }
  }
  if(found_missing) {
    if(has_missing_u) missing = missing_u.floatval;
    else              missing = missing_v.floatval;
    for(i = 0; i < nt*nlat*nlon; i++) {
      sf[i] = vp[i] = missing;
    }
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpf: The input array cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlon * nlat;
  work = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(geomatv,GEOMATV)(&nlon,&nlat,&u[j],&v[j],work);
    j += nlat*nlon;
  }
  free(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 * dynamically allocate various temporary space *
 * Note the order "vhaec(...,v,u,....)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*(nlat+1),nlat*(2*nt*nlon+max(6*l2,nlon)));
  ldwork = 2*(nlat+2);
  lvhaec = 4*nlat*l2+3*max(l1-2,0)*(nlat+nlat-l1-1)+nlon+15;

  br =     (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  bi =     (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  cr =     (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  ci =     (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  wvhaec = (float*)calloc(        lvhaec*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  dwork = (double*)calloc(        ldwork*sizeof(double),1);

  if( br == NULL || bi == NULL || cr == NULL || ci == NULL ||
      wvhaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(vhaeci,VHAECI)(&nlat,&nlon,wvhaec,&lvhaec,dwork,&ldwork,&jer);
  NGCALLF(vhaec,VHAEC)(&nlat,&nlon,&isym,&nt,&v[0],&u[0],
		       &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
		       wvhaec,&lvhaec,work,&lwork,&ker);
  free(wvhaec);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("uv2sfvpf","vhaec",&ier,&jer,&ker,&mer,10,5);

  l1     = min(nlat,(nlon+2)/2);
  lwork  = max(nlat+1,nlat*((nt*nlon+max(3*l2,nlon))+2*l1*nt+1));
  ldwork = nlat+1;
  lshsec = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;

  wshsec = (float*)calloc(lshsec*sizeof(float),1);
  work   = (float*)calloc( lwork*sizeof(float),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);

  if( wshsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(shseci,SHSECI)(&nlat,&nlon,wshsec,&lshsec,dwork,&ldwork,&jer);
  NGCALLF(sfvpec,SFVPEC)(&nlat,&nlon,&isym,&nt,&sf[0],&vp[0],
			 &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
			 wshsec,&lshsec,work,&lwork,&ker);

  free(br);
  free(bi);
  free(cr);
  free(ci);
  free(wshsec);
  free(work);
  free(dwork);

  NGCALLF(chkerr,CHKERR)("uv2sfvpf","sfvpec+shseci",&ier,&jer,&ker,&mer,10,13);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
 
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&sf[j],work);
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&vp[j],work);
    NGCALLF(matgeov,MATGEOV)(&nlat,&nlon,&u[j],&v[j],work);
    j += nlat*nlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 6.37122e+6;        /* radius of earth */
  
  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&sf[0],&scale,&ner);
  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&vp[0],&scale,&ner);
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return
 */
  return(NhlNOERROR);
}


NhlErrorTypes uv2sfvpg_W( void )
{
/*
 * Input array variables
 */
  float *u, *v;
  int ndims_u, dsizes_u[NCL_MAX_DIMENSIONS], nt, nlat, nlon;
  int ndims_v, dsizes_v[NCL_MAX_DIMENSIONS];
  NclScalar missing_u, missing_v;
  float missing;
  int has_missing_u, has_missing_v, found_missing=0;
/*
 * Output array variables
 */
  float *sf, *vp;
  int dsizes_sf[NCL_MAX_DIMENSIONS], dsizes_vp[NCL_MAX_DIMENSIONS];
  int ndims_sf, ndims_vp;
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  float scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhagc, lshsgc;
  float *work, *wvhagc, *wshsgc, *br, *bi, *cr, *ci;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  u = (float*)NclGetArgValue(
           0,
           4,
           &ndims_u, 
           dsizes_u,
           &missing_u,
           &has_missing_u,
           NULL,
           2);
  v = (float*)NclGetArgValue(
           1,
           4,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           NULL,
           2);
/*
 * The grids coming in must be at least 2-dimensional and the same size.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpg: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_v[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpg: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  nlat = dsizes_u[ndims_u-2];
  nlon = dsizes_u[ndims_u-1];
/*
 * Compute the total number of elements in our array.
 */
  nt = 1;
  for(i = 0; i < ndims_u-2; i++) {
    nt *= dsizes_u[i];
  }
/*
 * Get output arrays.
 */
  sf = (float*)NclGetArgValue(
           2,
           4,
           &ndims_sf, 
           dsizes_sf,
           NULL,
           NULL,
           NULL,
           1);
  vp = (float*)NclGetArgValue(
           3,
           4,
           &ndims_vp, 
           dsizes_vp,
           NULL,
           NULL,
           NULL,
           1);
/*
 * The output arrays must also be at least 2-dimensional and the same sizes.
 */
  if( ndims_sf != ndims_u || ndims_vp != ndims_u ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpg: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_sf[i] || dsizes_u[i] != dsizes_vp[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpg: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Check for missing values.
 */
  if(has_missing_u || has_missing_v) {
    l = 0;
    while( l < nt*nlat*nlon && !found_missing ) {
      if(u[l]==missing_u.floatval || v[l]==missing_v.floatval) {
	found_missing = 1;
      }
      l++;
    }
  }
  if(found_missing) {
    if(has_missing_u) missing = missing_u.floatval;
    else              missing = missing_v.floatval;
    for(i = 0; i < nt*nlat*nlon; i++) {
      sf[i] = vp[i] = missing;
    }
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpg: The input array cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlon * nlat;
  work = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */

  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(geomatv,GEOMATV)(&nlon,&nlat,&u[j],&v[j],work);
    j += nlat*nlon;
  }
  free(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 * dynamically allocate various temporary space *
 * Note the order "vhagc(...,v,u,....)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*nlat*(nlat+1)+2,2*nlat*(2*nlon*nt+3*l2 ));
  lvhagc = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+l2+15;
  ldwork = 2*nlat*(nlat+1)+1;

  br =     (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  bi =     (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  cr =     (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  ci =     (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  wvhagc = (float*)calloc(        lvhagc*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  dwork = (double*)calloc(  ldwork*sizeof(double),1);

  if( br == NULL || bi == NULL || cr == NULL || ci == NULL ||
      wvhagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(vhagci,VHAGCI)(&nlat,&nlon,wvhagc,&lvhagc,dwork,&ldwork,&jer);
  NGCALLF(vhagc,VHAGC)(&nlat,&nlon,&isym,&nt,&v[0],&u[0],
		       &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
		       wvhagc,&lvhagc,work,&lwork,&ker);
  free(wvhagc);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("uv2sfvpg","vhagc",&ier,&jer,&ker,&mer,10,5);

  l1     = min(nlat,(nlon+2)/2);
  lwork  = max(4*nlat*(nlat+2)+2,nlat*((nt*nlon+max(3*l2,nlon))+2*l1*nt+1));
  ldwork = nlat*(nlat+4);
  lshsgc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;

  wshsgc = (float*)calloc(lshsgc*sizeof(float),1);
  work   = (float*)calloc( lwork*sizeof(float),1);

  dwork = (double*)calloc(ldwork*sizeof(double),1);
  if( wshsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(shsgci,SHSGCI)(&nlat,&nlon,wshsgc,&lshsgc,dwork,&ldwork,&jer);
  NGCALLF(sfvpgc,SFVPGC)(&nlat,&nlon,&isym,&nt,&sf[0],&vp[0],
			 &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
			 wshsgc,&lshsgc,work,&lwork,&ker);

  free(br);
  free(bi);
  free(cr);
  free(ci);
  free(wshsgc);
  free(work);
  free(dwork);

  NGCALLF(chkerr,CHKERR)("uv2sfvpg","sfvpgc+shsgci",&ier,&jer,&ker,&mer,10,13);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
 
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&sf[j],work);
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&vp[j],work);
    NGCALLF(matgeov,MATGEOV)(&nlat,&nlon,&u[j],&v[j],work);
    j += nlat*nlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 6.37122e+6;        /* radius of earth */
  
  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&sf[0],&scale,&ner);
  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&vp[0],&scale,&ner);
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return
 */
  return(NhlNOERROR);
}


NhlErrorTypes lderuvf_W( void )
{
/*
 * Input array variables
 */
  float *u, *v;
  int ndims_u, dsizes_u[NCL_MAX_DIMENSIONS], nt, nlat, nlon;
  int ndims_v, dsizes_v[NCL_MAX_DIMENSIONS];
  NclScalar missing_u, missing_v;
  float missing;
  int has_missing_u, has_missing_v, found_missing=0;
/*
 * Output array variables
 */
  float *uy, *vy;
  int dsizes_uy[NCL_MAX_DIMENSIONS], dsizes_vy[NCL_MAX_DIMENSIONS];
  int ndims_uy, ndims_vy;
/*
 * various
 */
  int i, j, l, ityp, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  float scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhaec, lwvts;
  float *work, *wvhaec, *wvts, *br, *bi, *cr, *ci;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  u = (float*)NclGetArgValue(
           0,
           4,
           &ndims_u, 
           dsizes_u,
           &missing_u,
           &has_missing_u,
           NULL,
           2);
  v = (float*)NclGetArgValue(
           1,
           4,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           NULL,
           2);
/*
 * The grids coming in must be at least 2-dimensional and the same size.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvf: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_v[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvf: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  nlat = dsizes_u[ndims_u-2];
  nlon = dsizes_u[ndims_u-1];
/*
 * Compute the total number of elements in our array.
 */
  nt = 1;
  for(i = 0; i < ndims_u-2; i++) {
    nt *= dsizes_u[i];
  }
/*
 * Get output arrays.
 */
  uy = (float*)NclGetArgValue(
           2,
           4,
           &ndims_uy, 
           dsizes_uy,
           NULL,
           NULL,
           NULL,
           1);
  vy = (float*)NclGetArgValue(
           3,
           4,
           &ndims_vy, 
           dsizes_vy,
           NULL,
           NULL,
           NULL,
           1);
/*
 * The output arrays must also be at least 2-dimensional and the same sizes.
 */
  if( ndims_uy != ndims_u || ndims_vy != ndims_u ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvf: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_uy[i] != dsizes_u[i] || dsizes_vy[i] != dsizes_u[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvf: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Check for missing values.
 */
  if(has_missing_u || has_missing_v) {
    l = 0;
    while( l < nt*nlat*nlon && !found_missing ) {
      if(u[l]==missing_u.floatval || v[l]==missing_v.floatval) {
	found_missing = 1;
      }
      l++;
    }
  }
  if(found_missing) {
    if(has_missing_u) missing = missing_u.floatval;
    else              missing = missing_v.floatval;
    for(i = 0; i < nt*nlat*nlon; i++) {
      uy[i] = vy[i] = missing;
    }
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvf: The input array cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlon * nlat;
  work = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(geomatv,GEOMATV)(&nlon,&nlat,&u[j],&v[j],work);
    j += nlat*nlon;
  }
  free(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 * dynamically allocate various temporary space *
 * Note the order "vhaec(...,v,u,....)
 */
  ityp   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  /*  lwork  = max(4*(nlat+1),nlat*(2*nlon*nt+max(6*l2,nlon)));*/
  lwork = nlat*(2*nt*nlon+max(6*l2,nlon));
  ldwork = 2*(nlat+2);
  lwvts  = 4*nlat*l2 +3*max(l1-2,0)*(nlat+nlat-l1-1)+nlon +15;
  lvhaec = 4*nlat*l2+3*max(l1-2,0)*(nlat+nlat-l1-1)+nlon+15;

  br =     (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  bi =     (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  cr =     (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  ci =     (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  wvhaec = (float*)calloc(        lvhaec*sizeof(float),1);
  wvts =   (float*)calloc(         lwvts*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  dwork = (double*)calloc(        ldwork*sizeof(double),1);

  if( br == NULL || bi == NULL || cr == NULL || ci == NULL ||
      wvts == NULL || wvhaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(vhaeci,VHAECI)(&nlat,&nlon,wvhaec,&lvhaec,dwork,&ldwork,&jer);
  NGCALLF(vhaec,VHAEC)(&nlat,&nlon,&ityp,&nt,&v[0],&u[0],
		       &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
		       wvhaec,&lvhaec,work,&lwork,&ker);
  free(wvhaec);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("lderuvf","vhaec",&ier,&jer,&ker,&mer,9,5);
/*
 * compute derivative of (u,v) with respect to colatitude theta
 * [upon return: derivative of (u,v) with respect to latitude]
 */ 

  lwork = nlat*(2*nt*nlon+max(6*l2,nlon));
  work  = (float*)calloc(lwork*sizeof(float),1);
  ldwork = 2*(nlat+1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);

  if( work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(vtseci,VTSECI)(&nlat,&nlon,wvts,&lwvts,dwork,&ldwork,&jer);
  NGCALLF(vtsec,VTSEC)(&nlat,&nlon,&ityp,&nt,&vy[0],&uy[0],&idvw,&jdvw,
		       br,bi,cr,ci,&mdab,&ndab,wvts,&lwvts,
		       work,&lwork,&ker);

  free(br);
  free(bi);
  free(cr);
  free(ci);
  free(wvts);
  free(work);
  free(dwork);

  NGCALLF(chkerr,CHKERR)("lderuvf","vtsec",&ier,&jer,&ker,&mer,9,5);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
  lwork = nlon * nlat;
  work   = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeov,MATGEOV)(&nlat,&nlon,&u[j],&v[j],work);
    NGCALLF(matgeov,MATGEOV)(&nlat,&nlon,&uy[j],&vy[j],work);
    j += nlat*nlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 1./6.37122e+6;      /* 1/(radius of earth) */
  
  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&uy[0],&scale,&ner);
  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&vy[0],&scale,&ner);
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return
 */
  return(NhlNOERROR);
}


NhlErrorTypes lderuvg_W( void )
{
/*
 * Input array variables
 */
  float *u, *v;
  int ndims_u, dsizes_u[NCL_MAX_DIMENSIONS], nt, nlat, nlon;
  int ndims_v, dsizes_v[NCL_MAX_DIMENSIONS];
  NclScalar missing_u, missing_v;
  float missing;
  int has_missing_u, has_missing_v, found_missing=0;
/*
 * Output array variables
 */
  float *uy, *vy;
  int dsizes_uy[NCL_MAX_DIMENSIONS], dsizes_vy[NCL_MAX_DIMENSIONS];
  int ndims_uy, ndims_vy;
/*
 * various
 */
  int i, j, l, ityp, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  float scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhagc, lwvts;
  float *work, *wvhagc, *wvts, *br, *bi, *cr, *ci;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  u = (float*)NclGetArgValue(
           0,
           4,
           &ndims_u, 
           dsizes_u,
           &missing_u,
           &has_missing_u,
           NULL,
           2);
  v = (float*)NclGetArgValue(
           1,
           4,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           NULL,
           2);
/*
 * The grids coming in must be at least 2-dimensional and the same size.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvg: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_v[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvg: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  nlat = dsizes_u[ndims_u-2];
  nlon = dsizes_u[ndims_u-1];
/*
 * Compute the total number of elements in our array.
 */
  nt = 1;
  for(i = 0; i < ndims_u-2; i++) {
    nt *= dsizes_u[i];
  }
/*
 * Get output arrays.
 */
  uy = (float*)NclGetArgValue(
           2,
           4,
           &ndims_uy, 
           dsizes_uy,
           NULL,
           NULL,
           NULL,
           1);
  vy = (float*)NclGetArgValue(
           3,
           4,
           &ndims_vy, 
           dsizes_vy,
           NULL,
           NULL,
           NULL,
           1);
/*
 * The output arrays must also be at least 2-dimensional and the same sizes.
 */
  if( ndims_uy != ndims_u || ndims_vy != ndims_u ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvg: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_uy[i] != dsizes_u[i] || dsizes_vy[i] != dsizes_u[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvg: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Check for missing values.
 */
  if(has_missing_u || has_missing_v) {
    l = 0;
    while( l < nt*nlat*nlon && !found_missing ) {
      if(u[l]==missing_u.floatval || v[l]==missing_v.floatval) {
	found_missing = 1;
      }
      l++;
    }
  }
  if(found_missing) {
    if(has_missing_u) missing = missing_u.floatval;
    else              missing = missing_v.floatval;
    for(i = 0; i < nt*nlat*nlon; i++) {
      uy[i] = vy[i] = missing;
    }
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvg: The input array cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlon * nlat;
  work = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(geomatv,GEOMATV)(&nlon,&nlat,&u[j],&v[j],work);
    j += nlat*nlon;
  }
  free(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 * dynamically allocate various temporary space *
 * Note the order "vhagc(...,v,u,....)
 */
  ityp   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  /*  lwork  = max(4*(nlat+1),nlat*(2*nlon*nt+max(6*l2,nlon)));*/
  lwork = 2*nlat*(2*nlon*nt+3*l2);
  ldwork = 2*nlat*(nlat+1)+1;
  lvhagc = 4*nlat*l2+3*max(l1-2,0)*(nlat+nlat-l1-1)+nlon+l2+15;

  br =     (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  bi =     (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  cr =     (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  ci =     (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  wvhagc = (float*)calloc(        lvhagc*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  dwork = (double*)calloc(        ldwork*sizeof(double),1);

  if( br == NULL || bi == NULL || cr == NULL || ci == NULL ||
      wvhagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(vhagci,VHAGCI)(&nlat,&nlon,wvhagc,&lvhagc,dwork,&ldwork,&jer);
  NGCALLF(vhagc,VHAGC)(&nlat,&nlon,&ityp,&nt,&v[0],&u[0],
		       &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
		       wvhagc,&lvhagc,work,&lwork,&ker);
  free(wvhagc);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("lderuvg","vhagc",&ier,&jer,&ker,&mer,9,5);
/*
 * compute derivative of (u,v) with respect to colatitude theta
 * [upon return: derivative of (u,v) with respect to latitude]
 */ 

  lwvts  = 4*nlat*l2+3*max(l1-2,0)*(nlat+nlat-l1-1)+nlon+15;
  lwork  = nlat*(2*nt*nlon+max(6*l2,nlon));
  ldwork = nlat*(nlat+4);

  wvts   =  (float*)calloc( lwvts*sizeof(float),1);
  work   =  (float*)calloc( lwork*sizeof(float),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);

  if( wvts == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(vtsgci,VTSGCI)(&nlat,&nlon,wvts,&lwvts,dwork,&ldwork,&jer);
  NGCALLF(vtsgc,VTSGC)(&nlat,&nlon,&ityp,&nt,&vy[0],&uy[0],&idvw,&jdvw,
		       br,bi,cr,ci,&mdab,&ndab,wvts,&lwvts,
		       work,&lwork,&ker);

  free(br);
  free(bi);
  free(cr);
  free(ci);
  free(wvts);
  free(work);
  free(dwork);

  NGCALLF(chkerr,CHKERR)("lderuvg","vtsec",&ier,&jer,&ker,&mer,9,5);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
  lwork = nlon * nlat;
  work   = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeov,MATGEOV)(&nlat,&nlon,&u[j],&v[j],work);
    NGCALLF(matgeov,MATGEOV)(&nlat,&nlon,&uy[j],&vy[j],work);
    j += nlat*nlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 1./6.37122e+6;      /* 1/(radius of earth) */
  
  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&uy[0],&scale,&ner);
  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&vy[0],&scale,&ner);
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return
 */
  return(NhlNOERROR);
}


NhlErrorTypes uv2dvf_W( void ){

/*
 * Input array variables
 */
  float *u, *v;
  int dsizes_u[NCL_MAX_DIMENSIONS], dsizes_v[NCL_MAX_DIMENSIONS];
  int ndims_u, ndims_v;
  NclScalar missing_u, missing_v;
  float missing;
  int has_missing_u, has_missing_v, found_missing=0;
  int nt, nlat, nlon;
/*
 * Output array variables
 */
  float *dv;
  int ndims_dv, dsizes_dv[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  float scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhaec, lshsec;
  float *work, *wvhaec, *wshsec, *br, *bi, *cr, *ci;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  u = (float*)NclGetArgValue(
           0,
           3,
           &ndims_u, 
           dsizes_u,
           &missing_u,
           &has_missing_u,
           NULL,
           2);
  v = (float*)NclGetArgValue(
           1,
           3,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           NULL,
           2);
/*
 * The grids coming in must be at least 2-dimensional.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvf: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_v[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvf: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  nlat = dsizes_u[ndims_u-2];
  nlon = dsizes_u[ndims_u-1];
/*
 * Compute the total number of elements in our array.
 */
  nt = 1;
  for(i = 0; i < ndims_u-2; i++) {
    nt *= dsizes_u[i];
  }
/*
 * Get output array.
 */
  dv = (float*)NclGetArgValue(
           2,
           3,
           &ndims_dv, 
           dsizes_dv,
           NULL,
           NULL,
           NULL,
           1);
/*
 * The output array must also be at least 2-dimensional.
 */
  if( ndims_dv != ndims_u  ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvf: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_dv[i] != dsizes_u[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvf: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Check for missing values.
 */
  if(has_missing_u || has_missing_v) {
    l = 0;
    while( l < nt*nlat*nlon && !found_missing ) {
      if(u[l] == missing_u.floatval || v[l] == missing_v.floatval) {
	found_missing = 1;
      }
      l++;
    }
  }
  if(found_missing) {
    if(has_missing_u) missing = missing_u.floatval;
    else              missing = missing_v.floatval;
    for(i = 0; i < nt*nlat*nlon; i++) {
      dv[i] = missing;
    }
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvf: The input arrays cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlon * nlat;
  work = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(geomatv,GEOMATV)(&nlon,&nlat,&u[j],&v[j],work);
    j += nlat*nlon;
  }
  free(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 * dynamically allocate various temporary space.
 * Note the order "vhaec(...,v,u,....)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*(nlat+1),nlat*(2*nlon*nt+max(6*l2,nlon)));
  ldwork = 2*(nlat+2);
  lvhaec = 4*nlat*l2+3*max(l1-2,0)*(nlat+nlat-l1-1)+nlon+15;

  br =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  bi =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  cr =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  ci =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  wvhaec = (float*)calloc(        lvhaec*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  dwork = (double*)calloc(        ldwork*sizeof(double),1);

  if( br == NULL || bi == NULL || cr == NULL || ci == NULL ||
      wvhaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(vhaeci,VHAECI)(&nlat,&nlon,wvhaec,&lvhaec,dwork,&ldwork,&jer);
  NGCALLF(vhaec,VHAEC)(&nlat,&nlon,&isym,&nt,&v[0],&u[0],
		       &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
		       wvhaec,&lvhaec,work,&lwork,&ker);
  free(cr);
  free(ci);
  free(wvhaec);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("uv2dvf","vhaec",&ier,&jer,&ker,&mer,8,5);
/*
 * compute the divergence using the vector spherical harmonic 
 * coefficients br and bi computed by 'sub vhaec'
 */
  lshsec = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;
  lwork  = nlat*(nt*nlon+max(3*l2,nlon)+2*nt*l1+1);
  ldwork = nlat+1;

  work   = (float*)calloc( lwork*sizeof(float),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);
  wshsec = (float*)calloc(lshsec*sizeof(float),1);

  if( wshsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(shseci,SHSECI)(&nlat,&nlon,wshsec,&lshsec,dwork,&ldwork,&jer);
  NGCALLF(divec,DIVEC)(&nlat,&nlon,&isym,&nt,&dv[0],&idvw,&jdvw,br,bi,
		       &mdab,&ndab,wshsec,&lshsec,work,&lwork,&ker);

  NGCALLF(chkerr,CHKERR)("uv2dvf","shseci+divec",&ier,&jer,&ker,&mer,8,12);

  free(br);
  free(bi);
  free(wshsec);
  free(work);
  free(dwork);
/* 
 * transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
 
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&dv[j],work);
    NGCALLF(matgeov,MATGEOV)(&nlat,&nlon,&u[j],&v[j],work);
    j += nlat*nlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 1./6.37122e+6;       /* 1/(radius of earth) */
  
  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&dv[0],&scale,&ner);
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return array.
 */
  return(NhlNOERROR);
}


NhlErrorTypes uv2dvF_W( void ){

/*
 * Input array variables
 */
  float *u, *v;
  int dsizes_u[NCL_MAX_DIMENSIONS], dsizes_v[NCL_MAX_DIMENSIONS];
  int ndims_u, ndims_v;
  NclScalar missing_u, missing_v;
  float missing;
  int has_missing_u, has_missing_v, found_missing=0;
  int nt, nlat, nlon;
/*
 * Output array variables
 */
  float *dv;
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  float scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhaec, lshsec;
  float *work, *wvhaec, *wshsec, *br, *bi, *cr, *ci;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  u = (float*)NclGetArgValue(
           0,
           2,
           &ndims_u, 
           dsizes_u,
           &missing_u,
           &has_missing_u,
           NULL,
           2);
  v = (float*)NclGetArgValue(
           1,
           2,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           NULL,
           2);
/*
 * The grids coming in must be at least 2-dimensional.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvF: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_v[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvF: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  nlat = dsizes_u[ndims_u-2];
  nlon = dsizes_u[ndims_u-1];
/*
 * Compute the total number of elements in our array.
 */
  nt = 1;
  for(i = 0; i < ndims_u-2; i++) {
    nt *= dsizes_u[i];
  }
/*
 * Allocate space for output array.
 */
  dv = (float*)calloc(nt*nlat*nlon*sizeof(float),1);
  if( dv == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvF: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Check for missing values.
 */
  if(has_missing_u || has_missing_v) {
    l = 0;
    while( l < nt*nlat*nlon && !found_missing ) {
      if(u[l] == missing_u.floatval || v[l] == missing_v.floatval) {
	found_missing = 1;
      }
      l++;
    }
  }
  if(found_missing) {
    if(has_missing_u) missing = missing_u.floatval;
    else              missing = missing_v.floatval;
    for(i = 0; i < nt*nlat*nlon; i++) {
      dv[i] = missing;
    }
    NhlPError(NhlWARNING,NhlEUNKNOWN,"uv2dvF: The input arrays cannot contain any missing values");
    return(NclReturnValue((void*)dv,ndims_u,dsizes_u,NULL,NCL_float,0));
  }
/*
 * Determine the workspace size.
 */
  lwork = nlon * nlat;
  work = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvF: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(geomatv,GEOMATV)(&nlon,&nlat,&u[j],&v[j],work);
    j += nlat*nlon;
  }
  free(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 * dynamically allocate various temporary space.
 * Note the order "vhaec(...,v,u,....)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*(nlat+1),nlat*(2*nlon*nt+max(6*l2,nlon)));
  ldwork = 2*(nlat+2);
  lvhaec = 4*nlat*l2+3*max(l1-2,0)*(nlat+nlat-l1-1)+nlon+15;

  br =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  bi =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  cr =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  ci =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  wvhaec = (float*)calloc(        lvhaec*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  dwork = (double*)calloc(        ldwork*sizeof(double),1);

  if( br == NULL || bi == NULL || cr == NULL || ci == NULL ||
      wvhaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvF: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(vhaeci,VHAECI)(&nlat,&nlon,wvhaec,&lvhaec,dwork,&ldwork,&jer);
  NGCALLF(vhaec,VHAEC)(&nlat,&nlon,&isym,&nt,&v[0],&u[0],
		       &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
		       wvhaec,&lvhaec,work,&lwork,&ker);
  free(cr);
  free(ci);
  free(wvhaec);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("uv2dvF","vhaec",&ier,&jer,&ker,&mer,8,5);
/*
 * compute the divergence using the vector spherical harmonic 
 * coefficients br and bi computed by 'sub vhaec'
 */
  lshsec = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;
  lwork  = nlat*(nt*nlon+max(3*l2,nlon)+2*nt*l1+1);
  ldwork = nlat+1;

  work   = (float*)calloc( lwork*sizeof(float),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);
  wshsec = (float*)calloc(lshsec*sizeof(float),1);

  if( wshsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvF: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(shseci,SHSECI)(&nlat,&nlon,wshsec,&lshsec,dwork,&ldwork,&jer);
  NGCALLF(divec,DIVEC)(&nlat,&nlon,&isym,&nt,&dv[0],&idvw,&jdvw,br,bi,
		       &mdab,&ndab,wshsec,&lshsec,work,&lwork,&ker);

  NGCALLF(chkerr,CHKERR)("uv2dvF","shseci+divec",&ier,&jer,&ker,&mer,8,12);

  free(br);
  free(bi);
  free(wshsec);
  free(work);
  free(dwork);
/* 
 * transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
 
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvF: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&dv[j],work);
    NGCALLF(matgeov,MATGEOV)(&nlat,&nlon,&u[j],&v[j],work);
    j += nlat*nlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 1./6.37122e+6;       /* 1/(radius of earth) */
  
  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&dv[0],&scale,&ner);
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return array.
 */
  return(NclReturnValue((void*)dv,ndims_u,dsizes_u,NULL,NCL_float,0));
}


NhlErrorTypes uv2dvg_W( void ){

/*
 * Input array variables
 */
  float *u, *v;
  int dsizes_u[NCL_MAX_DIMENSIONS], dsizes_v[NCL_MAX_DIMENSIONS];
  int ndims_u, ndims_v;
  NclScalar missing_u, missing_v;
  float missing;
  int has_missing_u, has_missing_v, found_missing=0;
  int nt, nlat, nlon;
/*
 * Output array variables
 */
  float *dv;
  int ndims_dv, dsizes_dv[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  float scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhagc, lshsgc;
  float *work, *wvhagc, *wshsgc, *br, *bi, *cr, *ci;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  u = (float*)NclGetArgValue(
           0,
           3,
           &ndims_u, 
           dsizes_u,
           &missing_u,
           &has_missing_u,
           NULL,
           2);
  v = (float*)NclGetArgValue(
           1,
           3,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           NULL,
           2);
/*
 * The grids coming in must be at least 2-dimensional.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvg: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_v[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvg: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  nlat = dsizes_u[ndims_u-2];
  nlon = dsizes_u[ndims_u-1];
/*
 * Compute the total number of elements in our array.
 */
  nt = 1;
  for(i = 0; i < ndims_u-2; i++) {
    nt *= dsizes_u[i];
  }
/*
 * Get output array.
 */
  dv = (float*)NclGetArgValue(
           2,
           3,
           &ndims_dv, 
           dsizes_dv,
           NULL,
           NULL,
           NULL,
           1);
/*
 * The output array must also be at least 2-dimensional.
 */
  if( ndims_dv != ndims_u  ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvg: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_dv[i] != dsizes_u[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvg: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Check for missing values.
 */
  if(has_missing_u || has_missing_v) {
    l = 0;
    while( l < nt*nlat*nlon && !found_missing ) {
      if(u[l] == missing_u.floatval || v[l] == missing_v.floatval) {
	found_missing = 1;
      }
      l++;
    }
  }
  if(found_missing) {
    if(has_missing_u) missing = missing_u.floatval;
    else              missing = missing_v.floatval;
    for(i = 0; i < nt*nlat*nlon; i++) {
      dv[i] = missing;
    }
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvg: The input arrays cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlon * nlat;
  work = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(geomatv,GEOMATV)(&nlon,&nlat,&u[j],&v[j],work);
    j += nlat*nlon;
  }
  free(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 * dynamically allocate various temporary space.
 * Note the order "vhagc(...,v,u,....)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*nlat*(nlat+1)+2,2*nlat*(2*nlon*nt+3*l2));
  lvhagc = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+l2+15;
  ldwork = 2*nlat*(nlat+1)+1;

  br =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  bi =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  cr =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  ci =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  wvhagc = (float*)calloc(        lvhagc*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  dwork = (double*)calloc(  ldwork*sizeof(double),1);

  if( br == NULL || bi == NULL || cr == NULL || ci == NULL ||
      wvhagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(vhagci,VHAGCI)(&nlat,&nlon,wvhagc,&lvhagc,dwork,&ldwork,&jer);
  NGCALLF(vhagc,VHAGC)(&nlat,&nlon,&isym,&nt,&v[0],&u[0],
		       &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
		       wvhagc,&lvhagc,work,&lwork,&ker);
  free(cr);
  free(ci);
  free(wvhagc);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("uv2dvg","vhagc",&ier,&jer,&ker,&mer,8,5);
/*
 * compute the divergence using the vector spherical harmonic 
 * coefficients br and bi computed by 'sub vhagc'
 */
  lwork  = max(4*nlat*(nlat+2)+2,nlat*(nlon*nt+max(3*l2,nlon)+2*nt*l1+1));
  ldwork = nlat*(nlat+4);
  lshsgc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;

  work   = (float*)calloc( lwork*sizeof(float),1);
  wshsgc = (float*)calloc(lshsgc*sizeof(float),1);

  dwork = (double*)calloc(ldwork*sizeof(double),1);
  if( wshsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(shsgci,SHSGCI)(&nlat,&nlon,wshsgc,&lshsgc,dwork,&ldwork,&jer);
  NGCALLF(divgc,DIVGC)(&nlat,&nlon,&isym,&nt,&dv[0],&idvw,&jdvw,br,bi,
		       &mdab,&ndab,wshsgc,&lshsgc,work,&lwork,&ker);

  NGCALLF(chkerr,CHKERR)("uv2dvg","shsgci+divgc",&ier,&jer,&ker,&mer,8,12);

  free(br);
  free(bi);
  free(wshsgc);
  free(work);
  free(dwork);
/* 
 * transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
 
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&dv[j],work);
    NGCALLF(matgeov,MATGEOV)(&nlat,&nlon,&u[j],&v[j],work);
    j += nlat*nlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 1./6.37122e+6;       /* 1/(radius of earth) */
  
  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&dv[0],&scale,&ner);
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return array.
 */
  return(NhlNOERROR);
}


NhlErrorTypes uv2dvG_W( void ){

/*
 * Input array variables
 */
  float *u, *v;
  int dsizes_u[NCL_MAX_DIMENSIONS], dsizes_v[NCL_MAX_DIMENSIONS];
  int ndims_u, ndims_v;
  NclScalar missing_u, missing_v;
  float missing;
  int has_missing_u, has_missing_v, found_missing=0;
  int nt, nlat, nlon;
/*
 * Output array variables
 */
  float *dv;
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  float scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhagc, lshsgc;
  float *work, *wvhagc, *wshsgc, *br, *bi, *cr, *ci;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  u = (float*)NclGetArgValue(
           0,
           2,
           &ndims_u, 
           dsizes_u,
           &missing_u,
           &has_missing_u,
           NULL,
           2);
  v = (float*)NclGetArgValue(
           1,
           2,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           NULL,
           2);
/*
 * The grids coming in must be at least 2-dimensional.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvG: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_v[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvG: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  nlat = dsizes_u[ndims_u-2];
  nlon = dsizes_u[ndims_u-1];
/*
 * Compute the total number of elements in our array.
 */
  nt = 1;
  for(i = 0; i < ndims_u-2; i++) {
    nt *= dsizes_u[i];
  }
/*
 * Allocate space for output array.
 */
  dv = (float*)calloc(nt*nlat*nlon*sizeof(float),1);
  if( dv == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvG: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Check for missing values.
 */
  if(has_missing_u || has_missing_v) {
    l = 0;
    while( l < nt*nlat*nlon && !found_missing ) {
      if(u[l] == missing_u.floatval || v[l] == missing_v.floatval) {
	found_missing = 1;
      }
      l++;
    }
  }
  if(found_missing) {
    if(has_missing_u) missing = missing_u.floatval;
    else              missing = missing_v.floatval;
    for(i = 0; i < nt*nlat*nlon; i++) {
      dv[i] = missing;
    }
    NhlPError(NhlWARNING,NhlEUNKNOWN,"uv2dvG: The input arrays cannot contain any missing values");
    return(NclReturnValue((void*)dv,ndims_u,dsizes_u,NULL,NCL_float,0));
  }
/*
 * Determine the workspace size.
 */
  lwork = nlon * nlat;
  work = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvG: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(geomatv,GEOMATV)(&nlon,&nlat,&u[j],&v[j],work);
    j += nlat*nlon;
  }
  free(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 * dynamically allocate various temporary space.
 * Note the order "vhagc(...,v,u,....)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*nlat*(nlat+1)+2,2*nlat*(2*nlon*nt+3*l2));
  lvhagc = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+l2+15;
  ldwork = 2*nlat*(nlat+1)+1;

  br =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  bi =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  cr =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  ci =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  wvhagc = (float*)calloc(        lvhagc*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  dwork = (double*)calloc(  ldwork*sizeof(double),1);

  if( br == NULL || bi == NULL || cr == NULL || ci == NULL ||
      wvhagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvG: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(vhagci,VHAGCI)(&nlat,&nlon,wvhagc,&lvhagc,dwork,&ldwork,&jer);
  NGCALLF(vhagc,VHAGC)(&nlat,&nlon,&isym,&nt,&v[0],&u[0],
		       &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
		       wvhagc,&lvhagc,work,&lwork,&ker);
  free(cr);
  free(ci);
  free(wvhagc);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("uv2dvG","vhagc",&ier,&jer,&ker,&mer,8,5);
/*
 * compute the divergence using the vector spherical harmonic 
 * coefficients br and bi computed by 'sub vhagc'
 */
  lwork  = max(4*nlat*(nlat+2)+2,nlat*(nlon*nt+max(3*l2,nlon)+2*nt*l1+1));
  ldwork = nlat*(nlat+4);
  lshsgc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;

  work   = (float*)calloc( lwork*sizeof(float),1);
  wshsgc = (float*)calloc(lshsgc*sizeof(float),1);

  dwork = (double*)calloc(ldwork*sizeof(double),1);
  if( wshsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvG: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(shsgci,SHSGCI)(&nlat,&nlon,wshsgc,&lshsgc,dwork,&ldwork,&jer);
  NGCALLF(divgc,DIVGC)(&nlat,&nlon,&isym,&nt,&dv[0],&idvw,&jdvw,br,bi,
		       &mdab,&ndab,wshsgc,&lshsgc,work,&lwork,&ker);

  NGCALLF(chkerr,CHKERR)("uv2dvG","shsgci+divgc",&ier,&jer,&ker,&mer,8,12);

  free(br);
  free(bi);
  free(wshsgc);
  free(work);
  free(dwork);
/* 
 * transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
 
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvG: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&dv[j],work);
    NGCALLF(matgeov,MATGEOV)(&nlat,&nlon,&u[j],&v[j],work);
    j += nlat*nlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 1./6.37122e+6;       /* 1/(radius of earth) */
  
  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&dv[0],&scale,&ner);
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return array.
 */
  return(NclReturnValue((void*)dv,ndims_u,dsizes_u,NULL,NCL_float,0));
}



NhlErrorTypes uv2vrf_W( void ){

/*
 * Input array variables
 */
  float *u, *v;
  int dsizes_u[NCL_MAX_DIMENSIONS], dsizes_v[NCL_MAX_DIMENSIONS];
  int ndims_u, ndims_v;
  NclScalar missing_u, missing_v;
  float missing;
  int has_missing_u, has_missing_v, found_missing=0;
  int nt, nlat, nlon;
/*
 * Output array variables
 */
  float *vort;
  int ndims_vort, dsizes_vort[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  float scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhaec, lshsec;
  float *work, *wvhaec, *wshsec, *br, *bi, *cr, *ci;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  u = (float*)NclGetArgValue(
           0,
           3,
           &ndims_u, 
           dsizes_u,
           &missing_u,
           &has_missing_u,
           NULL,
           2);
  v = (float*)NclGetArgValue(
           1,
           3,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           NULL,
           2);
/*
 * The grids coming in must be at least 2-dimensional.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrf: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_v[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrf: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  nlat = dsizes_u[ndims_u-2];
  nlon = dsizes_u[ndims_u-1];
/*
 * Compute the total number of elements in our array.
 */
  nt = 1;
  for(i = 0; i < ndims_u-2; i++) {
    nt *= dsizes_u[i];
  }
/*
 * Get output array.
 */
  vort = (float*)NclGetArgValue(
           2,
           3,
           &ndims_vort, 
           dsizes_vort,
           NULL,
           NULL,
           NULL,
           1);
/*
 * The output array must also be at least 2-dimensional.
 */
  if( ndims_vort != ndims_u  ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrf: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_vort[i] != dsizes_u[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrf: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Check for missing values.
 */
  if(has_missing_u || has_missing_v) {
    l = 0;
    while( l < nt*nlat*nlon && !found_missing ) {
      if(u[l] == missing_u.floatval || v[l] == missing_v.floatval) {
	found_missing = 1;
      }
      l++;
    }
  }
  if(found_missing) {
    if(has_missing_u) missing = missing_u.floatval;
    else              missing = missing_v.floatval;
    for(i = 0; i < nt*nlat*nlon; i++) {
      vort[i] = missing;
    }
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrf: The input arrays cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlon * nlat;
  work = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(geomatv,GEOMATV)(&nlon,&nlat,&u[j],&v[j],work);
    j += nlat*nlon;
  }
  free(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 * dynamically allocate various temporary space.
 * Note the order "vhaec(...,v,u,....)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*(nlat+1),nlat*(2*nt*nlon+max(6*l2,nlon)));
  ldwork = 2*(nlat+2);
  lvhaec = 4*nlat*l2+3*max(l1-2,0)*(nlat+nlat-l1-1)+nlon+15;

  br =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  bi =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  cr =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  ci =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  wvhaec = (float*)calloc(        lvhaec*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  dwork = (double*)calloc(        ldwork*sizeof(double),1);

  if( br == NULL || bi == NULL || cr == NULL || ci == NULL ||
      wvhaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(vhaeci,VHAECI)(&nlat,&nlon,wvhaec,&lvhaec,dwork,&ldwork,&jer);
  NGCALLF(vhaec,VHAEC)(&nlat,&nlon,&isym,&nt,&v[0],&u[0],
		       &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
		       wvhaec,&lvhaec,work,&lwork,&ker);
  free(br);
  free(bi);
  free(wvhaec);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("uv2vrf","vhaec",&ier,&jer,&ker,&mer,8,5);
/*
 * compute the vorticity using the vector spherical harmonic 
 * coefficients br and bi computed by 'sub vhaec'
 */

  l1     = min(nlat,(nlon+2)/2);
  /*  lwork  = max(nlat+1,nlat*((nt+1)*nlon+2*nt*l1+1)); */
  lwork = nlat*(nt*nlon+max(3*l2,nlon)+2*nt*l1+1);
  lshsec = 2*nlat*l2+3*(max(l1-2,0)*(2*nlat-l1-1))/2+nlon+15;
  ldwork = nlat+1;

  work   = (float*)calloc( lwork*sizeof(float),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);
  wshsec = (float*)calloc(lshsec*sizeof(float),1);

  if( wshsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(shseci,SHSECI)(&nlat,&nlon,wshsec,&lshsec,dwork,&ldwork,&jer);
  NGCALLF(vrtec,VRTEC)(&nlat,&nlon,&isym,&nt,&vort[0],&idvw,&jdvw,cr,ci,
		       &mdab,&ndab,wshsec,&lshsec,work,&lwork,&ker);

  NGCALLF(chkerr,CHKERR)("uv2vrf","shseci+vrtec",&ier,&jer,&ker,&mer,8,12);

  free(cr);
  free(ci);
  free(wshsec);
  free(work);
  free(dwork);
/* 
 * transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
 
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&vort[j],work);
    NGCALLF(matgeov,MATGEOV)(&nlat,&nlon,&u[j],&v[j],work);
    j += nlat*nlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 1./6.37122e+6;       /* 1/(radius of earth) */
  
  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&vort[0],&scale,&ner);
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return array.
 */
  return(NhlNOERROR);
}


NhlErrorTypes uv2vrF_W( void ){

/*
 * Input array variables
 */
  float *u, *v;
  int dsizes_u[NCL_MAX_DIMENSIONS], dsizes_v[NCL_MAX_DIMENSIONS];
  int ndims_u, ndims_v;
  NclScalar missing_u, missing_v;
  float missing;
  int has_missing_u, has_missing_v, found_missing=0;
  int nt, nlat, nlon;
/*
 * Output array variables
 */
  float *vort;
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  float scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhaec, lshsec;
  float *work, *wvhaec, *wshsec, *br, *bi, *cr, *ci;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  u = (float*)NclGetArgValue(
           0,
           2,
           &ndims_u, 
           dsizes_u,
           &missing_u,
           &has_missing_u,
           NULL,
           2);
  v = (float*)NclGetArgValue(
           1,
           2,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           NULL,
           2);
/*
 * The grids coming in must be at least 2-dimensional.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrF: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_v[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrF: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  nlat = dsizes_u[ndims_u-2];
  nlon = dsizes_u[ndims_u-1];
/*
 * Compute the total number of elements in our array.
 */
  nt = 1;
  for(i = 0; i < ndims_u-2; i++) {
    nt *= dsizes_u[i];
  }
/*
 * Allocate space for output array.
 */
  vort = (float*)calloc(nt*nlat*nlon*sizeof(float),1);
  if( vort == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrF: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Check for missing values.
 */
  if(has_missing_u || has_missing_v) {
    l = 0;
    while( l < nt*nlat*nlon && !found_missing ) {
      if(u[l] == missing_u.floatval || v[l] == missing_v.floatval) {
	found_missing = 1;
      }
      l++;
    }
  }
  if(found_missing) {
    if(has_missing_u) missing = missing_u.floatval;
    else              missing = missing_v.floatval;
    for(i = 0; i < nt*nlat*nlon; i++) {
      vort[i] = missing;
    }
    NhlPError(NhlWARNING,NhlEUNKNOWN,"uv2vrF: The input arrays cannot contain any missing values");
    return(NclReturnValue((void*)vort,ndims_u,dsizes_u,NULL,NCL_float,0));
  }
/*
 * Determine the workspace size.
 */
  lwork = nlon * nlat;
  work = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrF: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(geomatv,GEOMATV)(&nlon,&nlat,&u[j],&v[j],work);
    j += nlat*nlon;
  }
  free(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 * dynamically allocate various temporary space.
 * Note the order "vhaec(...,v,u,....)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*(nlat+1),nlat*(2*nt*nlon+max(6*l2,nlon)));
  ldwork = 2*(nlat+2);
  lvhaec = 4*nlat*l2+3*max(l1-2,0)*(nlat+nlat-l1-1)+nlon+15;

  br =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  bi =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  cr =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  ci =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  wvhaec = (float*)calloc(        lvhaec*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  dwork = (double*)calloc(        ldwork*sizeof(double),1);

  if( br == NULL || bi == NULL || cr == NULL || ci == NULL ||
      wvhaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrF: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(vhaeci,VHAECI)(&nlat,&nlon,wvhaec,&lvhaec,dwork,&ldwork,&jer);
  NGCALLF(vhaec,VHAEC)(&nlat,&nlon,&isym,&nt,&v[0],&u[0],
		       &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
		       wvhaec,&lvhaec,work,&lwork,&ker);
  free(br);
  free(bi);
  free(wvhaec);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("uv2vrF","vhaec",&ier,&jer,&ker,&mer,8,5);
/*
 * compute the vorticity using the vector spherical harmonic 
 * coefficients br and bi computed by 'sub vhaec'
 */

  l1     = min(nlat,(nlon+2)/2);
  /*  lwork  = max(nlat+1,nlat*((nt+1)*nlon+2*nt*l1+1)); */
  lwork = nlat*(nt*nlon+max(3*l2,nlon)+2*nt*l1+1);
  ldwork = nlat+1;
  lshsec = 2*nlat*l2+3*(max(l1-2,0)*(2*nlat-l1-1))/2+nlon+15;

  work   = (float*)calloc( lwork*sizeof(float),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);
  wshsec = (float*)calloc(lshsec*sizeof(float),1);

  if( wshsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrF: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(shseci,SHSECI)(&nlat,&nlon,wshsec,&lshsec,dwork,&ldwork,&jer);
  NGCALLF(vrtec,VRTEC)(&nlat,&nlon,&isym,&nt,&vort[0],&idvw,&jdvw,cr,ci,
		       &mdab,&ndab,wshsec,&lshsec,work,&lwork,&ker);

  NGCALLF(chkerr,CHKERR)("uv2vrF","shseci+vrtec",&ier,&jer,&ker,&mer,8,12);

  free(cr);
  free(ci);
  free(wshsec);
  free(work);
  free(dwork);
/* 
 * transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
 
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrF: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&vort[j],work);
    NGCALLF(matgeov,MATGEOV)(&nlat,&nlon,&u[j],&v[j],work);
    j += nlat*nlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 1./6.37122e+6;       /* 1/(radius of earth) */
  
  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&vort[0],&scale,&ner);
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return array.
 */
  return(NclReturnValue((void*)vort,ndims_u,dsizes_u,NULL,NCL_float,0));
}


NhlErrorTypes uv2vrg_W( void ){

/*
 * Input array variables
 */
  float *u, *v;
  int dsizes_u[NCL_MAX_DIMENSIONS], dsizes_v[NCL_MAX_DIMENSIONS];
  int ndims_u, ndims_v;
  NclScalar missing_u, missing_v;
  float missing;
  int has_missing_u, has_missing_v, found_missing=0;
  int nt, nlat, nlon;
/*
 * Output array variables
 */
  float *vort;
  int ndims_vort, dsizes_vort[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  float scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhagc, lshsgc;
  float *work, *wvhagc, *wshsgc, *br, *bi, *cr, *ci;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  u = (float*)NclGetArgValue(
           0,
           3,
           &ndims_u, 
           dsizes_u,
           &missing_u,
           &has_missing_u,
           NULL,
           2);
  v = (float*)NclGetArgValue(
           1,
           3,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           NULL,
           2);
/*
 * The grids coming in must be at least 2-dimensional.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrF: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_v[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrF: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  nlat = dsizes_u[ndims_u-2];
  nlon = dsizes_u[ndims_u-1];
/*
 * Compute the total number of elements in our array.
 */
  nt = 1;
  for(i = 0; i < ndims_u-2; i++) {
    nt *= dsizes_u[i];
  }
/*
 * Get output array.
 */
  vort = (float*)NclGetArgValue(
           2,
           3,
           &ndims_vort, 
           dsizes_vort,
           NULL,
           NULL,
           NULL,
           1);
/*
 * The output array must also be at least 2-dimensional.
 */
  if( ndims_vort != ndims_u  ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrg: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_vort[i] != dsizes_u[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrg: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Check for missing values.
 */
  if(has_missing_u || has_missing_v) {
    l = 0;
    while( l < nt*nlat*nlon && !found_missing ) {
      if(u[l] == missing_u.floatval || v[l] == missing_v.floatval) {
	found_missing = 1;
      }
      l++;
    }
  }
  if(found_missing) {
    if(has_missing_u) missing = missing_u.floatval;
    else              missing = missing_v.floatval;
    for(i = 0; i < nt*nlat*nlon; i++) {
      vort[i] = missing;
    }
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrg: The input arrays cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlon * nlat;
  work = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(geomatv,GEOMATV)(&nlon,&nlat,&u[j],&v[j],work);
    j += nlat*nlon;
  }
  free(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 * dynamically allocate various temporary space.
 * Note the order "vhagc(...,v,u,....)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  /*  lwork  = max(4*nlat*(nlat+1)+2,2*nlat*(2*nlon*nt+3*l2)); */
  lwork = 2*nlat*(2*nlon*nt+3*l2);
  lvhagc = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+l2+15;
  ldwork = 2*nlat*(nlat+1)+1;

  br =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  bi =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  cr =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  ci =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  wvhagc = (float*)calloc(        lvhagc*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  dwork = (double*)calloc(  ldwork*sizeof(double),1);

  if( br == NULL || bi == NULL || cr == NULL || ci == NULL ||
      wvhagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(vhagci,VHAGCI)(&nlat,&nlon,wvhagc,&lvhagc,dwork,&ldwork,&jer);
  NGCALLF(vhagc,VHAGC)(&nlat,&nlon,&isym,&nt,&v[0],&u[0],
		       &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
		       wvhagc,&lvhagc,work,&lwork,&ker);
  free(br);
  free(bi);
  free(wvhagc);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("uv2vrg","vhagc",&ier,&jer,&ker,&mer,8,5);
/*
 * compute the vorticity using the vector spherical harmonic 
 * coefficients br and bi computed by 'sub vhagc'
 */
  lshsgc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;
  /*  lwork  = max(4*nlat*(nlat+2)+2,nlat*((nt+1)*nlon+2*nt*l1+1)); */
  lwork = nlat*(nlon*nt+max(3*l2,nlon)+2*nt*l1+1);
  ldwork = nlat*(nlat+4);

  work   = (float*)calloc( lwork*sizeof(float),1);
  wshsgc = (float*)calloc(lshsgc*sizeof(float),1);

  dwork = (double*)calloc(ldwork*sizeof(double),1);
  if( wshsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(shsgci,SHSGCI)(&nlat,&nlon,wshsgc,&lshsgc,dwork,&ldwork,&jer);
  NGCALLF(vrtgc,VRTGC)(&nlat,&nlon,&isym,&nt,&vort[0],&idvw,&jdvw,cr,ci,
		       &mdab,&ndab,wshsgc,&lshsgc,work,&lwork,&ker);

  NGCALLF(chkerr,CHKERR)("uv2vrg","shsgci+vrtgc",&ier,&jer,&ker,&mer,8,12);

  free(cr);
  free(ci);
  free(wshsgc);
  free(work);
  free(dwork);
/* 
 * transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
 
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&vort[j],work);
    NGCALLF(matgeov,MATGEOV)(&nlat,&nlon,&u[j],&v[j],work);
    j += nlat*nlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 1./6.37122e+6;       /* 1/(radius of earth) */
  
  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&vort[0],&scale,&ner);
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return array.
 */
  return(NhlNOERROR);
}


NhlErrorTypes uv2vrG_W( void ){

/*
 * Input array variables
 */
  float *u, *v;
  int dsizes_u[NCL_MAX_DIMENSIONS], dsizes_v[NCL_MAX_DIMENSIONS];
  int ndims_u, ndims_v;
  NclScalar missing_u, missing_v;
  float missing;
  int has_missing_u, has_missing_v, found_missing=0;
  int nt, nlat, nlon;
/*
 * Output array variables
 */
  float *vort;
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  float scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhagc, lshsgc;
  float *work, *wvhagc, *wshsgc, *br, *bi, *cr, *ci;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  u = (float*)NclGetArgValue(
           0,
           2,
           &ndims_u, 
           dsizes_u,
           &missing_u,
           &has_missing_u,
           NULL,
           2);
  v = (float*)NclGetArgValue(
           1,
           2,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           NULL,
           2);
/*
 * The grids coming in must be at least 2-dimensional.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrF: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_v[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrF: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  nlat = dsizes_u[ndims_u-2];
  nlon = dsizes_u[ndims_u-1];
/*
 * Compute the total number of elements in our array.
 */
  nt = 1;
  for(i = 0; i < ndims_u-2; i++) {
    nt *= dsizes_u[i];
  }
/*
 * Allocate space for output array.
 */
  vort = (float*)calloc(nt*nlat*nlon*sizeof(float),1);
  if( vort == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrG: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Check for missing values.
 */
  if(has_missing_u || has_missing_v) {
    l = 0;
    while( l < nt*nlat*nlon && !found_missing ) {
      if(u[l] == missing_u.floatval || v[l] == missing_v.floatval) {
	found_missing = 1;
      }
      l++;
    }
  }
  if(found_missing) {
    if(has_missing_u) missing = missing_u.floatval;
    else              missing = missing_v.floatval;
    for(i = 0; i < nt*nlat*nlon; i++) {
      vort[i] = missing;
    }
    NhlPError(NhlWARNING,NhlEUNKNOWN,"uv2vrG: The input arrays cannot contain any missing values");
    return(NclReturnValue((void*)vort,ndims_u,dsizes_u,NULL,NCL_float,0));
  }
/*
 * Determine the workspace size.
 */
  lwork = nlon * nlat;
  work = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrG: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(geomatv,GEOMATV)(&nlon,&nlat,&u[j],&v[j],work);
    j += nlat*nlon;
  }
  free(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 * dynamically allocate various temporary space.
 * Note the order "vhagc(...,v,u,....)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  /*  lwork  = max(4*(nlat+1),nlat*(2*nt*nlon+max(6*l2,nlon)));*/
  lwork = 2*nlat*(2*nlon*nt+3*l2);
  lvhagc = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+l2+15;
  ldwork = 2*nlat*(nlat+1)+1;

  br =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  bi =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  cr =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  ci =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  wvhagc = (float*)calloc(        lvhagc*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  dwork = (double*)calloc(  ldwork*sizeof(double),1);

  if( br == NULL || bi == NULL || cr == NULL || ci == NULL ||
      wvhagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrG: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(vhagci,VHAGCI)(&nlat,&nlon,wvhagc,&lvhagc,dwork,&ldwork,&jer);
  NGCALLF(vhagc,VHAGC)(&nlat,&nlon,&isym,&nt,&v[0],&u[0],
		       &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
		       wvhagc,&lvhagc,work,&lwork,&ker);
  free(br);
  free(bi);
  free(wvhagc);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("uv2vrG","vhagc",&ier,&jer,&ker,&mer,8,5);
/*
 * compute the vorticity using the vector spherical harmonic 
 * coefficients br and bi computed by 'sub vhagc'
 */
  lshsgc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;
  /*  lwork  = max(nlat+1,nlat*(nt*nlon+max(3*l2,nlon)+2*nt*l1+1));*/
  lwork = nlat*(nlon*nt+max(3*l2,nlon)+2*nt*l1+1);
  ldwork = nlat*(nlat+4);

  work   = (float*)calloc( lwork*sizeof(float),1);
  wshsgc = (float*)calloc(lshsgc*sizeof(float),1);

  dwork = (double*)calloc(ldwork*sizeof(double),1);
  if( wshsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrG: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(shsgci,SHSGCI)(&nlat,&nlon,wshsgc,&lshsgc,dwork,&ldwork,&jer);
  NGCALLF(vrtgc,VRTGC)(&nlat,&nlon,&isym,&nt,&vort[0],&idvw,&jdvw,cr,ci,
		       &mdab,&ndab,wshsgc,&lshsgc,work,&lwork,&ker);

  NGCALLF(chkerr,CHKERR)("uv2vrG","shsgci+vrtgc",&ier,&jer,&ker,&mer,8,12);

  free(cr);
  free(ci);
  free(wshsgc);
  free(work);
  free(dwork);
/* 
 * transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
 
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrG: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&vort[j],work);
    NGCALLF(matgeov,MATGEOV)(&nlat,&nlon,&u[j],&v[j],work);
    j += nlat*nlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 1./6.37122e+6;       /* 1/(radius of earth) */
  
  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&vort[0],&scale,&ner);
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return array.
 */
  return(NclReturnValue((void*)vort,ndims_u,dsizes_u,NULL,NCL_float,0));
}


NhlErrorTypes uv2vrdvf_W( void )
{
/*
 * Input array variables
 */
  float *u, *v;
  int dsizes_u[NCL_MAX_DIMENSIONS], dsizes_v[NCL_MAX_DIMENSIONS];
  int ndims_u, ndims_v;
  NclScalar missing_u, missing_v;
  int has_missing_u, has_missing_v, found_missing=0;
  int nt, nlat, nlon;
/*
 * Output array variables
 */
  float *vr, *dv;
  int dsizes_vr[NCL_MAX_DIMENSIONS], dsizes_dv[NCL_MAX_DIMENSIONS];
  int ndims_vr, ndims_dv;
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  float scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhaec, lshsec;
  float *work, *wvhaec, *wshsec, *br, *bi, *cr, *ci;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  u = (float*)NclGetArgValue(
           0,
           4,
           &ndims_u, 
           dsizes_u,
           &missing_u,
           &has_missing_u,
           NULL,
           2);
  v = (float*)NclGetArgValue(
           1,
           4,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           NULL,
           2);
/*
 * The grids coming in must be at least 2-dimensional.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvf: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_v[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvf: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  nlat = dsizes_u[ndims_u-2];
  nlon = dsizes_u[ndims_u-1];
/*
 * Compute the total number of elements in our array.
 */
  nt = 1;
  for(i = 0; i < ndims_u-2; i++) {
    nt *= dsizes_u[i];
  }
/*
 * Get output arrays.
 */
  vr = (float*)NclGetArgValue(
           2,
           4,
           &ndims_vr, 
           dsizes_vr,
           NULL,
           NULL,
           NULL,
           1);
  dv = (float*)NclGetArgValue(
           3,
           4,
           &ndims_dv, 
           dsizes_dv,
           NULL,
           NULL,
           NULL,
           1);
/*
 * The output arrays must also be at least 2-dimensional.
 */
  if( ndims_vr != ndims_u || ndims_dv != ndims_u ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvf: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_vr[i] != dsizes_u[i] || dsizes_dv[i] != dsizes_u[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvf: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Check for missing values.
 */
  if(has_missing_u || has_missing_v) {
    l = 0;
    while( l < nt*nlat*nlon && !found_missing ) {
      if(u[l] == missing_u.floatval || v[l] == missing_v.floatval) {
	found_missing = 1;
      }
      l++;
    }
  }
  if(found_missing) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvf: The input arrays cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlon * nlat;
  work = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(geomatv,GEOMATV)(&nlon,&nlat,&u[j],&v[j],work);
    j += nlat*nlon;
  }
  free(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 *  dynamically allocate various temporary space
 *  Note the order "vhaec(...,v,u,....)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*(nlat+1),nlat*(2*nt*nlon+max(6*l2,nlon)));
  ldwork = 2*(nlat+2);
  lvhaec = 4*nlat*l2+3*max(l1-2,0)*(nlat+nlat-l1-1)+nlon+15;

  wvhaec = (float*)calloc(        lvhaec*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  br =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  bi =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  cr =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  ci =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  dwork = (double*)calloc(        ldwork*sizeof(double),1);

  if( br == NULL || bi == NULL || cr == NULL || ci == NULL ||
      wvhaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(vhaeci,VHAECI)(&nlat,&nlon,wvhaec,&lvhaec,dwork,&ldwork,&jer);
  NGCALLF(vhaec,VHAEC)(&nlat,&nlon,&isym,&nt,&v[0],&u[0],
		       &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
		       wvhaec,&lvhaec,work,&lwork,&ker);
  free(wvhaec);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("uv2vrdvf","vhaec",&ier,&jer,&ker,&mer,8,5);
/* 
 * compute the divergence using the vector spherical harmonic 
 *  coefficients br and bi computed by 'sub vhaec'
 */
  lshsec = 2*nlat*l2+3*(max(l1-2,0)*(2*nlat-l1-1))/2+nlon+15;
  lwork  = max(nlat+1,nlat*(nt*nlon+max(3*l2,nlon)+2*nt*l1+1));
  ldwork = nlat+1;

  wshsec = (float*)calloc(lshsec*sizeof(float),1);
  work   = (float*)calloc( lwork*sizeof(float),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);
  if( wshsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(shseci,SHSECI)(&nlat,&nlon,wshsec,&lshsec,dwork,&ldwork,&jer);
  NGCALLF(divec,DIVEC)(&nlat,&nlon,&isym,&nt,&dv[0],&idvw,&jdvw,br,bi,
		       &mdab,&ndab,wshsec,&lshsec,work,&lwork,&ker);
  NGCALLF(vrtec,VRTEC)(&nlat,&nlon,&isym,&nt,&vr[0],&idvw,&jdvw,cr,ci,
		       &mdab,&ndab,wshsec,&lshsec,work,&lwork,&ker);

  free(bi);
  free(br);
  free(ci);
  free(cr);
  free(wshsec);
  free(work);
  free(dwork);

  NGCALLF(chkerr,CHKERR)("uv2vrdvf","shseci+divec+vrtec",&ier,&jer,&ker,&mer,8,18);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
 
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&dv[j],work);
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&vr[j],work);
    NGCALLF(matgeov,MATGEOV)(&nlat,&nlon,&u[j],&v[j],work);
    j += nlat*nlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 1./6.37122e+6;      /* 1/(radius of earth) */

  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&dv[0],&scale,&ner);
  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&vr[0],&scale,&ner);
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return
 */
  return(NhlNOERROR);
}


NhlErrorTypes uv2vrdvg_W( void )
{
/*
 * Input array variables
 */
  float *u, *v;
  int dsizes_u[NCL_MAX_DIMENSIONS], dsizes_v[NCL_MAX_DIMENSIONS];
  int ndims_u, ndims_v;
  NclScalar missing_u, missing_v;
  int has_missing_u, has_missing_v, found_missing=0;
  int nt, nlat, nlon;
/*
 * Output array variables
 */
  float *vr, *dv;
  int dsizes_vr[NCL_MAX_DIMENSIONS], dsizes_dv[NCL_MAX_DIMENSIONS];
  int ndims_vr, ndims_dv;
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  float scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhagc, lshsgc;
  float *work, *wvhagc, *wshsgc, *br, *bi, *cr, *ci;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  u = (float*)NclGetArgValue(
           0,
           4,
           &ndims_u, 
           dsizes_u,
           &missing_u,
           &has_missing_u,
           NULL,
           2);
  v = (float*)NclGetArgValue(
           1,
           4,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           NULL,
           2);
/*
 * The grids coming in must be at least 2-dimensional.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvg: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_v[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvg: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  nlat = dsizes_u[ndims_u-2];
  nlon = dsizes_u[ndims_u-1];
/*
 * Compute the total number of elements in our array.
 */
  nt = 1;
  for(i = 0; i < ndims_u-2; i++) {
    nt *= dsizes_u[i];
  }
/*
 * Get output arrays.
 */
  vr = (float*)NclGetArgValue(
           2,
           4,
           &ndims_vr, 
           dsizes_vr,
           NULL,
           NULL,
           NULL,
           1);
  dv = (float*)NclGetArgValue(
           3,
           4,
           &ndims_dv, 
           dsizes_dv,
           NULL,
           NULL,
           NULL,
           1);
/*
 * The output arrays must also be at least 2-dimensional.
 */
  if( ndims_vr != ndims_u || ndims_dv != ndims_u ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvg: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_vr[i] != dsizes_u[i] || dsizes_dv[i] != dsizes_u[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvg: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Check for missing values.
 */
  if(has_missing_u || has_missing_v) {
    l = 0;
    while( l < nt*nlat*nlon && !found_missing ) {
      if(u[l] == missing_u.floatval || v[l] == missing_v.floatval) {
	found_missing = 1;
      }
      l++;
    }
  }
  if(found_missing) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvg: The input arrays cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlon * nlat;
  work = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(geomatv,GEOMATV)(&nlon,&nlat,&u[j],&v[j],work);
    j += nlat*nlon;
  }
  free(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 *  dynamically allocate various temporary space
 *  Note the order "vhaec(...,v,u,....)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*nlat*(nlat+1)+2,2*nlat*(2*nlon*nt+3*l2));
  lvhagc = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+l2+15;
  ldwork = 2*nlat*(nlat+1)+1;

  wvhagc = (float*)calloc(        lvhagc*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  br =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  bi =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  cr =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  ci =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  dwork = (double*)calloc(  ldwork*sizeof(double),1);

  if( br == NULL || bi == NULL || cr == NULL || ci == NULL ||
      wvhagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(vhagci,VHAGCI)(&nlat,&nlon,wvhagc,&lvhagc,dwork,&ldwork,&jer);
  NGCALLF(vhagc,VHAGC)(&nlat,&nlon,&isym,&nt,&v[0],&u[0],
		       &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
		       wvhagc,&lvhagc,work,&lwork,&ker);
  free(wvhagc);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("uv2vrdvg","vhagc",&ier,&jer,&ker,&mer,8,5);
/* 
 * compute the divergence using the vector spherical harmonic 
 *  coefficients br and bi computed by 'sub vhagc'
 */
  lshsgc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;
  lwork  = max(4*nlat*(nlat+2)+2,nlat*(nlon*nt+max(3*l2,nlon)+2*nt*l1+1));
  ldwork = nlat*(nlat+4);

  wshsgc = (float*)calloc(lshsgc*sizeof(float),1);
  work   = (float*)calloc( lwork*sizeof(float),1);
  dwork = (double*)calloc(ldwork*sizeof(double),1);
  if( wshsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(shsgci,SHSGCI)(&nlat,&nlon,wshsgc,&lshsgc,dwork,&ldwork,&jer);
  NGCALLF(divgc,DIVGC)(&nlat,&nlon,&isym,&nt,&dv[0],&idvw,&jdvw,br,bi,
		       &mdab,&ndab,wshsgc,&lshsgc,work,&lwork,&ker);
  NGCALLF(vrtgc,VRTGC)(&nlat,&nlon,&isym,&nt,&vr[0],&idvw,&jdvw,cr,ci,
		       &mdab,&ndab,wshsgc,&lshsgc,work,&lwork,&ker);

  free(bi);
  free(br);
  free(ci);
  free(cr);
  free(wshsgc);
  free(work);
  free(dwork);

  NGCALLF(chkerr,CHKERR)("uv2vrdvg","shsgci+divgc+vrtgc",&ier,&jer,&ker,&mer,8,18);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
 
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&dv[j],work);
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&vr[j],work);
    NGCALLF(matgeov,MATGEOV)(&nlat,&nlon,&u[j],&v[j],work);
    j += nlat*nlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 1./6.37122e+6;      /* 1/(radius of earth) */

  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&dv[0],&scale,&ner);
  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&vr[0],&scale,&ner);
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return
 */
  return(NhlNOERROR);
}


NhlErrorTypes vr2uvf_W( void )
{
/*
 * Input array variables
 */
  float *vort;
  int ndims_vort, dsizes_vort[NCL_MAX_DIMENSIONS], nt, nlat, nlon;
  NclScalar missing_vort;
  int has_missing_vort, found_missing=0;
/*
 * Output array variables
 */
  float *ur, *vr;
  int dsizes_ur[NCL_MAX_DIMENSIONS], dsizes_vr[NCL_MAX_DIMENSIONS];
  int ndims_ur, ndims_vr;
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2, l3;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  float scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lshaec, lvhsec;
  float *work, *wshaec, *wvhsec, *a, *b, *pertrb;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  vort = (float*)NclGetArgValue(
           0,
           3,
           &ndims_vort, 
           dsizes_vort,
           &missing_vort,
           &has_missing_vort,
           NULL,
           2);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_vort < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvf: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }
  nlat = dsizes_vort[ndims_vort-2];
  nlon = dsizes_vort[ndims_vort-1];
/*
 * Compute the total number of elements in our array.
 */
  nt = 1;
  for(i = 0; i < ndims_vort-2; i++) {
    nt *= dsizes_vort[i];
  }
/*
 * Get output arrays.
 */
  ur = (float*)NclGetArgValue(
           1,
           3,
           &ndims_ur, 
           dsizes_ur,
           NULL,
           NULL,
           NULL,
           1);
  vr = (float*)NclGetArgValue(
           2,
           3,
           &ndims_vr, 
           dsizes_vr,
           NULL,
           NULL,
           NULL,
           1);
/*
 * The output arrays must also be at least 2-dimensional.
 */
  if( ndims_ur != ndims_vort || ndims_vr != ndims_vort ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvf: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_vort; i++ ) {
    if( dsizes_ur[i] != dsizes_vort[i] || dsizes_vr[i] != dsizes_vort[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvf: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Check for missing values.
 */
  if(has_missing_vort) {
    l = 0;
    while( l < nt*nlat*nlon && !found_missing ) {
      if(vort[l++] == missing_vort.floatval) found_missing = 1;
    }
  }
  if(found_missing) {
    for(i = 0; i < nt*nlat*nlon; i++) {
      ur[i] = vr[i] = missing_vort.floatval;
    }
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvf: The input array cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlon * nlat;
  work = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(geomat,GEOMAT)(&nlon,&nlat,&vort[j],work);
    j += nlat*nlon;
  }
  free(work);
/*
 * shaec performs the spherical harmonic analysis on a (scalar) gaussian 
 * grid(s) and returns the coefficients in array(s) a,b.
 * Here the scalar grid is "vort" (relative vorticity)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(2*(nlat+1),nlat*(nt*nlon+max(3*l2,nlon)));
  lshaec = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;
  ldwork = nlat+1;

  a =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  b =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  wshaec = (float*)calloc(        lshaec*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  dwork =  (double*)calloc(        ldwork*sizeof(double),1);

  if( a == NULL || b == NULL || wshaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(shaeci,SHAECI)(&nlat,&nlon,wshaec,&lshaec,dwork,&ldwork,&jer);
  NGCALLF(shaec,SHAEC)(&nlat,&nlon,&isym,&nt,&vort[0],
		       &idvw,&jdvw,a,b,&mdab,&ndab,
		       wshaec,&lshaec,work,&lwork,&ker);
  free(wshaec);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("vr2uvf","shaec",&ier,&jer,&ker,&mer,8,5);
/*
 * reconstruct the divergent (irrotational) wind components
 * note the argument order idivec(...,v,u,...)
 */
  l3     = max(nlat,(nlon+1)/2 );
  lwork  = max(4*(nlat+1),nlat*(2*nt*nlon+max(6*l2,nlon)+2*nt*l3+1));
  lvhsec = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15;
  ldwork = 2*(nlat+2);

  pertrb = (float*)calloc(  nt*sizeof(float),1);
  work   = (float*)calloc( lwork*sizeof(float),1);
  dwork = (double*)calloc( ldwork*sizeof(double),1);
  wvhsec = (float*)calloc(lvhsec*sizeof(float),1);

  if( pertrb == NULL || wvhsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(vhseci,VHSECI)(&nlat,&nlon,wvhsec,&lvhsec,dwork,&ldwork,&jer);
  NGCALLF(ivrtec,IVRTEC)(&nlat,&nlon,&isym,&nt,&vr[0],&ur[0],
			 &idvw,&jdvw,a,b,&mdab,&ndab,
			 wvhsec,&lvhsec,work,&lwork,pertrb,&ker);

  free(a);
  free(b);
  free(pertrb);
  free(wvhsec);
  free(work);
  free(dwork);

  NGCALLF(chkerr,CHKERR)("vr2uvf","vhseci+ivrtec",&ier,&jer,&ker,&mer,8,13);
/* 
 * transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
 
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&vort[j],work);
    NGCALLF(matgeov,MATGEOV)(&nlat,&nlon,&ur[j],&vr[j],work);
    j += nlat*nlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 6.37122e+6;         /* radius of earth */
  
  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&ur[0],&scale,&ner);
  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&vr[0],&scale,&ner);
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return
 */
  return(NhlNOERROR);
}


NhlErrorTypes vr2uvg_W( void )
{
/*
 * Input array variables
 */
  float *vort;
  int ndims_vort, dsizes_vort[NCL_MAX_DIMENSIONS], nt, nlat, nlon;
  NclScalar missing_vort;
  int has_missing_vort, found_missing=0;
/*
 * Output array variables
 */
  float *ur, *vr;
  int dsizes_ur[NCL_MAX_DIMENSIONS], dsizes_vr[NCL_MAX_DIMENSIONS];
  int ndims_ur, ndims_vr;
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2, l3;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  float scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lshagc, lvhsgc;
  float *work, *wshagc, *wvhsgc, *a, *b, *pertrb;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  vort = (float*)NclGetArgValue(
           0,
           3,
           &ndims_vort, 
           dsizes_vort,
           &missing_vort,
           &has_missing_vort,
           NULL,
           2);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_vort < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvg: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }
  nlat = dsizes_vort[ndims_vort-2];
  nlon = dsizes_vort[ndims_vort-1];
/*
 * Compute the total number of elements in our array.
 */
  nt = 1;
  for(i = 0; i < ndims_vort-2; i++) {
    nt *= dsizes_vort[i];
  }
/*
 * Get output arrays.
 */
  ur = (float*)NclGetArgValue(
           1,
           3,
           &ndims_ur, 
           dsizes_ur,
           NULL,
           NULL,
           NULL,
           1);
  vr = (float*)NclGetArgValue(
           2,
           3,
           &ndims_vr, 
           dsizes_vr,
           NULL,
           NULL,
           NULL,
           1);
/*
 * The output arrays must also be at least 2-dimensional.
 */
  if( ndims_ur != ndims_vort || ndims_vr != ndims_vort ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvg: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_vort; i++ ) {
    if( dsizes_ur[i] != dsizes_vort[i] || dsizes_vr[i] != dsizes_vort[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvg: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Check for missing values.
 */
  if(has_missing_vort) {
    l = 0;
    while( l < nt*nlat*nlon && !found_missing ) {
      if(vort[l++] == missing_vort.floatval) found_missing = 1;
    }
  }
  if(found_missing) {
    for(i = 0; i < nt*nlat*nlon; i++) {
      ur[i] = vr[i] = missing_vort.floatval;
    }
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvg: The input array cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlon * nlat;
  work = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(geomat,GEOMAT)(&nlon,&nlat,&vort[j],work);
    j += nlat*nlon;
  }
  free(work);
/*
 * shagc performs the spherical harmonic analysis on a (scalar) gaussian 
 * grid(s) and returns the coefficients in array(s) a,b.
 * Here the scalar grid is "vort" (relative vorticity)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*nlat*(nlat+2)+2,nlat*(nlon*nt+max(3*l2,nlon)));
  lshagc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;
  ldwork = nlat*(nlat+4);

  a =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  b =      (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  wshagc = (float*)calloc(        lshagc*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  dwork =  (double*)calloc(       ldwork*sizeof(double),1);

  if( a == NULL || b == NULL || wshagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(shagci,SHAGCI)(&nlat,&nlon,wshagc,&lshagc,dwork,&ldwork,&jer);
  NGCALLF(shagc,SHAGC)(&nlat,&nlon,&isym,&nt,&vort[0],&idvw,&jdvw,
		       a,b,&mdab,&ndab,wshagc,&lshagc,work,&lwork,&ker);
  free(wshagc);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("vr2uvg","shagc",&ier,&jer,&ker,&mer,8,5);
/*
 * reconstruct the divergent (irrotational) wind components
 * note the argument order idivgc(...,v,u,...)
 */
  l3     = max(nlat,(nlon+1)/2 );
  lwork  = max(4*nlat*(nlat+1)+2,nlat*(2*nt*nlon+max(6*l2,nlon)+2*nt*l3+1));
  lvhsgc = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15;
  ldwork = 2*nlat*(nlat+1)+1;

  work   = (float*)calloc( lwork*sizeof(float),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);
  wvhsgc = (float*)calloc(lvhsgc*sizeof(float),1);
  pertrb = (float*)calloc( nt*sizeof(float),1);

  if( pertrb == NULL || wvhsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(vhsgci,VHSGCI)(&nlat,&nlon,wvhsgc,&lvhsgc,dwork,&ldwork,&jer);
  NGCALLF(ivrtgc,IVRTGC)(&nlat,&nlon,&isym,&nt,&vr[0],&ur[0],
			 &idvw,&jdvw,a,b,&mdab,&ndab,
			 wvhsgc,&lvhsgc,work,&lwork,pertrb,&ker);

  free(a);
  free(b);
  free(pertrb);
  free(wvhsgc);
  free(work);
  free(dwork);

  NGCALLF(chkerr,CHKERR)("vr2uvg","vhsgci+ivrtgc",&ier,&jer,&ker,&mer,8,13);
/* 
 * transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
 
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&vort[j],work);
    NGCALLF(matgeov,MATGEOV)(&nlat,&nlon,&ur[j],&vr[j],work);
    j += nlat*nlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 6.37122e+6;         /* radius of earth */
  
  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&ur[0],&scale,&ner);
  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&vr[0],&scale,&ner);
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return
 */
  return(NhlNOERROR);
}


NhlErrorTypes vrdv2uvf_W( void )
{
/*
 * Input array variables
 */
  float *vr, *dv;
  int ndims_vr, dsizes_vr[NCL_MAX_DIMENSIONS], nt, nlat, nlon;
  int ndims_dv, dsizes_dv[NCL_MAX_DIMENSIONS];
  NclScalar missing_vr, missing_dv;
  float missing;
  int has_missing_vr, has_missing_dv, found_missing=0;
/*
 * Output array variables
 */
  float *u, *v;
  int dsizes_u[NCL_MAX_DIMENSIONS], dsizes_v[NCL_MAX_DIMENSIONS];
  int ndims_u, ndims_v;
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  float scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lshaec, lvhsec;
  float *work, *wshaec, *wvhsec, *ad, *bd, *av, *bv, *pertbd, *pertbv;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  vr = (float*)NclGetArgValue(
           0,
           4,
           &ndims_vr, 
           dsizes_vr,
           &missing_vr,
           &has_missing_vr,
           NULL,
           2);
  dv = (float*)NclGetArgValue(
           1,
           4,
           &ndims_dv, 
           dsizes_dv,
           &missing_dv,
           &has_missing_dv,
           NULL,
           2);
/*
 * The grids coming in must be at least 2-dimensional and the same size.
 */
  if( ndims_vr != ndims_dv || ndims_vr < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvf: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_vr; i++ ) {
    if( dsizes_vr[i] != dsizes_dv[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvf: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  nlat = dsizes_vr[ndims_vr-2];
  nlon = dsizes_vr[ndims_vr-1];
/*
 * Compute the total number of elements in our array.
 */
  nt = 1;
  for(i = 0; i < ndims_vr-2; i++) {
    nt *= dsizes_vr[i];
  }
/*
 * Get output arrays.
 */
  u = (float*)NclGetArgValue(
           2,
           4,
           &ndims_u, 
           dsizes_u,
           NULL,
           NULL,
           NULL,
           1);
  v = (float*)NclGetArgValue(
           3,
           4,
           &ndims_v, 
           dsizes_v,
           NULL,
           NULL,
           NULL,
           1);
/*
 * The output arrays must also be at least 2-dimensional and the same sizes.
 */
  if( ndims_u != ndims_vr || ndims_v != ndims_vr ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvf: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_vr; i++ ) {
    if( dsizes_u[i] != dsizes_vr[i] || dsizes_v[i] != dsizes_vr[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvf: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Check for missing values.
 */
  if(has_missing_vr || has_missing_dv) {
    l = 0;
    while( l < nt*nlat*nlon && !found_missing ) {
      if(vr[l]==missing_vr.floatval || dv[l]==missing_dv.floatval) {
	found_missing = 1;
      }
      l++;
    }
  }
  if(found_missing) {
    if(has_missing_vr) missing = missing_vr.floatval;
    else              missing = missing_dv.floatval;
    for(i = 0; i < nt*nlat*nlon; i++) {
      u[i] = v[i] = missing;
    }
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvf: The input array cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlon * nlat;
  work = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(geomat,GEOMAT)(&nlon,&nlat,&dv[j],work);
    NGCALLF(geomat,GEOMAT)(&nlon,&nlat,&vr[j],work);
    j += nlat*nlon;
  }
  free(work);
/*
 * shaec performs the spherical harmonic analysis on a (scalar) equal 
 * grid(s) and returns the coefficients in array(s) ad,bd for divergence
 * and av,bv for vortivity
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(2*(nlat+1),nlat*(nt*nlon+max(3*l2,nlon)));
  lshaec = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;
  ldwork = nlat+1;

  ad =     (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  bd =     (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  av =     (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  bv =     (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  wshaec = (float*)calloc(        lshaec*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  dwork =  (double*)calloc(        ldwork*sizeof(double),1);

  if( ad == NULL || bd == NULL || av == NULL || bv == NULL ||
      wshaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(shaeci,SHAECI)(&nlat,&nlon,wshaec,&lshaec,dwork,&ldwork,&jer);
  NGCALLF(shaec,SHAEC)(&nlat,&nlon,&isym,&nt,&dv[0],&idvw,&jdvw,ad,bd,
		       &mdab,&ndab,wshaec,&lshaec,work,&lwork,&ker);
  NGCALLF(shaec,SHAEC)(&nlat,&nlon,&isym,&nt,&vr[0],&idvw,&jdvw,av,bv,
		       &mdab,&ndab,wshaec,&lshaec,work,&lwork,&ker);

  free(wshaec);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("vrdv2uvf","shaec",&ier,&jer,&ker,&mer,10,5);
/* 
 * compute the u and v components fron vr,dv
 */ 
 
  lwork  = max(4*(nlat+1),nlat*(2*nt*nlon+max(6*l2,nlon))+nlat*(4*l1*nt+1));
  ldwork = 2*(nlat+2);
  lvhsec = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15;

  wvhsec = (float*)calloc(lvhsec*sizeof(float),1);
  work   = (float*)calloc( lwork*sizeof(float),1);
  dwork = (double*)calloc( ldwork*sizeof(double),1);
  pertbd = (float*)calloc( nt*sizeof(float),1);
  pertbv = (float*)calloc( nt*sizeof(float),1);

  if( pertbd == NULL || pertbv == NULL || wvhsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(vhseci,VHSECI)(&nlat,&nlon,wvhsec,&lvhsec,dwork,&ldwork,&jer);
  NGCALLF(idvtec,IDVTEC)(&nlat,&nlon,&isym,&nt,&v[0],&u[0],&idvw,&jdvw,
			 ad,bd,av,bv,&mdab,&ndab,wvhsec,&lvhsec,
			 work,&lwork,pertbd,pertbv,&ker);

  free(ad);
  free(bd);
  free(av);
  free(bv);
  free(pertbd);
  free(pertbv);
  free(wvhsec);
  free(work);
  free(dwork);

  NGCALLF(chkerr,CHKERR)("vrdv2uvf","vhseci+idvtec",&ier,&jer,&ker,&mer,10,13);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
 
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&dv[j],work);	
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&vr[j],work);	
    NGCALLF(matgeov,MATGEOV)(&nlat,&nlon,&u[j],&v[j],work);
    j += nlat*nlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 6.37122e+6;         /* radius of earth */
  
  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&u[0],&scale,&ner);
  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&v[0],&scale,&ner);
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return
 */
  return(NhlNOERROR);
}


NhlErrorTypes vrdv2uvg_W( void )
{
/*
 * Input array variables
 */
  float *vr, *dv;
  int ndims_vr, dsizes_vr[NCL_MAX_DIMENSIONS], nt, nlat, nlon;
  int ndims_dv, dsizes_dv[NCL_MAX_DIMENSIONS];
  NclScalar missing_vr, missing_dv;
  float missing;
  int has_missing_vr, has_missing_dv, found_missing=0;
/*
 * Output array variables
 */
  float *u, *v;
  int dsizes_u[NCL_MAX_DIMENSIONS], dsizes_v[NCL_MAX_DIMENSIONS];
  int ndims_u, ndims_v;
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  float scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lshagc, lvhsgc;
  float *work, *wshagc, *wvhsgc, *ad, *bd, *av, *bv, *pertbd, *pertbv;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  vr = (float*)NclGetArgValue(
           0,
           4,
           &ndims_vr, 
           dsizes_vr,
           &missing_vr,
           &has_missing_vr,
           NULL,
           2);
  dv = (float*)NclGetArgValue(
           1,
           4,
           &ndims_dv, 
           dsizes_dv,
           &missing_dv,
           &has_missing_dv,
           NULL,
           2);
/*
 * The grids coming in must be at least 2-dimensional and the same size.
 */
  if( ndims_vr != ndims_dv || ndims_vr < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvg: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_vr; i++ ) {
    if( dsizes_vr[i] != dsizes_dv[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvg: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  nlat = dsizes_vr[ndims_vr-2];
  nlon = dsizes_vr[ndims_vr-1];
/*
 * Compute the total number of elements in our array.
 */
  nt = 1;
  for(i = 0; i < ndims_vr-2; i++) {
    nt *= dsizes_vr[i];
  }
/*
 * Get output arrays.
 */
  u = (float*)NclGetArgValue(
           2,
           4,
           &ndims_u, 
           dsizes_u,
           NULL,
           NULL,
           NULL,
           1);
  v = (float*)NclGetArgValue(
			     3,
           4,
           &ndims_v, 
           dsizes_v,
			     NULL,
			     NULL,
           NULL,
           1);
/*
 * The output arrays must also be at least 2-dimensional and the same sizes.
 */
  if( ndims_u != ndims_vr || ndims_v != ndims_vr ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvg: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_vr; i++ ) {
    if( dsizes_u[i] != dsizes_vr[i] || dsizes_v[i] != dsizes_vr[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvg: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Check for missing values.
 */
  if(has_missing_vr || has_missing_dv) {
    l = 0;
    while( l < nt*nlat*nlon && !found_missing ) {
      if(vr[l]==missing_vr.floatval || dv[l]==missing_dv.floatval) {
	found_missing = 1;
      }
      l++;
    }
  }
  if(found_missing) {
    if(has_missing_vr) missing = missing_vr.floatval;
    else               missing = missing_dv.floatval;
    for(i = 0; i < nt*nlat*nlon; i++) {
      u[i] = v[i] = missing;
    }
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvg: The input array cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlon * nlat;
  work = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(geomat,GEOMAT)(&nlon,&nlat,&dv[j],work);
    NGCALLF(geomat,GEOMAT)(&nlon,&nlat,&vr[j],work);
    j += nlat*nlon;
  }
  free(work);
/*
 * shagc performs the spherical harmonic analysis on a (scalar) equal 
 * grid(s) and returns the coefficients in array(s) ad,bd for divergence
 * and av,bv for vortivity
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*nlat*(nlat+2)+2,nlat*(nt*nlon+max(3*l2,nlon)));
  lshagc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;
  ldwork = nlat*(nlat+4);

  ad =     (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  bd =     (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  av =     (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  bv =     (float*)calloc(  mdab*ndab*nt*sizeof(float),1);
  wshagc = (float*)calloc(        lshagc*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  dwork =  (double*)calloc(       ldwork*sizeof(double),1);

  if( ad == NULL || bd == NULL || av == NULL || bv == NULL ||
      wshagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(shagci,SHAGCI)(&nlat,&nlon,wshagc,&lshagc,dwork,&ldwork,&jer);
  NGCALLF(shagc,SHAGC)(&nlat,&nlon,&isym,&nt,&dv[0],&idvw,&jdvw,ad,bd,
		       &mdab,&ndab,wshagc,&lshagc,work,&lwork,&ker);
  NGCALLF(shagc,SHAGC)(&nlat,&nlon,&isym,&nt,&vr[0],&idvw,&jdvw,av,bv,
		       &mdab,&ndab,wshagc,&lshagc,work,&lwork,&ker);
  free(wshagc);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("vrdv2uvg","shagc",&ier,&jer,&ker,&mer,10,5);
/* 
 * compute the u and v components fron vr,dv
 */ 
  lwork  = max(4*nlat*(nlat+1)+2,nlat*(2*nt*nlon+max(6*l2,nlon)+4*nt*l1+1));
  lvhsgc = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15;
  ldwork = 2*nlat*(nlat+1)+1;

  wvhsgc = (float*)calloc(lvhsgc*sizeof(float),1);
  work   = (float*)calloc( lwork*sizeof(float),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);
  pertbd = (float*)calloc( nt*sizeof(float),1);
  pertbv = (float*)calloc( nt*sizeof(float),1);

  if( pertbd == NULL || pertbv == NULL || wvhsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(vhsgci,VHSGCI)(&nlat,&nlon,wvhsgc,&lvhsgc,dwork,&ldwork,&jer);
  NGCALLF(idvtgc,IDVTGC)(&nlat,&nlon,&isym,&nt,&v[0],&u[0],&idvw,&jdvw,
			 ad,bd,av,bv,&mdab,&ndab,wvhsgc,&lvhsgc,
			 work,&lwork,pertbd,pertbv,&ker);

  free(ad);
  free(bd);
  free(av);
  free(bv);
  free(pertbd);
  free(pertbv);
  free(wvhsgc);
  free(work);
  free(dwork);

  NGCALLF(chkerr,CHKERR)("vrdv2uvg","vhsgci+idvtgc",&ier,&jer,&ker,&mer,10,13);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
 
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&dv[j],work);	
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&vr[j],work);	
    NGCALLF(matgeov,MATGEOV)(&nlat,&nlon,&u[j],&v[j],work);
    j += nlat*nlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 6.37122e+6;         /* radius of earth */
  
  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&u[0],&scale,&ner);
  NGCALLF(geoscl,GEOSCL)(&nlon,&nlat,&nt,&v[0],&scale,&ner);
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return
 */
  return(NhlNOERROR);
}


NhlErrorTypes vhaec_W( void )
{
/*
 * Input array variables
 */
  float *u, *v;
  int ndims_u, dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v, dsizes_v[NCL_MAX_DIMENSIONS];
  int nt, nlat, nlon;
/*
 * Output array variables
 */
  float *br, *bi, *cr, *ci;
  int ndims_br, dsizes_br[NCL_MAX_DIMENSIONS];
  int ndims_bi, dsizes_bi[NCL_MAX_DIMENSIONS];
  int ndims_cr, dsizes_cr[NCL_MAX_DIMENSIONS];
  int ndims_ci, dsizes_ci[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  int i, j, l, ityp, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhaec, lvhsec;
  float *work, *wvhaec;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  u = (float*)NclGetArgValue(
           0,
           6,
           &ndims_u, 
           dsizes_u,
           NULL,
           NULL,
           NULL,
           2);
  v = (float*)NclGetArgValue(
           1,
           6,
           &ndims_v, 
           dsizes_v,
	   NULL,
	   NULL,
           NULL,
           2);
/*
 * The grids coming in must be at least 2-dimensional.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhaec: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_v[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vhaec: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  nlat = dsizes_u[ndims_u-2];
  nlon = dsizes_u[ndims_u-1];
/*
 * Compute the total number of elements in our array.
 */
  nt = 1;
  for(i = 0; i < ndims_u-2; i++) {
    nt *= dsizes_u[i];
  }
/*
 * Get in/output arrays.
 */
  br = (float*)NclGetArgValue(
			      2,
           6,
           &ndims_br,
           dsizes_br,
			      NULL,
			      NULL,
           NULL,
           1);
  bi = (float*)NclGetArgValue(
			      3,
           6,
           &ndims_bi, 
           dsizes_bi,
			      NULL,
			      NULL,
           NULL,
           1);
  cr = (float*)NclGetArgValue(
			      4,
           6,
           &ndims_cr,
           dsizes_cr,
			      NULL,
			      NULL,
           NULL,
           1);
  ci = (float*)NclGetArgValue(
			      5,
           6,
           &ndims_ci, 
           dsizes_ci,
			      NULL,
			      NULL,
           NULL,
           1);
/*
 * The output arrays must also be at least 2-dimensional.
 */
  if( ndims_br != ndims_u || ndims_bi != ndims_u  ||
      ndims_cr != ndims_u || ndims_ci != ndims_u ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhaec: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u-2; i++ ) {
    if( dsizes_br[i] != dsizes_u[i] || dsizes_bi[i] != dsizes_u[i] ||
	dsizes_cr[i] != dsizes_u[i] || dsizes_ci[i] != dsizes_u[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vhaec: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  if((dsizes_br[ndims_br-1] != dsizes_br[ndims_br-2])||(dsizes_br[ndims_br-2]!=dsizes_u[ndims_u-2])) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhaec: The last two dimensions of output coefficient arrays must be the same size and have the same number of latitude points as the input grid");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlon * nlat;
  work = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhaec: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(geomatv,GEOMATV)(&nlon,&nlat,&u[j],&v[j],work);
    j += nlat*nlon;
  }
  free(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 * dynamically allocate various temporary space
 * Note the order "vhaec(...,v,u,....)
 */
  ityp   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = nlat;
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;

  lwork  = max(4*(nlat+1),nlat*(2*nlon*nt+max(6*l2,nlon)));
  ldwork = 2*(nlat+2);
  lvhaec = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15;

  wvhaec = (float*)calloc(        lvhaec*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  dwork = (double*)calloc(        ldwork*sizeof(double),1);

  if( wvhaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhaec: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(vhaeci,VHAECI)(&nlat,&nlon,wvhaec,&lvhaec,dwork,&ldwork,&jer);
  NGCALLF(vhaec,VHAEC)(&nlat,&nlon,&ityp,&nt,&v[0],&u[0],
		       &idvw,&jdvw,br,bi,cr,ci,
		       &mdab,&ndab,wvhaec,&lvhaec,work,&lwork,&ker);
  free(wvhaec);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("vhaec","vhaec",&ier,&jer,&ker,&mer,5,5);
/* 
 * transform from math coordinates to geophysical coordinates
 * (math) nlon is the first dim
 */
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhaec: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeov,MATGEOV)(&nlat,&nlon,&u[j],&v[j],work);
    j += nlat*nlon;
  }
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return
 */
  return(NhlNOERROR);
}


NhlErrorTypes vhagc_W( void )
{
/*
 * Input array variables
 */
  float *u, *v;
  int ndims_u, dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v, dsizes_v[NCL_MAX_DIMENSIONS];
  int nt, nlat, nlon;
/*
 * Output array variables
 */
  float *br, *bi, *cr, *ci;
  int ndims_br, dsizes_br[NCL_MAX_DIMENSIONS];
  int ndims_bi, dsizes_bi[NCL_MAX_DIMENSIONS];
  int ndims_cr, dsizes_cr[NCL_MAX_DIMENSIONS];
  int ndims_ci, dsizes_ci[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  int i, j, l, ityp, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhagc, lvhsec;
  float *work, *wvhagc;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  u = (float*)NclGetArgValue(
           0,
           6,
           &ndims_u, 
           dsizes_u,
	   NULL,
	   NULL,
           NULL,
           2);
  v = (float*)NclGetArgValue(
           1,
           6,
           &ndims_v, 
           dsizes_v,
	   NULL,
	   NULL,
           NULL,
           2);
/*
 * The grids coming in must be at least 2-dimensional.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhagc: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_v[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vhagc: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  nlat = dsizes_u[ndims_u-2];
  nlon = dsizes_u[ndims_u-1];
/*
 * Compute the total number of elements in our array.
 */
  nt = 1;
  for(i = 0; i < ndims_u-2; i++) {
    nt *= dsizes_u[i];
  }
/*
 * Get in/output arrays.
 */
  br = (float*)NclGetArgValue(
			      2,
           6,
           &ndims_br,
           dsizes_br,
			      NULL,
			      NULL,
           NULL,
           1);
  bi = (float*)NclGetArgValue(
			      3,
           6,
           &ndims_bi, 
           dsizes_bi,
			      NULL,
			      NULL,
           NULL,
           1);
  cr = (float*)NclGetArgValue(
			      4,
           6,
           &ndims_cr,
           dsizes_cr,
			      NULL,
			      NULL,
           NULL,
           1);
  ci = (float*)NclGetArgValue(
			      5,
           6,
           &ndims_ci, 
           dsizes_ci,
			      NULL,
			      NULL,
           NULL,
           1);
/*
 * The output arrays must also be at least 2-dimensional.
 */
  if( ndims_br != ndims_u || ndims_bi != ndims_u  ||
      ndims_cr != ndims_u || ndims_ci != ndims_u ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhagc: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u-2; i++ ) {
    if( dsizes_br[i] != dsizes_u[i] || dsizes_bi[i] != dsizes_u[i] ||
	dsizes_cr[i] != dsizes_u[i] || dsizes_ci[i] != dsizes_u[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vhagc: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  if((dsizes_br[ndims_br-1] != dsizes_br[ndims_br-2])||(dsizes_br[ndims_br-2]!=dsizes_u[ndims_u-2])) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhagc: The last two dimensions of output coefficient arrays must be the same size and have the same number of latitude points as the input grid");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlon * nlat;
  work = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhagc: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(geomatv,GEOMATV)(&nlon,&nlat,&u[j],&v[j],work);
    j += nlat*nlon;
  }
  free(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 * dynamically allocate various temporary space
 * Note the order "vhagc(...,v,u,....)
 */
  ityp   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = nlat;
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;

  lwork  = max(4*nlat*(nlat+1)+2,2*nlat*(2*nlon*nt +3*l2 ));
  lvhagc = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+l2+15;
  ldwork = 2*nlat*(nlat+1)+1;

  wvhagc = (float*)calloc( lvhagc*sizeof(float),1);
  work =   (float*)calloc(  lwork*sizeof(float),1);
  dwork = (double*)calloc( ldwork*sizeof(double),1);

  if( wvhagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhagc: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(vhagci,VHAGCI)(&nlat,&nlon,wvhagc,&lvhagc,dwork,&ldwork,&jer);
  NGCALLF(vhagc,VHAGC)(&nlat,&nlon,&ityp,&nt,&v[0],&u[0],
		       &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
		       wvhagc,&lvhagc,work,&lwork,&ker);
  free(wvhagc);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("vhagc","vhagc",&ier,&jer,&ker,&mer,5,5);
/* 
 * transform from math coordinates to geophysical coordinates
 * (math) nlon is the first dim
 */
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhagc: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeov,MATGEOV)(&nlat,&nlon,&u[j],&v[j],work);
    j += nlat*nlon;
  }
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return
 */
  return(NhlNOERROR);
}


NhlErrorTypes vhsec_W( void )
{
/*
 * Input array variables
 */
  float *br, *bi, *cr, *ci;
  int ndims_br, dsizes_br[NCL_MAX_DIMENSIONS];
  int ndims_bi, dsizes_bi[NCL_MAX_DIMENSIONS];
  int ndims_cr, dsizes_cr[NCL_MAX_DIMENSIONS];
  int ndims_ci, dsizes_ci[NCL_MAX_DIMENSIONS];
  int nt, nlat, nlon;
/*
 * Output array variables
 */
  float *u, *v;
  int ndims_u, dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v, dsizes_v[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  int i, j, l, ityp, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhsec;
  float *work, *wvhsec;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  br = (float*)NclGetArgValue(
			      0,
           6,
           &ndims_br,
           dsizes_br,
			      NULL,
			      NULL,
           NULL,
           2);
  bi = (float*)NclGetArgValue(
			      1,
           6,
           &ndims_bi, 
           dsizes_bi,
			      NULL,
			      NULL,
           NULL,
           2);
  cr = (float*)NclGetArgValue(
			      2,
           6,
           &ndims_cr,
           dsizes_cr,
			      NULL,
			      NULL,
           NULL,
           2);
  ci = (float*)NclGetArgValue(
			      3,
           6,
           &ndims_ci, 
           dsizes_ci,
			      NULL,
			      NULL,
           NULL,
           2);
/*
 * The grids coming in must be at least 2-dimensional.
 */
  if( ndims_br != ndims_bi || ndims_br != ndims_ci || ndims_br != ndims_cr ||
      ndims_bi != ndims_ci || ndims_bi != ndims_cr ||
      ndims_cr != ndims_ci || ndims_br < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsec: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_br; i++ ) {
    if( dsizes_br[i] != dsizes_bi[i] || dsizes_br[i] != dsizes_ci[i] ||
	dsizes_br[i] != dsizes_cr[i] || dsizes_bi[i] != dsizes_cr[i] ||
	dsizes_bi[i] != dsizes_ci[i] || dsizes_ci[i] != dsizes_cr[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsec: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Compute the total number of elements in our array.
 */
  nt = 1;
  for(i = 0; i < ndims_br-2; i++) {
    nt *= dsizes_br[i];
  }
/*
 * Get in/output arrays.
 */
  u = (float*)NclGetArgValue(
           4,
           6,
           &ndims_u, 
           dsizes_u,
	   NULL,
           NULL,
           NULL,
           1);
  v = (float*)NclGetArgValue(
           5,
           6,
           &ndims_v, 
           dsizes_v,
	   NULL,
           NULL,
           NULL,
           1);
  nlat = dsizes_u[ndims_u-2];
  nlon = dsizes_u[ndims_u-1];
/*
 * The output arrays must also be at least 2-dimensional.
 */
  if( ndims_u != ndims_br || ndims_v != ndims_br ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsec: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_br-2; i++ ) {
    if( dsizes_u[i] != dsizes_br[i] || dsizes_v[i] != dsizes_br[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsec: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  if((dsizes_br[ndims_br-1] != dsizes_br[ndims_br-2])||(dsizes_br[ndims_br-2]!=dsizes_u[ndims_u-2])) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsec: The last two dimensions of input coefficient arrays must be the same size and have the same number of latitude points as the output grid");
    return(NhlFATAL);
  }

  ityp   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = nlat;
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;

  lwork  = max(4*(nlat+1),nlat*(2*nt*nlon+max(6*l2,nlon)));
  lvhsec = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15;
  ldwork = 2*(nlat+2);

  wvhsec = (float*)calloc(        lvhsec*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  dwork = (double*)calloc( ldwork*sizeof(double),1);

  if( wvhsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsec: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(vhseci,VHSECI)(&nlat,&nlon,wvhsec,&lvhsec,dwork,&ldwork,&jer);
  NGCALLF(vhsec,VHSEC)(&nlat,&nlon,&ityp,&nt,&v[0],&u[0],
		       &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
		       wvhsec,&lvhsec,work,&lwork,&ker);
  free(wvhsec);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("vhsec","vhsec",&ier,&jer,&ker,&mer,5,5);
/* 
 * transform from math coordinates to geophysical coordinates
 * (math) nlon is the first dim
 */
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsec: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeov,MATGEOV)(&nlat,&nlon,&u[j],&v[j],work);
    j += nlat*nlon;
  }
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return
 */
  return(NhlNOERROR);
}


NhlErrorTypes vhsgc_W( void )
{
/*
 * Input array variables
 */
  float *br, *bi, *cr, *ci;
  int ndims_br, dsizes_br[NCL_MAX_DIMENSIONS];
  int ndims_bi, dsizes_bi[NCL_MAX_DIMENSIONS];
  int ndims_cr, dsizes_cr[NCL_MAX_DIMENSIONS];
  int ndims_ci, dsizes_ci[NCL_MAX_DIMENSIONS];
  int nt, nlat, nlon;
/*
 * Output array variables
 */
  float *u, *v;
  int ndims_u, dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v, dsizes_v[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  int i, j, l, ityp, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhsgc;
  float *work, *wvhsgc;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  br = (float*)NclGetArgValue(
			      0,
           6,
           &ndims_br,
           dsizes_br,
			      NULL,
			      NULL,
           NULL,
           2);
  bi = (float*)NclGetArgValue(
			      1,
           6,
           &ndims_bi, 
           dsizes_bi,
			      NULL,
			      NULL,
           NULL,
           2);
  cr = (float*)NclGetArgValue(
			      2,
           6,
           &ndims_cr,
           dsizes_cr,
			      NULL,
			      NULL,
           NULL,
           2);
  ci = (float*)NclGetArgValue(
			      3,
           6,
           &ndims_ci, 
           dsizes_ci,
			      NULL,
			      NULL,
           NULL,
           2);
/*
 * The grids coming in must be at least 2-dimensional.
 */
  if( ndims_br != ndims_bi || ndims_br != ndims_ci || ndims_br != ndims_cr ||
      ndims_bi != ndims_ci || ndims_bi != ndims_cr ||
      ndims_cr != ndims_ci || ndims_br < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsgc: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_br; i++ ) {
    if( dsizes_br[i] != dsizes_bi[i] || dsizes_br[i] != dsizes_ci[i] ||
	dsizes_br[i] != dsizes_cr[i] || dsizes_bi[i] != dsizes_cr[i] ||
	dsizes_bi[i] != dsizes_ci[i] || dsizes_ci[i] != dsizes_cr[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsgc: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Compute the total number of elements in our array.
 */
  nt = 1;
  for(i = 0; i < ndims_br-2; i++) {
    nt *= dsizes_br[i];
  }
/*
 * Get in/output arrays.
 */
  u = (float*)NclGetArgValue(
           4,
           6,
           &ndims_u, 
           dsizes_u,
	   NULL,
           NULL,
           NULL,
           1);
  v = (float*)NclGetArgValue(
           5,
           6,
           &ndims_v, 
           dsizes_v,
	   NULL,
           NULL,
           NULL,
           1);
  nlat = dsizes_u[ndims_u-2];
  nlon = dsizes_u[ndims_u-1];
/*
 * The output arrays must also be at least 2-dimensional.
 */
  if( ndims_u != ndims_br || ndims_v != ndims_br ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsgc: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_br-2; i++ ) {
    if( dsizes_u[i] != dsizes_br[i] || dsizes_v[i] != dsizes_br[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsgc: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  if((dsizes_br[ndims_br-1] != dsizes_br[ndims_br-2])||(dsizes_br[ndims_br-2]!=dsizes_u[ndims_u-2])) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsgc: The last two dimensions of input coefficient arrays must be the same size and have the same number of latitude points as the output grid");
    return(NhlFATAL);
  }
  ityp   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = nlat;
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;

  lwork  = max(4*nlat*(nlat+1)+2,nlat*(2*nt*nlon+max(6*l2,nlon)));
  lvhsgc = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15;
  ldwork = 2*nlat*(nlat+1)+1;

  wvhsgc = (float*)calloc(        lvhsgc*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);

  if( wvhsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsgc: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(vhsgci,VHSGCI)(&nlat,&nlon,wvhsgc,&lvhsgc,dwork,&ldwork,&jer);
  NGCALLF(vhsgc,VHSGC)(&nlat,&nlon,&ityp,&nt,&v[0],&u[0],
		       &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
		       wvhsgc,&lvhsgc,work,&lwork,&ker);
  free(wvhsgc);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("vhsgc","vhsgc",&ier,&jer,&ker,&mer,5,5);
/* 
 * transform from math coordinates to geophysical coordinates
 * (math) nlon is the first dim
 */
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsgc: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeov,MATGEOV)(&nlat,&nlon,&u[j],&v[j],work);
    j += nlat*nlon;
  }
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return
 */
  return(NhlNOERROR);
}


NhlErrorTypes shaec_W( void )
{
/*
 * Input array variables
 */
  float *g;
  int ndims_g, dsizes_g[NCL_MAX_DIMENSIONS];
  int nt, nlat, nlon;
/*
 * Output array variables
 */
  float *a, *b;
  int ndims_a, dsizes_a[NCL_MAX_DIMENSIONS];
  int ndims_b, dsizes_b[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  int i, j, l, isym, idg, jdg, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0;
/*
 * Workspace variables
 */
  int lwork, ldwork, lshaec;
  float *work, *wshaec;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  g = (float*)NclGetArgValue(
			     0,
           3,
           &ndims_g,
           dsizes_g,
			     NULL,
			     NULL,
           NULL,
           2);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if(ndims_g < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shaec: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }
  nlat = dsizes_g[ndims_g-2];
  nlon = dsizes_g[ndims_g-1];
/*
 * Get coefficient arrays.
 */
  a = (float*)NclGetArgValue(
           1,
           3,
           &ndims_a, 
           dsizes_a,
	   NULL,
           NULL,
           NULL,
           1);
  b = (float*)NclGetArgValue(
           2,
           3,
           &ndims_b, 
           dsizes_b,
	   NULL,
           NULL,
           NULL,
           1);
/*
 * The output arrays must have same dimensionality as input arrays, except
 * for last two dimensions.
 */
  if( ndims_a != ndims_g || ndims_b != ndims_g ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shaec: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_g-2; i++ ) {
    if( dsizes_a[i] != dsizes_g[i] || dsizes_b[i] != dsizes_g[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"shaec: The first n-2 dimensions of the input/output arrays must be the same size");
      return(NhlFATAL);
    }
  }
/*
 * Check last two dimensions of a and b.
 */
  ndab   = nlat;
  if (nlon % 2) {
    mdab   = min(nlat,(nlon+1)/2);
  }
  else {
    mdab   = min(nlat,(nlon+2)/2);
  }
  if(dsizes_a[ndims_a-1] != mdab || dsizes_a[ndims_a-2] != ndab) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shaec: The last two dimensions of the output coefficient arrays must be nlat x min(nlat,(nlon+2)/2) if nlon is even or nlat x min(nlat,(nlon+1)/2) if nlon is odd");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array, minus last
 * two dimensions.
 */
  nt = 1;
  for(i = 0; i < ndims_g-2; i++) {
    nt *= dsizes_g[i];
  }

/*
 * Determine the workspace size.
 */
  lwork = nlon * nlat;
  work = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shaec: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(geomat,GEOMAT)(&nlon,&nlat,&g[j],work);
    j += nlat*nlon;
  }
  free(work);
/*
 * shaec performs the spherical harmonic analysis on a (scalar) equal 
 * grid(s) and returns the coefficients in array(s) a,b
 * Here the scalar grid is "g" 
 */
  isym   = 0;
  idg    = nlat;
  jdg    = nlon;
  l1     = mdab;
  l2     = (nlat+1)/2;

  lwork  = max(2*(nlat+1),nlat*(nt*nlon+max(3*l2,nlon)));
  lshaec = 2*nlat*l2+3*(max(l1-2,0)*(2*nlat-l1-1))/2+nlon+15;
  ldwork = nlat+1;

  wshaec = (float*)calloc(        lshaec*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  dwork =  (double*)calloc(       ldwork*sizeof(double),1);

  if( wshaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shaec: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(shaeci,SHAECI)(&nlat,&nlon,wshaec,&lshaec,dwork,&ldwork,&jer);
  NGCALLF(shaec,SHAEC)(&nlat,&nlon,&isym,&nt,&g[0],&idg,&jdg,a,b,
                       &mdab,&ndab,wshaec,&lshaec,work,&lwork,&ker);
  free(wshaec);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("shaec","shaec",&ier,&jer,&ker,&mer,5,5);
/* 
 * transform from math coordinates to geophysical coordinates
 * (math) nlon is the first dim
 */
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shaec: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&g[j],work);
    j += nlat*nlon;
  }
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return
 */
  return(NhlNOERROR);
}


NhlErrorTypes shagc_W( void )
{
/*
 * Input array variables
 */
  float *g;
  int ndims_g, dsizes_g[NCL_MAX_DIMENSIONS];
  int nt, nlat, nlon;
/*
 * Output array variables
 */
  float *a, *b;
  int ndims_a, dsizes_a[NCL_MAX_DIMENSIONS];
  int ndims_b, dsizes_b[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  int i, j, l, isym, idg, jdg, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0;
/*
 * Workspace variables
 */
  int lwork, ldwork, lshagc;
  float *work, *wshagc;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  g = (float*)NclGetArgValue(
			     0,
           3,
           &ndims_g,
           dsizes_g,
			     NULL,
			     NULL,
           NULL,
           2);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if(ndims_g < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shagc: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }
  nlat = dsizes_g[ndims_g-2];
  nlon = dsizes_g[ndims_g-1];
/*
 * Get coefficient arrays.
 */
  a = (float*)NclGetArgValue(
           1,
           3,
           &ndims_a, 
           dsizes_a,
	   NULL,
           NULL,
           NULL,
           1);
  b = (float*)NclGetArgValue(
           2,
           3,
           &ndims_b, 
           dsizes_b,
	   NULL,
           NULL,
           NULL,
           1);
/*
 * The output arrays must have same dimensionality as input arrays, except
 * for last two dimensions.
 */
  if( ndims_a != ndims_g || ndims_b != ndims_g ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shagc: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_g-2; i++ ) {
    if( dsizes_a[i] != dsizes_g[i] || dsizes_b[i] != dsizes_g[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"shagc: The first n-2 dimensions of the input/output arrays must be the same size");
      return(NhlFATAL);
    }
  }
/*
 * Check last two dimensions of a and b.
 */
  ndab   = nlat;
  if (nlon % 2) {
    mdab   = min(nlat,(nlon+1)/2);
  }
  else {
    mdab   = min(nlat,(nlon+2)/2);
  }

  if(dsizes_a[ndims_a-1] != mdab || dsizes_a[ndims_a-2] != ndab) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shagc: The last two dimensions of the output coefficient arrays must be nlat x min(nlat,(nlon+2)/2) if nlon is even or nlat x min(nlat,(nlon+1)/2) if nlon is odd");
    return(NhlFATAL);
  }
  
/*
 * Compute the total number of elements in our array, minus last
 * two dimensions.
 */
  nt = 1;
  for(i = 0; i < ndims_g-2; i++) {
    nt *= dsizes_g[i];
  }

/*
 * Determine the workspace size.
 */
  lwork = nlon * nlat;
  work = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shagc: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(geomat,GEOMAT)(&nlon,&nlat,&g[j],work);
    j += nlat*nlon;
  }
  free(work);
/*
 * shagc performs the spherical harmonic analysis on a (scalar) gaussian
 * grid(s) and returns the coefficients in array(s) a,b
 * Here the scalar grid is "g" 
 */
  isym   = 0;
  idg    = nlat;
  jdg    = nlon;
  l1     = mdab;
  l2     = (nlat+1)/2;

  lwork  = max(4*nlat*(nlat+2)+2,nlat*(nlon*nt+max(3*l2,nlon)));
  lshagc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;
  ldwork = nlat*(nlat+4);

  wshagc = (float*)calloc(        lshagc*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  dwork =  (double*)calloc(       ldwork*sizeof(double),1);

  if( wshagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shagc: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(shagci,SHAGCI)(&nlat,&nlon,wshagc,&lshagc,dwork,&ldwork,&jer);
  NGCALLF(shagc,SHAGC)(&nlat,&nlon,&isym,&nt,&g[0],&idg,&jdg,a,b,
                       &mdab,&ndab,wshagc,&lshagc,work,&lwork,&ker);
  free(wshagc);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("shagc","shagc",&ier,&jer,&ker,&mer,5,5);
/* 
 * transform from math coordinates to geophysical coordinates
 * (math) nlon is the first dim
 */
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shagc: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&g[j],work);
    j += nlat*nlon;
  }
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return
 */
  return(NhlNOERROR);
}


NhlErrorTypes shsec_W( void )
{
/*
 * Input array variables
 */
  float *a, *b;
  int ndims_a, dsizes_a[NCL_MAX_DIMENSIONS];
  int ndims_b, dsizes_b[NCL_MAX_DIMENSIONS];
  int nt, nlat, nlon;
/*
 * Output array variables
 */
  float *g;
  int ndims_g, dsizes_g[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  int i, j, l, isym, idg, jdg, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0;
/*
 * Workspace variables
 */
  int lwork, ldwork, lshsec;
  float *work, *wshsec;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  a = (float*)NclGetArgValue(
           0,
           3,
           &ndims_a, 
           dsizes_a,
	   NULL,
           NULL,
           NULL,
           2);
  b = (float*)NclGetArgValue(
           1,
           3,
           &ndims_b, 
           dsizes_b,
	   NULL,
           NULL,
           NULL,
           2);
/*
 * The grids coming in must be at least 2-dimensional.
 */
  if(ndims_a != ndims_b || ndims_a < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsec: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_a; i++ ) {
    if( dsizes_a[i] != dsizes_b[i] ){
      NhlPError(NhlFATAL,NhlEUNKNOWN,"shsec: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }

/*
 * Get output array.
 */
  g = (float*)NclGetArgValue(
			     2,
           3,
           &ndims_g,
           dsizes_g,
			     NULL,
			     NULL,
           NULL,
           1);
  nlat = dsizes_g[ndims_g-2];
  nlon = dsizes_g[ndims_g-1];
/*
 * The input/output arrays must have the same number of dimensions and
 * all but the last two dimension sizes must be the same.
 */
  if( ndims_g != ndims_a ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsec: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_a-2; i++ ) {
    if( dsizes_g[i] != dsizes_a[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"shsec: The first n-2 dimensions of the input/output arrays must be the same size");
      return(NhlFATAL);
    }
  }
/*
 * Check last two dimensions of a and b.
 */
  ndab   = nlat;
  if (nlon % 2) {
    mdab   = min(nlat,(nlon+1)/2);
  }
  else {
    mdab   = min(nlat,(nlon+2)/2);
  }
  if(dsizes_a[ndims_a-1] != mdab || dsizes_a[ndims_a-2] != ndab) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsec: The last two dimensions of the coefficient arrays must be nlat x min(nlat,(nlon+2)/2) if nlon is even or nlat x min(nlat,(nlon+1)/2) if nlon is odd");
    return(NhlFATAL);
  }

/*
 * Compute the total number of elements in our array, minus last
 * two dimensions.
 */
  nt = 1;
  for(i =0; i < ndims_a-2; nt*=dsizes_a[i],i++);

/*
 * Calculate work space sizes.
 */
  isym   = 0; 
  idg    = nlat;
  jdg    = nlon;
  l1     = mdab;
  l2     = (nlat+1)/2;

  lwork  = nlat*(nlon*nt+max(3*l2,nlon));
  lshsec = 2*nlat*l2+3*(max(l1-2,0)*(2*nlat-l1-1))/2+nlon+15;
  ldwork = nlat+1;

  wshsec = (float*)calloc(        lshsec*sizeof(float),1);
  work   = (float*)calloc(         lwork*sizeof(float),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);

  if( wshsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsec: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(shseci,SHSECI)(&nlat,&nlon,wshsec,&lshsec,dwork,&ldwork,&jer);
  NGCALLF(shsec,SHSEC)(&nlat,&nlon,&isym,&nt,&g[0],&idg,&jdg,a,b,
                       &mdab,&ndab,wshsec,&lshsec,work,&lwork,&ker);
  free(wshsec);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("shsec","shseci+shsec",&ier,&jer,&ker,&mer,5,12);
/* 
 * transform from math coordinates to geophysical coordinates
 * (math) nlon is the first dim
 */
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsec: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&g[j],work);
    j += nlat*nlon;
  }
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return
 */
  return(NhlNOERROR);
}


NhlErrorTypes shsgc_W( void )
{
/*
 * Input array variables
 */
  float *a, *b;
  int ndims_a, dsizes_a[NCL_MAX_DIMENSIONS];
  int ndims_b, dsizes_b[NCL_MAX_DIMENSIONS];
  int nt, nlat, nlon;
/*
 * Output array variables
 */
  float *g;
  int ndims_g, dsizes_g[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  int i, j, l, isym, idg, jdg, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0;
/*
 * Workspace variables
 */
  int lwork, ldwork, lshsgc;
  float *work, *wshsgc;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  a = (float*)NclGetArgValue(
           0,
           3,
           &ndims_a, 
           dsizes_a,
	   NULL,
           NULL,
           NULL,
           2);
  b = (float*)NclGetArgValue(
           1,
           3,
           &ndims_b, 
           dsizes_b,
	   NULL,
           NULL,
           NULL,
           2);
/*
 * The grids coming in must be at least 2-dimensional.
 */
  if(ndims_a != ndims_b || ndims_a < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgc: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_a; i++ ) {
    if( dsizes_a[i] != dsizes_b[i] ){
      NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgc: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }

/*
 * Get output array.
 */
  g = (float*)NclGetArgValue(
			     2,
           3,
           &ndims_g,
           dsizes_g,
			     NULL,
			     NULL,
           NULL,
           1);
  nlat = dsizes_g[ndims_g-2];
  nlon = dsizes_g[ndims_g-1];
/*
 * The input/output arrays must have the same number of dimensions and
 * all but the last two dimension sizes must be the same.
 */
  if( ndims_g != ndims_a ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgc: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_a-2; i++ ) {
    if( dsizes_g[i] != dsizes_a[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgc: The first n-2 dimensions of the input/output arrays must be the same size");
      return(NhlFATAL);
    }
  }
/*
 * Check last two dimensions of a and b.
 */
  ndab   = nlat;
  if (nlon % 2) {
    mdab   = min(nlat,(nlon+1)/2);
  }
  else {
    mdab   = min(nlat,(nlon+2)/2);
  }
  if(dsizes_a[ndims_a-1] != mdab || dsizes_a[ndims_a-2] != ndab) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgc: The last two dimensions of the coefficient arrays must be nlat x min(nlat,(nlon+2)/2) if nlon is even or nlat x min(nlat,(nlon+1)/2) if nlon is odd");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array, minus last
 * two dimensions.
 */
  nt = 1;
  for(i =0; i < ndims_a-2; nt*=dsizes_a[i],i++);

/*
 * Calculate work space sizes.
 */
  isym   = 0;
  idg    = nlat;
  jdg    = nlon;
  l1     = mdab;
  l2     = (nlat+1)/2;

  lwork  = max(4*nlat*(nlat+2)+2,nlat*(nlon*nt+max(3*l2,nlon)));
  ldwork = nlat*(nlat+4);
  lshsgc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;

  wshsgc = (float*)calloc(        lshsgc*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  dwork = (double*)calloc(ldwork*sizeof(double),1);

  if( wshsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgc: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(shsgci,SHSGCI)(&nlat,&nlon,wshsgc,&lshsgc,dwork,&ldwork,&jer);
  NGCALLF(shsgc,SHSGC)(&nlat,&nlon,&isym,&nt,&g[0],&idg,&jdg,a,b,
                       &mdab,&ndab,wshsgc,&lshsgc,work,&lwork,&ker);
  free(wshsgc);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("shsgc","shsgci+shsgc",&ier,&jer,&ker,&mer,5,12);
/* 
 * transform from math coordinates to geophysical coordinates
 * (math) nlon is the first dim
 */
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgc: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&g[j],work);
    j += nlat*nlon;
  }
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return
 */
  return(NhlNOERROR);
}


NhlErrorTypes shseC_W( void )
{
/*
 * Input array variables
 */
  float *ab;
  int ndims_ab, dsizes_ab[NCL_MAX_DIMENSIONS];
  int nt, nlat, *nlon;
/*
 * Output array variables
 */
  float *g;
  int ndims_g, dsizes_g[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  int i, j, l, isym, idg, jdg, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0;
/*
 * Workspace variables
 */
  int lwork, ldwork, lshsec;
  float *work, *wshsec;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  ab = (float*)NclGetArgValue(
           0,
           2,
           &ndims_ab, 
           dsizes_ab,
	   NULL,
           NULL,
           NULL,
           2);
/*
 * The grid coming in must be at least 3-dimensional.
 */
  if(ndims_ab < 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shseC: The input array must be at least 3-dimensional");
    return(NhlFATAL);
  }
  if(dsizes_ab[0] != 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shseC: The first dimension of the input array must be 2");
    return(NhlFATAL);
  }
/*
 * Get nlon
 */
  nlon = (int*)NclGetArgValue(
           1,
           2,
           NULL,
           NULL,
	   NULL,
           NULL,
           NULL,
           2);

/*
 * Check the last dimension of ab.
 */
  ndab = nlat = dsizes_ab[ndims_ab-2];
  if (*nlon % 2) {
    mdab   = min(nlat,(*nlon+1)/2);
  }
  else {
    mdab   = min(nlat,(*nlon+2)/2);
  }
  if(dsizes_ab[ndims_ab-1] != mdab) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shseC: The last dimension of the coefficient array must be min(nlat,(nlon+2)/2) if nlon is even or min(nlat,(nlon+1)/2) if nlon is odd");
    return(NhlFATAL);
  }

/*
 * Compute the total number of elements in our array, minus last
 * two dimensions.
 */
  nt = 1;
  for(i = 1; i < ndims_ab-2; nt*=dsizes_ab[i],i++);

/*
 * Compute size for output array.
 */
  g = (float *)calloc(*nlon*nlat*nt*sizeof(float),1);
  if( g == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shseC: Unable to allocate memory for ouput array");
    return(NhlFATAL);
  }

/*
 * Calculate work space sizes.
 */
  isym   = 0;
  idg    = nlat;
  jdg    = *nlon;
  l1     = mdab;
  l2     = (nlat+1)/2;

  lwork  = nlat*(*nlon*nt+max(3*l2,*nlon));
  lshsec = 2*nlat*l2+3*(max(l1-2,0)*(2*nlat-l1-1))/2+ *nlon +15;
  ldwork = nlat+1;

  wshsec = (float*)calloc(        lshsec*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);

  if( wshsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shseC: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(shseci,SHSECI)(&nlat,nlon,wshsec,&lshsec,dwork,&ldwork,&jer);
  j = nt * ndab * mdab;
  NGCALLF(shsec,SHSEC)(&nlat,nlon,&isym,&nt,&g[0],&idg,&jdg,&ab[0],&ab[j],
		       &mdab,&ndab,wshsec,&lshsec,work,&lwork,&ker);
  free(wshsec);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("shseC","shseci+shsec",&ier,&jer,&ker,&mer,5,12);
/* 
 * transform from math coordinates to geophysical coordinates
 * (math) nlon is the first dim
 */
  lwork = *nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shseC: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeo,MATGEO)(&nlat,nlon,&g[j],work);
    j += *nlon*nlat;
  }
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return
 */
  ndims_g = ndims_ab-1;
  for( i = 0; i <= ndims_g-2; i++ ) dsizes_g[i] = dsizes_ab[i+1];
  dsizes_g[ndims_g-1] = *nlon;
  return(NclReturnValue((void*)g,ndims_g,dsizes_g,NULL,NCL_float,0));
}


NhlErrorTypes shsgC_W( void )
{
/*
 * Input array variables
 */
  float *ab;
  int ndims_ab, dsizes_ab[NCL_MAX_DIMENSIONS];
  int nt, nlat, *nlon;
/*
 * Output array variables
 */
  float *g;
  int ndims_g, dsizes_g[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  int i, j, l, isym, idg, jdg, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0;
/*
 * Workspace variables
 */
  int lwork, ldwork, lshsgc;
  float *work, *wshsgc;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  ab = (float*)NclGetArgValue(
           0,
           2,
           &ndims_ab, 
           dsizes_ab,
	   NULL,
           NULL,
           NULL,
           2);
/*
 * The grid coming in must be at least 3-dimensional.
 */
  if(ndims_ab < 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgC: The input array must be at least 3-dimensional");
    return(NhlFATAL);
  }
  if(dsizes_ab[0] != 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgC: The first dimension of the input array must be 2");
    return(NhlFATAL);
  }
/*
 * Get nlon
 */
  nlon = (int*)NclGetArgValue(
           1,
           2,
           NULL,
           NULL,
	   NULL,
           NULL,
           NULL,
           2);
/*
 * Check the last dimension of ab.
 */
  ndab = nlat = dsizes_ab[ndims_ab-2];
  if (*nlon % 2) {
    mdab   = min(nlat,(*nlon+1)/2);
  }
  else {
    mdab   = min(nlat,(*nlon+2)/2);
  }
  if(dsizes_ab[ndims_ab-1] != mdab) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgC: The last dimension of the coefficient array must be min(nlat,(nlon+2)/2) if nlon is even or min(nlat,(nlon+1)/2) if nlon is odd");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array, minus last
 * two dimensions.
 */
  nt = 1;
  for(i =1; i < ndims_ab-2; nt*=dsizes_ab[i],i++);

/*
 * Compute size for output array.
 */
  g = (float *)calloc(*nlon*nlat*nt*sizeof(float),1);
  if( g == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgC: Unable to allocate memory for ouput array");
    return(NhlFATAL);
  }

/*
 * Calculate work space sizes.
 */
  isym   = 0;
  idg    = nlat;
  jdg    = *nlon;
  l1     = mdab;
  l2     = (nlat+1)/2;

  lwork  = max(4*nlat*(nlat+2)+2,nlat*(*nlon*nt+max(3*l2,*nlon)));
  ldwork = nlat*(nlat+4);
  lshsgc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+ *nlon +15;

  wshsgc = (float*)calloc(        lshsgc*sizeof(float),1);
  work   = (float*)calloc(         lwork*sizeof(float),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);

  if( wshsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgC: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(shsgci,SHSGCI)(&nlat,nlon,wshsgc,&lshsgc,dwork,&ldwork,&jer);
  j = nt * ndab * mdab;
  NGCALLF(shsgc,SHSGC)(&nlat,nlon,&isym,&nt,&g[0],&idg,&jdg,&ab[0],&ab[j],
		       &mdab,&ndab,wshsgc,&lshsgc,work,&lwork,&ker);
  free(wshsgc);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("shsgC","shsgci+shsgc",&ier,&jer,&ker,&mer,5,12);
/* 
 * transform from math coordinates to geophysical coordinates
 * (math) nlon is the first dim
 */
  lwork = *nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgC: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeo,MATGEO)(&nlat,nlon,&g[j],work);
    j += *nlon*nlat;
  }
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return
 */
  ndims_g = ndims_ab-1;
  for( i = 0; i <= ndims_g-2; i++ ) dsizes_g[i] = dsizes_ab[i+1];
  dsizes_g[ndims_g-1] = *nlon;
  return(NclReturnValue((void*)g,ndims_g,dsizes_g,NULL,NCL_float,0));
}


NhlErrorTypes shaeC_W( void )
{
/*
 * Input array variables
 */
  float *g;
  int ndims_g, dsizes_g[NCL_MAX_DIMENSIONS];
  int nt, nlat, nlon;
/*
 * Output array variables
 */
  float *ab;
  int ndims_ab, dsizes_ab[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  int i, j, l, isym, idg, jdg, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0;
/*
 * Workspace variables
 */
  int lwork, ldwork, lshaec;
  float *work, *wshaec;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  g = (float*)NclGetArgValue(
			     0,
           1,
           &ndims_g,
           dsizes_g,
			     NULL,
			     NULL,
           NULL,
           2);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if(ndims_g < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shaeC: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }
  nlat = dsizes_g[ndims_g-2];
  nlon = dsizes_g[ndims_g-1];
/*
 * Compute the total number of elements in our array, minus last
 * two dimensions.
 */
  nt = 1;
  for(i =0; i < ndims_g-2; nt*=dsizes_g[i],i++);

/*
 * Compute size for coefficient array.
 */
  ndab   = nlat;
  if (nlon % 2) {
    mdab   = min(nlat,(nlon+1)/2);
  }
  else {
    mdab   = min(nlat,(nlon+2)/2);
  }

  ab  = (float*)calloc(2*ndab*mdab*nt*sizeof(float),1);
  if( ab == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shaeC: Unable to allocate memory for coefficient arrays");
    return(NhlFATAL);
  }

/*
 * Determine the workspace size.
 */
  lwork = nlon * nlat;
  work = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shaeC: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(geomat,GEOMAT)(&nlon,&nlat,&g[j],work);
    j += nlat*nlon;
  }
  free(work);

/*
 * Calculate work space sizes.
 */
  isym   = 0;
  idg    = nlat;
  jdg    = nlon;
  l1     = mdab;
  l2     = (nlat+1)/2;

  lwork  = max(2*(nlat+1),nlat*(nt*nlon+max(3*l2,nlon)));
  lshaec = 2*nlat*l2+3*(max(l1-2,0)*(2*nlat-l1-1))/2+nlon+15;
  ldwork = nlat+1;

  wshaec = (float*)calloc(        lshaec*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  dwork =  (double*)calloc(       ldwork*sizeof(double),1);

  if( wshaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shaeC: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(shaeci,SHAECI)(&nlat,&nlon,wshaec,&lshaec,dwork,&ldwork,&jer);
  j = nt * ndab * mdab;
  NGCALLF(shaec,SHAEC)(&nlat,&nlon,&isym,&nt,&g[0],&idg,&jdg,&ab[0],&ab[j],
                       &mdab,&ndab,wshaec,&lshaec,work,&lwork,&ker);
  free(wshaec);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("shaeC","shaec",&ier,&jer,&ker,&mer,5,5);
/* 
 * transform from math coordinates to geophysical coordinates
 * (math) nlon is the first dim
 */
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shaeC: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&g[j],work);
    j += nlat*nlon;
  }
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return
 */
  ndims_ab = ndims_g + 1;
  dsizes_ab[0] = 2;
  for( i = 1; i <= ndims_ab-2; i++ ) dsizes_ab[i] = dsizes_g[i-1];
  dsizes_ab[ndims_ab-1] = mdab;
  return(NclReturnValue((void*)ab,ndims_ab,dsizes_ab,NULL,NCL_float,0));
}

NhlErrorTypes shagC_W( void )
{
/*
 * Input array variables
 */
  float *g;
  int ndims_g, dsizes_g[NCL_MAX_DIMENSIONS];
  int nt, nlat, nlon;
/*
 * Output array variables
 */
  float *ab;
  int ndims_ab, dsizes_ab[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  int i, j, l, isym, idg, jdg, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0;
/*
 * Workspace variables
 */
  int lwork, ldwork, lshagc;
  float *work, *wshagc;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  g = (float*)NclGetArgValue(
			     0,
           1,
           &ndims_g,
           dsizes_g,
			     NULL,
			     NULL,
           NULL,
           2);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if(ndims_g < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shagC: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }
  nlat = dsizes_g[ndims_g-2];
  nlon = dsizes_g[ndims_g-1];

/*
 * Compute the total number of elements in our array, minus last
 * two dimensions.
 */
  nt = 1;
  for(i =0; i < ndims_g-2; nt*=dsizes_g[i],i++);

/*
 * Compute size for coefficient array.
 */
  ndab   = nlat;
  if (nlon % 2) {
    mdab   = min(nlat,(nlon+1)/2);
  }
  else {
    mdab   = min(nlat,(nlon+2)/2);
  }

  ab  = (float*)calloc(2*ndab*mdab*nt*sizeof(float),1);
  if( ab == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shagC: Unable to allocate memory for coefficient arrays");
    return(NhlFATAL);
  }

/*
 * Determine the workspace size.
 */
  lwork = nlon * nlat;
  work = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shagC: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(geomat,GEOMAT)(&nlon,&nlat,&g[j],work);
    j += nlat*nlon;
  }
  free(work);

/*
 * Calculate work space sizes.
 */
  isym   = 0;
  idg    = nlat;
  jdg    = nlon;
  l1     = mdab;
  l2     = (nlat+1)/2;

  lwork  = max(4*nlat*(nlat+2)+2,nlat*(nlon*nt+max(3*l2,nlon)));
  lshagc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;
  ldwork = nlat*(nlat+4);

  wshagc = (float*)calloc(        lshagc*sizeof(float),1);
  work =   (float*)calloc(         lwork*sizeof(float),1);
  dwork =  (double*)calloc(       ldwork*sizeof(double),1);

  if( wshagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shagC: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(shagci,SHAGCI)(&nlat,&nlon,wshagc,&lshagc,dwork,&ldwork,&jer);
  j = nt * ndab * mdab;
  NGCALLF(shagc,SHAGC)(&nlat,&nlon,&isym,&nt,&g[0],&idg,&jdg,&ab[0],&ab[j],
                       &mdab,&ndab,wshagc,&lshagc,work,&lwork,&ker);
  free(wshagc);
  free(work);
  free(dwork);
  NGCALLF(chkerr,CHKERR)("shagC","shagc",&ier,&jer,&ker,&mer,5,5);
/* 
 * transform from math coordinates to geophysical coordinates
 * (math) nlon is the first dim
 */
  lwork = nlon * nlat;
  work   = (float*)calloc( lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shagC: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(matgeo,MATGEO)(&nlat,&nlon,&g[j],work);
    j += nlat*nlon;
  }
/*
 * Free workspace array.
 */
  free(work);
/*
 * Return
 */
  ndims_ab = ndims_g + 1;
  dsizes_ab[0] = 2;
  for( i = 1; i <= ndims_ab-2; i++ ) dsizes_ab[i] = dsizes_g[i-1];
  dsizes_ab[ndims_ab-1] = mdab;
  return(NclReturnValue((void*)ab,ndims_ab,dsizes_ab,NULL,NCL_float,0));
}



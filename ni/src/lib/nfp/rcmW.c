#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(drcm2rgrid,DRCM2RGRID)(int *,int *,int *,double *,double *,
                                           double *,int *,double *,int*,
                                           double *,double *,double*,
                                           int *,int *,int *);

extern void NGCALLF(drgrid2rcm,DRGRID2RCM)(int *,int *,int *,double *,double *,
                                           double *,int *,int *,double *,
                                           double *,double *,double*,
                                           int *,int *,int *);

extern void NGCALLF(drcm2points,DRCM2POINTS)(int *,int *,int *,double *,
                                             double *,double *,int *,double *,
                                             double *,double *,double*,
                                             int *,int *,int *);


NhlErrorTypes rcm2rgrid_W( void )
{
/*
 * Input variables
 */
  void *lat2d, *lon2d, *fi, *lat1d, *lon1d, *opt;
  double *tmp_lat2d, *tmp_lon2d, *tmp_lat1d, *tmp_lon1d, *tmp_fi;
  int tmp_opt, tmp_ncrit;
  int dsizes_lat2d[2], dsizes_lon2d[2], dsizes_lat1d[2], dsizes_lon1d[1];
  int size_fi, ndims_fi, dsizes_fi[NCL_MAX_DIMENSIONS], has_missing_fi;
  NclScalar missing_fi, missing_dfi, missing_rfi;
  NclBasicDataTypes type_lat2d, type_lon2d, type_lat1d, type_lon1d;
  NclBasicDataTypes type_fi, type_opt;
/*
 * Output variables.
 */
  void *fo;
  double *tmp_fo;
  int *dsizes_fo;
  NclBasicDataTypes type_fo;
  NclScalar missing_fo;
/*
 * Other variables
 */
  int nlon2d, nlat2d, nfi, nlat1d, nlon1d, nfo, ngrid, size_fo;
  int i, ier, ret;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  lat2d = (void*)NclGetArgValue(
          0,
          6,
          NULL,
          dsizes_lat2d,
          NULL,
          NULL,
          &type_lat2d,
          DONT_CARE);

  lon2d = (void*)NclGetArgValue(
          1,
          6,
          NULL,
          dsizes_lon2d,
          NULL,
          NULL,
          &type_lon2d,
          DONT_CARE);

  fi = (void*)NclGetArgValue(
          2,
          6,
          &ndims_fi,
          dsizes_fi,
          &missing_fi,
          &has_missing_fi,
          &type_fi,
          DONT_CARE);

  lat1d = (void*)NclGetArgValue(
          3,
          6,
          NULL,
          dsizes_lat1d,
          NULL,
          NULL,
          &type_lat1d,
          DONT_CARE);

  lon1d = (void*)NclGetArgValue(
          4,
          6,
          NULL,
          dsizes_lon1d,
          NULL,
          NULL,
          &type_lon1d,
          DONT_CARE);

  opt = (void*)NclGetArgValue(
          5,
          6,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_opt,
          DONT_CARE);
/*
 * Check the input lat/lon arrays. They must be the same size, and larger
 * than one element.
 */
  if(dsizes_lat2d[0] != dsizes_lon2d[0] ||
     dsizes_lat2d[1] != dsizes_lon2d[1]) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rcm2rgrid: The input lat/lon grids must be the same size");
    return(NhlFATAL);
  }

  nlat2d = dsizes_lat2d[0];
  nlon2d = dsizes_lat2d[1];     /* same as dsizes_lon2d[1] */
  nlat1d = dsizes_lat1d[0];
  nlon1d = dsizes_lon1d[0];

  if(nlon2d <= 1 || nlat2d <= 1 || nlat1d <= 1 || nlon1d <= 1) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rcm2rgrid: The input/output lat/lon grids must have at least 2 elements");
    return(NhlFATAL);
  }

/*
 * Compute the total number of elements in our arrays.
 */
  nfi  = nlon2d * nlat2d;
  nfo  = nlat1d * nlon1d;

/*
 * Check dimensions of fi.
 */
  if(ndims_fi < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rcm2rgrid: fi must be at least two dimensions");
    return(NhlFATAL);
  }
  if(dsizes_fi[ndims_fi-2] != nlat2d || dsizes_fi[ndims_fi-1] != nlon2d) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rcm2rgrid: The rightmost dimensions of fi must be nlat2d x nlon2d, where nlat2d and nlon2d are the dimensions of the lat2d/lon2d arrays");
    return(NhlFATAL);
  }
/*
 * Compute the total size of the input/output arrays.
 */
  ngrid = 1;
  for( i = 0; i < ndims_fi-2; i++ ) ngrid *= dsizes_fi[i];
  size_fi = ngrid * nfi;
  size_fo = ngrid * nfo;
/*
 * Coerce missing values.
 */
  coerce_missing(type_fi,has_missing_fi,&missing_fi,&missing_dfi,
                 &missing_rfi);
/*
 * Allocate space for output array.
 */
  if(type_fi == NCL_double) {
    fo      = (void*)calloc(size_fo,sizeof(double));
    tmp_fo  = &((double*)fo)[0];
    type_fo = NCL_double;
    missing_fo.doubleval = missing_dfi.doubleval;
  }
  else {
    fo      = (void*)calloc(size_fo,sizeof(float));
    tmp_fo  = (double*)calloc(size_fo,sizeof(double));
    type_fo = NCL_float;
    missing_fo.floatval = missing_rfi.floatval;
    if(tmp_fo == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rcm2rgrid: Unable to allocate memory for temporary array");
      return(NhlFATAL);
    }
  }
  dsizes_fo = (int*)calloc(ndims_fi,sizeof(int));
  if(fo == NULL || dsizes_fo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rcm2rgrid: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_fi-2; i++) dsizes_fo[i] = dsizes_fi[i];
  dsizes_fo[ndims_fi-2] = nlat1d;
  dsizes_fo[ndims_fi-1] = nlon1d;

/*
 * Coerce input arrays to double if necessary.
 */
  tmp_lat2d = coerce_input_double(lat2d,type_lat2d,nfi,0,NULL,NULL);
  tmp_lon2d = coerce_input_double(lon2d,type_lon2d,nfi,0,NULL,NULL);
  tmp_lat1d = coerce_input_double(lat1d,type_lat1d,nlat1d,0,NULL,NULL);
  tmp_lon1d = coerce_input_double(lon1d,type_lon1d,nlon1d,0,NULL,NULL); 
  tmp_fi    = coerce_input_double(fi,type_fi,size_fi,has_missing_fi,
                                  &missing_fi,&missing_dfi);

  if(tmp_lat2d == NULL || tmp_lon2d == NULL ||
     tmp_lat1d == NULL || tmp_lon1d == NULL || tmp_fi == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rcm2rgrid: Unable to coerce input lat/lon arrays to double precision");
    return(NhlFATAL);
  }

/*
 * Force opt to zero and ncrit to 1, since they are not used yet.
 */
  tmp_opt   = 0;
  tmp_ncrit = 1;

  NGCALLF(drcm2rgrid,DRCM2RGRID)(&ngrid,&nlat2d,&nlon2d,tmp_lat2d,tmp_lon2d,
                                 tmp_fi,&nlat1d,tmp_lat1d,&nlon1d,
                                 tmp_lon1d,tmp_fo,&missing_dfi.doubleval,
                                 &tmp_ncrit,&tmp_opt,&ier);

  if(ier) {
    if(ier == 1) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"rcm2rgrid: not enough points in input/output array");
    }
    if(2 <= ier && ier <= 5) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"rcm2rgrid: lat2d, lon2d, lat1d, lon1d must be monotonically increasing");
    }
    set_subset_output_missing(fo,0,type_fo,size_fo,missing_dfi.doubleval);
  }
  else {
    if(type_fo != NCL_double) {
      coerce_output_float_only(fo,tmp_fo,size_fo,0);
    }
  }
/*
 * Free temp arrays.
 */
  if(type_lat2d != NCL_double) NclFree(tmp_lat2d);
  if(type_lon2d != NCL_double) NclFree(tmp_lon2d);
  if(type_lat1d != NCL_double) NclFree(tmp_lat1d);
  if(type_lon1d != NCL_double) NclFree(tmp_lon1d);
  if(type_fi    != NCL_double) NclFree(tmp_fi);
  if(type_fo    != NCL_double) NclFree(tmp_fo);

/*
 * Return.
 */
  ret = NclReturnValue(fo,ndims_fi,dsizes_fo,&missing_fo,type_fo,0);
  NclFree(dsizes_fo);
  return(ret);
}


NhlErrorTypes rgrid2rcm_W( void )
{
/*
 * Input variables
 */
  void *lat2d, *lon2d, *fi, *lat1d, *lon1d, *opt;
  double *tmp_lat2d, *tmp_lon2d, *tmp_lat1d, *tmp_lon1d, *tmp_fi;
  int tmp_opt, tmp_ncrit;
  int dsizes_lat2d[2], dsizes_lon2d[2], dsizes_lat1d[2], dsizes_lon1d[1];
  int ndims_fi, dsizes_fi[NCL_MAX_DIMENSIONS], has_missing_fi;
  NclScalar missing_fi, missing_dfi, missing_rfi;
  NclBasicDataTypes type_lat2d, type_lon2d, type_lat1d, type_lon1d;
  NclBasicDataTypes type_fi, type_opt;
/*
 * Output variables.
 */
  void *fo;
  double *tmp_fo;
  int *dsizes_fo;
  NclBasicDataTypes type_fo;
  NclScalar missing_fo;
/*
 * Other variables
 */
  int nlon2d, nlat2d, nfi, nlat1d, nlon1d, nfo, ngrid, size_fi, size_fo;
  int i, ier, ret;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  lat1d = (void*)NclGetArgValue(
          0,
          6,
          NULL,
          dsizes_lat1d,
          NULL,
          NULL,
          &type_lat1d,
          DONT_CARE);

  lon1d = (void*)NclGetArgValue(
          1,
          6,
          NULL,
          dsizes_lon1d,
          NULL,
          NULL,
          &type_lon1d,
          DONT_CARE);

  fi = (void*)NclGetArgValue(
          2,
          6,
          &ndims_fi,
          dsizes_fi,
          &missing_fi,
          &has_missing_fi,
          &type_fi,
          DONT_CARE);

  lat2d = (void*)NclGetArgValue(
          3,
          6,
          NULL,
          dsizes_lat2d,
          NULL,
          NULL,
          &type_lat2d,
          DONT_CARE);

  lon2d = (void*)NclGetArgValue(
          4,
          6,
          NULL,
          dsizes_lon2d,
          NULL,
          NULL,
          &type_lon2d,
          DONT_CARE);

  opt = (void*)NclGetArgValue(
          5,
          6,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_opt,
          DONT_CARE);
/*
 * Check the output lat/lon arrays. They must be the same size, and larger
 * than one element.
 */
  if(dsizes_lat2d[0] != dsizes_lon2d[0] ||
     dsizes_lat2d[1] != dsizes_lon2d[1]) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rgrid2rcm: The output lat/lon grids must be the same size");
    return(NhlFATAL);
  }

  nlat2d = dsizes_lat2d[0];
  nlon2d = dsizes_lat2d[1];     /* same as dsizes_lon2d[1] */
  nlat1d = dsizes_lat1d[0];
  nlon1d = dsizes_lon1d[0];

  if(nlon2d <= 1 || nlat2d <= 1 || nlat1d <= 1 || nlon1d <= 1) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rgrid2rcm: The input/output lat/lon grids must have at least 2 elements");
    return(NhlFATAL);
  }

/*
 * Compute the total number of elements in our arrays.
 */
  nfi  = nlat1d * nlon1d;
  nfo  = nlon2d * nlat2d;

/*
 * Check dimensions of fi.
 */
  if(ndims_fi < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rgrid2rcm: fi must be at least two dimensions");
    return(NhlFATAL);
  }
  if(dsizes_fi[ndims_fi-2] != nlat1d || dsizes_fi[ndims_fi-1] != nlon1d) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rgrid2rcm: The rightmost dimensions of fi must be nlat1d x nlon1d, where nlat1d and nlon1d are the dimensions of the lat1d/lon1d arrays");
    return(NhlFATAL);
  }
/*
 * Compute the total size of the input/output arrays.
 */
  ngrid = 1;
  for( i = 0; i < ndims_fi-2; i++ ) ngrid *= dsizes_fi[i];
  size_fi = ngrid * nfi;
  size_fo = ngrid * nfo;
/*
 * Coerce missing values.
 */
  coerce_missing(type_fi,has_missing_fi,&missing_fi,&missing_dfi,
                 &missing_rfi);
/*
 * Allocate space for output array.
 */
  if(type_fi == NCL_double) {
    fo      = (void*)calloc(size_fo,sizeof(double));
    tmp_fo  = &((double*)fo)[0];
    type_fo = NCL_double;
    missing_fo.doubleval = missing_dfi.doubleval;
  }
  else {
    tmp_fo  = (double*)calloc(size_fo,sizeof(double));
    fo      = (void*)calloc(size_fo,sizeof(float));
    type_fo = NCL_float;
    missing_fo.floatval = missing_rfi.floatval;
    if(tmp_fo == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rgrid2rcm: Unable to allocate memory for temporary arrays");
      return(NhlFATAL);
    }
  }
  dsizes_fo = (int*)calloc(ndims_fi,sizeof(int));
  if(fo == NULL || dsizes_fo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rgrid2rcm: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_fi-2; i++) dsizes_fo[i] = dsizes_fi[i];
  dsizes_fo[ndims_fi-2] = nlat2d;
  dsizes_fo[ndims_fi-1] = nlon2d;

/*
 * Coerce input arrays to double if necessary.
 */
  tmp_lat2d = coerce_input_double(lat2d,type_lat2d,nfo,0,NULL,NULL);
  tmp_lon2d = coerce_input_double(lon2d,type_lon2d,nfo,0,NULL,NULL);
  tmp_lat1d = coerce_input_double(lat1d,type_lat1d,nlat1d,0,NULL,NULL);
  tmp_lon1d = coerce_input_double(lon1d,type_lon1d,nlon1d,0,NULL,NULL);
  tmp_fi    = coerce_input_double(fi,type_fi,size_fi,has_missing_fi,
                                  &missing_fi,&missing_dfi);

  if(tmp_lat2d == NULL || tmp_lon2d == NULL ||
     tmp_lat1d == NULL || tmp_lon1d == NULL || tmp_fi == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rgrid2rcm: Unable to coerce input lat/lon arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Force opt to zero and ncrit to 1, since they are not used yet.
 */
  tmp_opt   = 0;
  tmp_ncrit = 1;

  NGCALLF(drgrid2rcm,DRGRID2RCM)(&ngrid,&nlat1d,&nlon1d,tmp_lat1d,tmp_lon1d,
                                 tmp_fi,&nlat2d,&nlon2d,tmp_lat2d,
                                 tmp_lon2d,tmp_fo,&missing_dfi.doubleval,
                                 &tmp_ncrit,&tmp_opt,&ier);

  if(ier) {
    if(ier == 1) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"rgrid2rcm: not enough points in input/output array");
    }
    if(2 <= ier && ier <= 5) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"rgrid2rcm: lat2d, lon2d, lat1d, lon1d must be monotonically increasing");
    }
    set_subset_output_missing(fo,0,type_fo,size_fo,missing_dfi.doubleval);
  }
  else {
    if(type_fo != NCL_double) {
      coerce_output_float_only(fo,tmp_fo,size_fo,0);
    }
  }
/*
 * Free temp arrays.
 */
  if(type_lat2d != NCL_double) NclFree(tmp_lat2d);
  if(type_lon2d != NCL_double) NclFree(tmp_lon2d);
  if(type_lat1d != NCL_double) NclFree(tmp_lat1d);
  if(type_lon1d != NCL_double) NclFree(tmp_lon1d);
  if(type_fi    != NCL_double) NclFree(tmp_fi);
  if(type_fo    != NCL_double) NclFree(tmp_fo);

  ret = NclReturnValue(fo,ndims_fi,dsizes_fo,&missing_fo,type_fo,0);
  NclFree(dsizes_fo);
  return(ret);
}


NhlErrorTypes rcm2points_W( void )
{
/*
 * Input variables
 */
  void *lat2d, *lon2d, *fi, *lat1d, *lon1d, *opt;
  double *tmp_lat2d, *tmp_lon2d, *tmp_lat1d, *tmp_lon1d, *tmp_fi;
  int tmp_opt, tmp_ncrit;
  int dsizes_lat2d[2], dsizes_lon2d[2], dsizes_lat1d[2], dsizes_lon1d[1];
  int ndims_fi, dsizes_fi[NCL_MAX_DIMENSIONS], has_missing_fi;
  NclScalar missing_fi, missing_dfi, missing_rfi;
  NclBasicDataTypes type_lat2d, type_lon2d, type_lat1d, type_lon1d;
  NclBasicDataTypes type_fi, type_opt;
/*
 * Output variables.
 */
  void *fo;
  double *tmp_fo;
  int ndims_fo, *dsizes_fo;
  NclBasicDataTypes type_fo;
  NclScalar missing_fo;
/*
 * Other variables
 */
  int nlon2d, nlat2d, nfi, nlat1d, nfo, ngrid, size_fi, size_fo;
  int i, ier, ret;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  lat2d = (void*)NclGetArgValue(
          0,
          6,
          NULL,
          dsizes_lat2d,
          NULL,
          NULL,
          &type_lat2d,
          DONT_CARE);

  lon2d = (void*)NclGetArgValue(
          1,
          6,
          NULL,
          dsizes_lon2d,
          NULL,
          NULL,
          &type_lon2d,
          DONT_CARE);

  fi = (void*)NclGetArgValue(
          2,
          6,
          &ndims_fi,
          dsizes_fi,
          &missing_fi,
          &has_missing_fi,
          &type_fi,
          DONT_CARE);

  lat1d = (void*)NclGetArgValue(
          3,
          6,
          NULL,
          dsizes_lat1d,
          NULL,
          NULL,
          &type_lat1d,
          DONT_CARE);

  lon1d = (void*)NclGetArgValue(
          4,
          6,
          NULL,
          dsizes_lon1d,
          NULL,
          NULL,
          &type_lon1d,
          DONT_CARE);

  opt = (void*)NclGetArgValue(
          5,
          6,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_opt,
          DONT_CARE);
/*
 * Check the input lat/lon arrays. They must be the same size, and larger
 * than one element.
 */
  if(dsizes_lat2d[0] != dsizes_lon2d[0] ||
     dsizes_lat2d[1] != dsizes_lon2d[1]) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rcm2points: The input lat/lon grids must be the same size");
    return(NhlFATAL);
  }

  nlat2d = dsizes_lat2d[0];
  nlon2d = dsizes_lat2d[1];     /* same as dsizes_lon2d[1] */
  nlat1d = dsizes_lat1d[0];

  if(dsizes_lon1d[0] != nlat1d) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rcm2points: The output lat/lon arrays must be the same length");
    return(NhlFATAL);
  }

  if(nlon2d < 2 || nlat2d < 2 || nlat1d < 1) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rcm2points: The input lat/lon grids must have at least 2 elements, and the output lat/lon arrays 1 element");
    return(NhlFATAL);
  }

/*
 * Compute the total number of elements in our arrays.
 */
  nfi  = nlon2d * nlat2d;
  nfo  = nlat1d;

/*
 * Check dimensions of fi.
 */
  if(ndims_fi < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rcm2points: fi must be at least two dimensions");
    return(NhlFATAL);
  }
  if(dsizes_fi[ndims_fi-2] != nlat2d || dsizes_fi[ndims_fi-1] != nlon2d) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rcm2points: The rightmost dimensions of fi must be nlat2d x nlon2d, where nlat2d and nlon2d are the dimensions of the lat2d/lon2d arrays");
    return(NhlFATAL);
  }
/*
 * Compute the sizes of the input/output arrays.
 */
  ngrid = 1;
  for( i = 0; i < ndims_fi-2; i++ ) ngrid *= dsizes_fi[i];
  size_fi = ngrid * nfi;
  size_fo = ngrid * nfo;

/*
 * Coerce missing values.
 */
  coerce_missing(type_fi,has_missing_fi,&missing_fi,&missing_dfi,
                 &missing_rfi);
/*
 * Allocate space for output array.
 */
  if(type_fi == NCL_double) {
    fo      = (void*)calloc(size_fo,sizeof(double));
    tmp_fo  = &((double*)fo)[0];
    type_fo = NCL_double;
    missing_fo.doubleval = missing_dfi.doubleval;
  }
  else {
    fo      = (void*)calloc(size_fo,sizeof(float));
    tmp_fo  = (double*)calloc(size_fo,sizeof(double));
    type_fo = NCL_float;
    missing_fo.floatval = missing_rfi.floatval;
    if(tmp_fo == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rcm2points: Unable to allocate memory for temporary array");
      return(NhlFATAL);
    }
  }
  ndims_fo  = ndims_fi-1;
  dsizes_fo = (int*)calloc(ndims_fo,sizeof(int));
  if(fo == NULL || dsizes_fo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rcm2points: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_fi-2; i++) dsizes_fo[i] = dsizes_fi[i];
  dsizes_fo[ndims_fi-2] = nlat1d;

/*
 * Coerce input arrays to double if necessary.
 */
  tmp_lat2d = coerce_input_double(lat2d,type_lat2d,nfi,0,NULL,NULL);
  tmp_lon2d = coerce_input_double(lon2d,type_lon2d,nfi,0,NULL,NULL);
  tmp_lat1d = coerce_input_double(lat1d,type_lat1d,nlat1d,0,NULL,NULL);
  tmp_lon1d = coerce_input_double(lon1d,type_lon1d,nlat1d,0,NULL,NULL);
  tmp_fi    = coerce_input_double(fi,type_fi,size_fi,has_missing_fi,
                                  &missing_fi,&missing_dfi);

  if(tmp_lat2d == NULL || tmp_lon2d == NULL ||
     tmp_lat1d == NULL || tmp_lon1d == NULL || tmp_fi == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rcm2points: Unable to coerce input lat/lon arrays to double precision");
    return(NhlFATAL);
  }

/*
 * Force opt to zero and ncrit to 1, since they are not used yet.
 */
  tmp_opt   = 0;
  tmp_ncrit = 1;

  NGCALLF(drcm2points,DRCM2POINTS)(&ngrid,&nlat2d,&nlon2d,tmp_lat2d,tmp_lon2d,
                                   tmp_fi,&nlat1d,tmp_lat1d,tmp_lon1d,
                                   tmp_fo,&missing_dfi.doubleval,
                                   &tmp_opt,&tmp_ncrit,&ier);

  if(ier) {
    if(ier == 1) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"rcm2points: not enough points in input/output array");
    }
    if(2 <= ier && ier <= 5) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"rcm2points: lat2d, lon2d, lat1d, lon1d must be monotonically increasing");
    }
    set_subset_output_missing(fo,0,type_fo,size_fo,missing_dfi.doubleval);
  }
  else {
    if(type_fo != NCL_double) {
      coerce_output_float_only(fo,tmp_fo,size_fo,0);
    }
  }
/*
 * Free temp arrays.
 */
  if(type_lat2d != NCL_double) NclFree(tmp_lat2d);
  if(type_lon2d != NCL_double) NclFree(tmp_lon2d);
  if(type_lat1d != NCL_double) NclFree(tmp_lat1d);
  if(type_lon1d != NCL_double) NclFree(tmp_lon1d);
  if(type_fi    != NCL_double) NclFree(tmp_fi);
  if(type_fo    != NCL_double) NclFree(tmp_fo);

/*
 * Return.
 */
  ret = NclReturnValue(fo,ndims_fo,dsizes_fo,&missing_fo,type_fo,0);
  NclFree(dsizes_fo);
  return(ret);
}

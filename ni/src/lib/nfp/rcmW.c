#include <stdio.h>

/*
 * The following are the required NCAR Graphics include files.
 * They should be located in ${NCARG_ROOT}/include.
 */
#include "wrapper.h"

extern void NGCALLF(drcm2rgrid,DRCM2RGRID)(int *,int *,double *,double *,
                                           double *,int *,double *,int*,
                                           double *,double *,double*,
                                           double *,int *);

extern void NGCALLF(drgrid2rcm,DRGRID2RCM)(int *,int *,double *,double *,
                                           double *,int *,int *,double *,
                                           double *,double *,double*,
                                           double *,int *);

NhlErrorTypes rcm2rgrid_W( void )
{
/*
 * Input variables
 */
  void *lat2d, *lon2d, *fi, *lat1d, *lon1d, *opt;
  double *tmp_lat2d, *tmp_lon2d, *tmp_lat1d, *tmp_lon1d, *tmp_fi, *tmp_fo;
  double *tmp_opt;
  int dsizes_lat2d[2], dsizes_lon2d[2], dsizes_lat1d[2], dsizes_lon1d[1];
  int ndims_fi, dsizes_fi[NCL_MAX_DIMENSIONS], has_missing_fi;
  int *dsizes_fo;
  NclScalar missing_fi, missing_dfi, missing_rfi;
  NclBasicDataTypes type_lat2d, type_lon2d, type_lat1d, type_lon1d;
  NclBasicDataTypes type_fi, type_opt;
/*
 * Output variables.
 */
  void *fo;
/*
 * Other variables
 */
  int nlon2d, nlat2d, nfi, nlat1d, nlon1d, nfo, size_leftmost, size_fo;
  int i, j, index_fi, index_fo, ier;
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
          2);

  lon2d = (void*)NclGetArgValue(
          1,
          6,
          NULL,
          dsizes_lon2d,
          NULL,
          NULL,
          &type_lon2d,
          2);

  fi = (void*)NclGetArgValue(
          2,
          6,
          &ndims_fi,
          dsizes_fi,
          &missing_fi,
          &has_missing_fi,
          &type_fi,
          2);

  lat1d = (void*)NclGetArgValue(
          3,
          6,
          NULL,
          dsizes_lat1d,
          NULL,
          NULL,
          &type_lat1d,
          2);

  lon1d = (void*)NclGetArgValue(
          4,
          6,
          NULL,
          dsizes_lon1d,
          NULL,
          NULL,
          &type_lon1d,
          2);

  opt = (void*)NclGetArgValue(
          5,
          6,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_opt,
          2);
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
  if(dsizes_fi[ndims_fi-2] != nlat2d || dsizes_fi[ndims_fi-1] != nlon2d) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rcm2rgrid: The rightmost dimensions of fi must be nlat2d x nlon2d, where nlat2d and nlon2d are the dimensions of the lat2d/lon2d arrays");
    return(NhlFATAL);
  }
/*
 * Compute the total size of the output array (minus the last two
 * dimensions).
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_fi-2; i++ ) size_leftmost *= dsizes_fi[i];
  size_fo = size_leftmost * nfo;
/*
 * Coerce missing values.
 */
  coerce_missing(type_fi,has_missing_fi,&missing_fi,&missing_dfi,
                 &missing_rfi);
/*
 * Allocate space for temporary output array.
 */
  tmp_fo = (double*)calloc(nfo,sizeof(double));
  if(tmp_fo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rcm2rgrid: Unable to allocate memory for temporary arrays");
    return(NhlFATAL);
  }

/*
 * Allocate space for output array.
 */
  dsizes_fo = (int*)calloc(ndims_fi,sizeof(int));
  if(type_fi == NCL_double) {
    fo = (void*)calloc(size_fo,sizeof(double));
  }
  else {
    fo = (void*)calloc(size_fo,sizeof(float));
  }
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
  tmp_opt   = coerce_input_double(opt,type_opt,1,0,NULL,NULL);
  if(tmp_lat2d == NULL || tmp_lon2d == NULL ||
     tmp_lat1d == NULL || tmp_lon1d == NULL || tmp_opt == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rcm2rgrid: Unable to coerce input lat/lon arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Force opt to zero, since it's not used yet.
 */
  *tmp_opt = 0.;

  if(type_fi != NCL_double) {
    tmp_fi = (double*)calloc(nfi,sizeof(double));
    if(tmp_fi == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rcm2rgrid: Unable to allocate memory for coercing input array to double precision");
      return(NhlFATAL);
    }
  }

/*
 * Call Fortran function.
 */
  index_fi = index_fo = 0;
  for( i = 0; i < size_leftmost; i++ ) {
    if(type_fi != NCL_double) { 
      coerce_subset_input_double(fi,tmp_fi,index_fi,type_fi,nfi,0,NULL,NULL);
    }
    else {
      tmp_fi = &((double*)fi)[index_fi];
    }

    NGCALLF(drcm2rgrid,DRCM2RGRID)(&nlat2d,&nlon2d,tmp_lat2d,tmp_lon2d,
                                   tmp_fi,&nlat1d,tmp_lat1d,&nlon1d,
                                   tmp_lon1d,tmp_fo,&missing_dfi.doubleval,
                                   tmp_opt,&ier);

    if(ier) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"rcm2rgrid: lat2d, lon2d, lat1d, lon1d must be monotonically increasing");
      set_subset_output_missing(fo,index_fo,type_fi,nfo,missing_dfi.doubleval);
    }
    else {
      coerce_output_float_or_double(fo,tmp_fo,type_fi,nfo,index_fo);
    }
    index_fi += nfi;
    index_fo += nfo;
  }
/*
 * Free temp arrays.
 */
  if(type_lat2d != NCL_double) NclFree(tmp_lat2d);
  if(type_lon2d != NCL_double) NclFree(tmp_lon2d);
  if(type_lat1d != NCL_double) NclFree(tmp_lat1d);
  if(type_lon1d != NCL_double) NclFree(tmp_lon1d);
  if(type_fi    != NCL_double) NclFree(tmp_fi);
  if(type_opt   != NCL_double) NclFree(tmp_opt);
  NclFree(tmp_fo);

  if(type_fi == NCL_double) {
/*
 * Return double values with missing value set.
 */
    return(NclReturnValue(fo,ndims_fi,dsizes_fo,&missing_dfi,NCL_double,0));
  }
  else {
/*
 * Return float values with missing value set.
 */
    return(NclReturnValue(fo,ndims_fi,dsizes_fo,&missing_rfi,NCL_float,0));
  }
}


NhlErrorTypes rgrid2rcm_W( void )
{
/*
 * Input variables
 */
  void *lat2d, *lon2d, *fi, *lat1d, *lon1d, *opt;
  double *tmp_lat2d, *tmp_lon2d, *tmp_lat1d, *tmp_lon1d, *tmp_fi, *tmp_fo;
  double *tmp_opt;
  int dsizes_lat2d[2], dsizes_lon2d[2], dsizes_lat1d[2], dsizes_lon1d[1];
  int ndims_fi, dsizes_fi[NCL_MAX_DIMENSIONS], has_missing_fi;
  int *dsizes_fo;
  NclScalar missing_fi, missing_dfi, missing_rfi;
  NclBasicDataTypes type_lat2d, type_lon2d, type_lat1d, type_lon1d;
  NclBasicDataTypes type_fi, type_opt;
/*
 * Output variables.
 */
  void *fo;
/*
 * Other variables
 */
  int nlon2d, nlat2d, nfi, nlat1d, nlon1d, nfo, size_leftmost, size_fo;
  int i, j, index_fi, index_fo, ier;
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
          2);

  lon1d = (void*)NclGetArgValue(
          1,
          6,
          NULL,
          dsizes_lon1d,
          NULL,
          NULL,
          &type_lon1d,
          2);

  fi = (void*)NclGetArgValue(
          2,
          6,
          &ndims_fi,
          dsizes_fi,
          &missing_fi,
          &has_missing_fi,
          &type_fi,
          2);

  lat2d = (void*)NclGetArgValue(
          3,
          6,
          NULL,
          dsizes_lat2d,
          NULL,
          NULL,
          &type_lat2d,
          2);

  lon2d = (void*)NclGetArgValue(
          4,
          6,
          NULL,
          dsizes_lon2d,
          NULL,
          NULL,
          &type_lon2d,
          2);

  opt = (void*)NclGetArgValue(
          5,
          6,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_opt,
          2);
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
  if(dsizes_fi[ndims_fi-2] != nlat1d || dsizes_fi[ndims_fi-1] != nlon1d) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rgrid2rcm: The rightmost dimensions of fi must be nlat1d x nlon1d, where nlat1d and nlon1d are the dimensions of the lat1d/lon1d arrays");
    return(NhlFATAL);
  }
/*
 * Compute the total size of the output array (minus the last two
 * dimensions).
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_fi-2; i++ ) size_leftmost *= dsizes_fi[i];
  size_fo = size_leftmost * nfo;
/*
 * Coerce missing values.
 */
  coerce_missing(type_fi,has_missing_fi,&missing_fi,&missing_dfi,
                 &missing_rfi);
/*
 * Allocate space for temporary output array.
 */
  tmp_fo = (double*)calloc(nfo,sizeof(double));
  if(tmp_fo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rgrid2rcm: Unable to allocate memory for temporary arrays");
    return(NhlFATAL);
  }

/*
 * Allocate space for output array.
 */
  dsizes_fo = (int*)calloc(ndims_fi,sizeof(int));
  if(type_fi == NCL_double) {
    fo = (void*)calloc(size_fo,sizeof(double));
  }
  else {
    fo = (void*)calloc(size_fo,sizeof(float));
  }
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
  tmp_opt   = coerce_input_double(opt,type_opt,1,0,NULL,NULL);
  if(tmp_lat2d == NULL || tmp_lon2d == NULL ||
     tmp_lat1d == NULL || tmp_lon1d == NULL || tmp_opt == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rgrid2rcm: Unable to coerce input lat/lon arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Force opt to zero, since it's not used yet.
 */
  *tmp_opt = 0.;

  if(type_fi != NCL_double) {
    tmp_fi = (double*)calloc(nfi,sizeof(double));
    if(tmp_fi == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rgrid2rcm: Unable to allocate memory for coercing input array to double precision");
      return(NhlFATAL);
    }
  }

/*
 * Call Fortran function.
 */
  index_fi = index_fo = 0;
  for( i = 0; i < size_leftmost; i++ ) {
    if(type_fi != NCL_double) { 
      coerce_subset_input_double(fi,tmp_fi,index_fi,type_fi,nfi,0,NULL,NULL);
    }
    else {
      tmp_fi = &((double*)fi)[index_fi];
    }

    NGCALLF(drgrid2rcm,DRGRID2RCM)(&nlat1d,&nlon1d,tmp_lat1d,tmp_lon1d,
                                   tmp_fi,&nlat2d,&nlon2d,tmp_lat2d,
                                   tmp_lon2d,tmp_fo,&missing_dfi.doubleval,
                                   tmp_opt,&ier);

    if(ier) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"rgrid2rcm: lat2d, lon2d, lat1d, lon1d must be monotonically increasing");
      set_subset_output_missing(fo,index_fo,type_fi,nfo,missing_dfi.doubleval);
    }
    else {
      coerce_output_float_or_double(fo,tmp_fo,type_fi,nfo,index_fo);
    }
    index_fi += nfi;
    index_fo += nfo;
  }
/*
 * Free temp arrays.
 */
  if(type_lat2d != NCL_double) NclFree(tmp_lat2d);
  if(type_lon2d != NCL_double) NclFree(tmp_lon2d);
  if(type_lat1d != NCL_double) NclFree(tmp_lat1d);
  if(type_lon1d != NCL_double) NclFree(tmp_lon1d);
  if(type_fi    != NCL_double) NclFree(tmp_fi);
  if(type_opt   != NCL_double) NclFree(tmp_opt);
  NclFree(tmp_fo);

  if(type_fi == NCL_double) {
/*
 * Return double values with missing value set.
 */
    return(NclReturnValue(fo,ndims_fi,dsizes_fo,&missing_dfi,NCL_double,0));
  }
  else {
/*
 * Return float values with missing value set.
 */
    return(NclReturnValue(fo,ndims_fi,dsizes_fo,&missing_rfi,NCL_float,0));
  }
}


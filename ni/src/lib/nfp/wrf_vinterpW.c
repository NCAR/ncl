#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(wrf_monotonic,WRF_MONOTONIC)(double *, double *, double *, 
                                                 double *, int *, double *, 
                                                 int *, int *, int *, int *);

NhlErrorTypes wrf_monotonic_W( void )
{

/*
 * Input variables
 */
/*
 * Argument # 0
 */
  void *x;
  double *tmp_x;
  int       ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_x;

/*
 * Argument # 1
 */
  void *pres;
  double *tmp_pres;
  int       ndims_pres;
  ng_size_t dsizes_pres[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_pres;

/*
 * Argument # 2
 */
  void *cor;
  double *tmp_cor;
  int       ndims_cor;
  ng_size_t dsizes_cor[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_cor;

/*
 * Argument # 3
 */
  int *idir;
/*
 * Argument # 4
 */
  void *delta;
  double *tmp_delta;
  NclBasicDataTypes type_delta;

/*
 * Argument # 5
 */
  int *icorsw;
/*
 * Return variable
 */
  void *xout;
  double *tmp_xout;
  NclBasicDataTypes type_xout;


/*
 * Various
 */
  int nlev, nlat, nlon, nlevnlatnlon, nlatnlon;
  int index_x, index_cor;
  int i, size_leftmost, size_output, ret;

/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
/*
 * Get argument # 0
 */
  x = (void*)NclGetArgValue(
           0,
           6,
           &ndims_x,
           dsizes_x,
           NULL,
           NULL,
           &type_x,
           DONT_CARE);

/*
 * Check dimension sizes.
 */
  if(ndims_x < 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_monotonic: The x array must have at least 3 dimensions");
    return(NhlFATAL);
  }
  nlev = dsizes_x[ndims_x-3];
  nlat = dsizes_x[ndims_x-2];
  nlon = dsizes_x[ndims_x-1];

/*
 * Get argument # 1
 */
  pres = (void*)NclGetArgValue(
           1,
           6,
           &ndims_pres,
           dsizes_pres,
           NULL,
           NULL,
           &type_pres,
           DONT_CARE);

/*
 * Check dimension sizes.
 */
  if(ndims_pres < 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_monotonic: The pres array must have at least 3 dimensions");
    return(NhlFATAL);
  }
  if(dsizes_pres[ndims_pres-3] != nlev || 
     dsizes_pres[ndims_pres-2] != nlat || 
     dsizes_pres[ndims_pres-1] != nlon) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_monotonic: The rightmost dimensions of pres must be nlev x nlat x nlon");
    return(NhlFATAL);
  }

/*
 * Get argument # 2
 */
  cor = (void*)NclGetArgValue(
           2,
           6,
           &ndims_cor,
           dsizes_cor,
           NULL,
           NULL,
           &type_cor,
           DONT_CARE);

/*
 * Check dimension sizes.
 */
  if(ndims_cor < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_monotonic: The cor array must have at least 2 dimensions");
    return(NhlFATAL);
  }
  if(dsizes_cor[ndims_cor-2] != nlat || dsizes_cor[ndims_cor-1] != nlon) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_monotonic: The rightmost two dimensions of cor must be nlat x nlon");
    return(NhlFATAL);
  }

/*
 * Get argument # 3
 */
  idir = (int*)NclGetArgValue(
           3,
           6,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
/*
 * Get argument # 4
 */
  delta = (void*)NclGetArgValue(
           4,
           6,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_delta,
           DONT_CARE);
/*
 * Get argument # 5
 */
  icorsw = (int*)NclGetArgValue(
           5,
           6,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

/*
 * Calculate size of leftmost dimensions.
 */
  size_leftmost  = 1;
  for(i = 0; i < ndims_x-3; i++) {
    if(dsizes_pres[i] != dsizes_x[i] || dsizes_cor[i] != dsizes_x[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_monotonic: The leftmost dimensions of x, pres and cor must be the same");
      return(NhlFATAL);
    }
    size_leftmost *= dsizes_x[i];
  }


/* 
 * Allocate space for coercing input arrays.  If any of the input
 * is already double, then we don't need to allocate space for
 * temporary arrays, because we'll just change the pointer into
 * the void array appropriately.
 */

/*
 * Allocate space for tmp_x.
 */
  nlatnlon     = nlat * nlon;
  nlevnlatnlon = nlev * nlatnlon;

  if(type_x != NCL_double) {
    type_xout = NCL_float;
    tmp_x = (double *)calloc(nlevnlatnlon,sizeof(double));
    if(tmp_x == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_monotonic: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_xout = NCL_double;
  }
/*
 * Allocate space for tmp_pres.
 */
  if(type_pres != NCL_double) {
    tmp_pres = (double *)calloc(nlevnlatnlon,sizeof(double));
    if(tmp_pres == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_monotonic: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
/*
 * Allocate space for tmp_cor.
 */
  if(type_cor != NCL_double) {
    tmp_cor = (double *)calloc(nlatnlon,sizeof(double));
    if(tmp_cor == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_monotonic: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
/*
 * Allocate space for tmp_delta.
 */
  tmp_delta = coerce_input_double(delta,type_delta,1,0,NULL,NULL);
  if(tmp_delta == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_monotonic: Unable to allocate memory for coercing input array to double");
    return(NhlFATAL);
  }

/*
 * Calculate size of output array, which is same as x.
 */
  size_output = size_leftmost * nlevnlatnlon;

/* 
 * Allocate space for output array.
 */
  if(type_xout != NCL_double) {
    xout = (void *)calloc(size_output, sizeof(float));
    tmp_xout = (double *)calloc(nlevnlatnlon,sizeof(double));
    if(tmp_xout == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_monotonic: Unable to allocate memory for temporary output array");
      return(NhlFATAL);
    }
  }
  else {
    xout = (void *)calloc(size_output, sizeof(double));
  }
  if(xout == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_monotonic: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Loop across leftmost dimensions and call the Fortran routine for each
 * subsection of the input arrays.
 */
  index_x = index_cor = 0;

  for(i = 0; i < size_leftmost; i++) {
/*
 * Coerce subsection of x (tmp_x) to double if necessary.
 */
    if(type_x != NCL_double) {
      coerce_subset_input_double(x,tmp_x,index_x,type_x,nlevnlatnlon,0,NULL,NULL);
    }
    else {
      tmp_x = &((double*)x)[index_x];
    }

/*
 * Coerce subsection of pres (tmp_pres) to double if necessary.
 */
    if(type_pres != NCL_double) {
      coerce_subset_input_double(pres,tmp_pres,index_x,type_pres,nlevnlatnlon,0,NULL,NULL);
    }
    else {
      tmp_pres = &((double*)pres)[index_x];
    }

/*
 * Coerce subsection of cor (tmp_cor) to double if necessary.
 */
    if(type_cor != NCL_double) {
      coerce_subset_input_double(cor,tmp_cor,index_cor,type_cor,nlatnlon,0,NULL,NULL);
    }
    else {
      tmp_cor = &((double*)cor)[index_cor];
    }


/*
 * Point temporary output array to void output array if appropriate.
 */
    if(type_xout == NCL_double) tmp_xout = &((double*)xout)[index_x];


/*
 * Call the Fortran routine.
 */
    NGCALLF(wrf_monotonic,WRF_MONOTONIC)(tmp_xout, tmp_x, tmp_pres, 
                                         tmp_cor, idir, tmp_delta, &nlon,
                                         &nlat, &nlev, icorsw);

/*
 * Coerce output back to float if necessary.
 */
    if(type_xout == NCL_float) {
      coerce_output_float_only(xout,tmp_xout,nlevnlatnlon,index_x);
    }
    index_x   += nlevnlatnlon;
    index_cor += nlatnlon;
  }

/*
 * Free unneeded memory.
 */
  if(type_x     != NCL_double) NclFree(tmp_x);
  if(type_pres  != NCL_double) NclFree(tmp_pres);
  if(type_cor   != NCL_double) NclFree(tmp_cor);
  if(type_delta != NCL_double) NclFree(tmp_delta);
  if(type_xout  != NCL_double) NclFree(tmp_xout);

/*
 * Return value back to NCL script.
 */
  ret = NclReturnValue(xout,ndims_x,dsizes_x,NULL,type_xout,0);
  return(ret);
}

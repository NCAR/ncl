#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(dpsigma,DPSIGMA)(double*,double*,int*,int*,int*,double*);


NhlErrorTypes pres_sigma_W( void )
{
/*
 * Input variables
 */
  void *sigma, *ps;
  double *tmp_sigma;
  double *tmp_ps = NULL;
  int ndims_ps;
  ng_size_t dsizes_sigma[1];
  ng_size_t dsizes_ps[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_ps, type_sigma;
/*
 * Output variables
 */
  void *psigma;
  double *tmp_psigma = NULL;
  int ndims_psigma;
  ng_size_t *dsizes_psigma;
  NclBasicDataTypes type_psigma;
/*
 * Various.
 */
  ng_size_t i, j, nlat, nlon, klvl, nlatnlon, klvlnlatnlon;
  ng_size_t index_psigma, index_ps;
  int ret;
  ng_size_t size_leftmost, size_psigma;
  int inlon, inlat, iklvl;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  sigma = (void*)NclGetArgValue(
          0,
          2,
          NULL,
          dsizes_sigma,
          NULL,
          NULL,
          &type_sigma,
          DONT_CARE);

  ps = (void*)NclGetArgValue(
          1,
          2,
          &ndims_ps,
          dsizes_ps,
          NULL,
          NULL,
          &type_ps,
          DONT_CARE);

/*
 * Check dimensions.
 */
  if(ndims_ps < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pres_sigma: ps array must be at least 2-dimensional, where the last two dimensions are nlat x mlon");
    return(NhlFATAL);
  }
  klvl = dsizes_sigma[0];
  nlat = dsizes_ps[ndims_ps-2];
  nlon = dsizes_ps[ndims_ps-1];

  nlatnlon     = nlat * nlon;
  klvlnlatnlon = klvl * nlatnlon;

/*
 * Test input dimension sizes.
 */
  if((nlon > INT_MAX) || (nlat > INT_MAX) || (klvl > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pres_sigma: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  iklvl = (int) klvl;

/*
 * Determine type of output.
 */
  if(type_ps == NCL_double) {
    type_psigma = NCL_double;
  }
  else {
    type_psigma = NCL_float;
  }
/*
 * Calculate total size of output array.
 */
  ndims_psigma = ndims_ps + 1;

  dsizes_psigma = (ng_size_t*)calloc(ndims_psigma,sizeof(ng_size_t));  
  if( dsizes_psigma == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pres_sigma: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }

/*
 * psigma's last three dimensions must be klvl x nlat x nlon. The
 * leftmost dimensions will be the leftmost-2 dimensions of ps.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_ps-2; i++ ) {
    size_leftmost   *= dsizes_ps[i];
    dsizes_psigma[i] = dsizes_ps[i];
  }
  size_psigma = size_leftmost * klvlnlatnlon;
  dsizes_psigma[ndims_psigma-3] = klvl;
  dsizes_psigma[ndims_psigma-2] = nlat;
  dsizes_psigma[ndims_psigma-1] = nlon;
/*
 * Coerce sigma to double if necessary.
 */
  tmp_sigma  = coerce_input_double(sigma,type_sigma,klvl,0,NULL,NULL);
  if(tmp_sigma == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pres_sigma: Unable to coerce input to double precision");
    return(NhlFATAL);
  }

/*
 * Allocate space for ps.
 */
  if(type_ps != NCL_double) {
    tmp_ps = (double*)calloc(nlatnlon,sizeof(double));
    if( tmp_ps == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pres_ps: Unable to allocate memory for coercing ps array to double precision");
      return(NhlFATAL);
    }
  }
/*
 * Allocate space for output array.
 */
  if(type_psigma == NCL_float) {
    psigma     = (void*)calloc(size_psigma,sizeof(float));
    tmp_psigma = (double*)calloc(klvlnlatnlon,sizeof(double));
    if(tmp_psigma == NULL || psigma == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pres_sigma: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    psigma = (void*)calloc(size_psigma,sizeof(double));
    if(psigma == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pres_sigma: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }

/*
 * Call function.
 */
  index_psigma = index_ps = 0;
  for( i = 0; i < size_leftmost; i++ ) {
    if(type_ps != NCL_double) {
/*
 * Coerce subsection of ps (tmp_ps) to double.
 */
      coerce_subset_input_double(ps,tmp_ps,index_ps,type_ps,nlatnlon,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_ps to appropriate location in ps.
 */
      tmp_ps = &((double*)ps)[index_ps];
    }

    if(type_psigma == NCL_double) {
      tmp_psigma = &((double*)psigma)[index_psigma];
    }

    NGCALLF(dpsigma,DPSIGMA)(tmp_sigma,tmp_ps,&inlon,&inlat,&iklvl,tmp_psigma);

/*
 * Copy output values from temporary tmp_psigma to psigma.
 */
    if(type_psigma != NCL_double) {
      for( j = 0; j < klvlnlatnlon; j++ ) {
        ((float*)psigma)[index_psigma+j] = (float)tmp_psigma[j];
      }
    }
    index_psigma += klvlnlatnlon;
    index_ps     += nlatnlon;
  }
/*
 * Free memory.
 */
  if(type_sigma  != NCL_double) NclFree(tmp_sigma);
  if(type_ps     != NCL_double) NclFree(tmp_ps);
  if(type_psigma != NCL_double) NclFree(tmp_psigma);
/*
 * Return.
 */
  ret = NclReturnValue(psigma,ndims_psigma,dsizes_psigma,NULL,type_psigma,0);
  NclFree(dsizes_psigma);
  return(ret);
}



#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(chisub,CHISUB)(double*,double*,double*);

NhlErrorTypes chiinv_W( void )
{
/*
 * Input array variables
 */
  void *p, *df;
  double *dp, *ddf;
  int ndims_p;
  ng_size_t dsizes_p[NCL_MAX_DIMENSIONS];
  int ndims_df;
  ng_size_t dsizes_df[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_p, type_df;
/*
 * output variable 
 */
  void *chi;
  double *tmp_chi = NULL;
  ng_size_t size_chi;
  NclBasicDataTypes type_chi;
/*
 * Declare various variables for random purposes.
 */
  ng_size_t i;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */

/*
 * Retrieve argument #1
 */
  p = (void*)NclGetArgValue(
          0,
          2,
          &ndims_p,
          dsizes_p,
          NULL,
          NULL,
          &type_p,
          DONT_CARE);
/*
 * Retrieve argument #2
 */
  df = (void*)NclGetArgValue(
          1,
          2,
          &ndims_df,
          dsizes_df,
          NULL,
          NULL,
          &type_df,
          DONT_CARE);
/*
 * Check number of dimensions and/or dimension sizes for arguments #1
 * and #2 .
 */
  if (ndims_p != ndims_df) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"chiinv: The two input arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_p; i++ ) {
    if (dsizes_p[i] != dsizes_df[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"chiinv: The two input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Calculate size of output value.
 */
  size_chi = 1;
  for( i = 0; i < ndims_p; i++ ) size_chi *= dsizes_p[i];

/*
 * Coerce p and df to double if necessary.
 */
  dp  = coerce_input_double( p, type_p,size_chi,0,NULL,NULL);
  ddf = coerce_input_double(df,type_df,size_chi,0,NULL,NULL);
  if(ddf == NULL || dp == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"chiinv: unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }

/*
 * Allocate space for output array.
 */
  if(type_p != NCL_double && type_df != NCL_double) {
    type_chi = NCL_float;

    chi     = (float*)calloc(size_chi,sizeof(float));
    tmp_chi = (double *)calloc(1,sizeof(double));

    if(tmp_chi == NULL || chi == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"chiinv: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    type_chi = NCL_double;
    chi = (double*)calloc(size_chi,sizeof(double));
    if(chi == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"chiinv: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }

/*
 * Call the Fortran version of this routine.
 */
  for( i = 0; i < size_chi; i++ ) {
    if(type_chi == NCL_double) tmp_chi = &((double*)chi)[i];
    NGCALLF(chisub,CHISUB)(&dp[i],&ddf[i],tmp_chi);
    if(type_chi != NCL_double) ((float*)chi)[i] = (float)*tmp_chi;
  }
/*
 * free memory.
 */
  if((void*)dp  != p) NclFree(dp);
  if((void*)ddf != df) NclFree(ddf);
  if(type_chi != NCL_double) NclFree(tmp_chi);

/*
 * Return.
 */
  return(NclReturnValue(chi,ndims_p,dsizes_p,NULL,type_chi,0));
}

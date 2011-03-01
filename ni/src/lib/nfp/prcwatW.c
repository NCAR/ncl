#include <stdio.h>
#include <math.h>
#include "wrapper.h"

extern void NGCALLF(dprcwatdp,DPRCWATDP)(double *,double *,int *,double *,
                                         double*,double*);

NhlErrorTypes prcwater_dp_W( void )
{
/*
 * Input array variables
 */
  void *q, *dp;
  double *tmp_q = NULL;
  double *tmp_dp = NULL;
  int ndims_q;
  ng_size_t dsizes_q[NCL_MAX_DIMENSIONS];
  int has_missing_q;
  int ndims_dp;
  ng_size_t dsizes_dp[NCL_MAX_DIMENSIONS];
  int has_missing_dp;
  NclScalar missing_dp, missing_ddp;
  NclScalar missing_q, missing_dq, missing_rq;
  NclBasicDataTypes type_q, type_dp;
/*
 * Output array variables
 */
  void *prcwat;
  int ndims_prcwat;
  ng_size_t *dsizes_prcwat; 
  double *tmp_prcwat = NULL;
  NclBasicDataTypes type_prcwat;
/*
 * various
 */
  ng_size_t i, klvl, total_size_leftmost, index_q, ret;
  int iklvl;

/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  q = (void*)NclGetArgValue(
                            0,
                            2,
                            &ndims_q, 
                            dsizes_q,
                            &missing_q,
                            &has_missing_q,
                            &type_q,
                            DONT_CARE);

  dp = (void*)NclGetArgValue(
                            1,
                            2,
                            &ndims_dp, 
                            dsizes_dp,
                            &missing_dp,
                            &has_missing_dp,
                            &type_dp,
                            DONT_CARE);
/*
 * Some error checking. dp must either be a one-dimensional array with the
 * same length as the last dimension of q, or else dp must be the same
 * size as q.
 */
  if( (ndims_dp > 1 && ndims_q != ndims_dp) || 
      (ndims_dp == 1 && dsizes_dp[0] != dsizes_q[ndims_q-1])) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"prcwater_dp: 'dp' must either be one-dimensionial and have the same length as the last (rightmost) dimension of 'q', or it must be the same size as 'q'");
    return(NhlFATAL);
  }

  if(ndims_dp > 1) {
    for(i = 0; i < ndims_q; i++) {
      if(dsizes_q[i] != dsizes_dp[i]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"prcwater_dp: 'dp' must either be one-dimensionial and have the same length as the last (rightmost) dimension of 'q', or it must be the same size as 'q'");
        return(NhlFATAL);
      }
    }
  }
/*
 * Calculate size of leftmost dimensions, which will also become size
 * of output array.
 */
  klvl = dsizes_q[ndims_q-1];

/*
 * Test input dimension sizes.
 */
  if(klvl > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"prcwater_dp: klvl = %ld is greater than INT_MAX", klvl);
    return(NhlFATAL);
  }
  iklvl = (int) klvl;

  total_size_leftmost = 1;
  for( i = 0; i < ndims_q-1; i++ ) {
    total_size_leftmost *= dsizes_q[i];
  }

/*
 * Check for missing values.
 */
  coerce_missing(type_dp,has_missing_dp,&missing_dp,&missing_ddp,NULL);
  coerce_missing(type_q,has_missing_q,&missing_q,&missing_dq,&missing_rq);

/*
 * Create memory to allocate q and dp to double precision if necessary.
 */
  if(type_q != NCL_double) {
    tmp_q = (double*)calloc(klvl,sizeof(double));
    if( tmp_q == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"prcwater_dp: Unable to allocate memory for coercing q array to double precision");
      return(NhlFATAL);
    }
  }

  if(type_dp != NCL_double) {
    tmp_dp = (double*)calloc(klvl,sizeof(double));
    if( tmp_dp == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"prcwater_dp: Unable to allocate memory for coercing dp array to double precision");
      return(NhlFATAL);
    }
  }
/*
 * Allocate space for output value.
 */
  if(ndims_q > 1) {
    ndims_prcwat = ndims_q - 1;
    dsizes_prcwat = (ng_size_t*)calloc(ndims_prcwat,sizeof(ng_size_t));  
    for(i = 0; i < ndims_q-1; i++ ) dsizes_prcwat[i] = dsizes_q[i];
  }
  else {
    ndims_prcwat = 1;
    dsizes_prcwat = (ng_size_t*)calloc(1,sizeof(ng_size_t));  
    dsizes_prcwat[0] = 1;
  }

  if(type_dp != NCL_double && type_q != NCL_double) {
    type_prcwat = NCL_float;

    tmp_prcwat = (double*)calloc(1,sizeof(double));
    prcwat     = (void*)calloc(total_size_leftmost,sizeof(float));
    if(prcwat == NULL || tmp_prcwat == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"prcwater_dp: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    type_prcwat = NCL_double;
    prcwat = (void*)calloc(total_size_leftmost,sizeof(double));
    if(prcwat == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"prcwater_dp: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }

/*
 * Call the f77 version of 'dprcwaterdp' with the full argument list.
 */
  index_q = 0;
  for( i = 0; i < total_size_leftmost; i++ ) {
/*
 * Coerce q to double precision if necessary.
 */
    if(type_q != NCL_double) {
      coerce_subset_input_double(q,tmp_q,index_q,type_q,klvl,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_q to q.
 */
      tmp_q = &((double*)q)[index_q];
    }

/*
 * Coerce dp to double precision if necessary. If dp is one-dimensional,
 * the coercion will only happen the first time through this loop.
 */
    if(ndims_dp != 1 || (ndims_dp == 1 && !i)) {
      if(type_dp != NCL_double) {
        coerce_subset_input_double(dp,tmp_dp,index_q,type_dp,
                                   klvl,0,NULL,NULL);
      }
      else {
/*
 * Point tmp_dp to dp.
 */
        tmp_dp = &((double*)dp)[index_q];
      }
    }

    if(type_prcwat == NCL_double) tmp_prcwat = &((double*)prcwat)[i];

        NGCALLF(dprcwatdp,DPRCWATDP)(tmp_q,tmp_dp,&iklvl,&missing_dq.doubleval,
                                  &missing_ddp.doubleval,tmp_prcwat);
/*
 * Coerce output to float if necessary.
 */
    
    if(type_prcwat != NCL_double) {
      coerce_output_float_only(prcwat,tmp_prcwat,1,i);
    }
      
    index_q += klvl;
  }

/*
 * free memory.
 */
  if(type_dp != NCL_double) NclFree(tmp_dp);
  if(type_q  != NCL_double) NclFree(tmp_q);
  if(type_prcwat != NCL_double) NclFree(tmp_prcwat);

/*
 * Return.
 */
  if(type_prcwat != NCL_double) {
/*
 * None of the input is double, so return float values.
 */
/*
 * Return float values with missing value set.
 */
    ret = NclReturnValue(prcwat,ndims_prcwat,dsizes_prcwat,&missing_rq,
                          NCL_float,0);
  }
  else {
/*
 * Return double values with missing value set.
 */
    ret = NclReturnValue(prcwat,ndims_prcwat,dsizes_prcwat,&missing_dq,
                          NCL_double,0);
  }
  NclFree(dsizes_prcwat);
  return(ret);
}

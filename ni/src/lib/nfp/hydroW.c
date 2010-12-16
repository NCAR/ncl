#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(dhydro,DHYDRO)(double *,double *,double *,int *,
                                   double *,int *);

NhlErrorTypes hydro_W( void )
{
/*
 * Declare various variables for random purposes.
 */
  ng_size_t i, index_zh, nlvl;
  int inlvl, ier=0;
/*
 * Input array variables
 */
  void *p, *tkv, *zsfc;
  double *tmp_p = NULL;
  double *tmp_tkv = NULL;
  double *tmp_zsfc = NULL;
  int ndims_p;
  ng_size_t dsizes_p[NCL_MAX_DIMENSIONS];
  int has_missing_p;
  int ndims_tkv;
  ng_size_t dsizes_tkv[NCL_MAX_DIMENSIONS];
  int has_missing_tkv;
  int ndims_zsfc;
  ng_size_t dsizes_zsfc[NCL_MAX_DIMENSIONS];
  int has_missing_zsfc;
  NclBasicDataTypes type_p, type_tkv, type_zsfc;
  NclScalar missing_p, missing_tkv, missing_zsfc;
  NclScalar missing_dp, missing_dtkv, missing_dzsfc;
  ng_size_t size_zsfc, size_p;
  int found_missing_p, found_missing_tkv, found_missing_zsfc, any_missing;
/*
 * Output array variables
 */
  void *zh;
  double *tmp_zh = NULL;
  NclBasicDataTypes type_zh;
  NclScalar missing_zh;
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
          3,
          &ndims_p,
          dsizes_p,
          &missing_p,
          &has_missing_p,
          &type_p,
          DONT_CARE);

/*
 * Retrieve argument #2
 */
  tkv = (void*)NclGetArgValue(
          1,
          3,
          &ndims_tkv,
          dsizes_tkv,
          &missing_tkv,
          &has_missing_tkv,
          &type_tkv,
          DONT_CARE);

/*
 * Retrieve argument #3
 */
  zsfc = (void*)NclGetArgValue(
          2,
          3,
          &ndims_zsfc,
          dsizes_zsfc,
          &missing_zsfc,
          &has_missing_zsfc,
          &type_zsfc,
          DONT_CARE);

/*
 * Check number of dimensions and/or dimension sizes for p and tkv.
 */
  if(ndims_tkv != ndims_p) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"hydro: The input arrays 'p' and 'tkv' must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_p; i++ ) {
    if (dsizes_tkv[i] != dsizes_p[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"hydro: The input arrays 'p' and 'tkv' must have the same dimensions");
      return(NhlFATAL);
    }
  }

/*
 * Check number of dimensions and/or dimension sizes for zsfc.
 */
  if ((ndims_p == 1 && ndims_zsfc != 1) || 
      (ndims_p > 1 && ndims_zsfc != ndims_p-1)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"hydro: The array 'zsfc' must be a scalar or one less dimension than the other arrays");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_p-1; i++ ) {
    if (dsizes_zsfc[i] != dsizes_p[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"hydro: The dimensions of the input array 'zsfc' must be the same as the arrays 'p' and 'tkv', minus the last dimension");
      return(NhlFATAL);
    }
  }

/*
 * Compute the total size of the output array.
 */
  if (dsizes_p[ndims_p-1] < 1) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"hydro: The rightmost dimension of p and tkv must be at least one");
    return(NhlFATAL);
  }

  if(dsizes_p[ndims_p-1] > INT_MAX) { 
    NhlPError(NhlFATAL,NhlEUNKNOWN,"hydro: The rightmost dimension of p is greater than INT_MAX");
    return(NhlFATAL);
  }
  nlvl  = dsizes_p[ndims_p-1];
  inlvl = (int) nlvl;

  size_zsfc = 1;
  for( i = 0; i < ndims_zsfc; i++ ) size_zsfc *= dsizes_zsfc[i];

  size_p = nlvl * size_zsfc;
/*
 * allocate space for temporary input/output arrays.
 */
  if(type_p != NCL_double) {
    tmp_p = (double*)calloc(nlvl,sizeof(double));
    if(tmp_p == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"hydro: Unable to allocate memory for coercing input array to double precision");
      return(NhlFATAL);
    }
  }
  if(type_tkv != NCL_double) {
    tmp_tkv = (double*)calloc(nlvl,sizeof(double));
    if(tmp_tkv == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"hydro: Unable to allocate memory for coercing input array to double precision");
      return(NhlFATAL);
    }
  }
  if(type_zsfc != NCL_double) {
    tmp_zsfc = (double*)calloc(1,sizeof(double));
    if(tmp_zsfc == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"hydro: Unable to allocate memory for coercing input array to double precision");
      return(NhlFATAL);
    }
  }
  if(type_p != NCL_double && type_tkv != NCL_double &&
     type_zsfc != NCL_double) {

    type_zh = NCL_float;

    zh     = (void*)calloc(size_p,sizeof(float));
    tmp_zh = (double*)calloc(nlvl,sizeof(double));
    if(zh == NULL || tmp_zh == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"hydro: Unable to allocate memory for output array");
      return(NhlFATAL);
    }    
  }
  else {
    type_zh = NCL_double;
    zh = (void*)calloc(size_p,sizeof(double));
    if(zh == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"hydro: Unable to allocate memory for output array");
      return(NhlFATAL);
    }    
  }
/*
 * Check for missing values
 */
  coerce_missing(type_tkv, has_missing_tkv, &missing_tkv, &missing_dtkv,
                 NULL);
  coerce_missing(type_p,   has_missing_p,   &missing_p,   &missing_dp,
                 NULL);
  coerce_missing(type_zsfc,has_missing_zsfc,&missing_zsfc,&missing_dzsfc,
                 NULL);

  if(has_missing_tkv || has_missing_p || has_missing_zsfc) {
    if(type_zh == NCL_double) {
      missing_zh.doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
    }
    else {
      missing_zh.floatval = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
    }
  }

/*
 * Call the Fortran version of this routine.
 */
  index_zh = 0;
  any_missing = 0;
  for( i = 0; i < size_zsfc; i++ ) {
    if(type_p != NCL_double) {
/*
 * Coerce nlvl subsection of p (tmp_p) to double.
 */
      coerce_subset_input_double(p,tmp_p,index_zh,type_p,nlvl,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_p to appropriate location in p.
 */
      tmp_p = &((double*)p)[index_zh];
    }
    if(type_tkv != NCL_double) {
/*
 * Coerce nlvl subsection of tkv (tmp_tkv) to double.
 */
      coerce_subset_input_double(tkv,tmp_tkv,index_zh,type_tkv,nlvl,0,
                                 NULL,NULL);
    }
    else {
/*
 * Point tmp_tkv to appropriate location in tkv.
 */
      tmp_tkv = &((double*)tkv)[index_zh];
    }
    if(type_zsfc != NCL_double) {
/*
 * Coerce subsection of zsfc (tmp_zsfc) to double.
 */
      coerce_subset_input_double(zsfc,tmp_zsfc,i,type_zsfc,1,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_zsfc to appropriate location in zsfc.
 */
      tmp_zsfc = &((double*)zsfc)[i];
    }

    if(type_zh == NCL_double) tmp_zh = &((double*)zh)[index_zh];

/*
 * Test for missing values.
 */
    found_missing_p = contains_missing(tmp_p,nlvl,has_missing_p,
                                       missing_dp.doubleval);

    found_missing_tkv = contains_missing(tmp_tkv,nlvl,has_missing_tkv,
                                         missing_dtkv.doubleval);
    
    found_missing_zsfc = (*tmp_zsfc == missing_dzsfc.doubleval) ? 1 : 0;

    if(found_missing_p || found_missing_tkv || found_missing_zsfc) {
      any_missing = 1;
      if(type_zh == NCL_double) {
        set_subset_output_missing(zh,index_zh,type_zh,nlvl,
                                  missing_zh.doubleval);
      }
      else {
        set_subset_output_missing(zh,index_zh,type_zh,nlvl,
                                  (double)missing_zh.floatval);
      }
      NhlPError(NhlWARNING,NhlEUNKNOWN,"hydro: one of the input arrays contains missing values. No geopotential heights calculated for this set of values.");
    }
    else {
      NGCALLF(dhydro,DHYDRO)(tmp_p,tmp_tkv,tmp_zsfc,&inlvl,tmp_zh,&ier);
/*
 * Copy output values from temporary tmp_zh to zh.
 */
      if(type_zh != NCL_double) {
        coerce_output_float_only(zh,tmp_zh,nlvl,index_zh);
      }
    }
    index_zh += nlvl;
  }
/*
 * free memory.
 */
  if(type_p    != NCL_double) NclFree(tmp_p);
  if(type_tkv  != NCL_double) NclFree(tmp_tkv);
  if(type_zsfc != NCL_double) NclFree(tmp_zsfc);
  if(type_zh   != NCL_double) NclFree(tmp_zh);

  if(any_missing) {
    return(NclReturnValue(zh,ndims_p,dsizes_p,&missing_zh,type_zh,0));
  }
  else {
    return(NclReturnValue(zh,ndims_p,dsizes_p,NULL,type_zh,0));
  }
}

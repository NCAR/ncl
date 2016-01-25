#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(wetbulbprofs,WETBULBPROFS)(int *, double *, double *, double *, double *, 
                                               double *, double *, double *, int *);

NhlErrorTypes wetbulb_W( void )
{

/*
 * Input variables
 */
/*
 * Argument # 0
 */
  void *p;
  double *tmp_p = NULL;
  int       ndims_p;
  ng_size_t dsizes_p[NCL_MAX_DIMENSIONS];
  int has_missing_p;
  NclScalar missing_p, missing_dbl_p;
  NclBasicDataTypes type_p;

/*
 * Argument # 1
 */
  void *t;
  double *tmp_t = NULL;
  int       ndims_t;
  ng_size_t dsizes_t[NCL_MAX_DIMENSIONS];
  int has_missing_t;
  NclScalar missing_t, missing_dbl_t, missing_flt_t;
  NclBasicDataTypes type_t;

/*
 * Argument # 2
 */
  void *td;
  double *tmp_td = NULL;
  int       ndims_td;
  ng_size_t dsizes_td[NCL_MAX_DIMENSIONS];
  int has_missing_td;
  NclScalar missing_td, missing_dbl_td;
  NclBasicDataTypes type_td;

/*
 * Return variable
 */
  void *twb;
  double *tmp_twb = NULL;
  int has_missing_twb;
  NclScalar missing_twb;
  NclBasicDataTypes type_twb;


/*
 * Various
 */
  ng_size_t i, size_leftmost, size_output, npts, index_p;
  int inpts, ier, ret;

/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
/*
 * Get argument # 0
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

  npts = dsizes_p[ndims_p-1];

  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wetbulb: npts is greater than INT_MAX");
    return(NhlFATAL);
  }
  inpts = (int) npts;

/*
 * Get argument # 1
 */
  t = (void*)NclGetArgValue(
           1,
           3,
           &ndims_t,
           dsizes_t,
           &missing_t,
           &has_missing_t,
           &type_t,
           DONT_CARE);

/*
 * Check dimension sizes.
 */
  if(ndims_t != ndims_p) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wetbulb: The input arrays must have the same dimensionality.");
    return(NhlFATAL);
  }

/*
 * Get argument # 2
 */
  td = (void*)NclGetArgValue(
           2,
           3,
           &ndims_td,
           dsizes_td,
           &missing_td,
           &has_missing_td,
           &type_td,
           DONT_CARE);

/*
 * Check dimension sizes.
 */
  if(ndims_td != ndims_p) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wetbulb: The input arrays must have the same dimensionality.");
    return(NhlFATAL);
  }

/*
 * Calculate size of leftmost dimensions and check dimensions.
 */
  size_leftmost  = 1;
  if(ndims_p > 1) {
    for(i = 0; i < (ndims_p-1); i++) {
      if(dsizes_p[i] != dsizes_t[i] || dsizes_p[i] != dsizes_td[i]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"wetbulb: The input arrays must have the same dimensionality.");
        return(NhlFATAL);
      }
      size_leftmost *= dsizes_p[i];
    }
  }

/*
 * The output type defaults to float, unless this input array is double.
 */
  type_twb = NCL_float;

/* 
 * Allocate space for coercing input arrays.  If any of the input
 * is already double, then we don't need to allocate space for
 * temporary arrays, because we'll just change the pointer into
 * the void array appropriately.
 */
/*
 * Allocate space for tmp_p.
 */
  if(type_p != NCL_double) {
    tmp_p = (double *)calloc(npts,sizeof(double));
    if(tmp_p == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wetbulb: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_twb = NCL_double;
  }
/*
 * Allocate space for tmp_t.
 */
  if(type_t != NCL_double) {
    tmp_t = (double *)calloc(npts,sizeof(double));
    if(tmp_t == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wetbulb: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_twb = NCL_double;
  }

/*
 * Set the output missing value, if any of the input contains a _FillValue.
 * Note: the missing value will be set to t's missing value, or the 
 * default missing value for a float/double otherwise. The coerce_missing
 * call will cause a default missing value to be returned, if none is set.
 */ 
  coerce_missing(type_p, has_missing_p, &missing_p, &missing_dbl_p, NULL);
  coerce_missing(type_td,has_missing_td,&missing_td,&missing_dbl_td,NULL);
  coerce_missing(type_t, has_missing_t, &missing_t, &missing_dbl_t, &missing_flt_t);

  if(has_missing_t || has_missing_td || has_missing_p) {
    has_missing_twb = 1;
    if(type_twb == NCL_double) {
      missing_twb = missing_dbl_t;
    }
    else {
      missing_twb = missing_flt_t;
    }
  }
  else {
    has_missing_twb = 0;
  }

/*
 * Allocate space for tmp_td.
 */
  if(type_td != NCL_double) {
    tmp_td = (double *)calloc(npts,sizeof(double));
    if(tmp_td == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wetbulb: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_twb = NCL_double;
  }

/*
 * Calculate size of output array.
 */
  size_output = size_leftmost * npts;

/* 
 * Allocate space for output array.
 */
  if(type_twb != NCL_double) {
    twb = (void *)calloc(size_output, sizeof(float));
    tmp_twb = (double *)calloc(npts,sizeof(double));
    if(tmp_twb == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wetbulb: Unable to allocate memory for temporary output array");
      return(NhlFATAL);
    }
  }
  else {
    twb = (void *)calloc(size_output, sizeof(double));
  }
  if(twb == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wetbulb: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Loop across leftmost dimensions and call the Fortran routine for each
 * subsection of the input arrays.
 */
  index_p = 0;
  for(i = 0; i < size_leftmost; i++) {
/*
 * Coerce subsection of p (tmp_p) to double if necessary.
 */
    if(type_p != NCL_double) {
      coerce_subset_input_double(p,tmp_p,index_p,type_p,npts,0,NULL,NULL);
    }
    else {
      tmp_p = &((double*)p)[index_p];
    }

/*
 * Coerce subsection of t (tmp_t) to double if necessary.
 */
    if(type_t != NCL_double) {
      coerce_subset_input_double(t,tmp_t,index_p,type_t,npts,0,NULL,NULL);
    }
    else {
      tmp_t = &((double*)t)[index_p];
    }

/*
 * Coerce subsection of td (tmp_td) to double if necessary.
 */
    if(type_td != NCL_double) {
      coerce_subset_input_double(td,tmp_td,index_p,type_td,npts,0,NULL,NULL);
    }
    else {
      tmp_td = &((double*)td)[index_p];
    }

/*
 * Point temporary output array to void output array if appropriate.
 */
    if(type_twb == NCL_double) tmp_twb = &((double*)twb)[index_p];

/*
 * Call the Fortran routine.
 */
    NGCALLF(wetbulbprofs,WETBULBPROFS)(&inpts, tmp_t, tmp_td, tmp_p, tmp_twb, 
                                       &missing_dbl_t.doubleval, 
                                       &missing_dbl_td.doubleval,
                                       &missing_dbl_p.doubleval, &ier);

/*
 * Coerce output back to float if necessary.
 */
    if(type_twb == NCL_float) {
      coerce_output_float_only(twb,tmp_twb,npts,index_p);
    }
    index_p += npts;
  }

/*
 * Free unneeded memory.
 */
  if(type_p != NCL_double)   NclFree(tmp_p);
  if(type_t != NCL_double)   NclFree(tmp_t);
  if(type_td != NCL_double)  NclFree(tmp_td);
  if(type_twb != NCL_double) NclFree(tmp_twb);

/*
 * Return value back to NCL script.
 */
  if(has_missing_twb) {
    ret = NclReturnValue(twb,ndims_p,dsizes_p,&missing_twb,type_twb,0);
  }
  else {
    ret = NclReturnValue(twb,ndims_p,dsizes_p,NULL,type_twb,0);
  }
  return(ret);
}

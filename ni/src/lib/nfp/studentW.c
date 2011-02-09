#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(stuprobt,STUPROBT)(double *, double *, double *, double *);

NhlErrorTypes student_t_W( void )
{

/*
 * Input variables
 */
/*
 * Argument # 0
 */
  void *t;
  double *tmp_t = NULL;
  int ndims_t;
  ng_size_t dsizes_t[NCL_MAX_DIMENSIONS];
  int has_missing_t;
  NclScalar missing_t, missing_flt_t, missing_dbl_t;
  NclBasicDataTypes type_t;

/*
 * Argument # 1
 */
  void *df;
  double *tmp_df = NULL;
  int ndims_df;
  ng_size_t dsizes_df[NCL_MAX_DIMENSIONS];
  int is_scalar_df;
  NclBasicDataTypes type_df;

/*
 * Return variable
 */
  void *prob;
  double *tmp_prob = NULL;
  NclScalar missing_prob;
  NclBasicDataTypes type_prob;

/*
 * Various
 */
  ng_size_t i, size_output;

/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
/*
 * Get argument # 0
 */
  t = (void*)NclGetArgValue(
           0,
           2,
           &ndims_t,
           dsizes_t,
           &missing_t,
           &has_missing_t,
           &type_t,
           DONT_CARE);

/*
 * Coerce missing value to double if necessary.
 */
  coerce_missing(type_t,has_missing_t,&missing_t,
                 &missing_dbl_t,&missing_flt_t);

/*
 * Get argument # 1
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
 * Checked df's dimension sizes. It must be a scalar, or the same
 * size as t.
 */
  is_scalar_df = is_scalar(ndims_df,dsizes_df);
  if(!is_scalar_df && ndims_df != ndims_t) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"student_t: df must be a scalar or an array of the same dimensionality as t");
    return(NhlFATAL);
  }
  if(!is_scalar_df) {
    for(i = 0; i < ndims_df; i++) {
      if(dsizes_df[i] != dsizes_t[i]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"student_t: df must be a scalar or an array of the same dimensionality as t");
        return(NhlFATAL);
      }
    }
  }

/*
 * Calculate size of output array.
 */
  size_output = 1;
  for(i = 0; i < ndims_t; i++) size_output *= dsizes_t[i];

/*
 * The output type defaults to float, unless either input array is double.
 */
  if(type_t == NCL_double || type_df == NCL_double) {
    type_prob = NCL_double;
  }
  else {
    type_prob = NCL_float;
  }

/* 
 * Allocate space for coercing input arrays.  If any of the input
 * is already double, then we don't need to allocate space for
 * temporary arrays, because we'll just change the pointer into
 * the void array appropriately.
 *
 * Allocate space for tmp_t.
 */
  if(type_t != NCL_double) {
    tmp_t = (double *)calloc(1,sizeof(double));
    if(tmp_t == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"student_t: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
/*
 * Allocate space for tmp_df.
 */
  if(is_scalar_df) {
    tmp_df = coerce_input_double(df,type_df,1,0,NULL,NULL);
  }
  else {
    if(type_df != NCL_double) {
      tmp_df = (double *)calloc(1,sizeof(double));
      if(tmp_df == NULL) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"student_t: Unable to allocate memory for coercing input array to double");
        return(NhlFATAL);
      }
    }
  }

/* 
 * Allocate space for output array.
 */
  if(type_prob != NCL_double) {
    prob = (void *)calloc(size_output, sizeof(float));
    tmp_prob = (double *)calloc(1,sizeof(double));
    if(tmp_prob == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"student_t: Unable to allocate memory for temporary output array");
      return(NhlFATAL);
    }
  }
  else {
    prob = (void *)calloc(size_output, sizeof(double));
  }
  if(prob == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"student_t: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
  if(has_missing_t) {
    if(type_prob == NCL_double) missing_prob = missing_dbl_t;
    else                        missing_prob = missing_flt_t;
  }

/*
 * Loop across leftmost dimensions and call the Fortran routine for each
 * subsection of the input arrays.
 */

  for(i = 0; i < size_output; i++) {
/*
 * Coerce subsection of t (tmp_t) to double if necessary.
 */
    if(type_t != NCL_double) {
      coerce_subset_input_double(t,tmp_t,i,type_t,1,0,NULL,NULL);
    }
    else {
      tmp_t = &((double*)t)[i];
    }

/*
 * Coerce subsection of df (tmp_df) to double if necessary.
 */
    if(!is_scalar_df) {
      if(type_df != NCL_double) {
        coerce_subset_input_double(df,tmp_df,i,type_df,1,0,NULL,NULL);
      }
      else {
        tmp_df = &((double*)df)[i];
      }
    }

/*
 * Point temporary output array to void output array if appropriate.
 */
    if(type_prob == NCL_double) tmp_prob = &((double*)prob)[i];

/*
 * Call the Fortran routine.
 */
    NGCALLF(stuprobt,STUPROBT)(tmp_t, tmp_df, &missing_dbl_t.doubleval, 
                               tmp_prob);

/*
 * Coerce output back to float if necessary.
 */
    if(type_prob == NCL_float) {
      coerce_output_float_only(prob,tmp_prob,1,i);
    }
  }

/*
 * Free unneeded memory.
 */
  if(type_t    != NCL_double) NclFree(tmp_t);
  if(type_df   != NCL_double) NclFree(tmp_df);
  if(type_prob != NCL_double) NclFree(tmp_prob);

/*
 * Return value back to NCL script.
 */
  if(has_missing_t) {
    return(NclReturnValue(prob,ndims_t,dsizes_t,&missing_prob,
                          type_prob,0));
  }
  else {
    return(NclReturnValue(prob,ndims_t,dsizes_t,NULL,type_prob,0));
  }
}

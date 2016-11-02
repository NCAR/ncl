#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(betainc,BETAINC)(double*,double*,double*,double*);
extern void NGCALLF(cumgam,CUMGAM)(double*,double*,double*,double*);

NhlErrorTypes betainc_W( void )
{
/*
 * Input array variables
 */
  void *x, *a, *b;
  double *tmp_x = NULL;
  double *tmp_a = NULL;
  double *tmp_b = NULL;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  int ndims_a;
  ng_size_t dsizes_a[NCL_MAX_DIMENSIONS];
  int ndims_b;
  ng_size_t dsizes_b[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_x, type_a, type_b, type_output;
  NclScalar missing_x, missing_dx;
/*
 * output variable 
 */
  void *alpha;
  double *tmp_alpha = NULL;
  ng_size_t size_alpha;
/*
 * Declare various variables for random purposes.
 */
  ng_size_t i;
  int ret;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */

/*
 * Retrieve argument #1
 */
  x = (void*)NclGetArgValue(
          0,
          3,
          &ndims_x,
          dsizes_x,
          &missing_x,
          &has_missing_x,
          &type_x,
          DONT_CARE);

  a = (void*)NclGetArgValue(
          1,
          3,
          &ndims_a,
          dsizes_a,
          NULL,
          NULL,
          &type_a,
          DONT_CARE);
  b = (void*)NclGetArgValue(
          2,
          3,
          &ndims_b,
          dsizes_b,
          NULL,
          NULL,
          &type_b,
          DONT_CARE);
/*
 * Check dimensions.
 */
  if (ndims_x != ndims_a || ndims_x != ndims_b) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"betainc: The three input arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_x; i++ ) {
    if (dsizes_x[i] != dsizes_a[i] || dsizes_x[i] != dsizes_b[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"betainc: The three input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Coerce x's missing value to double, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,NULL);

/*
 * Calculate size of output value.
 */
  size_alpha = 1;
  for( i = 0; i < ndims_x; i++ ) size_alpha *= dsizes_x[i];

/*
 * Coerce x, a, and b to double if necessary.
 */
  if(type_x != NCL_double) {
    tmp_x = (double*)calloc(1,sizeof(double));
    if( tmp_x == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"betainc: Unable to allocate memory for coercing x to double precision");
      return(NhlFATAL);
    }
  }

  if(type_a != NCL_double) {
    tmp_a = (double*)calloc(1,sizeof(double));
    if( tmp_a == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"betainc: Unable to allocate memory for coercing a to double precision");
      return(NhlFATAL);
    }
  }

  if(type_b != NCL_double) {
    tmp_b = (double*)calloc(1,sizeof(double));
    if( tmp_b == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"betainc: Unable to allocate memory for coercing b to double precision");
      return(NhlFATAL);
    }
  }

/*
 * Allocate space for output array.
 */
  if(type_x == NCL_double) {
    alpha = (double*)calloc(size_alpha,sizeof(double));
    if(alpha == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"betainc: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    type_output = NCL_double;
  }
  else {
    alpha     = (float*)calloc(size_alpha,sizeof(float));
    tmp_alpha = (double *)calloc(1,sizeof(double));

    if(tmp_alpha == NULL || alpha == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"betainc: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    type_output = NCL_float;
  }

/*
 * Call the Fortran version of this routine.
 */
  for( i = 0; i < size_alpha; i++ ) {
    if(type_x != NCL_double) {
/*
 * Coerce subsection of x (tmp_x) to double.
 */
      coerce_subset_input_double(x,tmp_x,i,type_x,1,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_x to appropriate location in x.
 */
      tmp_x = &((double*)x)[i];
    }
    if(type_a != NCL_double) {
/*
 * Coerce subsection of a (tmp_a) to double.
 */
      coerce_subset_input_double(a,tmp_a,i,type_a,1,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_a to appropriate location in a.
 */
      tmp_a = &((double*)a)[i];
    }
    if(type_b != NCL_double) {
/*
 * Coerce subsection of b (tmp_b) to double.
 */
      coerce_subset_input_double(b,tmp_b,i,type_b,1,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_b to appropriate location in b.
 */
      tmp_b = &((double*)b)[i];
    }
    if(type_output == NCL_double) tmp_alpha = &((double*)alpha)[i];

    if(contains_missing(tmp_x,1,has_missing_x,missing_dx.doubleval)) {
      *tmp_alpha = missing_dx.doubleval;
    }
    else {
      NGCALLF(betainc,BETAINC)(tmp_x,tmp_a,tmp_b,tmp_alpha);
    }
    if(type_output != NCL_double) ((float*)alpha)[i] = (float)*tmp_alpha;
  }
/*
 * Free memory.
 */
  if(type_x != NCL_double) {
    NclFree(tmp_x);
    NclFree(tmp_alpha);
  }
  if(type_a != NCL_double) NclFree(tmp_a);
  if(type_b != NCL_double) NclFree(tmp_b);

/*
 * Return. 
 */
  if(has_missing_x) {
    ret = NclReturnValue(alpha,ndims_x,dsizes_x,&missing_x,type_output,0);
  }
  else {
    ret = NclReturnValue(alpha,ndims_x,dsizes_x,NULL,type_output,0);
  }
  return(ret);
}


NhlErrorTypes gammainc_W( void )
{
/*
 * Input array variables
 */
  void *x, *a;
  double *tmp_x = NULL;
  double *tmp_a = NULL;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int ndims_a;
  ng_size_t dsizes_a[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_x, type_a, type_output;
/*
 * output variable 
 */
  void *cum;
  double *tmp_cum = NULL;
  double tmp_ccum;
  ng_size_t size_cum;
/*
 * Declare various variables for random purposes.
 */
  ng_size_t i;
  int ret;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */

/*
 * Retrieve argument #1
 */
  x = (void*)NclGetArgValue(
          0,
          2,
          &ndims_x,
          dsizes_x,
          NULL,
          NULL,
          &type_x,
          DONT_CARE);

  a = (void*)NclGetArgValue(
          1,
          2,
          &ndims_a,
          dsizes_a,
          NULL,
          NULL,
          &type_a,
          DONT_CARE);


/*
 * Check dimensions.
 */
  if (ndims_x != ndims_a) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gammainc: The two input arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_x; i++ ) {
    if (dsizes_x[i] != dsizes_a[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"gammainc: The two input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Calculate size of output value.
 */
  size_cum = 1;
  for( i = 0; i < ndims_x; i++ ) size_cum *= dsizes_x[i];

/*
 * Coerce x and a to double if necessary, and create space for output array.
 */
  if(type_x != NCL_double) {
    tmp_x   = (double*)calloc(1,sizeof(double));
    cum     =  (float*)calloc(size_cum,sizeof(float));
    tmp_cum = (double*)calloc(1,sizeof(double));

    if(tmp_x== NULL || tmp_cum == NULL || cum == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"gammainc: Unable to allocate memory for input/output arrays");
      return(NhlFATAL);
    }
    type_output = NCL_float;
  }
  else {
    cum = (double*)calloc(size_cum,sizeof(double));
    if(cum == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"gammainc: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    type_output = NCL_double;
  }

  if(type_a != NCL_double) {
    tmp_a = (double*)calloc(1,sizeof(double));
    if( tmp_a == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"gammainc: Unable to allocate memory for coercing a to double precision");
      return(NhlFATAL);
    }
  }

/*
 * Call the Fortran version of this routine.
 */
  for( i = 0; i < size_cum; i++ ) {
    if(type_x != NCL_double) {
/*
 * Coerce subsection of x (tmp_x) to double, if necessary.
 */
      coerce_subset_input_double(x,tmp_x,i,type_x,1,0,NULL,NULL);
    }
    else {
/*
 * Otherwise, point tmp_x to appropriate location in x.
 */
      tmp_x = &((double*)x)[i];
    }
    if(type_a != NCL_double) {
/*
 * Coerce subsection of a (tmp_a) to double, if necessary.
 */
      coerce_subset_input_double(a,tmp_a,i,type_a,1,0,NULL,NULL);
    }
    else {
/*
 * Otherwise, point tmp_a to appropriate location in a.
 */
      tmp_a = &((double*)a)[i];
    }

    if(type_output == NCL_double) tmp_cum = &((double*)cum)[i];
    NGCALLF(cumgam,CUMGAM)(tmp_x,tmp_a,tmp_cum,&tmp_ccum);
    if(type_output != NCL_double) ((float*)cum)[i] = (float)*tmp_cum;
  }
/*
 * Free memory.
 */
  if(type_x != NCL_double) {
    NclFree(tmp_x);
    NclFree(tmp_cum);
  }
  if(type_a != NCL_double) NclFree(tmp_a);

/*
 * Return.
 */
  ret = NclReturnValue(cum,ndims_x,dsizes_x,NULL,type_output,0);
  return(ret);

}

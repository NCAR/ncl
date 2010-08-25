#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(derrf,DERRF)(double *, double *);
extern void NGCALLF(derrcf,DERRCF)(int *, double *, double *);


NhlErrorTypes erf_W( void )
{
/*
 * Input variables
 */
/*
 * Argument # 0
 */
  void *x;
  double *tmp_x = NULL;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_x;

/*
 * Return variable
 */
  void *result;
  double *tmp_result = NULL;
  ng_size_t *dsizes_result;
  NclBasicDataTypes type_result;


/*
 * Various
 */
  int i, size_output, ret;

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
           1,
           &ndims_x,
           dsizes_x,
           NULL,
           NULL,
           &type_x,
           DONT_CARE);

/* 
 * Allocate space for coercing input to double if necessary.
 */
  if(type_x != NCL_double) {
    type_result = NCL_float;
    tmp_x = (void *)calloc(1, sizeof(double));
    if(tmp_x == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"erf: Unable to allocate memory for temporary input array");
      return(NhlFATAL);
    }
  }
  else {
    type_result = NCL_double;
  }
/*
 * Calculate size of leftmost dimensions, which is basically the whole
 * array because the Fortran routine is expecting scalars.
 */
  size_output = 1;
  for(i = 0; i < ndims_x; i++) size_output *= dsizes_x[i];

/* 
 * Allocate space for output array.
 */
  if(type_result != NCL_double) {
    result = (void *)calloc(size_output, sizeof(float));
    tmp_result = (double *)calloc(1,sizeof(double));
    if(tmp_result == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"erfc: Unable to allocate memory for temporary output array");
      return(NhlFATAL);
    }
  }
  else {
    result = (void *)calloc(size_output, sizeof(double));
  }
  if(result == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"erfc: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/* 
 * Allocate space for output dimension sizes and set them.
 */
  dsizes_result = (ng_size_t*)calloc(ndims_x,sizeof(ng_size_t));  
  if( dsizes_result == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"erf: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_x; i++) dsizes_result[i] = dsizes_x[i];

/*
 * Loop across dimensions and call the Fortran routine for each
 * one-dimensional subsection.
 */

  for(i = 0; i < size_output; i++) {
/*
 * Coerce subsection of x (tmp_x) to double if necessary.
 */
    if(type_x != NCL_double) {
      coerce_subset_input_double(x,tmp_x,i,type_x,1,0,NULL,NULL);
    }
    else {
      tmp_x = &((double*)x)[i];
    }

/*
 * Point temporary output array to void output array if appropriate.
 */
    if(type_result == NCL_double) tmp_result = &((double*)result)[i];

/*
 * Call the Fortran routine.
 */
    NGCALLF(derrf,DERRF)(tmp_x, tmp_result);

/*
 * Coerce output back to float if necessary.
 */
    if(type_result == NCL_float) {
      coerce_output_float_only(result,tmp_result,1,i);
    }
  }

/*
 * Free unneeded memory.
 */
  if(type_x != NCL_double) NclFree(tmp_x);
  if(type_result != NCL_double) NclFree(tmp_result);

/*
 * Return value back to NCL script.
 */
  ret = NclReturnValue(result,ndims_x,dsizes_result,NULL,type_result,0);
  NclFree(dsizes_result);
  return(ret);
}

NhlErrorTypes erfc_W( void )
{
/*
 * Input variables
 */
/*
 * Argument # 0
 */
  void *x;
  double *tmp_x = NULL;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_x;

/*
 * Return variable
 */
  void *result;
  double *tmp_result = NULL;
  ng_size_t *dsizes_result;
  NclBasicDataTypes type_result;

/*
 * Various
 */
  int i, size_output, iopt = 0, ret;

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
           1,
           &ndims_x,
           dsizes_x,
           NULL,
           NULL,
           &type_x,
           DONT_CARE);

/* 
 * Allocate space for coercing input to double if necessary.
 */
  if(type_x != NCL_double) {
    type_result = NCL_float;
    tmp_x = (void *)calloc(1, sizeof(double));
    if(tmp_x == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"erfc: Unable to allocate memory for temporary input array");
      return(NhlFATAL);
    }
  }
  else {
    type_result = NCL_double;
  }

/*
 * Calculate size of leftmost dimensions, which is basically the whole
 * array because the Fortran routine is expecting scalars.
 */
  size_output = 1;
  for(i = 0; i < ndims_x; i++) size_output *= dsizes_x[i];

/* 
 * Allocate space for output array.
 */
  if(type_result != NCL_double) {
    result = (void *)calloc(size_output, sizeof(float));
    tmp_result = (double *)calloc(1,sizeof(double));
    if(tmp_result == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"erfc: Unable to allocate memory for temporary output array");
      return(NhlFATAL);
    }
  }
  else {
    result = (void *)calloc(size_output, sizeof(double));
  }
  if(result == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"erfc: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/* 
 * Allocate space for output dimension sizes and set them.
 */
  dsizes_result = (ng_size_t*)calloc(ndims_x,sizeof(ng_size_t));  
  if( dsizes_result == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"erfc: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_x; i++) dsizes_result[i] = dsizes_x[i];

/*
 * Loop across all dimensions and call the Fortran routine for each
 * one-dimensional subsection.
 */

  for(i = 0; i < size_output; i++) {
/*
 * Coerce subsection of x (tmp_x) to double if necessary.
 */
    if(type_x != NCL_double) {
      coerce_subset_input_double(x,tmp_x,i,type_x,1,0,NULL,NULL);
    }
    else {
      tmp_x = &((double*)x)[i];
    }


/*
 * Point temporary output array to void output array if appropriate.
 */
    if(type_result == NCL_double) tmp_result = &((double*)result)[i];

/*
 * Call the Fortran routine.
 */
    NGCALLF(derrcf,DERRCF)(&iopt, tmp_x, tmp_result);

/*
 * Coerce output back to float if necessary.
 */
    if(type_result == NCL_float) {
      coerce_output_float_only(result,tmp_result,1,i);
    }
  }

/*
 * Free unneeded memory.
 */
  if(type_x != NCL_double) NclFree(tmp_x);
  if(type_result != NCL_double) NclFree(tmp_result);

/*
 * Return value back to NCL script.
 */
  ret = NclReturnValue(result,ndims_x,dsizes_result,NULL,type_result,0);
  NclFree(dsizes_result);
  return(ret);
}

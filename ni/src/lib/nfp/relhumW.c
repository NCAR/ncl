#include <stdio.h>

/*
 * The following are the required NCAR Graphics include files.
 * They should be located in ${NCARG_ROOT}/include.
 */
#include "wrapper.h"

NhlErrorTypes relhum_W( void )
{
  extern double NGCALLF(drelhum,DRELHUM)(double*,double*,double*);
  int i, j, total_size_rh;
/*
 * Input variables
 */
  void *t, *w, *p;
  double *tmp_t, *tmp_w, *tmp_p;
  int ndims_t, dsizes_t[NCL_MAX_DIMENSIONS];
  int ndims_w, dsizes_w[NCL_MAX_DIMENSIONS];
  int ndims_p, dsizes_p[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_t, type_w, type_p;
/*
 * Output variables
 */
  void *rh;
  double *tmp_rh;
  NclBasicDataTypes type_rh;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 *
 * Retrieve argument #1
 */
  t = (void*)NclGetArgValue(
          0,
          3,
          &ndims_t, 
          dsizes_t,
          NULL,
          NULL,
          &type_t,
          2);
/*
 * Retrieve argument #2
 */
  w = (void*)NclGetArgValue(
          1,
          3,
          &ndims_w, 
          dsizes_w,
          NULL,
          NULL,
          &type_w,
          2);
/*
 * Retrieve argument #3
 */
  p = (void*)NclGetArgValue(
          2,
          3,
          &ndims_p, 
          dsizes_p,
          NULL,
          NULL,
          &type_p,
          2);
/*
 * Check dimensions and calculate total size of arrays.
 */
  if( ndims_t != ndims_w || ndims_t != ndims_p ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"relhum: The input arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
/*
 * Calculate total size of arrays.
 */
  total_size_rh = 1;
  for( i = 0; i < ndims_t; i++ ) {
    if( dsizes_t[i] != dsizes_p[i] || dsizes_t[i] != dsizes_w[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"relhum: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
    total_size_rh *= dsizes_t[i];
  }
/*
 * Coerce data to double if necessary.
 */
  if(type_t != NCL_double) {
    tmp_t = (double*)calloc(1,sizeof(double));
    if( tmp_t == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"relhum: Unable to allocate memory for coercing t array to double precision");
      return(NhlFATAL);
    }
  }
/*
 * Coerce w.
 */
  if(type_w != NCL_double) {
    tmp_w = (double*)calloc(1,sizeof(double));
    if( tmp_w == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"relhum: Unable to allocate memory for coercing w array to double precision");
      return(NhlFATAL);
    }
  }
/*
 * Coerce p.
 */
  if(type_p != NCL_double) {
    tmp_p = (double*)calloc(1,sizeof(double));
    if( tmp_p == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"relhum: Unable to allocate memory for coercing p array to double precision");
      return(NhlFATAL);
    }
  }
/*
 * Allocate space for output array.
 */
  if(type_t != NCL_double && type_w != NCL_double && type_p != NCL_double) {

    type_rh = NCL_float;
    rh     = (void*)calloc(total_size_rh,sizeof(float));
    tmp_rh = (void*)calloc(1,sizeof(float));
    if(tmp_rh == NULL || rh == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"relhum: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    type_rh = NCL_double;
    rh = (void*)calloc(total_size_rh,sizeof(double));
    if(rh == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"relhum: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
/*
 * Call function.
 */
  for( i = 0; i < total_size_rh; i++ ) {
    if(type_t != NCL_double) {
/*
 * Coerce subsection of t (tmp_t) to double.
 */
      coerce_subset_input_double(t,tmp_t,i,type_t,1,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_t to appropriate location in t.
 */
      tmp_t = &((double*)t)[i];
    }

    if(type_w != NCL_double) {
/*
 * Coerce subsection of w (tmp_w) to double.
 */
      coerce_subset_input_double(w,tmp_w,i,type_w,1,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_w to appropriate location in w.
 */
      tmp_w = &((double*)w)[i];
    }

    if(type_p != NCL_double) {
/*
 * Coerce subsection of p (tmp_p) to double.
 */
      coerce_subset_input_double(p,tmp_p,i,type_p,1,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_p to appropriate location in p.
 */
      tmp_p = &((double*)p)[i];
    }

    if(type_rh == NCL_double) tmp_rh = &((double*)rh)[i];

    *tmp_rh = NGCALLF(drelhum,DRELHUM)(tmp_t,tmp_w,tmp_p);
/*
 * Copy output values from temporary tmp_zh to zh.
 */
    if(type_rh != NCL_double) ((float*)rh)[i] = (float)(*tmp_rh);
  }
/*
 * Free memory.
 */
  if(type_t  != NCL_double) NclFree(tmp_t);
  if(type_w  != NCL_double) NclFree(tmp_w);
  if(type_p  != NCL_double) NclFree(tmp_p);
  if(type_rh != NCL_double) NclFree(tmp_rh);

/*
 * Return.
 */
  if(type_rh != NCL_double) {
/*
 * Return float values.
 */
    return(NclReturnValue(rh,ndims_t,dsizes_t,NULL,NCL_float,0));
  }
  else {
/*
 * Return double values.
 */
    return(NclReturnValue(rh,ndims_t,dsizes_t,NULL,NCL_double,0));
  }
}


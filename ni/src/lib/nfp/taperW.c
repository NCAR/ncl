#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(dtaper,DTAPER)(double*,int*,double*,double*,int*);


NhlErrorTypes taper_W( void )
{
/*
 * Input array variables
 */
  void *x, *p;
  int *option;
  double *tmp_x = NULL;
  double *tmp_p;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclScalar missing_x, missing_dx;
  NclBasicDataTypes type_x, type_p;
/*
 * Output array variables
 */
  void *taper;
  double *tmp_taper = NULL;
  NclBasicDataTypes type_taper;
/*
 * Declare various variables for random purposes.
 */
  ng_size_t i, index_x, npts, size_leftmost, size_x;
  logical any_missing;
  int inpts;
/*
 * Retrieve arguments.
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

  p = (void*)NclGetArgValue(
          1,
          3,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_p,
          DONT_CARE);

  option = (int*)NclGetArgValue(
          2,
          3,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);
  
  if(*option != 0 && *option != 1) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"taper: the third argument can currently only be 0 or 1. Defaulting to 0.");
    *option = 0;
  }
/*
 * Compute the total size of the output array.
 */
  npts = dsizes_x[ndims_x-1];
  size_leftmost = 1;
  for( i = 0; i < ndims_x-1; i++ ) size_leftmost *= dsizes_x[i];
  size_x = size_leftmost * npts;

  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"taper: npts = %ld is greater than INT_MAX", npts);
    return(NhlFATAL);
  }
  inpts = (int) npts;

/*
 * Check for missing values.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,NULL);
/*
 * Create temporary array to hold double precision values if necessary.
 */
  if(type_x != NCL_double) {
    tmp_x = (double*)calloc(npts,sizeof(double));
    if( tmp_x == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"taper: Unable to allocate memory for coercing x array to double precision");
      return(NhlFATAL);
    }
  }
/*
 * Coerce p.
 */
  tmp_p = coerce_input_double(p,type_p,1,0,NULL,NULL);
  if( tmp_p == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"taper: Unable to coerce 'p' to double");
    return(NhlFATAL);
  }
/*
 * Check p.
 */
  if( *tmp_p <= 0. || *tmp_p >= 1.) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"taper: p must be 0. < p < 1.");
    return(NhlFATAL);
  }

/*
 * Allocate space for output array
 */
  if(type_x != NCL_double) {
    type_taper = NCL_float;
    taper      = (void*)calloc(size_x,sizeof(float));
    tmp_taper  = (double*)calloc(npts,sizeof(double));
    if(tmp_taper == NULL || taper == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"taper: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    type_taper = NCL_double;
    taper      = (void*)calloc(size_x,sizeof(double));
    if( taper == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"taper: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }

/*
 * Call the Fortran version of this routine.
 */
  any_missing = False;
  index_x = 0;
  for( i = 0; i < size_leftmost; i++ ) {
    if(type_x != NCL_double) {
/*
 * Coerce subsection of x (tmp_x) to double.
 */
      coerce_subset_input_double(x,tmp_x,index_x,type_x,npts,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_x to appropriate location in x.
 */
      tmp_x = &((double*)x)[index_x];
    }
/*
 * Check for missing values.
 */
    if(contains_missing(tmp_x,npts,has_missing_x,missing_dx.doubleval)) {
      any_missing = True;
      set_subset_output_missing(taper,index_x,type_taper,npts,
                                missing_dx.doubleval);
    }
    else {
      if(type_taper == NCL_double) tmp_taper = &((double*)taper)[index_x];

      NGCALLF(dtaper,DTAPER)(tmp_x,&inpts,tmp_p,tmp_taper,option);

      if(type_taper == NCL_float) {
        coerce_output_float_only(taper,tmp_taper,npts,index_x);
      }
    }
    index_x += npts;
  }
  if(any_missing) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"taper_n: One or more input arrays contained missing values. No tapering performed on these arrays.");
  }
/*
 * Free memory.
 */
  if(type_x     != NCL_double) NclFree(tmp_x);
  if(type_p     != NCL_double) NclFree(tmp_p);
  if(type_taper != NCL_double) NclFree(tmp_taper);

  if(has_missing_x) {
    return(NclReturnValue(taper,ndims_x,dsizes_x,&missing_x,type_taper,0));
  }
  else {
    return(NclReturnValue(taper,ndims_x,dsizes_x,NULL,type_taper,0));
  }
}

NhlErrorTypes taper_n_W( void )
{
/*
 * Input array variables
 */
  void *x, *p;
  int *option, *dim;
  double *tmp_x, *tmp_p;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclScalar missing_x, missing_dx;
  NclBasicDataTypes type_x, type_p;
/*
 * Output array variables
 */
  void *taper;
  double *tmp_taper;
  NclBasicDataTypes type_taper;
/*
 * Declare various variables for random purposes.
 */
  ng_size_t i, j, index_x, index_nrnpts, npts;
  ng_size_t size_rl, size_leftmost, size_rightmost, size_x;
  logical any_missing;
  int inpts;
/*
 * Retrieve arguments.
 */
  x = (void*)NclGetArgValue(
          0,
          4,
          &ndims_x,
          dsizes_x,
          &missing_x,
          &has_missing_x,
          &type_x,
          DONT_CARE);

  p = (void*)NclGetArgValue(
          1,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_p,
          DONT_CARE);

  option = (int*)NclGetArgValue(
          2,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

  if(*option != 0 && *option != 1) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"taper_n: the third argument can currently only be 0 or 1. Defaulting to 0.");
    *option = 0;
  }

  dim = (int*)NclGetArgValue(
          3,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);
  
/*
 * Make sure "dim" is a valid dimension.
 */
  if (*dim < 0 || *dim >= ndims_x) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"taper_n: Invalid dimension index for tapering");
    return(NhlFATAL);
  }

/*
 * Compute the total size of the output array.
 */
  npts = dsizes_x[*dim];
  size_rightmost = size_leftmost = 1;
  for( i =      0; i < *dim;    i++ ) size_leftmost  *= dsizes_x[i];
  for( i = *dim+1; i < ndims_x; i++ ) size_rightmost *= dsizes_x[i];

  size_rl = size_leftmost * size_rightmost;
  size_x = size_rl * npts;

  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"taper_n: npts = %ld is greater than INT_MAX", npts);
    return(NhlFATAL);
  }
  inpts = (int) npts;

/*
 * Check for missing values.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,NULL);
/*
 * Create temporary array to hold double precision values if necessary.
 */
  tmp_x = (double*)calloc(npts,sizeof(double));
  if( tmp_x == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"taper_n: Unable to allocate memory for coercing x array to double precision");
    return(NhlFATAL);
  }
/*
 * Coerce p.
 */
  tmp_p = coerce_input_double(p,type_p,1,0,NULL,NULL);
  if( tmp_p == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"taper_n: Unable to coerce 'p' to double");
    return(NhlFATAL);
  }
/*
 * Check p.
 */
  if( *tmp_p <= 0. || *tmp_p >= 1.) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"taper_n: p must be 0. < p < 1.");
    return(NhlFATAL);
  }

/*
 * Allocate space for output array
 */
  tmp_taper = (double*)calloc(npts,sizeof(double));
  if(type_x != NCL_double) {
    type_taper = NCL_float;
    taper      = (void*)calloc(size_x,sizeof(float));
  }
  else {
    type_taper = NCL_double;
    taper      = (void*)calloc(size_x,sizeof(double));
  }

  if( tmp_taper == NULL || taper == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"taper_n: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Call the Fortran version of this routine.
 */
  any_missing = False;
  for( i = 0; i < size_leftmost; i++ ) {
    index_nrnpts = i * size_rightmost * npts;
    for( j = 0; j < size_rightmost; j++ ) {
      index_x = index_nrnpts + j;
/*
 * Coerce subsection of x (tmp_x) to double.
 */
      coerce_subset_input_double_step(x,tmp_x,index_x,size_rightmost,
                                      type_x,npts,0,NULL,NULL);
/*
 * Check for missing values.
 */
      if(contains_missing(tmp_x,npts,has_missing_x,missing_dx.doubleval)) {
        any_missing = True;
        set_subset_output_missing_step(taper,index_x,size_rightmost,
                                       type_taper,npts,missing_dx.doubleval);
      }
      else {
        NGCALLF(dtaper,DTAPER)(tmp_x,&inpts,tmp_p,tmp_taper,option);

        coerce_output_float_or_double_step(taper,tmp_taper,type_taper,npts,
                                           index_x,size_rightmost);
      }
    }
  }
  if(any_missing) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"taper_n: One or more input arrays contained missing values. No tapering performed on these arrays.");
  }
/*
 * Free memory.
 */
  NclFree(tmp_x);
  NclFree(tmp_taper);
  if(type_p     != NCL_double) NclFree(tmp_p);

  if(has_missing_x) {
    return(NclReturnValue(taper,ndims_x,dsizes_x,&missing_x,type_taper,0));
  }
  else {
    return(NclReturnValue(taper,ndims_x,dsizes_x,NULL,type_taper,0));
  }
}


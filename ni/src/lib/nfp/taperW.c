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
  double *tmp_x, *tmp_p;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x, found_missing;
  NclScalar missing_x, missing_dx, missing_rx;
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
  int i, j, index_x, npts, size_leftmost, size_x;
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
          2);

  p = (void*)NclGetArgValue(
          1,
          3,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_p,
          2);

  option = (int*)NclGetArgValue(
          2,
          3,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);
  
/*
 * Compute the total size of the output array.
 */
  npts = dsizes_x[ndims_x-1];
  size_leftmost = 1;
  for( i = 0; i < ndims_x-1; i++ ) size_leftmost *= dsizes_x[i];
  size_x = size_leftmost * npts;

/*
 * Check for missing values.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
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
      set_subset_output_missing(taper,index_x,type_taper,npts,
                                missing_dx.doubleval);
      NhlPError(NhlWARNING,NhlEUNKNOWN,"taper: An input array contains missing values. No tapering performed on this array.");
    }
    else {
      if(type_taper == NCL_double) tmp_taper = &((double*)taper)[index_x];

      NGCALLF(dtaper,DTAPER)(tmp_x,&npts,tmp_p,tmp_taper,option);

      if(type_taper == NCL_float) {
        coerce_output_float_only(taper,tmp_taper,npts,index_x);
      }
    }
    index_x += npts;
  }
/*
 * Free memory.
 */
  if(type_x     != NCL_double) NclFree(tmp_x);
  if(type_taper != NCL_double) NclFree(tmp_taper);

  if(has_missing_x) {
    if(type_taper == NCL_float) {
      return(NclReturnValue(taper,ndims_x,dsizes_x,&missing_rx,
                            type_taper,0));
    }
    else {
      return(NclReturnValue(taper,ndims_x,dsizes_x,&missing_dx,
                            type_taper,0));
    }
  }
  else {
    return(NclReturnValue(taper,ndims_x,dsizes_x,NULL,type_taper,0));
  }
}


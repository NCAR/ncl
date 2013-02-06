#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(gammacomplete,GAMMACOMPLETE)(int*, double*, double*, int*,
                                                 double *);

NhlErrorTypes gamma_W( void )
{
/*
 * Input array variables
 */
  void *x;
  double *tmp_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int ndims_x;
  int has_missing_x;
  NclScalar missing_x, missing_flt_x, missing_dbl_x;
  NclBasicDataTypes type_x;
/*
 * Output array variables
 */
  void *xout = NULL;
  double *tmp_xout;
  NclBasicDataTypes type_xout;
  NclScalar missing_xout;
/*
 * Declare various variables for random purposes.
 */
  int inx;
  ng_size_t i, nx, size_leftmost, size_x, index_x;
/*
 * Retrieve argument.
 */
  x = (void*)NclGetArgValue(
          0,
          1,
          &ndims_x,
          dsizes_x,
          &missing_x,
          &has_missing_x,
          &type_x,
          DONT_CARE);

  nx = dsizes_x[ndims_x-1];
  if(nx > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gamma: nx = %ld is greater than INT_MAX", nx);
    return(NhlFATAL);
  }
  inx = (int) nx;


/*
 * Coerce missing value to double if necessary.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,
                 &missing_dbl_x,&missing_flt_x);

/*
 * Compute the total size of the input array.
 */
  size_leftmost = 1;
  for(i = 0; i < (ndims_x-1); i++) size_leftmost *= dsizes_x[i];
  size_x = size_leftmost * nx;

/*
 * Create temporary array for input if necessary.
 */
  if(type_x != NCL_double) {
    type_xout = NCL_float;

    tmp_x     = (double *)calloc(nx,sizeof(double));
    tmp_xout  = (double *)calloc(nx,sizeof(double));
    xout      = (void*)calloc(size_x,sizeof(float));
    missing_xout.floatval = missing_flt_x.floatval;
    if(tmp_x == NULL || tmp_xout == NULL || xout == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"gamma: Unable to allocate memory for arrays");
      return(NhlFATAL);
    }
  }
  else {
    type_xout = NCL_double;
    xout = (void*)calloc(size_x,sizeof(double));
    missing_xout.doubleval = missing_dbl_x.doubleval;
    if(xout == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"gamma: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }

  index_x = 0;
  for(i = 0; i < size_leftmost; i++) {

/* Point temporary output array to void output array if appropriate. */
    if(type_x == NCL_double) {
      tmp_x    = &((double*)x)[index_x];
      tmp_xout = &((double*)xout)[index_x];
    }
    else {
      coerce_subset_input_double(x,tmp_x,index_x,type_x,nx,0,NULL,NULL);
    }

/* Call the Fortran version of this routine. */
    NGCALLF(gammacomplete,GAMMACOMPLETE)(&inx,tmp_x,tmp_xout,
                                         &has_missing_x,
                                         &missing_dbl_x.doubleval);

/* Coerce back to float. */
    if(type_x != NCL_double) {
      coerce_output_float_only(xout,tmp_xout,nx,index_x);
    }
    index_x += nx;
  }

/* Free memory. */
  if(type_x != NCL_double) {
    NclFree(tmp_x);
    NclFree(tmp_xout);
  }

/*
 * Return.
 */
  if(has_missing_x) {
    return(NclReturnValue(xout,ndims_x,dsizes_x,&missing_xout,
                          type_xout,0));
  }
  else {
    return(NclReturnValue(xout,ndims_x,dsizes_x,NULL,type_xout,0));
  }
}


#include <stdio.h>
#include <math.h>
#include "wrapper.h"

extern double NGCALLF(dflxedy,DFLXEDY)(double *,double *,int *,double *,
                                       int *);

NhlErrorTypes fluxEddy_W( void )
{
/*
 * Input array variables
 */
  void *x, *y;
  double *tmp_x = NULL;
  double *tmp_y = NULL;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  int ndims_y;
  ng_size_t dsizes_y[NCL_MAX_DIMENSIONS];
  int has_missing_y;
  NclScalar missing_x, missing_y;
  NclScalar missing_dx, missing_dy, missing_rx;
  NclBasicDataTypes type_x, type_y;
/*
 * Output array variables
 */
  void *fluxeddy;
  double *tmp_fluxeddy = NULL;
  int ndims_fluxeddy;
  ng_size_t size_fluxeddy;
  ng_size_t dsizes_fluxeddy[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_fluxeddy;
/*
 * Declare various variables for random purposes.
 */
  ng_size_t i, ntime, index_xy;
  int intime, ier;

/*
 * Retrieve arguments.
 */
  x = (void*)NclGetArgValue(
          0,
          2,
          &ndims_x,
          dsizes_x,
          &missing_x,
          &has_missing_x,
          &type_x,
          DONT_CARE);

  y = (void*)NclGetArgValue(
          1,
          2,
          &ndims_y,
          dsizes_y,
          &missing_y,
          &has_missing_y,
          &type_y,
          DONT_CARE);
/*
 * x and y must have the same dimensions.
 */
  if( ndims_x < 1 || ndims_x != ndims_y ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"fluxEddy: The input arrays x and y must have the same number of dimensions");
    return(NhlFATAL);
  }
  if( dsizes_x[ndims_x-1] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"fluxEddy: The rightmost dimension of x is greater than INT_MAX");
    return(NhlFATAL);
  }
  ntime  = dsizes_x[ndims_x-1];
  intime = (int) ntime;

  for( i = 0; i < ndims_x; i++ ) {
    if(dsizes_x[i] != dsizes_y[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"fluxEddy: The input arrays x and y must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Compute the total size of the output array (minus the last dimension).
 */
  size_fluxeddy = 1;

  dsizes_fluxeddy[0] = 1;
  for( i = 0; i < ndims_x-1; i++ ) {
    size_fluxeddy     *= dsizes_x[i];
    dsizes_fluxeddy[i] = dsizes_x[i];
  }
  ndims_fluxeddy = ndims_x - 1 < 1 ? 1 : ndims_x - 1;
        
/*
 * Coerce missing values to double.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
  coerce_missing(type_y,has_missing_y,&missing_y,&missing_dy,NULL);
/*
 * allocate space for temporary input/output arrays.
 */
  if(type_x != NCL_double) {
    tmp_x = (double*)calloc(ntime,sizeof(double));
    if(tmp_x == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"fluxEddy: Unable to allocate memory for input array");
      return(NhlFATAL);
    }
  }
  if(type_y != NCL_double) {
    tmp_y = (double*)calloc(ntime,sizeof(double));
    if(tmp_y == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"fluxEddy: Unable to allocate memory for input array");
      return(NhlFATAL);
    }
  }
  if(type_x != NCL_double && type_y != NCL_double) {
    type_fluxeddy = NCL_float;
    fluxeddy     = (void*)calloc(size_fluxeddy,sizeof(float));
    tmp_fluxeddy = (double*)calloc(1,sizeof(double));
    if(fluxeddy == NULL || tmp_fluxeddy == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"fluxEddy: Unable to allocate memory for output arrays");
      return(NhlFATAL);
    }    
  }
  else {
    type_fluxeddy = NCL_double;
    fluxeddy = (void*)calloc(size_fluxeddy,sizeof(double));
    if(fluxeddy == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"fluxEddy: Unable to allocate memory for output array");
      return(NhlFATAL);
    }    
  }
/*
 * Call the Fortran version of this routine.
 *
 */
  index_xy = 0;
  for( i = 0; i < size_fluxeddy; i++ ) {
    if(type_x != NCL_double) {
/*
 * Coerce ntime subsection of x (tmp_x) to double.
 */
      coerce_subset_input_double(x,tmp_x,index_xy,type_x,ntime,0,&missing_x,
                                 &missing_dx);
    }
    else {
/*
 * Point tmp_x to appropriate locations in x.
 */
      tmp_x = &((double*)x)[index_xy];
    }
    if(type_y != NCL_double) {
/*
 * Coerce ntime subsection of y (tmp_y) to double.
 */
      coerce_subset_input_double(y,tmp_y,index_xy,type_y,ntime,0,&missing_y,
                                 &missing_dy);
    }
    else {
/*
 * Point tmp_y to appropriate locations in y.
 */
      tmp_y = &((double*)y)[index_xy];
    }
    if(type_fluxeddy == NCL_double) tmp_fluxeddy = &((double*)fluxeddy)[i];

    *tmp_fluxeddy = NGCALLF(dflxedy,DFLXEDY)(tmp_x,tmp_y,&intime,
                                             &missing_dx.doubleval,&ier);
/*
 * Copy output values from temporary tmp_fluxeddy to "fluxeddy".
 */
    if(type_fluxeddy != NCL_double) {
      ((float*)fluxeddy)[i] = (float)(*tmp_fluxeddy);
    }
    index_xy += ntime;
  }

/*
 * free memory.
 */
  if(type_x != NCL_double) NclFree(tmp_x);
  if(type_y != NCL_double) NclFree(tmp_y);
  if(type_fluxeddy != NCL_double) NclFree(tmp_fluxeddy);

/*
 * Return values. 
 */
  if(type_fluxeddy != NCL_double) {
/*
 * Return float values.  A missing value is returned regardless if a
 * missing value was originally set (the default is used if none set).
 */
    return(NclReturnValue(fluxeddy,ndims_fluxeddy,dsizes_fluxeddy,
                          &missing_rx,NCL_float,0));
  }
  else {
/*
 * Return double values.  A missing value is returned regardless if a
 * missing value was originally set (the default is used if none set).
 */
    return(NclReturnValue(fluxeddy,ndims_fluxeddy,dsizes_fluxeddy,
                          &missing_dx,NCL_double,0));
  }
}


#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(dlspoly,DLSPOLY)(int *, int *, double *, double *,
                                     double *, double *, int *);

NhlErrorTypes lspoly_W( void )
{
/*
 * Input variables
 */
  void *x, *y, *wgt;
  double *tmp_x = NULL;
  double *tmp_y = NULL;
  double *tmp_wgt, *wgt_scalar;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  int ndims_y;
  ng_size_t dsizes_y[NCL_MAX_DIMENSIONS];
  int has_missing_y;
  int ndims_wgt;
  ng_size_t dsizes_wgt[NCL_MAX_DIMENSIONS];
  NclScalar missing_x, missing_dx, missing_y, missing_dy;
  int *ncoef;
  NclBasicDataTypes type_x, type_y, type_wgt;
/*
 * Output variables.
 */
  void *coef;
  double *tmp_coef = NULL;
  NclBasicDataTypes type_coef;
  ng_size_t *dsizes_coef;
/*
 * Other variables
 */
  int ierr, ret, inpts;
  ng_size_t i, j, index_x, index_coef;
  ng_size_t size_leftmost, total_size_x, total_size_coef, npts, is_scalar_wgt;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
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

  y = (void*)NclGetArgValue(
          1,
          4,
          &ndims_y,
          dsizes_y,
          &missing_y,
          &has_missing_y,
          &type_y,
          DONT_CARE);

  wgt = (void*)NclGetArgValue(
          2,
          4,
          &ndims_wgt,
          dsizes_wgt,
          NULL,
          NULL,
          &type_wgt,
          DONT_CARE);

  is_scalar_wgt = is_scalar(ndims_wgt,dsizes_wgt);

  ncoef = (int*)NclGetArgValue(
          3,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);
/*
 * Error checking.
 */
  if(ndims_x != ndims_y) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lspoly: The x, y arrays must have the same number of dimensions");
    return(NhlFATAL);
  }

  if(!is_scalar_wgt && ndims_x != ndims_wgt) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lspoly: wgt must be a scalar or an array of the same number of dimensions as x and y");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_x; i++) {
    if(dsizes_x[i] != dsizes_y[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lspoly: The x, y arrays must be the same dimensionality");
      return(NhlFATAL);
    }
    if(!is_scalar_wgt && dsizes_wgt[i] != dsizes_x[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lspoly: wgt must be a scalar or an array of the same dimensionality as x and y");
      return(NhlFATAL);
    }
  }
  npts = dsizes_x[ndims_x-1];

  if(npts < *ncoef) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lspoly: The number of coefficients must be less than or equal to the rightmost dimension of x and y");
    return(NhlFATAL);
  }

/*
 * Test input dimension size.
 */
  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lspoly: npts = %ld is greater than INT_MAX", npts);
    return(NhlFATAL);
  }
  inpts = (int) npts;

/*
 * Compute the total number of elements in our x,y arrays, and set the 
 * dimension sizes for the output array.
 */
  dsizes_coef = (ng_size_t*)calloc(ndims_x,sizeof(ng_size_t));
  size_leftmost = 1;
  for( i = 0; i < ndims_x-1; i++ ) {
    size_leftmost *= dsizes_x[i];
    dsizes_coef[i] = dsizes_x[i];
  }
  dsizes_coef[ndims_x-1] = *ncoef;
  total_size_coef = *ncoef * size_leftmost;
  total_size_x    =   npts * size_leftmost;
/*
 * Allocate space for temporary weights.
 *
 * If wgt is a scalar, then copy it to an nD array of same size as x.
 * Otherwise, since we don't want to change the weights that are
 * inputted, make a copy of this array even if it is already double.
 * coerce_subset_input_double is used over coerce_input_double because 
 * it does the copy no matter what.
 */
  tmp_wgt = (double*)calloc(total_size_x,sizeof(double));
  if( tmp_wgt == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lspoly: Unable to allocate memory for temporary array");
    return(NhlFATAL);
  }
  if(!is_scalar_wgt) {
    coerce_subset_input_double(wgt,tmp_wgt,0,type_wgt,total_size_x,
                               0,NULL,NULL);
  }
  else {
    wgt_scalar = coerce_input_double(wgt,type_wgt,1,0,NULL,NULL);
    for (i = 0; i < total_size_x; i++) tmp_wgt[i] = *wgt_scalar;
  }

/*
 * Coerce missing values.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,NULL);
  coerce_missing(type_y,has_missing_y,&missing_y,&missing_dy,NULL);

/*
 * Get type of output array and then allocate space for it. Also create
 * space for temporary input arrays.
 */
  type_coef = NCL_float;

  if(type_x != NCL_double) {
    tmp_x = (double*)calloc(npts,sizeof(double));
    if( tmp_x == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lspoly: Unable to allocate memory for temporary array");
      return(NhlFATAL);
    }
  }
  else {
    type_coef = NCL_double;
  }

  if(type_y != NCL_double) {
    tmp_y = (double*)calloc(npts,sizeof(double));
    if( tmp_y == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lspoly: Unable to allocate memory for temporary array");
      return(NhlFATAL);
    }
  }
  else {
    type_coef = NCL_double;
  }

  if(type_coef == NCL_double) {
    coef = (void*)calloc(total_size_coef,sizeof(double));
    if(coef == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lspoly: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    coef     = (void*)calloc(total_size_coef,sizeof(float));
    tmp_coef = (double*)calloc(npts,sizeof(double));
    if(coef == NULL || tmp_coef == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lspoly: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }

/*
 * Loop across leftmost dimensions and call Fortran function, passing in
 * subsets of the input at a time.
 */
  index_x = index_coef = 0;

  for( i = 0; i < size_leftmost; i++ ) {
    if(type_x != NCL_double) {
/*
 * Coerce subsection of x (tmp_x) to double.
 */
      coerce_subset_input_double(x,tmp_x,index_x,type_x,npts,has_missing_x,
                                 &missing_x,&missing_dx);
    }
    else {
      tmp_x = &((double*)x)[index_x];
    }
    if(type_y != NCL_double) {
/*
 * Coerce subsection of y (tmp_y) to double.
 */
      coerce_subset_input_double(y,tmp_y,index_x,type_y,npts,has_missing_y,
                                 &missing_y,&missing_dy);
    }
    else {
      tmp_y = &((double*)y)[index_x];
    }
/*
 * Test for missing values. If x or y are missing for a particular 
 * coordinate pair, set the weight to 0.0 for that pair. 
 */
    for(j = 0; j < npts; j++) {
      if((tmp_x[j] == missing_dx.doubleval) || 
         (tmp_y[j] == missing_dy.doubleval)) {
        tmp_wgt[index_x+j] = 0.0;
      }
    }

    if(type_coef == NCL_double) tmp_coef = &((double*)coef)[index_coef];

    NGCALLF(dlspoly,DLSPOLY)(ncoef,&inpts,tmp_x,tmp_y,&tmp_wgt[index_x],
                             tmp_coef,&ierr);

    coerce_output_float_or_double(coef,tmp_coef,type_coef,*ncoef,index_coef);

    index_x    += npts;
    index_coef += *ncoef;
  }
/*
 * Free temp array.
 */
  if(type_x    != NCL_double) NclFree(tmp_x);
  if(type_y    != NCL_double) NclFree(tmp_y);
  if(type_coef != NCL_double) NclFree(tmp_coef);
  NclFree(tmp_wgt);

  ret = NclReturnValue(coef,ndims_x,dsizes_coef,NULL,type_coef,0);
  NclFree(dsizes_coef);
  return(ret);
}

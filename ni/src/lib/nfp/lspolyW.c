#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(dlspoly,DLSPOLY)(int *, int *, double *, double *,
                                     double *, double *, int *);

extern void NGCALLF(polft,POLFT)(int *, double *, double *, double *,
                                 int *, int *, double *, double *, 
                                 int *, double *, double *, int *, int *,
                                 double *);

extern void NGCALLF(polftmsg,POLFTMSG)(int *, double *, double *, double *,
                                       double *, double *,
                                       int *, int *, double *, double *, 
                                       int *, double *, double *, int *, int *,
                                       double *, double *, double *, double*);

/*
 * Any changes to this routine may also need to be 
 * made to lspoly_n_W below. 
 */

NhlErrorTypes lspoly_old_W( void )
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
  int ierr, ret, inpts, is_scalar_wgt;
  ng_size_t i, j, index_y, index_coef;
  ng_size_t size_leftmost, total_size_coef, npts;
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
 * Originally this routine was written to handle multi-d X and Y, and
 * required them to be the same size. It doesn't really make sense for X
 * to be anything but 1D, so for V6.2.0 this routine was updated to allow
 * X to be 1D while Y can be multi-d. The rightmost dimensions of X and Y
 * must be the same in this case.
 */

/*
 * Error checking.
 *
 * wgt can be scalar, 1D, or nD.
 */
  npts          = dsizes_x[ndims_x-1];
  is_scalar_wgt = is_scalar(ndims_wgt,dsizes_wgt);

  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lspoly_old: npts = %ld is greater than INT_MAX", npts);
    return(NhlFATAL);
  }
  inpts = (int) npts;

  if(npts < *ncoef) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lspoly_old: The number of coefficients must be less than or equal to the rightmost dimension of x and y");
    return(NhlFATAL);
  }

/*
 * Error checking. This is a bit ridiculous because x, y, and wgt can be:
 *
 *  x (1d), y (nd), wgt (scalar) (rightmost x and y same length)
 *  x (1d), y (nd), wgt (1d) (x, wgt, rightmost y same length)
 *  x (nd), y (nd), wgt (scalar) (x and y the same dimensionality)
 *  x (nd), y (nd), wgt (nd) (all the same dimensionality)
 */
  if( (ndims_x == 1 && ndims_y > 1 && dsizes_y[ndims_y-1] != npts) ||
      (ndims_x  > 1 && ndims_x != ndims_y)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lspoly_old: The x, y arrays must be the same dimensionality, or x must be one-dimensional and the rightmost dimension of y must be the same as the length of x");
    return(NhlFATAL);
  }

  if(ndims_x > 1) {
    for(i = 0; i < ndims_x; i++) {
      if(dsizes_x[i] != dsizes_y[i]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"lspoly_old: The x, y arrays must be the same dimensionality, or x must be one-dimensional with the same length as the rightmost dimension of y");
        return(NhlFATAL);
      }
    }
  }
  if(!is_scalar_wgt) {
    if((ndims_wgt == 1 && dsizes_wgt[0] != npts) || (ndims_wgt > 1 && ndims_wgt != ndims_y)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lspoly_old: wgt must be a scalar, a one-dimensional array of the same dimensionality as x, or a multi-dimensional array the same size as y");
      return(NhlFATAL);
    }
    if(ndims_wgt > 1) {
      for(i = 0; i < ndims_y; i++) {
        if(dsizes_wgt[i] != dsizes_y[i]) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,"lspoly_old: wgt must be a scalar, a one-dimensional array of the same dimensionality as x, or a multi-dimensional array the same size as y");
          return(NhlFATAL);
        }
      }
    }
  }

/*
 * Compute the total number of elements in our x,y arrays, and set the 
 * dimension sizes for the output array.
 */
  dsizes_coef = (ng_size_t*)calloc(ndims_y,sizeof(ng_size_t));
  size_leftmost = 1;
  for( i = 0; i < ndims_y-1; i++ ) {
    size_leftmost *= dsizes_y[i];
    dsizes_coef[i] = dsizes_y[i];
  }
  dsizes_coef[ndims_y-1] = *ncoef;
  total_size_coef = *ncoef * size_leftmost;
/*
 * Allocate space for temporary weights.
 */
  tmp_wgt = (double*)calloc(npts,sizeof(double));
  if( tmp_wgt == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lspoly_old: Unable to allocate memory for temporary weights array");
    return(NhlFATAL);
  }
  if(is_scalar_wgt) {
    wgt_scalar = coerce_input_double(wgt,type_wgt,1,0,NULL,NULL);
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
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lspoly_old: Unable to allocate memory for temporary array");
      return(NhlFATAL);
    }
  }
  else {
    type_coef = NCL_double;
  }

  if(type_y != NCL_double) {
    tmp_y = (double*)calloc(npts,sizeof(double));
    if( tmp_y == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lspoly_old: Unable to allocate memory for temporary array");
      return(NhlFATAL);
    }
  }
  else {
    type_coef = NCL_double;
  }

  if(type_coef == NCL_double) {
    coef = (void*)calloc(total_size_coef,sizeof(double));
    if(coef == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lspoly_old: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    coef     = (void*)calloc(total_size_coef,sizeof(float));
    tmp_coef = (double*)calloc(*ncoef,sizeof(double));
    if(coef == NULL || tmp_coef == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lspoly_old: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }

/*
 * Loop across leftmost dimensions and call Fortran function, passing in
 * subsets of the input at a time.
 */
  index_y = index_coef = 0;

  for( i = 0; i < size_leftmost; i++ ) {
    if(ndims_x > 1 || (ndims_x == 1 && i == 0)) { 
      if(type_x != NCL_double) {
/*
 * Coerce subsection of x (tmp_x) to double. Do this only once if
 * x is 1D.
 */
        coerce_subset_input_double(x,tmp_x,index_y,type_x,npts,has_missing_x,
                                   &missing_x,&missing_dx);
      }
      else {
        tmp_x = &((double*)x)[index_y];
      }
    }
    if(type_y != NCL_double) {
/*
 * Coerce subsection of y (tmp_y) to double.
 */
      coerce_subset_input_double(y,tmp_y,index_y,type_y,npts,has_missing_y,
                                 &missing_y,&missing_dy);
    }
    else {
      tmp_y = &((double*)y)[index_y];
    }
/*
 * If wgt is a scalar, then copy it to an nD array of same size as x.
 *
 * If wgt is not a scalar, copy the weights to tmp_wgt every time in
 * the loop no matter what, because the weight array might be modified
 * in order to set weights to 0 where x/y are missing.
 * coerce_subset_input_double is  used because it does the copy no 
 * matter what.
 */
    if(is_scalar_wgt) {
      for(j = 0; j < npts; j++) tmp_wgt[j] = *wgt_scalar;
    }
    else if(ndims_wgt > 1) {
      coerce_subset_input_double(wgt,tmp_wgt,index_y,type_wgt,npts,0,NULL,NULL);
    }
    else {
      coerce_subset_input_double(wgt,tmp_wgt,0,type_wgt,npts,0,NULL,NULL);
    }
/*
 * Test for missing values. If x or y are missing for a particular 
 * coordinate pair, set the weight to 0.0 for that pair. 
 */
    for(j = 0; j < npts; j++) {
      if((has_missing_x && tmp_x[j] == missing_dx.doubleval) || 
         (has_missing_y && tmp_y[j] == missing_dy.doubleval)) {
        tmp_wgt[j] = 0.0;
      }
    }

/*
 * Point output array to appropriate place if necessary.
 */
    if(type_coef == NCL_double) tmp_coef = &((double*)coef)[index_coef];

    NGCALLF(dlspoly,DLSPOLY)(ncoef,&inpts,tmp_x,tmp_y,tmp_wgt,tmp_coef,&ierr);

    coerce_output_float_or_double(coef,tmp_coef,type_coef,*ncoef,index_coef);

    index_y    += npts;
    index_coef += *ncoef;
  }
/*
 * Free temp array.
 */
  if(type_x    != NCL_double) NclFree(tmp_x);
  if(type_y    != NCL_double) NclFree(tmp_y);
  if(type_coef != NCL_double) NclFree(tmp_coef);
  NclFree(tmp_wgt);

  ret = NclReturnValue(coef,ndims_y,dsizes_coef,NULL,type_coef,0);
  NclFree(dsizes_coef);
  return(ret);
}

/*
 * Any changes to this routine may also need to be 
 * made to lspoly_old_W above.
 */
NhlErrorTypes lspoly_n_old_W( void )
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
  int *dim;
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
  int ierr, ret, inpts, is_scalar_wgt;
  ng_size_t i, j, k, index_y, index_coef;
  ng_size_t nrny, nrnc, index_nry, index_nrc;
  ng_size_t size_leftmost, size_rightmost, size_rl;
  ng_size_t total_size_coef, npts;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  x = (void*)NclGetArgValue(
          0,
          5,
          &ndims_x,
          dsizes_x,
          &missing_x,
          &has_missing_x,
          &type_x,
          DONT_CARE);

  y = (void*)NclGetArgValue(
          1,
          5,
          &ndims_y,
          dsizes_y,
          &missing_y,
          &has_missing_y,
          &type_y,
          DONT_CARE);

  wgt = (void*)NclGetArgValue(
          2,
          5,
          &ndims_wgt,
          dsizes_wgt,
          NULL,
          NULL,
          &type_wgt,
          DONT_CARE);

  ncoef = (int*)NclGetArgValue(
          3,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);
/*
 * Retrieve argument #5
 */
  dim = (int*)NclGetArgValue(
          4,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

/*
 * Make sure "dim" is a valid dimension.
 */
  if (*dim < 0 || *dim >= ndims_y) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lspoly_n_old: Invalid dimension index (%d) to do calculation on", *dim);
    return(NhlFATAL);
  }

/*
 * Error checking.
 *
 * wgt can be scalar, 1D, or nD.
 */
  npts          = dsizes_y[*dim];
  is_scalar_wgt = is_scalar(ndims_wgt,dsizes_wgt);

  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lspoly_n_old: npts = %ld is greater than INT_MAX", npts);
    return(NhlFATAL);
  }
  inpts = (int) npts;

  if(npts < *ncoef) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lspoly_n_old: The number of coefficients must be less than or equal to the rightmost dimension of x and y");
    return(NhlFATAL);
  }

/*
 * Error checking. This is a bit ridiculous because x, y, and wgt can be:
 *
 *  x (1d), y (nd), wgt (scalar) (rightmost x and y same length)
 *  x (1d), y (nd), wgt (1d) (x, wgt, rightmost y same length)
 *  x (nd), y (nd), wgt (scalar) (x and y the same dimensionality)
 *  x (nd), y (nd), wgt (nd) (all the same dimensionality)
 */
  if( (ndims_x == 1 && ndims_y > 1 && dsizes_y[ndims_y-1] != npts) ||
      (ndims_x  > 1 && ndims_x != ndims_y)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lspoly_n_old: The x, y arrays must be the same dimensionality, or x must be one-dimensional and the rightmost dimension of y must be the same as the length of x");
    return(NhlFATAL);
  }

  if(ndims_x > 1) {
    for(i = 0; i < ndims_x; i++) {
      if(dsizes_x[i] != dsizes_y[i]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"lspoly_n_old: The x, y arrays must be the same dimensionality, or x must be one-dimensional with the same length as the rightmost dimension of y");
        return(NhlFATAL);
      }
    }
  }
  if(!is_scalar_wgt) {
    if((ndims_wgt == 1 && dsizes_wgt[0] != npts) || (ndims_wgt > 1 && ndims_wgt != ndims_y)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lspoly_n_old: wgt must be a scalar, a one-dimensional array of the same dimensionality as x, or a multi-dimensional array the same size as y");
      return(NhlFATAL);
    }
    if(ndims_wgt > 1) {
      for(i = 0; i < ndims_y; i++) {
        if(dsizes_wgt[i] != dsizes_y[i]) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,"lspoly_n_old: wgt must be a scalar, a one-dimensional array of the same dimensionality as x, or a multi-dimensional array the same size as y");
          return(NhlFATAL);
        }
      }
    }
  }

/*
 * Calculate the size of the leftmost and rightmost dimensions
 * of x.
 */
  dsizes_coef = (ng_size_t*)calloc(ndims_y,sizeof(ng_size_t));
  size_rightmost = size_leftmost = 1;
  for( i = 0; i < *dim;    i++ ) {
    dsizes_coef[i] = dsizes_y[i];
    size_leftmost  *= dsizes_y[i];
  }
  dsizes_coef[*dim] = *ncoef;
  for( i = *dim+1; i < ndims_y; i++ ) {
    dsizes_coef[i] = dsizes_y[i];
    size_rightmost *= dsizes_y[i];
  }
  size_rl         = size_leftmost * size_rightmost;
  total_size_coef = *ncoef * size_rl;
/*
 * Allocate space for temporary weights.
 */
  tmp_wgt = (double*)calloc(npts,sizeof(double));
  if( tmp_wgt == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lspoly_n_old: Unable to allocate memory for temporary weights array");
    return(NhlFATAL);
  }
  if(is_scalar_wgt) {
    wgt_scalar = coerce_input_double(wgt,type_wgt,1,0,NULL,NULL);
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
  if(type_x == NCL_double || type_y == NCL_double) {
    type_coef = NCL_double;
  }
  else {
    type_coef = NCL_float;
  }
  tmp_x = (double*)calloc(npts,sizeof(double));
  tmp_y = (double*)calloc(npts,sizeof(double));
  if( tmp_x == NULL || tmp_y == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lspoly_n_old: Unable to allocate memory for temporary arrays");
    return(NhlFATAL);
  }

/*
 * We have to allocate tmp_coef no matter what, because the
 * values may not be contiguous in memory for the xxx_n
 * version of this function.
 */
  if(type_coef == NCL_double) {
    coef = (void*)calloc(total_size_coef,sizeof(double));
  }
  else {
    coef = (void*)calloc(total_size_coef,sizeof(float));
  }
  tmp_coef = (double*)calloc(*ncoef,sizeof(double));
  if(coef == NULL || tmp_coef == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lspoly_n_old: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Loop across leftmost dimensions and call Fortran function, passing in
 * subsets of the input at a time.
 */
  nrny = size_rightmost * npts;
  nrnc = size_rightmost * *ncoef;
  for( i = 0; i < size_leftmost; i++ ) {
    index_nry = i*nrny;
    index_nrc = i*nrnc;
    for( j = 0; j < size_rightmost; j++ ) {
      index_y    = index_nry + j;
      index_coef = index_nrc + j;
/*
 * Coerce subsection of x/y to double. x might be 1D, so only
 * coerce once if that's the case.
 */
      if((ndims_x == 1 && !i && !j) || ndims_x > 1) {
        coerce_subset_input_double_step(x,tmp_x,index_y,
                                        size_rightmost,type_x,
                                        npts,has_missing_x,
                                        &missing_x,&missing_dx);
      }
      coerce_subset_input_double_step(y,tmp_y,index_y,
                                      size_rightmost,type_y,
                                      npts,has_missing_y,
                                      &missing_y,&missing_dy);
/*
 * Coerce weights if they are not scalar.
 */
      if(is_scalar_wgt) {
        for(k = 0; k < npts; k++) tmp_wgt[k] = *wgt_scalar;
      }
      else if(ndims_wgt > 1) {
        coerce_subset_input_double_step(wgt,tmp_wgt,index_y,
                                        size_rightmost,type_wgt,
                                        npts,0,NULL,NULL);
      }
      else {
        coerce_subset_input_double(wgt,tmp_wgt,0,type_wgt,npts,0,NULL,NULL);
      }
/*
 * Test for missing values. If x or y are missing for a particular 
 * coordinate pair, set the weight to 0.0 for that pair. 
 */
      for(k = 0; k < npts; k++) {
        if((has_missing_x && tmp_x[k] == missing_dx.doubleval) || 
           (has_missing_y && tmp_y[k] == missing_dy.doubleval)) {
          tmp_wgt[k] = 0.0;
        }
      }

      NGCALLF(dlspoly,DLSPOLY)(ncoef,&inpts,tmp_x,tmp_y,tmp_wgt,
                               tmp_coef,&ierr);

      coerce_output_float_or_double_step(coef,tmp_coef,type_coef,*ncoef,
                                         index_coef,size_rightmost);
    }
  }
/*
 * Free temp arrays.
 */
  NclFree(tmp_x);
  NclFree(tmp_y);
  NclFree(tmp_coef);
  NclFree(tmp_wgt);

  ret = NclReturnValue(coef,ndims_y,dsizes_coef,NULL,type_coef,0);
  NclFree(dsizes_coef);
  return(ret);
}

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
  NclScalar missing_coef, missing_dcoef;
/*
 * Other variables
 */
  int ierr, ret, inpts, is_scalar_wgt, lwork, ndeg, maxdeg, maxdeg1;
  ng_size_t i, j, index_y, index_coef;
  ng_size_t size_leftmost, total_size_coef, npts;
  double eps, *tmp_r, *work;
  double *tmp_x_nomsg, *tmp_y_nomsg, *tmp_wgt_nomsg;
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
 * Originally this routine was written to handle multi-d X and Y, and
 * required them to be the same size. It doesn't really make sense for X
 * to be anything but 1D, so for V6.2.0 this routine was updated to allow
 * X to be 1D while Y can be multi-d. The rightmost dimensions of X and Y
 * must be the same in this case.
 */

/*
 * Error checking.
 *
 * wgt can be scalar, 1D, or nD.
 */
  npts          = dsizes_x[ndims_x-1];
  is_scalar_wgt = is_scalar(ndims_wgt,dsizes_wgt);

  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"slatec_lspoly: npts = %ld is greater than INT_MAX", npts);
    return(NhlFATAL);
  }
  inpts = (int) npts;

  if(npts < *ncoef) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"slatec_lspoly: The number of coefficients must be less than or equal to the rightmost dimension of x and y");
    return(NhlFATAL);
  }

/*
 * Error checking. This is a bit ridiculous because x, y, and wgt can be:
 *
 *  x (1d), y (nd), wgt (scalar) (rightmost x and y same length)
 *  x (1d), y (nd), wgt (1d) (x, wgt, rightmost y same length)
 *  x (nd), y (nd), wgt (scalar) (x and y the same dimensionality)
 *  x (nd), y (nd), wgt (nd) (all the same dimensionality)
 */
  if( (ndims_x == 1 && ndims_y > 1 && dsizes_y[ndims_y-1] != npts) ||
      (ndims_x  > 1 && ndims_x != ndims_y)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"slatec_lspoly: The x, y arrays must be the same dimensionality, or x must be one-dimensional and the rightmost dimension of y must be the same as the length of x");
    return(NhlFATAL);
  }

  if(ndims_x > 1) {
    for(i = 0; i < ndims_x; i++) {
      if(dsizes_x[i] != dsizes_y[i]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"slatec_lspoly: The x, y arrays must be the same dimensionality, or x must be one-dimensional with the same length as the rightmost dimension of y");
        return(NhlFATAL);
      }
    }
  }
  if(!is_scalar_wgt) {
    if((ndims_wgt == 1 && dsizes_wgt[0] != npts) || (ndims_wgt > 1 && ndims_wgt != ndims_y)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"slatec_lspoly: wgt must be a scalar, a one-dimensional array of the same dimensionality as x, or a multi-dimensional array the same size as y");
      return(NhlFATAL);
    }
    if(ndims_wgt > 1) {
      for(i = 0; i < ndims_y; i++) {
        if(dsizes_wgt[i] != dsizes_y[i]) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,"slatec_lspoly: wgt must be a scalar, a one-dimensional array of the same dimensionality as x, or a multi-dimensional array the same size as y");
          return(NhlFATAL);
        }
      }
    }
  }

/*
 * Compute the total number of elements in our x,y arrays, and set the 
 * dimension sizes for the output array.
 */
  dsizes_coef = (ng_size_t*)calloc(ndims_y,sizeof(ng_size_t));
  size_leftmost = 1;
  for( i = 0; i < ndims_y-1; i++ ) {
    size_leftmost *= dsizes_y[i];
    dsizes_coef[i] = dsizes_y[i];
  }
  dsizes_coef[ndims_y-1] = *ncoef;
  total_size_coef = *ncoef * size_leftmost;
/*
 * Allocate space for temporary weights.
 */
  tmp_wgt = (double*)calloc(npts,sizeof(double));
  if( tmp_wgt == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"slatec_lspoly: Unable to allocate memory for temporary weights array");
    return(NhlFATAL);
  }
  if(is_scalar_wgt) {
    wgt_scalar = coerce_input_double(wgt,type_wgt,1,0,NULL,NULL);
  }

/*
 * Allocate space for work arrays
 */
  maxdeg  = *ncoef-1;
  maxdeg1 = *ncoef;
  lwork = 3*npts+3*maxdeg+3;
  tmp_r = (double*)calloc(npts,sizeof(double));
  work  = (double*)calloc(lwork,sizeof(double));
  if( tmp_r == NULL || work == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"slatec_lspoly: Unable to allocate memory for work arrays");
    return(NhlFATAL);
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
      NhlPError(NhlFATAL,NhlEUNKNOWN,"slatec_lspoly: Unable to allocate memory for temporary array");
      return(NhlFATAL);
    }
  }
  else {
    type_coef = NCL_double;
  }

  if(type_y != NCL_double) {
    tmp_y = (double*)calloc(npts,sizeof(double));
    if( tmp_y == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"slatec_lspoly: Unable to allocate memory for temporary array");
      return(NhlFATAL);
    }
  }
  else {
    type_coef = NCL_double;
  }

  if(type_coef == NCL_double) {
    coef = (void*)calloc(total_size_coef,sizeof(double));
    if(coef == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"slatec_lspoly: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    coef     = (void*)calloc(total_size_coef,sizeof(float));
    tmp_coef = (double*)calloc(*ncoef,sizeof(double));
    if(coef == NULL || tmp_coef == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"slatec_lspoly: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }

  if(type_coef == NCL_float) {
    missing_coef.floatval   = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
    missing_dcoef.doubleval = (double)missing_coef.floatval;
  }
  else {
    missing_coef.doubleval  = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
    missing_dcoef.doubleval = missing_coef.doubleval;
  }


/*
 * Loop across leftmost dimensions and call Fortran function, passing in
 * subsets of the input at a time.
 */
  index_y = index_coef = 0;

  eps = 0.0;

/*
 * Allocate space for removing missing values from x and y arrays if necessary.
 */
  if(has_missing_x || has_missing_y){
    tmp_x_nomsg   = (double*)calloc(npts,sizeof(double));
    tmp_y_nomsg   = (double*)calloc(npts,sizeof(double));
    tmp_wgt_nomsg = (double*)calloc(npts,sizeof(double));
    if( tmp_x_nomsg == NULL || tmp_y_nomsg == NULL || tmp_wgt_nomsg == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"slatec_lspoly: Unable to allocate memory for temporarily removing missing values");
      return(NhlFATAL);
    }
  }

  for( i = 0; i < size_leftmost; i++ ) {
    if(ndims_x > 1 || (ndims_x == 1 && i == 0)) { 
      if(type_x != NCL_double) {
/*
 * Coerce subsection of x (tmp_x) to double. Do this only once if
 * x is 1D.
 */
        coerce_subset_input_double(x,tmp_x,index_y,type_x,npts,has_missing_x,
                                   &missing_x,&missing_dx);
      }
      else {
        tmp_x = &((double*)x)[index_y];
      }
    }
    if(type_y != NCL_double) {
/*
 * Coerce subsection of y (tmp_y) to double.
 */
      coerce_subset_input_double(y,tmp_y,index_y,type_y,npts,has_missing_y,
                                 &missing_y,&missing_dy);
    }
    else {
      tmp_y = &((double*)y)[index_y];
    }
/*
 * If wgt is a scalar, then copy it to an nD array of same size as x.
 *
 * If wgt is not a scalar, copy the weights to tmp_wgt every time in
 * the loop no matter what, because the weight array might be modified
 * in order to set weights to 0 where x/y are missing.
 * coerce_subset_input_double is  used because it does the copy no 
 * matter what.
 */
    if(is_scalar_wgt) {
      for(j = 0; j < npts; j++) tmp_wgt[j] = *wgt_scalar;
    }
    else if(ndims_wgt > 1) {
      coerce_subset_input_double(wgt,tmp_wgt,index_y,type_wgt,npts,0,NULL,NULL);
    }
    else {
      coerce_subset_input_double(wgt,tmp_wgt,0,type_wgt,npts,0,NULL,NULL);
    }

/*
 * Point output array to appropriate place if necessary.
 */
    if(type_coef == NCL_double) tmp_coef = &((double*)coef)[index_coef];

    if(has_missing_x || has_missing_y) {
      NGCALLF(polftmsg,POLFTMSG)(&inpts,tmp_x,tmp_y,tmp_wgt,
                                 &missing_dx.doubleval,&missing_dy.doubleval,
                                 &maxdeg,&ndeg,&eps,tmp_r,&ierr,work,
                                 tmp_coef,&maxdeg1,&lwork,
                                 &missing_dcoef.doubleval,tmp_x_nomsg,
                                 tmp_y_nomsg,tmp_wgt_nomsg);
    }
    else {
      NGCALLF(polft,POLFT)(&inpts,tmp_x,tmp_y,tmp_wgt,&maxdeg,&ndeg,&eps,
                           tmp_r,&ierr,work,tmp_coef,&maxdeg1,&lwork,
                           &missing_dcoef.doubleval);
    }

    coerce_output_float_or_double(coef,tmp_coef,type_coef,*ncoef,index_coef);

    index_y    += npts;
    index_coef += *ncoef;
  }
/*
 * Free temp array.
 */
  if(type_x    != NCL_double) NclFree(tmp_x);
  if(type_y    != NCL_double) NclFree(tmp_y);
  if(type_coef != NCL_double) NclFree(tmp_coef);
  if(has_missing_x || has_missing_y){
    NclFree(tmp_x_nomsg);
    NclFree(tmp_y_nomsg);
    NclFree(tmp_wgt_nomsg);
  }
  NclFree(tmp_wgt);
  NclFree(tmp_r);
  NclFree(work);

  ret = NclReturnValue(coef,ndims_y,dsizes_coef,&missing_coef,type_coef,0);
  NclFree(dsizes_coef);
  return(ret);
}

/*
 * Any changes to this routine may also need to be 
 * made to slatec_lspoly_W above.
 */
NhlErrorTypes lspoly_n_W( void )
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
  int *dim;
  NclBasicDataTypes type_x, type_y, type_wgt;
/*
 * Output variables.
 */
  void *coef;
  double *tmp_coef = NULL;
  NclScalar missing_coef, missing_dcoef;
  NclBasicDataTypes type_coef;
  ng_size_t *dsizes_coef;
/*
 * Other variables
 */
  int ierr, ret, inpts, is_scalar_wgt, lwork, ndeg, maxdeg, maxdeg1;
  ng_size_t i, j, k, index_y, index_coef;
  ng_size_t nrny, nrnc, index_nry, index_nrc;
  ng_size_t size_leftmost, size_rightmost, size_rl;
  ng_size_t total_size_coef, npts;
  double eps, *tmp_r, *work;
  double *tmp_x_nomsg, *tmp_y_nomsg, *tmp_wgt_nomsg;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  x = (void*)NclGetArgValue(
          0,
          5,
          &ndims_x,
          dsizes_x,
          &missing_x,
          &has_missing_x,
          &type_x,
          DONT_CARE);

  y = (void*)NclGetArgValue(
          1,
          5,
          &ndims_y,
          dsizes_y,
          &missing_y,
          &has_missing_y,
          &type_y,
          DONT_CARE);

  wgt = (void*)NclGetArgValue(
          2,
          5,
          &ndims_wgt,
          dsizes_wgt,
          NULL,
          NULL,
          &type_wgt,
          DONT_CARE);

  ncoef = (int*)NclGetArgValue(
          3,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);
/*
 * Retrieve argument #5
 */
  dim = (int*)NclGetArgValue(
          4,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

/*
 * Make sure "dim" is a valid dimension.
 */
  if (*dim < 0 || *dim >= ndims_y) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lspoly_n: Invalid dimension index (%d) to do calculation on", *dim);
    return(NhlFATAL);
  }

/*
 * Error checking.
 *
 * wgt can be scalar, 1D, or nD.
 */
  npts          = dsizes_y[*dim];
  is_scalar_wgt = is_scalar(ndims_wgt,dsizes_wgt);

  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lspoly_n: npts = %ld is greater than INT_MAX", npts);
    return(NhlFATAL);
  }
  inpts = (int) npts;

  if(npts < *ncoef) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lspoly_n: The number of coefficients must be less than or equal to the rightmost dimension of x and y");
    return(NhlFATAL);
  }

/*
 * Error checking. This is a bit ridiculous because x, y, and wgt can be:
 *
 *  x (1d), y (nd), wgt (scalar) (rightmost x and y same length)
 *  x (1d), y (nd), wgt (1d) (x, wgt, rightmost y same length)
 *  x (nd), y (nd), wgt (scalar) (x and y the same dimensionality)
 *  x (nd), y (nd), wgt (nd) (all the same dimensionality)
 */
  if( (ndims_x == 1 && ndims_y > 1 && dsizes_y[ndims_y-1] != npts) ||
      (ndims_x  > 1 && ndims_x != ndims_y)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lspoly_n: The x, y arrays must be the same dimensionality, or x must be one-dimensional and the rightmost dimension of y must be the same as the length of x");
    return(NhlFATAL);
  }

  if(ndims_x > 1) {
    for(i = 0; i < ndims_x; i++) {
      if(dsizes_x[i] != dsizes_y[i]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"lspoly_n: The x, y arrays must be the same dimensionality, or x must be one-dimensional with the same length as the rightmost dimension of y");
        return(NhlFATAL);
      }
    }
  }
  if(!is_scalar_wgt) {
    if((ndims_wgt == 1 && dsizes_wgt[0] != npts) || (ndims_wgt > 1 && ndims_wgt != ndims_y)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lspoly_n: wgt must be a scalar, a one-dimensional array of the same dimensionality as x, or a multi-dimensional array the same size as y");
      return(NhlFATAL);
    }
    if(ndims_wgt > 1) {
      for(i = 0; i < ndims_y; i++) {
        if(dsizes_wgt[i] != dsizes_y[i]) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,"lspoly_n: wgt must be a scalar, a one-dimensional array of the same dimensionality as x, or a multi-dimensional array the same size as y");
          return(NhlFATAL);
        }
      }
    }
  }

/*
 * Calculate the size of the leftmost and rightmost dimensions
 * of x.
 */
  dsizes_coef = (ng_size_t*)calloc(ndims_y,sizeof(ng_size_t));
  size_rightmost = size_leftmost = 1;
  for( i = 0; i < *dim;    i++ ) {
    dsizes_coef[i] = dsizes_y[i];
    size_leftmost  *= dsizes_y[i];
  }
  dsizes_coef[*dim] = *ncoef;
  for( i = *dim+1; i < ndims_y; i++ ) {
    dsizes_coef[i] = dsizes_y[i];
    size_rightmost *= dsizes_y[i];
  }
  size_rl         = size_leftmost * size_rightmost;
  total_size_coef = *ncoef * size_rl;
/*
 * Allocate space for temporary weights.
 */
  tmp_wgt = (double*)calloc(npts,sizeof(double));
  if( tmp_wgt == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lspoly_n: Unable to allocate memory for temporary weights array");
    return(NhlFATAL);
  }
  if(is_scalar_wgt) {
    wgt_scalar = coerce_input_double(wgt,type_wgt,1,0,NULL,NULL);
  }

/*
 * Allocate space for work arrays
 */
  maxdeg  = *ncoef-1;
  maxdeg1 = *ncoef;
  lwork = 3*npts+3*maxdeg+3;
  tmp_r = (double*)calloc(npts,sizeof(double));
  work  = (double*)calloc(lwork,sizeof(double));
  if( tmp_r == NULL || work == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lspoly_n: Unable to allocate memory for work arrays");
    return(NhlFATAL);
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
  if(type_x == NCL_double || type_y == NCL_double) {
    type_coef = NCL_double;
  }
  else {
    type_coef = NCL_float;
  }
  tmp_x = (double*)calloc(npts,sizeof(double));
  tmp_y = (double*)calloc(npts,sizeof(double));
  if( tmp_x == NULL || tmp_y == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lspoly_n: Unable to allocate memory for temporary arrays");
    return(NhlFATAL);
  }

/*
 * We have to allocate tmp_coef no matter what, because the
 * values may not be contiguous in memory for the xxx_n
 * version of this function.
 */
  if(type_coef == NCL_double) {
    coef = (void*)calloc(total_size_coef,sizeof(double));
  }
  else {
    coef = (void*)calloc(total_size_coef,sizeof(float));
  }
  tmp_coef = (double*)calloc(*ncoef,sizeof(double));
  if(coef == NULL || tmp_coef == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lspoly_n: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

  if(type_coef == NCL_float) {
    missing_coef.floatval   = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
    missing_dcoef.doubleval = (double)missing_coef.floatval;
  }
  else {
    missing_coef.doubleval  = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
    missing_dcoef.doubleval = missing_coef.doubleval;
  }
/*
 * Loop across leftmost dimensions and call Fortran function, passing in
 * subsets of the input at a time.
 */
  nrny = size_rightmost * npts;
  nrnc = size_rightmost * *ncoef;
  eps = 0.0;

/*
 * Allocate space for collapsing x and y arrays if necessary.
 */
  if(has_missing_x || has_missing_y){
    tmp_x_nomsg   = (double*)calloc(npts,sizeof(double));
    tmp_y_nomsg   = (double*)calloc(npts,sizeof(double));
    tmp_wgt_nomsg = (double*)calloc(npts,sizeof(double));
    if( tmp_x_nomsg == NULL || tmp_y_nomsg == NULL || tmp_wgt_nomsg == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lspoly_n: Unable to allocate memory for temporarily removing missing values");
      return(NhlFATAL);
    }
  }

  for( i = 0; i < size_leftmost; i++ ) {
    index_nry = i*nrny;
    index_nrc = i*nrnc;
    for( j = 0; j < size_rightmost; j++ ) {
      index_y    = index_nry + j;
      index_coef = index_nrc + j;
/*
 * Coerce subsection of x/y to double. x might be 1D, so only
 * coerce once if that's the case.
 */
      if((ndims_x == 1 && !i && !j) || ndims_x > 1) {
        coerce_subset_input_double_step(x,tmp_x,index_y,
                                        size_rightmost,type_x,
                                        npts,has_missing_x,
                                        &missing_x,&missing_dx);
      }
      coerce_subset_input_double_step(y,tmp_y,index_y,
                                      size_rightmost,type_y,
                                      npts,has_missing_y,
                                      &missing_y,&missing_dy);
/*
 * Coerce weights if they are not scalar.
 */
      if(is_scalar_wgt) {
        for(k = 0; k < npts; k++) tmp_wgt[k] = *wgt_scalar;
      }
      else if(ndims_wgt > 1) {
        coerce_subset_input_double_step(wgt,tmp_wgt,index_y,
                                        size_rightmost,type_wgt,
                                        npts,0,NULL,NULL);
      }
      else {
        coerce_subset_input_double(wgt,tmp_wgt,0,type_wgt,npts,0,NULL,NULL);
      }
      if(has_missing_x || has_missing_y) {
        NGCALLF(polftmsg,POLFTMSG)(&inpts,tmp_x,tmp_y,tmp_wgt,
                                   &missing_dx.doubleval,&missing_dy.doubleval,
                                   &maxdeg,&ndeg,&eps,tmp_r,&ierr,work,
                                   tmp_coef,&maxdeg1,&lwork,
                                   &missing_dcoef.doubleval,tmp_x_nomsg,
                                   tmp_y_nomsg,tmp_wgt_nomsg);
      }
      else {
        NGCALLF(polft,POLFT)(&inpts,tmp_x,tmp_y,tmp_wgt,&maxdeg,&ndeg,&eps,
                             tmp_r,&ierr,work,tmp_coef,&maxdeg1,&lwork,
                             &missing_dcoef.doubleval);      
      }

      coerce_output_float_or_double_step(coef,tmp_coef,type_coef,*ncoef,
                                         index_coef,size_rightmost);
    }
  }
/*
 * Free temp arrays.
 */
  NclFree(tmp_x);
  NclFree(tmp_y);
  NclFree(tmp_coef);
  NclFree(tmp_wgt);
  NclFree(tmp_r);
  NclFree(work);
  if(has_missing_x || has_missing_y){
    NclFree(tmp_x_nomsg);
    NclFree(tmp_y_nomsg);
    NclFree(tmp_wgt_nomsg);
  }

  ret = NclReturnValue(coef,ndims_y,dsizes_coef,&missing_coef,type_coef,0);
  NclFree(dsizes_coef);
  return(ret);
}


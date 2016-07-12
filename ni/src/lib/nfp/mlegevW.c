#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(dmlegevi,DMLEGEVI)(double *, int *, double *, double *, 
                                       double *, int *);

NhlErrorTypes extval_mlegev_W( void )
{

/*
 * Input variables
 */
/*
 * Argument # 0
 */
  void *x;
  double *tmp_x = NULL;
  int       ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclScalar missing_x, missing_flt_x, missing_dbl_x;
  NclBasicDataTypes type_x;

/*
 * Argument # 1
 */
  int *dims;
  ng_size_t dsizes_dims[1];
/*
 * Argument # 2
 */
  logical *opt;
/*
 * Return variable
 */
  void *vals;
  double tmp_vals[6];
  int       ndims_vals;
  ng_size_t *dsizes_vals;
  NclScalar missing_vals;
  NclBasicDataTypes type_vals;

/*
 * Various
 */
  double *tmp_xx;
  ng_size_t i, j, size_output, nl, nr, nx, nrnx, nr6;
  ng_size_t index_nrx, index_nrv, index_x, index_v;
  int inx, ndims, ierr, ret;


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
           3,
           &ndims_x,
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);

/*
 * Coerce missing value to double if necessary.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,
                 &missing_dbl_x,&missing_flt_x);
/*
 * Get argument # 1
 */
  dims = (int*)NclGetArgValue(
           1,
           3,
           NULL,
           dsizes_dims,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
  ndims = dsizes_dims[0];

/*
 * Make sure input dimensions are valid.
 */
  if(ndims > ndims_x) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"extval_mlegev: Invalid dimension index(es) argument.");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims; i++ ) {
    if(dims[i] < 0 || dims[i] >= ndims_x) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"extval_mlegev: Invalid dimension index(es) argument.");
        return(NhlFATAL);
    }
    if(i > 0 && dims[i] != (dims[i-1]+1)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"extval_mlegev: Input dimension indexes must be monotonically increasing, can't continue");
      return(NhlFATAL);
    }
  }

/*
 * Get argument # 2
 */
  opt = (logical*)NclGetArgValue(
           2,
           3,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

/*
 * Calculate size of leftmost dimensions (nl) up to the dims[0]-th
 *   dimensions.
 * Calculate size of rightmost dimensions (nr) from the
 *   ndims[ndims-1]-th dimension
 * Calculate size of "dims" dimensions (nm) 
 *
 * The dimension(s) to do the calculation across are "dims".
 */
  nl = nr = nx = 1;
  if(ndims_x > 1) {
    ndims_vals = ndims_x-ndims+1;
    dsizes_vals = (ng_size_t *)NclMalloc(ndims_vals * sizeof(ng_size_t));
    for(i = 0; i < dims[0] ; i++) {
      nl = nl*dsizes_x[i];
      dsizes_vals[i] = dsizes_x[i];
    }
    for(i = 0; i < ndims ; i++) {
      nx = nx*dsizes_x[dims[i]];
    }
    for(i = dims[ndims-1]+1; i < ndims_x; i++) {
      nr = nr*dsizes_x[i];
      dsizes_vals[i-ndims] = dsizes_x[i];
    }
    dsizes_vals[ndims_vals-1] = 6;
  } 
  else {
    ndims_vals = 1;
    dsizes_vals = (ng_size_t *)NclMalloc(sizeof(ng_size_t));
    *dsizes_vals = 6;
    nx = dsizes_x[0];
    nr = nl = 1;
  }
  size_output = 6 * nr * nl * nx;

/*
 * Check input dimension size
 */
  if(nx > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"extval_mlegev: nx = %ld is greater than INT_MAX", nx);
    return(NhlFATAL);
  }
  inx = (int) nx;


/*
 * Allocate space for tmp_x.
 */
  tmp_x = (double *)calloc(nx,sizeof(double));
  if(tmp_x == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"extval_mlegev: Unable to allocate memory for coercing input array to double");
    return(NhlFATAL);
  }
/* 
 * Allocate space for output array. Also figure out output missing 
 * value. Use default one if necessary.
 */
  if(type_x != NCL_double) {
    type_vals = NCL_float;
    vals = (void *)calloc(size_output, sizeof(float));
    missing_vals = missing_flt_x;
  }
  else {
    type_vals = NCL_double;
    vals = (void *)calloc(size_output, sizeof(double));
    missing_vals = missing_dbl_x;
  }
  if(vals == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"extval_mlegev: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
  
/*
 * Allocate space for work arrays.
 */
  tmp_xx = (double *)NclMalloc(nx*sizeof(double));
  if(tmp_xx == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"extval_mlegev: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  
/*
 * Loop through leftmost and rightmost dimensions and call Fortran code.
 */
  nrnx = nr * nx;
  nr6  = nr * 6;
  for(i = 0; i < nl; i++) {
    index_nrx = i*nrnx;
    index_nrv = i*nr6;
    for(j = 0; j < nr; j++) {
      index_x = index_nrx + j;
      index_v = index_nrv + (j*6);
/*
 * Coerce subsection of x (tmp_x) to double.
 */
      coerce_subset_input_double_step(x,tmp_x,index_x,nr,type_x,
                                      nx,0,NULL,NULL);
/*
 * Call the Fortran routine.
 */
      NGCALLF(dmlegevi,DMLEGEVI)(tmp_x, &inx, &missing_dbl_x.doubleval, 
                                 tmp_vals, tmp_xx, &ierr);
      
/*
 * Coerce tmp_vals back into vals.
 */
      coerce_output_float_or_double(vals,&tmp_vals[0],type_vals,6,index_v);
    }
  }

/*
 * Free unneeded memory.
 */
  if(type_x != NCL_double) NclFree(tmp_x);
  NclFree(tmp_xx);

/*
 * Return value back to NCL script.
 */
  ret = NclReturnValue(vals,ndims_vals,dsizes_vals,&missing_vals,type_vals,0);
  NclFree(dsizes_vals);
  return(ret);
}

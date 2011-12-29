#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(spigamd,SPIGAMD)(int *, double *, double *, int *, 
                                     double *);

NhlErrorTypes dim_spi_n_W( void )
{

/*
 * Input variables
 */
/*
 * Argument # 0
 */
  void *x;
  double *tmp_x;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclScalar missing_x, missing_flt_x, missing_dbl_x;
  NclBasicDataTypes type_x;

/*
 * Argument # 1
 */
  int *nrun;

/*
 * Argument # 2
 */
  logical *optspi;

/*
 * Argument # 3
 */
  int *dims;
  ng_size_t dsizes_dims;

/*
 * Return variable
 */
  void *spi;
  double *tmp_spi;
  NclScalar missing_spi;
  NclBasicDataTypes type_spi;

/*
 * Various
 */
  ng_size_t ntim;
  int intim, ret;
  ng_size_t index_x, index_nrx;
  ng_size_t i, j, nrnx, total_nr, total_nl, size_output;

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
           4,
           &ndims_x,
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);

/*
 * Get argument # 1
 */
  nrun = (int*)NclGetArgValue(
           1,
           4,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

/*
 * Get argument # 2
 */
  optspi = (logical*)NclGetArgValue(
           2,
           4,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

/*
 * Coerce missing value to double if necessary.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,
                 &missing_dbl_x,&missing_flt_x);

/*
 * Get dimension(s) to do computation on.
 */
  dims = (int*)NclGetArgValue(
           3,
           4,
           NULL,
           &dsizes_dims,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

/*
 * Some error checking. Make sure input dimensions are valid.
 */
  for(i = 0; i < dsizes_dims; i++ ) {
    if(dims[i] < 0 || dims[i] >= ndims_x) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_spi_n: Invalid dimension sizes to do calculations across, can't continue");
      return(NhlFATAL);
    }
    if(i > 0 && dims[i] != (dims[i-1]+1)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_spi_n: Input dimension sizes must be monotonically increasing, can't continue");
      return(NhlFATAL);
    }
  }

/*
 * Calculate size of leftmost dimensions (nl) up to the dims[0]-th
 *   dimensions.
 *
 * Calculate number of points that will be passed to Fortran
 *   routine (ntim).
 *
 * Calculate size of rightmost dimensions (nr) from the
 *   ndims[ndims-1]-th dimension.
 *
 * The dimension(s) to do the calculations across are "dims".
 */
  total_nl = total_nr = ntim = 1;
  if(ndims_x > 1) {
    for(i = 0; i < dims[0] ; i++) {
      total_nl = total_nl*dsizes_x[i];
    }
    for(i = 0; i < dsizes_dims ; i++) {
      ntim = ntim*dsizes_x[dims[i]];
    }
    for(i = dims[dsizes_dims-1]+1; i < ndims_x; i++) {
      total_nr = total_nr*dsizes_x[i];
    }
  } else {
    ntim = dsizes_x[dims[0]];
  }
  size_output = total_nl * ntim * total_nr;

  if( ntim > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_spi_n: ntim is greater than INT_MAX");
    return(NhlFATAL);
  }
  intim = (int) ntim;

/*
 * Allocate space for tmp_x and tmp_index.
 */
  tmp_x   = (double *)calloc(ntim,sizeof(double));
  if(tmp_x == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_spi_n: Unable to allocate memory for coercing input array to double");
    return(NhlFATAL);
  }

/* 
 * Allocate space for output array.
 */
  tmp_spi = (double *)calloc(ntim, sizeof(double));
  if(type_x != NCL_double) {
    type_spi = NCL_float;
    spi     = (void *)calloc(size_output, sizeof(float));
  }
  else {
    type_spi = NCL_double;
    spi      = (void *)calloc(size_output, sizeof(double));
  }
  if(tmp_spi == NULL || spi == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_spi_n: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
  if(has_missing_x) {
    if(type_spi == NCL_double) missing_spi = missing_dbl_x;
    else                       missing_spi = missing_flt_x;
  }

/*
 * Loop across leftmost dimensions and call the Fortran routine for each
 * subsection of the input arrays.
 */
  nrnx = total_nr * ntim;
  for(i = 0; i < total_nl; i++) {
    index_nrx = i*nrnx;
    for(j = 0; j < total_nr; j++) {
      index_x = index_nrx + j;
/*
 * Coerce subsection of x (tmp_x) to double.
 */
      coerce_subset_input_double_step(x,tmp_x,index_x,total_nr,type_x,
                                      ntim,0,NULL,NULL);
/*
 * Call the Fortran routine.
 */
      NGCALLF(spigamd,SPIGAMD)(&intim, tmp_x, &missing_dbl_x.doubleval, 
                               nrun, tmp_spi);
/*
 * Coerce output back to float or double
 */
      coerce_output_float_or_double_step(spi,tmp_spi,type_spi,ntim,
                                         index_x,total_nr);
    }
  }

/*
 * Free unneeded memory.
 */
  NclFree(tmp_x);
  NclFree(tmp_spi);

/*
 * Return value back to NCL script.
 */
  if(has_missing_x) {
    ret = NclReturnValue(spi,ndims_x,dsizes_x,&missing_spi,type_spi,0);
  }
  else {
    ret = NclReturnValue(spi,ndims_x,dsizes_x,NULL,type_spi,0);
  }
  return(ret);
}

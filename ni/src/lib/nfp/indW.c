#include <stdio.h>
#include <string.h>
#include "wrapper.h"
/*
 * This wrapper takes the output returned from the "ind" function
 * and returns an nD array that contains the actual indices represented
 * by the 1D array.
 */

NhlErrorTypes ind_resolve_W( void )
{
/*
 * Input array variables
 */
  int *ind, *dsizes;
  int ndims_ind, ndims_dsizes, dsizes_ind[1], dsizes_dsizes[1];
  NclScalar missing_ind_resolve;
/*
 * Output array variables
 */
  int *ind_resolve, dsizes_ind_resolve[2];
/*
 * various
 */
  int i, j, l, size_input, new_value, *ind_product;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  ind = (int*)NclGetArgValue(
           0,
           2,
           NULL,
           dsizes_ind,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

  dsizes = (int*)NclGetArgValue(
           1,
           2,
           NULL,
           dsizes_dsizes,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
/*
 * Allocate space for output.
 */
  dsizes_ind_resolve[0] = ndims_ind    = dsizes_ind[0];;
  dsizes_ind_resolve[1] = ndims_dsizes = dsizes_dsizes[0];
  ind_resolve = (int*)calloc(ndims_ind * ndims_dsizes,sizeof(int));
  if( ind_resolve == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ind_resolve: Unable to allocate memory for output array");
  }
  
/*
 * Set missing value to the default missing value for integers.
 */
  missing_ind_resolve.intval = (int)((NclTypeClass)nclTypeintClass)->type_class.default_mis.intval;

/*
 * Calculate the products necessary to compute the indices.
 * Also compute the total size of the input dimensions so that we can
 * check that the input indices are in the range.
 */
  ind_product = (int*)calloc(ndims_dsizes,sizeof(int));
  if( ind_product == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ind_resolve: Unable to allocate memory for temporary array");
  }

  ind_product[ndims_dsizes-1] = 1;
  size_input                  = dsizes[0];
  for( i = ndims_dsizes-2; i >= 0; i--) {
    ind_product[i] = ind_product[i+1] * dsizes[i+1];
    size_input *= dsizes[i+1];
  }
  
/*
 * Compute the indices.
 */
  l = 0;
  for(i = 0; i < ndims_ind; i++ ) {
    if(ind[i] < 0 || ind[i] >= size_input) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ind_resolve: One of the input indices is invalid; will return a missing value");
      ind_resolve[l++] = missing_ind_resolve.intval;
    }
    else {
      new_value = ind[i];
      for(j = 0; j < ndims_dsizes; j++ ) {
	ind_resolve[l++] = new_value / ind_product[j];
	new_value           = new_value % ind_product[j];
      }
    }
  }
/*
 * Return values.
 */
  return(NclReturnValue(ind_resolve,2,dsizes_ind_resolve,
			&missing_ind_resolve,NCL_int,0));
}

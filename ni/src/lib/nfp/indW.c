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
  ng_size_t *ind, *dsizes;
  void *ind_input, *dsizes_input;
  int ndims_ind, ndims_dsizes;
  ng_size_t dsizes_ind[1], dsizes_dsizes[1];
  NclScalar missing_ind_resolve;
/*
 * Output array variables
 */
  void *ind_resolve;
  ng_size_t dsizes_ind_resolve[2];
/*
 * various
 */
  ng_size_t i, j, l;
  ng_size_t size_input, new_value, *ind_product;
  NclBasicDataTypes type_ind, type_dsizes, return_type;

/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  ind_input = (void*)NclGetArgValue(
           0,
           2,
           NULL,
           dsizes_ind,
           NULL,
           NULL,
           &type_ind,
           DONT_CARE);

  dsizes_input = (void*)NclGetArgValue(
           1,
           2,
           NULL,
           dsizes_dsizes,
           NULL,
           NULL,
           &type_dsizes,
           DONT_CARE);

  ndims_ind    = (int) dsizes_ind[0];
  ndims_dsizes = (int) dsizes_dsizes[0];

/*
 * Check the input dimensions and compute the total size of the input array.
 */
  dsizes = get_dimensions(dsizes_input,ndims_dsizes,type_dsizes,"ind_resolve");
  ind    = get_dimensions(ind_input,ndims_ind,type_ind,"ind_resolve");
  if(dsizes == NULL || ind == NULL) 
    return(NhlFATAL);

/*
 *  - Calculate the products necessary to compute the indices.
 *  - Compute the total size of the input dimensions so that we can
 *    check that the input indices are in the range of the input
 *    dimension sizes.
 *  - Determine the return type:
 *    - On a 32-bit system, return ints.
 *    - On a 64-bit system, return longs if any of the
 *      input dimension sizes are > INT_MAX, or if the
 *      product of the dimension sizes is > INT_MAX.
 */
  dsizes_ind_resolve[0] = dsizes_ind[0];;
  dsizes_ind_resolve[1] = dsizes_dsizes[0];
  ind_product = (ng_size_t*)calloc(ndims_dsizes,sizeof(ng_size_t));
  if( ind_product == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ind_resolve: Unable to allocate memory for temporary array");
  }

  size_input   = 1;
  return_type  = NCL_int;
  for( i = ndims_dsizes-1; i >= 0; i--) {
    if(i != (ndims_dsizes-1)) {
      ind_product[i] = ind_product[i+1] * dsizes[i+1];
    }
    else {
      ind_product[i] = 1;
    }
    size_input *= dsizes[i];
#if !defined(NG32BIT)
        if(dsizes[i] > INT_MAX || size_input > INT_MAX) {
          return_type = NCL_long;
        }
#endif
  }
  
/*
 * Allocate space for output and set missing value.
 */
  if(return_type == NCL_int) {
        ind_resolve = (void*)calloc(ndims_ind * ndims_dsizes,sizeof(int));
        missing_ind_resolve.intval = (int)((NclTypeClass)nclTypeintClass)->type_class.default_mis.intval;
  }
  else {
        ind_resolve = (void*)calloc(ndims_ind * ndims_dsizes,sizeof(long));
        missing_ind_resolve.longval = (long)((NclTypeClass)nclTypelongClass)->type_class.default_mis.longval;
  }
  if( ind_resolve == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"ind_resolve: Unable to allocate memory for output array");
  }
  
/*
 * Compute the indices.
 */
  l = 0;
  if(return_type == NCL_int) {
        for(i = 0; i < ndims_ind; i++ ) {
          if(ind[i] < 0 || ind[i] >= size_input) {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"ind_resolve: One of the input indices is invalid; will return a missing value");
                ((int*)ind_resolve)[l++] = missing_ind_resolve.intval;
          }
          else {
                new_value = ind[i];
                for(j = 0; j < ndims_dsizes; j++ ) {
                  ((int*)ind_resolve)[l++] = (int)(new_value / ind_product[j]);
                  new_value                = new_value % ind_product[j];
                }
          }
        }
  }
  else {
        for(i = 0; i < ndims_ind; i++ ) {
          if(ind[i] < 0 || ind[i] >= size_input) {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"ind_resolve: One of the input indices is invalid; will return a missing value");
                ((long*)ind_resolve)[l++] = missing_ind_resolve.longval;
          }
          else {
                new_value = ind[i];
                for(j = 0; j < ndims_dsizes; j++ ) {
                  ((long*)ind_resolve)[l++] = (long)(new_value / ind_product[j]);
                  new_value                 = new_value % ind_product[j];
                }
          }
        }
  }
/*
 * Return values.
 */
  NclFree(dsizes);
  NclFree(ind);
  return(NclReturnValue(ind_resolve,2,dsizes_ind_resolve,
                        &missing_ind_resolve,return_type,0));
}

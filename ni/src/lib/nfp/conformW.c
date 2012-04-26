#include <stdio.h>
#include <string.h>
#include "wrapper.h"


/*   
 * This wrapper takes three arguments: an array of any dimensionality
 * (x),  an nD array (or scalar) that's a subset (in size) of x,
 * and an array of dimensions to be conformed. 
 * 
 * The scalar case is a special case. For example, if
 * x(ntim,klvl,nlat,mlon) and dp (scalar), then the
 * third argument must be equal to -1. A new array of
 * size ntim x klvl x nlat x mlon will be created, and
 * dp will be copied to every element.
 *
 *     dpConform = conform (x, dp, -1)      ;  x(ntim,klvl,nlat,mlon)
 *                                          ;      0   1    2    3
 *
 *
 * If dp is NOT a scalar, say x(ntim,klvl,nlat,mlon) and dp(klvl):
 *
 *     dpConform = conform (x, dp, 1)   ;  x(ntim,klvl,nlat,mlon)
 *                                      ;      0   1    2    3
 *
 *  then dp will be copied to an ntim x klvl x nlat x mlon array
 *  (called "dpConform" in this case), and returned.
 *
 * If dp(ntim,nlat):
 *
 *     dpConform = conform (x, dp, (/0,2/))  ;  x(ntim,klvl,nlat,mlon)
 *                                           ;      0   1    2    3
 *
 *  then dp again will be copied to an ntim x klvl x nlat x mlon array
 *  array and returned.
 * 
 * If dp(ntim,nlat), then the following will produce error messages:
 *
 *     dpConform = conform (x, dp, (/3/))
 *     dpConform = conform (x, dp, (/0,3/))
 *     dpConform = conform (x, dp, (/3,1/))
 *     dpConform = conform (x, dp, (/3,4/))
 *                                         
 */

NhlErrorTypes conform_W( void )
{
/*
 * Input array variables
 */
  void *x;
  NclStackEntry data;
  int *cnfrm_dims;
  NclMultiDValData tmp_md = NULL;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_conform[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_x;

/*
 * Output array variables
 */
  void *conform;
  int ndims;
/*
 * various
 */
  int scalar_tmp_md, copy_scalar = 0;
  ng_size_t i, j;
  ng_size_t new_position, size_conform, conform_pos;
  int type_size;
  ng_size_t *skip_x = NULL;
  ng_size_t *skip_c = NULL;
  ng_size_t *indices = NULL;
  int ret;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  x = (void*)NclGetArgValue(
           0,
           3,
           &ndims_x, 
           dsizes_x,
           NULL,
           NULL,
           &type_x,
           DONT_CARE);

  data = _NclGetArg(1,3,DONT_CARE);
  switch(data.kind) {
  case NclStk_VAR:
    tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
    break;
  case NclStk_VAL:
    tmp_md = (NclMultiDValData)data.u.data_obj;
    break;
  default:
    NhlPError(NhlFATAL,NhlEUNKNOWN,"conform: invalid first input argument.");
    return(NhlFATAL);
  }

  cnfrm_dims = (int*)NclGetArgValue(
           2,
           3,
           NULL,
           dsizes_conform,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

/*
 * Check if we're dealing with the special case of where the second
 * argument is a scalar, and the third argument is -1.  If this 
 * is true, then the scalar value will be copied to every 
 * element of the new array.
 */
  scalar_tmp_md = is_scalar(tmp_md->multidval.n_dims,
                            tmp_md->multidval.dim_sizes);

  if(scalar_tmp_md && cnfrm_dims[0] == -1) copy_scalar = 1;

/*
 * If not dealing with the special scalar case, then the number of
 * dimensions of the second argument must be the same as the length
 * of the third argument.
 */
  if(!copy_scalar && tmp_md->multidval.n_dims != dsizes_conform[0]) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"conform: The array to be conformed must have the same number of dimensions as indicated by the length of the last argument");
    return(NhlFATAL);
  }

/*
 * If not a scalar situation, check for errors:
 *
 *    1. The dimension indices indicated by the third argument must
 *       be in the range of the dimension indices of x (i.e. if x is
 *       only 3-dimensional, then one of the indices in the 3rd
 *       argument cannot be a "4").
 *    2. The dimensions indices indicated by the third argument must
 *       be unique and increasing.
 *    4. The dimensions sizes of the second argument must be the same
 *       as those indicated by the third argument.
 */
  if(!copy_scalar) {
    for(i = 0; i < dsizes_conform[0]; i++ ) {
      if(cnfrm_dims[i] < 0 || cnfrm_dims[i] > (ndims_x-1)) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"conform: the third argument contains a dimension that is out-of-range of the dimensions of x");
        return(NhlFATAL);
      }
      if(i > 0) {
        if(cnfrm_dims[i-1] >= cnfrm_dims[i]) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,"conform: The dimensions specified by the third argument must be increasing");
          return(NhlFATAL);
        }
      }
      if(dsizes_x[cnfrm_dims[i]] != tmp_md->multidval.dim_sizes[i] ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"conform: the dimensions sizes of the second argument do not match those indicated by the third argument");
        return(NhlFATAL);
      }
    }
/*
 * Create an array to hold the number of linear elements you have to
 * skip over when you increment a particular index.
 *
 * For example, if you have an array that is 2 x 3 x 2 x 2, then
 * any time you increment the first index of the array, you skip
 * over 12 elements in the linear array (3 x 2 x 2). Any time
 * you increment the second index, you skip over 4 elements (2 x 2),
 * and so on.
 */
    skip_x  = (ng_size_t*)calloc(ndims_x,sizeof(ng_size_t));
    indices = (ng_size_t*)calloc(ndims_x,sizeof(ng_size_t));
    skip_c  = (ng_size_t*)calloc(dsizes_conform[0],sizeof(ng_size_t));

    size_conform = dsizes_x[ndims_x-1];
    skip_x[ndims_x-1] = 1; 

    for(i = ndims_x-2; i >= 0; i--) {
      skip_x[i] = skip_x[i+1]*dsizes_x[i+1];
      size_conform  *= dsizes_x[i];
    }
    skip_c[dsizes_conform[0]-1] = 1;
    for(i = dsizes_conform[0]-2; i >= 0; i--) {
      skip_c[i] = skip_c[i+1]*dsizes_x[cnfrm_dims[i+1]];
    }
    ndims  = ndims_x;
  }
  else {
/*
 * This is the scalar case. The output array will be the size of
 * the dimensions given.
 */
    ndims  = ndims_x;
    size_conform = 1;
    for(i = 0; i < ndims_x; i++) {
      size_conform *= dsizes_x[i];
    }
  }

/*
 * Allocate space for output array.
 */
  conform = (void*)NclMalloc(size_conform*tmp_md->multidval.type->type_class.size);
  if( conform == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"conform: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

  type_size = tmp_md->multidval.type->type_class.size;
  if(!copy_scalar) {
/*
 * This is the non-scalar case.
 *
 * Loop linearly through each element of x, and calculate the indices for
 * that particular element.  Then, using the indices that correspond with
 * the array being conformed, we calculate the linear position in the dp
 * array and do a memcpy.
 */
    for(i = 0; i < size_conform; i++) {
      new_position = i;
      for(j = 0; j < ndims_x; j++) {
        indices[j]    = new_position / skip_x[j];
        new_position -= indices[j]  * skip_x[j];
      }
      conform_pos = 0;
      for(j = 0; j < dsizes_conform[0]; j++) {
        conform_pos += indices[cnfrm_dims[j]] * skip_c[j];
      }
      memcpy((void*)((char*)conform + i*type_size),
             (void*)((char*)tmp_md->multidval.val + conform_pos*type_size),
             type_size);
    }
  }
  else {
/*
 * This is the scalar case. Copy the scalar value to the output array.
 */
    for(i = 0; i < size_conform; i++) {
      memcpy((void*)((char*)conform + i*type_size),
             (void*)((char*)tmp_md->multidval.val),type_size);
    }
  }
/*
 * Free unneeded memory.
 */
  if(!copy_scalar) {
    NclFree(skip_x);
    NclFree(indices);
    NclFree(skip_c);
  }

/*
 * Return values.
 */
  if(tmp_md->multidval.missing_value.has_missing) {
    ret = NclReturnValue(conform,ndims,dsizes_x,
                         &tmp_md->multidval.missing_value.value,
                         tmp_md->multidval.data_type,0);
  }
  else {
    ret = NclReturnValue(conform,ndims,dsizes_x,NULL,
                         tmp_md->multidval.data_type,0);
  }
  return(ret);
}


/*   
 * This wrapper is very similar to the "conform" wrapper above, 
 * except instead of an array to conform to as its first argument,
 * you give it the dimension sizes of the new array you want to 
 * conform to.
 */

NhlErrorTypes conform_dims_W( void )
{
/*
 * Input array variables
 */
  void *tmp_dsizes_x;
  ng_size_t *dsizes_x;
  NclStackEntry data;
  int *cnfrm_dims;
  NclMultiDValData tmp_md = NULL;
  ng_size_t ndims_x;
  NclBasicDataTypes type_dsizes_x;
  ng_size_t dsizes_conform[NCL_MAX_DIMENSIONS];
/*
 * Output array variables
 */
  void *conform;
  int ndims;
/*
 * various
 */
  ng_size_t i, j;
  int scalar_tmp_md, copy_scalar = 0;
  ng_size_t new_position, size_conform, conform_pos;
  int type_size;
  ng_size_t *skip_x = NULL;
  ng_size_t *skip_c = NULL;
  ng_size_t *indices = NULL;
  int ret;

/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  tmp_dsizes_x = (void*)NclGetArgValue(
           0,
           3,
           NULL,
           &ndims_x,
           NULL,
           NULL,
           &type_dsizes_x,
           DONT_CARE);

  data = _NclGetArg(1,3,DONT_CARE);
  switch(data.kind) {
  case NclStk_VAR:
    tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
    break;
  case NclStk_VAL:
    tmp_md = (NclMultiDValData)data.u.data_obj;
    break;
  default:
    NhlPError(NhlFATAL,NhlEUNKNOWN,"conform_dims: invalid first input argument.");
    return(NhlFATAL);
  }

  cnfrm_dims = (int*)NclGetArgValue(
           2,
           3,
           NULL,
           dsizes_conform,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

  dsizes_x = get_dimensions(tmp_dsizes_x,ndims_x,type_dsizes_x,"conform_dims");
  if(dsizes_x == NULL) 
    return(NhlFATAL);

/*
 * Check if we're dealing with the special case of where the second
 * argument is a scalar, and the third argument is -1.  If this 
 * is true, then the scalar value will be copied to every 
 * element of the new array.
 */
  scalar_tmp_md = is_scalar(tmp_md->multidval.n_dims,
                            tmp_md->multidval.dim_sizes);

  if(scalar_tmp_md && cnfrm_dims[0] == -1) copy_scalar = 1;

/*
 * If not dealing with the special scalar case, then the number of
 * dimensions of the second argument must be the same as the length
 * of the third argument.
 */
  if(!copy_scalar && tmp_md->multidval.n_dims != dsizes_conform[0]) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"conform_dims: The array to be conformed must have the same number of dimensions as indicated by the length of the last argument");
    return(NhlFATAL);
  }

/*
 * If not a scalar situation, check for errors:
 *
 *    1. The dimension indices indicated by the third argument must
 *       be in the range of the dimension indices of x (i.e. if x is
 *       only 3-dimensional, then one of the indices in the 3rd
 *       argument cannot be a "4").
 *    2. The dimensions indices indicated by the third argument must
 *       be unique and increasing.
 *    4. The dimensions sizes of the second argument must be the same
 *       as those indicated by the third argument.
 */
  if(!copy_scalar) {
    for(i = 0; i < dsizes_conform[0]; i++ ) {
      if(cnfrm_dims[i] < 0 || cnfrm_dims[i] > (ndims_x-1)) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"conform_dims: the third argument contains a dimension that is out-of-range of the dimensions of x");
        return(NhlFATAL);
      }
      if(i > 0) {
        if(cnfrm_dims[i-1] >= cnfrm_dims[i]) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,"conform_dims: The dimensions specified by the third argument must be increasing");
          return(NhlFATAL);
        }
      }
      if(dsizes_x[cnfrm_dims[i]] != tmp_md->multidval.dim_sizes[i] ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"conform_dims: the dimensions sizes of the second argument do not match those indicated by the third argument");
        return(NhlFATAL);
      }
    }
/*
 * Create an array to hold the number of linear elements you have to
 * skip over when you increment a particular index.
 *
 * For example, if you have an array that is 2 x 3 x 2 x 2, then
 * any time you increment the first index of the array, you skip
 * over 12 elements in the linear array (3 x 2 x 2). Any time
 * you increment the second index, you skip over 4 elements (2 x 2),
 * and so on.
 */
    skip_x  = (ng_size_t*)calloc(ndims_x,sizeof(ng_size_t));
    skip_c  = (ng_size_t*)calloc(dsizes_conform[0],sizeof(ng_size_t)); 
    indices = (ng_size_t*)calloc(ndims_x,sizeof(ng_size_t));

    size_conform = dsizes_x[ndims_x-1];
    skip_x[ndims_x-1] = 1; 

    for(i = ndims_x-2; i >= 0; i--) {
      skip_x[i] = skip_x[i+1]*dsizes_x[i+1];
      size_conform  *= dsizes_x[i];
    }
    skip_c[dsizes_conform[0]-1] = 1;
    for(i = dsizes_conform[0]-2; i >= 0; i--) {
      skip_c[i] = skip_c[i+1]*dsizes_x[cnfrm_dims[i+1]];
    }
    ndims  = ndims_x;
  }
  else {
/*
 * This is the scalar case. The output array will be the size of
 * the dimensions given.
 */
    ndims  = ndims_x;
    size_conform = 1;
    for(i = 0; i < ndims_x; i++) {
      size_conform *= dsizes_x[i];
    }
  }

/*
 * Allocate space for output array.
 */
  conform = (void*)NclMalloc(size_conform*tmp_md->multidval.type->type_class.size);
  if( conform == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"conform_dims: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

  type_size = tmp_md->multidval.type->type_class.size;
  if(!copy_scalar) {
/*
 * This is the non-scalar case.
 *
 * Loop linearly through each element of x, and calculate the indices for
 * that particular element.  Then, using the indices that correspond with
 * the array being conformed, we calculate the linear position in the dp
 * array and do a memcpy.
 */
    for(i = 0; i < size_conform; i++) {
      new_position = i;
      for(j = 0; j < ndims_x; j++) {
        indices[j]    = new_position / skip_x[j];
        new_position -= indices[j]  * skip_x[j];
      }
      conform_pos = 0;
      for(j = 0; j < dsizes_conform[0]; j++) {
        conform_pos += indices[cnfrm_dims[j]] * skip_c[j];
      }
      memcpy((void*)((char*)conform + i*type_size),
             (void*)((char*)tmp_md->multidval.val + conform_pos*type_size),
             type_size);
    }
  }
  else {
/*
 * This is the scalar case. Copy the scalar value to the output array.
 */
    for(i = 0; i < size_conform; i++) {
      memcpy((void*)((char*)conform + i*type_size),
             (void*)((char*)tmp_md->multidval.val),type_size);
    }
  }
/*
 * Free unneeded memory.
 */
  if(!copy_scalar) {
    NclFree(skip_x);
    NclFree(indices);
    NclFree(skip_c);
  }

/*
 * Return values.
 */
  if(tmp_md->multidval.missing_value.has_missing) {
    ret = NclReturnValue(conform,ndims,dsizes_x,
                         &tmp_md->multidval.missing_value.value,
                         tmp_md->multidval.data_type,0);
  }
  else {
    ret = NclReturnValue(conform,ndims,dsizes_x,NULL,
                         tmp_md->multidval.data_type,0);
  }
  NclFree(dsizes_x);
  return(ret);
}

/*   
 * This function takes an input array and reshapes it to the given
 * dimensions. A simple case would be reshaping an N x M array to 
 * a 1D array of length N*M.  Or, reshaping a 10 x 10 x 20 array to
 * a 100 x 20 array.
 *
 * Another case that was added later was to allow leftmost dimensions.
 * That is, if x is 10 x 20 x 100, you can say:
 * 
 * y = reshape(x,(/10,10/))
 *
 * and y will be 10 x 20 x 10 x 10.
 *
 * Yet another case: if x is 3 x 4, you can do:
 *
 * y = reshape(x,(/5,6,12/))
 *
 * and y will be 5 x 6 x 12.
 *
 * The restriction is that the product of the input dimensions
 * must equal the product of some subset of the rightmost 
 * dimensions of x OR the product of the x dimensions must equal
 * the product of some subset of the rightmost input dimensions.
 */

NhlErrorTypes reshape_W( void )
{
/*
 * Input argument #1
 */
  NclMultiDValData tmp_md = NULL;
  NclStackEntry data;

/*
 * Input argument #2
 */
  void *tmp_dsizes_reshape;
  ng_size_t *dsizes_reshape;
  ng_size_t dsizes_reshape_dsizes[1];
  NclBasicDataTypes type_dsizes_reshape;

/*
 * Output array variable.
 */
  void *xreshape;
  int ndims_xreshape;
  ng_size_t *dsizes_xreshape;

/*
 * various
 */
  ng_size_t i, size_x, product_dimsizes, size_leftmost;
  logical matched_x_sizes, matched_input_sizes;
  int leftmost_index=-1, ret;
/*
 * Retrieve parameters
 */

  data = _NclGetArg(0,2,DONT_CARE);

  switch(data.kind) {
  case NclStk_VAR:
    tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
    break;
  case NclStk_VAL:
    tmp_md = (NclMultiDValData)data.u.data_obj;
    break;
  default:
    break;
  }

  tmp_dsizes_reshape = (void*)NclGetArgValue(
           1,
           2,
           NULL,
           dsizes_reshape_dsizes,
           NULL,
           NULL,
           &type_dsizes_reshape,
           DONT_CARE);

  dsizes_reshape = get_dimensions(tmp_dsizes_reshape,
                                  dsizes_reshape_dsizes[0],
                                  type_dsizes_reshape,"reshape");
  if(dsizes_reshape == NULL) 
    return(NhlFATAL);

  product_dimsizes = 1;
  for(i = 0; i < dsizes_reshape_dsizes[0]; i++) product_dimsizes *= 
                                                  dsizes_reshape[i];

/*
 * Test if product of input dimension sizes matches product of
 * rightmost dimensions of x.
 */
  matched_x_sizes = matched_input_sizes = False;
  size_x = 1;
  for( i=(tmp_md->multidval.n_dims-1); i >= 0; i--) {
    size_x *= tmp_md->multidval.dim_sizes[i];
    if(size_x == product_dimsizes) {
      matched_x_sizes = True;
      leftmost_index = i-1;
    }
  }

/*
 * Test if product of x dimension sizes matches product of
 * rightmost input dimensions.
 */
  if(!matched_x_sizes) {
    product_dimsizes = 1;
    for(i=(dsizes_reshape_dsizes[0]-1); i >= 0; i--) {
      product_dimsizes *= dsizes_reshape[i];
      if(size_x == product_dimsizes) {
        matched_input_sizes = True;
        leftmost_index = i-1;
      }
    }
  }

  if(!matched_x_sizes && !matched_input_sizes) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"reshape: input dimension sizes cannot conform to dimensions of input array");
    return(NhlFATAL);
  }
/*
 * If matched_x_sizes is True, then the input dimension sizes are 
 * a subset of the dimensions of the input x array. This means 
 * we need to construct the new output dimension sizes.
 */
  size_leftmost = 1;
  if(matched_x_sizes) {
    ndims_xreshape  =  (leftmost_index+1) + dsizes_reshape_dsizes[0];
    dsizes_xreshape = (ng_size_t*)calloc(ndims_xreshape,sizeof(ng_size_t));
    for(i = 0; i <= leftmost_index; i++) {
      dsizes_xreshape[i] = tmp_md->multidval.dim_sizes[i];
    }
    for(i = 0; i < dsizes_reshape_dsizes[0]; i++) {
      dsizes_xreshape[leftmost_index+i+1] = dsizes_reshape[i];
    }
  }
/*
 * The x dimension sizes are a subset of the input dimension sizes.
 * Even though we can use the input dimension sizes as the dimension
 * sizes for the output array, go ahead and construct the output
 * dimensions, to stay consistent with previous code.
 */
  else {
    ndims_xreshape  = dsizes_reshape_dsizes[0];
    dsizes_xreshape = (ng_size_t*)calloc(ndims_xreshape,sizeof(ng_size_t));
    for(i = 0; i <= leftmost_index; i++) {
      size_leftmost *= dsizes_reshape[i];
    }
    for(i = 0; i < ndims_xreshape; i++) {
      dsizes_xreshape[i] = dsizes_reshape[i];
    }
  }
/*
 * Allocate space for output array. We want "size_leftmost" copies of
 * the original array.
 */
  xreshape = (void*)malloc(tmp_md->multidval.totalsize * size_leftmost);
  if( xreshape == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"reshape: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Copy values to new array.
 */
  if(matched_x_sizes) {
    memcpy((void*)((char*)xreshape),
           (void*)((char*)tmp_md->multidval.val),tmp_md->multidval.totalsize);
  }
  else {
    for(i = 0; i < size_leftmost; i++) {
      memcpy((void*)((char*)xreshape + i*tmp_md->multidval.totalsize),
             (void*)((char*)tmp_md->multidval.val),tmp_md->multidval.totalsize);
    }
  }

/*
 * Return values.
 */
  if(tmp_md->multidval.missing_value.has_missing) {
    ret = NclReturnValue(xreshape,ndims_xreshape,
                         dsizes_xreshape,
                         &tmp_md->multidval.missing_value.value,
                         tmp_md->multidval.data_type,0);
  }
  else {
    ret = NclReturnValue(xreshape,ndims_xreshape,dsizes_xreshape,
                         NULL,tmp_md->multidval.data_type,0);
  }
  NclFree(dsizes_reshape);
  NclFree(dsizes_xreshape);
  return(ret);
}


NhlErrorTypes reshape_ind_W( void )
{
/*
 * Input argument #0
 */
  void *x;
  int ndims_x, has_missing_x;
  NclScalar missing_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_x;
  NclTypeClass typeclass_x;
  int size_x_type;
/*
 * Input argument #1
 */
  void *tmp_indexes;
  ng_size_t *indexes;
  ng_size_t dsizes_indexes[1];
  NclBasicDataTypes type_indexes;

/*
 * Input argument #2
 */
  void *tmp_reshape_dsizes;
  ng_size_t *reshape_dsizes;
  ng_size_t dsizes_reshape_dsizes[1];
  NclBasicDataTypes type_reshape_dsizes;

/*
 * Output array variable.
 */
  void *xreshape;
  ng_size_t *dsizes_xreshape;
  int ndims_xreshape;
  NclScalar missing_xreshape;

/*
 * various
 */
  ng_size_t total_leftmost, total_reshape, total_output;
  ng_size_t i, j, nvals, invals, inreshape, index_x, index_xreshape;
  int ret;

  x = (void*)NclGetArgValue(
           0,
           3,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);

  nvals = dsizes_x[ndims_x-1];

  tmp_indexes = (void*)NclGetArgValue(
           1,
           3,
           NULL,
           dsizes_indexes,
           NULL,
           NULL,
           &type_indexes,
           DONT_CARE);

  if(dsizes_indexes[0] != nvals) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"reshape_ind: the number of indexes must be the same as the rightmost dimension of x");
    return(NhlFATAL);
  }

  indexes = get_dimensions(tmp_indexes,nvals,type_indexes,"reshape_ind");

  if(indexes == NULL) 
    return(NhlFATAL);

  tmp_reshape_dsizes = (void*)NclGetArgValue(
           2,
           3,
           NULL,
           dsizes_reshape_dsizes,
           NULL,
           NULL,
           &type_reshape_dsizes,
           DONT_CARE);

  reshape_dsizes = get_dimensions(tmp_reshape_dsizes,dsizes_reshape_dsizes[0],
                                  type_reshape_dsizes,"reshape_ind");
  if(reshape_dsizes == NULL) 
    return(NhlFATAL);

/*
 * Get size and default missing value of x.
 */
  typeclass_x = (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(type_x)));
  size_x_type = typeclass_x->type_class.size;

/*
 * Get input missing value, or default missing value for the input type.
 */
  if(has_missing_x) {
    missing_xreshape = missing_x;
  }
  else {
    missing_xreshape = typeclass_x->type_class.default_mis;
  }

/*
 * Construct dimensions for output array
 */
  ndims_xreshape = (ndims_x-1) + dsizes_reshape_dsizes[0];
  dsizes_xreshape = (ng_size_t*)malloc(ndims_xreshape*sizeof(ng_size_t));
  
  if(dsizes_xreshape == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"reshape_ind: unable to allocate space for output array dimension sizes");
    return(NhlFATAL);
  }

/*
 * Get total elements in leftmost dimensions. 
 */
  total_leftmost = 1;
  for(i = 0; i < ndims_x-1; i++) {
    total_leftmost *= dsizes_x[i];
    dsizes_xreshape[i] = dsizes_x[i];
  }
/*
 * Get total elements in reshape dimensions. 
 */
  total_reshape = 1;
  for(i = 0; i < dsizes_reshape_dsizes[0]; i++) {
    total_reshape *= reshape_dsizes[i];
    dsizes_xreshape[(ndims_x-1)+i] = reshape_dsizes[i];
  }

/*
 * Allocate space for output.
 */
  total_output = total_leftmost * total_reshape;
  xreshape     = (void*)malloc(total_output*size_x_type);
  
  if(xreshape == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"reshape_ind: unable to allocate space for output array");
    return(NhlFATAL);
  }

/*
 * Initially set all values to missing.
 */
  for(i = 0; i < total_output; i++) {
    memcpy((void*)((char*)xreshape + (i*size_x_type)),
           (void*)((char*)&missing_xreshape),size_x_type);
  }
/*
 * Copy values to new array.
 */
  for(i = 0; i < total_leftmost; i++) {
    invals    = i*nvals;
    inreshape = i*total_reshape;
    for(j = 0; j < nvals; j++) {
      index_xreshape = inreshape + indexes[j];
      index_x        = invals + j;
      memcpy((void*)((char*)xreshape + (index_xreshape*size_x_type)),
             (void*)((char*)x+(index_x*size_x_type)),size_x_type);
    }
  }

/*
 * Return values.
 */
  ret = NclReturnValue(xreshape,ndims_xreshape,dsizes_xreshape,
                       &missing_xreshape,type_x,0);
/*
 * Clean up
 */
  NclFree(dsizes_xreshape);
  NclFree(reshape_dsizes);
  NclFree(indexes);
  return(ret);
}

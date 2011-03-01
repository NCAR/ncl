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
  ng_size_t *dsizes;
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

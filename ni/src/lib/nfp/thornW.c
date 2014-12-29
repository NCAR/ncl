/* THIS WRAPPER IS NOT DONE!! */

#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(thorn2,THORN2)(double *, int *, double *, 
				   double *, double *, int *);

NhlErrorTypes dim_thornthwaite_n_W( void )
{

/*
 * Input variables
 *
 * Argument # 0
 */
  void *temp;
  double *tmp_temp;
  int       ndims_temp;
  ng_size_t dsizes_temp[NCL_MAX_DIMENSIONS];
  int has_missing_temp;
  NclScalar missing_temp, missing_flt_temp, missing_dbl_temp;
  NclBasicDataTypes type_temp;

/*
 * Argument # 1
 */
  void *lat;
  double *tmp_lat;
  int       ndims_lat;
  ng_size_t dsizes_lat[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_lat;

/*
 * Argument # 2
 */
  int *dims;
  ng_size_t dsizes_dims[1];
/*
 * Return variable
 */
  void *pet;
  double *tmp_pet;
  int       ndims_pet;
  ng_size_t dsizes_pet;
  int has_missing_pet;
  ng_size_t size_temp;
  NclScalar missing_pet, missing_flt_pet, missing_dbl_pet;
  NclBasicDataTypes type_pet;

/*
 * Various
 */
  int ndims;
  int index_temp, index_lat, index_pet;
  int ier, i, ndims_leftmost, size_leftmost, ret;
  ng_size_t nlat, nlon;

/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
/*
 * Get argument # 0
 */
  temp = (void*)NclGetArgValue(
           0,
           3,
           &ndims_temp,
           dsizes_temp,
           &missing_temp,
           &has_missing_temp,
           &type_temp,
           DONT_CARE);

/*
 * Coerce missing value to double if necessary.
 */
  coerce_missing(type_temp,has_missing_temp,&missing_temp,
                 &missing_dbl_temp,&missing_flt_temp);

/*
 * Get argument # 1
 */
  lat = (void*)NclGetArgValue(
           1,
           3,
           &ndims_lat,
           dsizes_lat,
           NULL,
           NULL,
           &type_lat,
           DONT_CARE);

/*
 * Check dimension sizes.
 */
  if(ndims_temp == 1 && !is_scalar(ndims_lat,dsizes_lat)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_thornthwaite_n: If temp is a 1D array, lat must be a scalar.");
    return(NhlFATAL);
  }
  if(ndims_lat > 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_thornthwaite_n: lat must either be a scalar, a 1D array, or a 2D array");
    return(NhlFATAL);
  }

  if(ndims_temp == 1) {
    nlat = 1;
    nlon = 1;
  }
  else {
    nlat = dsizes_temp[ndims_temp-2];
    nlon = dsizes_temp[ndims_temp-1];
    if( (ndims_temp >= 2 && ndims_lat == 1 && dsizes_lat[0] != nlat) ||
	((ndims_temp >= 2 && ndims_lat == 2) && 
	    (dsizes_lat[0] != nlat || dsizes_[1] != nlon))) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_thornthwaite_n: invalid dimensions for lat. Must be nlat or nlat x mlon");
    return(NhlFATAL);
  }

/*
 * Get argument # 2
 */
  dims = (int*)NclGetArgValue(
           2,
           3,
           NULL,
           dsizes_dims,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
  ndims = dsizes_dims[0];

  if(dsizes_dims[0] > ndims_temp) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_thornthwaite_n: the dims dimension must not contain more dimensions than temp");
    return(NhlFATAL);
  }
  for(i = 0; i < dsizes_dims[0]; i++) {
    if(dims[i] >= ndims_temp) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_thornthwaite_n: invalid dimension values for 'dims'");
    return(NhlFATAL);
  }
/*
 * Calculate size of leftmost dimensions.
 */
  size_leftmost  = 1;
  if(ndims_temp==1) {
    ndims_leftmost = 1;
    size_leftmost = dsizes_temp[0];
  }
  else {
    ndims_leftmost = ndims_temp-2;
    for(i = 0; i < ndims_leftmost; i++) size_leftmost *= dsizes_temp[i];
  }


/* 
 * Allocate space for coercing input arrays.  If any of the input
 * is already double, then we don't need to allocate space for
 * temporary arrays, because we'll just change the pointer into
 * the void array appropriately.
 */
/*
 * Allocate space for tmp_temp.
 */
  if(type_temp != NCL_double) {
    tmp_temp = (double *)calloc(ntim,sizeof(double));
    if(tmp_temp == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_thornthwaite_n: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
    type_pet = NCL_float;
  }
  else {
    type_pet = NCL_double;
  }
/*
 * Allocate space for tmp_lat.
 */
  if(type_lat != NCL_double) {
    tmp_lat = (double *)calloc(nlat,sizeof(double));
    if(tmp_lat == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_thornthwaite_n: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }

/*
 * Calculate size of output array.
 */
  size_output = size_leftmost * ntim;

/* 
 * Allocate space for output array.
 */
  if(type_pet != NCL_double) {
    pet = (void *)calloc(size_output, sizeof(float));
    tmp_pet = (double *)calloc(ntim,sizeof(double));
    if(tmp_pet == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_thornthwaite_n: Unable to allocate memory for temporary output array");
      return(NhlFATAL);
    }
  }
  else {
    pet = (void *)calloc(size_output, sizeof(double));
  }
  if(pet == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_thornthwaite_n: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
  if(has_missing_temp) {
    if(type_pet == NCL_double) missing_pet = missing_dbl_temp;
    else                 missing_pet = missing_flt_temp;
    missing_dbl_pet = missing_dbl_temp;
  }

/* 
 * Allocate space for output dimension sizes and set them.
 */
  ndims_pet = ndims_leftmost + 1;
  dsizes_pet = (ng_size_t*)calloc(ndims_pet,sizeof(ng_size_t));  
  if( dsizes_pet == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_thornthwaite_n: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_pet-1; i++) dsizes_pet[i] = dsizes_temp[i];
  dsizes_pet[ndims_pet-1] = ntim;

/*
 * Loop across leftmost dimensions and call the Fortran routine for each
 * subsection of the input arrays.
 */
  index_temp = index_lat = index_pet = 0;

  for(i = 0; i < size_leftmost; i++) {
/*
 * Coerce subsection of temp (tmp_temp) to double if necessary.
 */
    if(type_temp != NCL_double) {
      coerce_subset_input_double(temp,tmp_temp,index_temp,type_temp,ntim,0,NULL,NULL);
    }
    else {
      tmp_temp = &((double*)temp)[index_temp];
    }

/*
 * Coerce subsection of lat (tmp_lat) to double if necessary.
 */
    if(type_lat != NCL_double) {
      coerce_subset_input_double(lat,tmp_lat,index_lat,type_lat,nlat,0,NULL,NULL);
    }
    else {
      tmp_lat = &((double*)lat)[index_lat];
    }


/*
 * Point temporary output array to void output array if appropriate.
 */
    if(type_pet == NCL_double) tmp_pet = &((double*)pet)[index_pet];


/*
 * Call the Fortran routine.
 */
    NGCALLF(thorn2,THORN2)(tmp_temp, &ntim, &missing_dbl_temp.doubleval, tmp_lat, tmp_pet, &ier);

/*
 * Coerce output back to float if necessary.
 */
    if(type_pet == NCL_float) {
      coerce_output_float_only(pet,tmp_pet,ntim,index_pet);
    }
    index_temp += ntim;
    index_lat += nlat;
    index_pet += ntim;
  }

/*
 * Free unneeded memory.
 */
  if(type_temp != NCL_double) NclFree(tmp_temp);
  if(type_lat != NCL_double) NclFree(tmp_lat);
  if(type_pet != NCL_double) NclFree(tmp_pet);

/*
 * Return value back to NCL script.
 */
  if(type_pet != NCL_double) {
    ret = NclReturnValue(pet,ndims_pet,dsizes_pet,&missing_flt_pet,type_pet,0);
  }
  else {
    ret = NclReturnValue(pet,ndims_pet,dsizes_pet,&missing_dbl_pet,type_pet,0);
  }
  NclFree(dsizes_pet);
  return(ret);
}

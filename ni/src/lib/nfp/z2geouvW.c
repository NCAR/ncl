#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(z2geouv,Z2GEOUV)(double *, int *, int *, double *,
                                     double *, double *, double *, double* ,
                                     int *);

NhlErrorTypes z2geouv_W( void )
{
/*
 * Input variables
 */
  void *z, *lat, *lon;
  int *iopt;
  double *tmp_z = NULL;
  double *tmp_lat, *tmp_lon;
  int ndims_z;
  ng_size_t dsizes_z[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_lat[1];
  ng_size_t dsizes_lon[1];
  ng_size_t size_z;
  int has_missing_z;
  NclScalar missing_z, missing_dz, missing_rz;
  NclBasicDataTypes type_z, type_lat, type_lon;
/*
 * Output variables
 */
  void *uv;
  double *tmp_u = NULL;
  double *tmp_v = NULL;
  int ndims_uv;
  ng_size_t *dsizes_uv, size_uv;
  NclBasicDataTypes type_uv;
/*
 * Various.
 */
  ng_size_t i, index_z, size_leftmost;
  ng_size_t nlat, nlon, nlatlon;
  int ret, inlat, inlon;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  z = (void*)NclGetArgValue(
          0,
          4,
          &ndims_z, 
          dsizes_z,
          &missing_z,
          &has_missing_z,
          &type_z,
          DONT_CARE);
  lat = (void*)NclGetArgValue(
          1,
          4,
          NULL,
          dsizes_lat,
          NULL,
          NULL,
          &type_lat,
          DONT_CARE);
  lon = (void*)NclGetArgValue(
          2,
          4,
          NULL,
          dsizes_lon,
          NULL,
          NULL,
          &type_lon,
          DONT_CARE);
  iopt = (void*)NclGetArgValue(
          3,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);
/*
 * Check dimensions.
 */
  if(ndims_z < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"z2geouv: The 'z' array must have at least two dimensions");
    return(NhlFATAL);
  }
  nlat    = dsizes_z[ndims_z-2];
  nlon    = dsizes_z[ndims_z-1];
  nlatlon = nlat * nlon;

  if(dsizes_lat[0] != nlat) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"z2geouv: The 'lat' array must be the same length as the second rightmost dimension of 'z'");
    return(NhlFATAL);
  }

  if(dsizes_lon[0] != nlon) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"z2geouv: The 'lon' array must be the same length as the last dimension of 'z'");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) || (nlat > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"z2geouv: nlat and/or nlon is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
/*
 * Coerce missing value to double.
 */
  coerce_missing(type_z,has_missing_z,&missing_z,&missing_dz,&missing_rz);

/*
 * Determine type of output.
 */
  if(type_z == NCL_double) {
    type_uv = NCL_double;
  }
  else {
    type_uv = NCL_float;
  }
/*
 * Compute size of leftmost dimensions.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_z-2; i++ ) size_leftmost *= dsizes_z[i];
  size_z = size_leftmost * nlatlon;

/*
 * Calculate total size of output array, and set dimension sizes.
 */
  size_uv  = 2 * size_z;
  ndims_uv = ndims_z+1;

  dsizes_uv = (ng_size_t*)malloc(ndims_uv*sizeof(ng_size_t));  
  if( dsizes_uv == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"z2geouv: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }

  dsizes_uv[0] = 2;
  for( i = 0; i < ndims_z; i++ ) dsizes_uv[i+1] = dsizes_z[i];
 
/*
 * Allocate space for coercing z later.
 */
  if(type_z != NCL_double) {
    tmp_z = (double*)malloc(nlatlon*sizeof(double));
    if( tmp_z == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"z2geouv: Unable to allocate memory for coercing z array to double precision");
      return(NhlFATAL);
    }
  }
/*
 * Coerce lat/lon to double if necessary.
 */
  tmp_lat = coerce_input_double(lat,type_lat,nlat,0,NULL,NULL);
  tmp_lon = coerce_input_double(lon,type_lon,nlon,0,NULL,NULL);
  if( tmp_lat == NULL || tmp_lon == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"z2geouv: Unable to coerce 'lat' and/or 'lon' to double");
    return(NhlFATAL);
  }

/*
 * Allocate space for output arrays.
 */
  if(type_uv == NCL_float) {
    uv     = (void*)calloc(size_uv,sizeof(float));
    tmp_u = (double*)calloc(nlatlon,sizeof(double));
    tmp_v = (double*)calloc(nlatlon,sizeof(double));
    if(tmp_u == NULL || tmp_v == NULL || uv == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"z2geouv: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    uv = (void*)calloc(size_uv,sizeof(double));
    if(uv == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"z2geouv: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }

/*
 * Call function.
 */
  index_z = 0;
  for( i = 0; i < size_leftmost; i++ ) {
    if(type_z != NCL_double) {
/*
 * Coerce subsection of z (tmp_z) to double.
 */
      coerce_subset_input_double(z,tmp_z,index_z,type_z,nlatlon,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_z to appropriate location in z.
 */
      tmp_z = &((double*)z)[index_z];
    }
    
    if(type_uv == NCL_double) {
      tmp_u = &((double*)uv)[index_z];
      tmp_v = &((double*)uv)[index_z+size_z];
    }

    NGCALLF(z2geouv,Z2GEOUV)(tmp_z,&inlon,&inlat,&missing_dz.doubleval,
			     tmp_lon,tmp_lat,tmp_u,tmp_v,iopt);
/*
 * Copy output values from temporary tmp_u, tmp_v to uv.
 */
    if(type_uv != NCL_double) {
      coerce_output_float_only(uv,tmp_u,nlatlon,index_z);
      coerce_output_float_only(uv,tmp_v,nlatlon,index_z+size_z);
    }
    index_z  += nlatlon;
  }
/*
 * Free memory.
 */
  if(type_z     != NCL_double) NclFree(tmp_z);
  if(type_lat   != NCL_double) NclFree(tmp_lat);
  if(type_lon   != NCL_double) NclFree(tmp_lon);
  if(type_uv != NCL_double) {
    NclFree(tmp_u);
    NclFree(tmp_v);
  }
/*
 * Return.
 */
  if(type_uv != NCL_double) {
     ret = NclReturnValue(uv,ndims_uv,dsizes_uv,&missing_rz,
                           NCL_float,0);
  }
  else {
    ret = NclReturnValue(uv,ndims_uv,dsizes_uv,&missing_dz,
                          NCL_double,0);
  }
  NclFree(dsizes_uv);
  return(ret);
}

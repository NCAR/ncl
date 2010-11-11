#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(dzpsidrv,DZPSIDRV)(int*,int*,int*,double*,double*,
                                       double*,double*,double*,double*);

NhlErrorTypes zonal_mpsi_W( void )
{
/*
 * Input variables
 */
  void *v, *lat, *p, *ps;
  double *tmp_v = NULL;
  double *tmp_lat;
  double *tmp_p;
  double *tmp_ps = NULL;
  int ndims_v;
  ng_size_t dsizes_v[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_lat[1];
  int ndims_ps;
  ng_size_t dsizes_ps[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_p[1];
  int has_missing_v;
  NclScalar missing_v, missing_dv, missing_rv;
  NclBasicDataTypes type_lat, type_v, type_p, type_ps;
/*
 * Output variables
 */
  void *zmpsi;
  double *tmp_zmpsi = NULL;
  int ndims_zmpsi;
  ng_size_t *dsizes_zmpsi;
  NclBasicDataTypes type_zmpsi;
/*
 * Various.
 */
  ng_size_t i, index_v, index_ps, index_zmpsi, size_zmpsi;
  ng_size_t nlat, nlon, nlev, ntim, nlatlon, nlatlev, nlatlonlev;
  int ret, inlon, inlat, inlev;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  v = (void*)NclGetArgValue(
          0,
          4,
          &ndims_v, 
          dsizes_v,
          &missing_v,
          &has_missing_v,
          &type_v,
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
  p = (void*)NclGetArgValue(
          2,
          4,
          NULL,
          dsizes_p,
          NULL,
          NULL,
          &type_p,
          DONT_CARE);
  ps = (void*)NclGetArgValue(
          3,
          4,
          &ndims_ps, 
          dsizes_ps,
          NULL,
          NULL,
          &type_ps,
          DONT_CARE) ;
/*
 * Check dimensions.
 */
  if(ndims_v < 3 || ndims_v > 4) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"zonal_mpsi: The 'v' array must be three or four-dimensional");
    return(NhlFATAL);
  }
  nlev = dsizes_v[ndims_v-3];
  nlat = dsizes_v[ndims_v-2];
  nlon = dsizes_v[ndims_v-1];
  nlatlon    = nlat * nlon;
  nlatlev    = nlat * nlev;
  nlatlonlev = nlatlon * nlev;

  if(ndims_v == 4) ntim = dsizes_v[0];
  else             ntim = 1;

  if(dsizes_lat[0] != nlat) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"zonal_mpsi: The 'lat' array must be the same length as the second rightmost dimension of 'v'");
    return(NhlFATAL);
  }

  if( dsizes_p[0] != nlev) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"zonal_mpsi: The 'p' array must be the same length as the third rightmost dimension of 'v'");
    return(NhlFATAL);
  }

  if(ndims_ps != ndims_v-1) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"zonal_mpsi: The 'ps' array must have one less dimension than the 'v' array");
    return(NhlFATAL);
  }
  if(ndims_v == 3) {
    if(dsizes_ps[0] != nlat || dsizes_ps[1] != nlon) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"zonal_mpsi: The 'ps' array must be dimensioned nlat x nlon if 'v' is 3-dimensional");
      return(NhlFATAL);
    }
  }
  else {
    if(dsizes_ps[0] != ntim || dsizes_ps[1] != nlat || dsizes_ps[2] != nlon) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"zonal_mpsi: The 'ps' array must be dimensioned ntim x nlat x nlon if 'v' is 4-dimensional");
      return(NhlFATAL);
    }
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) || (nlat > INT_MAX) || (nlev > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"zonal_mpsi: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  inlev = (int) nlev;

/*
 * Coerce missing value to double.
 */
  coerce_missing(type_v,has_missing_v,&missing_v,&missing_dv,&missing_rv);

/*
 * Determine type of output.
 */
  if(type_v == NCL_double) {
    type_zmpsi = NCL_double;
  }
  else {
    type_zmpsi = NCL_float;
  }
/*
 * Calculate total size of output array.
 */
  size_zmpsi  = ntim * nlatlev;
  ndims_zmpsi = ndims_v-1;

  dsizes_zmpsi = (ng_size_t*)calloc(ndims_zmpsi,sizeof(ng_size_t));  
  if( dsizes_zmpsi == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"zonal_mpsi: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }

  if(ndims_v == 4) {
    dsizes_zmpsi[0] = ntim;
    dsizes_zmpsi[1] = nlev;
    dsizes_zmpsi[2] = nlat;
  }
  else {
    dsizes_zmpsi[0] = nlev;
    dsizes_zmpsi[1] = nlat;
  }
/*
 * Allocate space for coercing v later.
 */
  if(type_v != NCL_double) {
    tmp_v = (double*)calloc(nlatlonlev,sizeof(double));
    if( tmp_v == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"zonal_mpsi: Unable to allocate memory for coercing v array to double precision");
      return(NhlFATAL);
    }
  }
/*
 * Coerce lat to double if necessary.
 */
  tmp_lat = coerce_input_double(lat,type_lat,nlat,0,NULL,NULL);
  if( tmp_lat == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"zonal_mpsi: Unable to coerce 'lat' to double");
    return(NhlFATAL);
  }
/*
 * Coerce p.
 */
  tmp_p = coerce_input_double(p,type_p,nlev,0,NULL,NULL);
  if( tmp_p == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"zonal_mpsi: Unable to coerce 'p' to double");
    return(NhlFATAL);
  }
/*
 * Check p values. They must be increasing, and the first value
 * must be greater than 500 Pa (5mb), and the last value must be less
 * than 100500 Pa (1005mb), i.e. 500 < p(0) < p(1) < ... < 100500.
 */
  for(i = 0; i < nlev; i++ ) {
    if( tmp_p[i] <= 500 || tmp_p[i] >= 100500) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"zonal_mpsi: The pressure array must be between the values of 500 and 100500 (exclusive), and monotonically increasing");
      return(NhlFATAL);
    }
    if(i < nlev-1) {
      if( tmp_p[i] >= tmp_p[i+1]) { 
        NhlPError(NhlFATAL,NhlEUNKNOWN,"zonal_mpsi: The pressure array must be monotonically increasing");
      return(NhlFATAL);
      }
    }
  }
      
/*
 * Coerce ps.
 */
  if(type_ps != NCL_double) {
    tmp_ps = (double*)calloc(nlatlon,sizeof(double));
    if( tmp_ps == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"zonal_mpsi: Unable to allocate memory for coercing ps array to double precision");
      return(NhlFATAL);
    }
  }

/*
 * Allocate space for output array.
 */
  if(type_zmpsi == NCL_float) {
    zmpsi     = (void*)calloc(size_zmpsi,sizeof(float));
    tmp_zmpsi = (double*)calloc(nlatlev,sizeof(double));
    if(tmp_zmpsi == NULL || zmpsi == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"zonal_mpsi: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    zmpsi = (void*)calloc(size_zmpsi,sizeof(double));
    if(zmpsi == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"zonal_mpsi: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }

/*
 * Call function.
 */
  index_zmpsi = index_v = index_ps = 0;
  for( i = 0; i < ntim; i++ ) {
    if(type_v != NCL_double) {
/*
 * Coerce subsection of v (tmp_v) to double.
 */
      coerce_subset_input_double(v,tmp_v,index_v,type_v,nlatlonlev,0,
                                 NULL,NULL);
    }
    else {
/*
 * Point tmp_v to appropriate location in v.
 */
      tmp_v = &((double*)v)[index_v];
    }
    
    if(type_ps != NCL_double) {
/*
 * Coerce subsection of ps (tmp_ps) to double.
 */
      coerce_subset_input_double(ps,tmp_ps,index_ps,type_ps,nlatlon,0,
                                 NULL,NULL);
    }
    else {
/*
 * Point tmp_ps to appropriate location in ps.
 */
      tmp_ps = &((double*)ps)[index_ps];
    }
    
    if(type_zmpsi == NCL_double) tmp_zmpsi = &((double*)zmpsi)[index_zmpsi];

    NGCALLF(dzpsidrv,DZPSIDRV)(&inlon,&inlat,&inlev,tmp_v,tmp_lat,tmp_p,tmp_ps,
                                 &missing_dv.doubleval,tmp_zmpsi);
/*
 * Copy output values from temporary tmp_zmpsi to zmpsi.
 */
    if(type_zmpsi != NCL_double) {
      coerce_output_float_only(zmpsi,tmp_zmpsi,nlatlev,index_zmpsi);
    }
    index_v     += nlatlonlev;
    index_ps    += nlatlon;
    index_zmpsi += nlatlev;
  }
/*
 * Free memory.
 */
  if(type_v     != NCL_double) NclFree(tmp_v);
  if(type_lat   != NCL_double) NclFree(tmp_lat);
  if(type_p     != NCL_double) NclFree(tmp_p);
  if(type_ps    != NCL_double) NclFree(tmp_ps);
  if(type_zmpsi != NCL_double) NclFree(tmp_zmpsi);
/*
 * Return.
 */
  if(type_zmpsi != NCL_double) {
     ret = NclReturnValue(zmpsi,ndims_zmpsi,dsizes_zmpsi,&missing_rv,
                           NCL_float,0);
  }
  else {
    ret = NclReturnValue(zmpsi,ndims_zmpsi,dsizes_zmpsi,&missing_dv,
                          NCL_double,0);
  }
  NclFree(dsizes_zmpsi);
  return(ret);
}

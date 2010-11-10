#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(daamom1,daamom1)(double*,double*,double*,double*,int*,
                                     int*,int*,double*,double*);

extern void NGCALLF(daamom3,daamom3)(double*,double*,double*,double*,int*,
                                     int*,int*,double*,double*);

extern void NGCALLF(dcosweight,DCOSWEIGHT)(double*,int*,double*);

NhlErrorTypes angmom_atm_W( void )
{
/*
 * Input variables
 */
  void *u, *dp, *lat, *wgt;
  double *tmp_u = NULL;
  double *tmp_dp = NULL;
  double *tmp_lat, *tmp1_wgt, *tmp_wgt;
  int has_missing_u;
  int ndims_u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  NclScalar missing_u, missing_du, missing_ru;
  int ndims_dp;
  ng_size_t dsizes_dp[NCL_MAX_DIMENSIONS];
  int ndims_lat;
  ng_size_t dsizes_lat[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_wgt[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_u, type_dp, type_lat, type_wgt;
   
/*
 * Output variables
 */
  void *angmom;
  ng_size_t dsizes_angmom[1];
  double *tmp_angmom = NULL;
  NclBasicDataTypes type_angmom;
/*
 * Various.
 */
  ng_size_t ntim, klev, nlat, nlon, i, index_dp;
  ng_size_t klevnlatnlon;
  int inlon, inlat, iklev;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  u = (void*)NclGetArgValue(
          0,
          4,
          &ndims_u, 
          dsizes_u,
          &missing_u,
          &has_missing_u,
          &type_u,
          DONT_CARE);
  dp = (void*)NclGetArgValue(
          1,
          4,
          &ndims_dp, 
          dsizes_dp,
          NULL,
          NULL,
          &type_dp,
          DONT_CARE);
  lat = (void*)NclGetArgValue(
          2,
          4,
          &ndims_lat, 
          dsizes_lat,
          NULL,
          NULL,
          &type_lat,
          DONT_CARE);
  wgt = (void*)NclGetArgValue(
          3,
          4,
          NULL,
          dsizes_wgt,
          NULL,
          NULL,
          &type_wgt,
          DONT_CARE);
/*
 * Check dimensions.
 */
  if(ndims_u != 3 && ndims_u != 4) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"angmom_atm: u must be a 3 or 4 dimensional array");
    return(NhlFATAL);
  }
  nlon = dsizes_u[ndims_u-1];
  nlat = dsizes_u[ndims_u-2];
  klev = dsizes_u[ndims_u-3];
  klevnlatnlon = klev * nlat * nlon;

  if(ndims_u == 3) ntim = 1; 
  else             ntim = dsizes_u[0];

  if((ndims_dp == 1 && dsizes_dp[0] != klev) ||
     (ndims_dp > 1 && ndims_dp != ndims_u)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"angmom_atm: dp must either be a one-dimensional array of size 'klev', or the same size as u");
    return(NhlFATAL);
  }
  else {
    if(ndims_dp == ndims_u) {
      for(i = 0; i < ndims_u; i++ ) {
        if(dsizes_u[i] != dsizes_dp[i]) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,"angmom_atm: dp must either be a one-dimensional array of size 'klev', or the same size as u");
          return(NhlFATAL);
        }
      }
    }
  }

  if(dsizes_lat[0] != nlat) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"angmom_atm: the 'lats' array must be of length 'nlat'");
    return(NhlFATAL);
  }

  if(dsizes_wgt[0] != 1 && dsizes_wgt[0] != nlat) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"angmom_atm: wgt must be a scalar or a 1-dimensional vector the same size as the 'lats' array");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) || (nlat > INT_MAX) || (klev > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"angmom_atm: nlon = %ld is greater than INT_MAX", nlon);
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  iklev = (int) klev;

/*
 * Check for missing values.
 */
  coerce_missing(type_u,has_missing_u,&missing_u,&missing_du,&missing_ru);
/*
 * Determine type of output.
 */
  if(type_u == NCL_double) {
    type_angmom = NCL_double;
  }
  else {
    type_angmom = NCL_float;
  }
/*
 * Create temporary arrays if necessary.
 */
  if(type_u != NCL_double) {
    tmp_u = (double*)calloc(klevnlatnlon,sizeof(double));
    if( tmp_u == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"angmom_atm: Unable to allocate memory for coercing u array to double precision");
      return(NhlFATAL);
    }
  }

  if(ndims_dp == 1) {
    tmp_dp = coerce_input_double(dp,type_dp,klev,0,NULL,NULL);
  }
  else {
    if(type_dp != NCL_double) {
      tmp_dp = (double*)calloc(klevnlatnlon,sizeof(double));
      if( tmp_dp == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"angmom_atm: Unable to allocate memory for coercing dp array to double precision");
        return(NhlFATAL);
      }
    }
  }

  tmp_lat = coerce_input_double(lat,type_lat,nlat,0,NULL,NULL);
  if( tmp_lat == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"angmom_atm: Unable to allocate memory for coercing lat array to double precision");
    return(NhlFATAL);
  }

/*
 * Coerce weights to double if necessary. If scalar weight, then copy to
 * to full array.
 */
  tmp1_wgt = coerce_input_double(wgt,type_wgt,dsizes_wgt[0],0,NULL,NULL);

  if(tmp1_wgt == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"angmom_atm: Unable to allocate memory for coercing weights to double precision");
    return(NhlFATAL);
  }

  if(dsizes_wgt[0] == 1) {
    tmp_wgt = (double*)calloc(nlat,sizeof(double));
    if( tmp_wgt == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"angmom_atm: Unable to allocate memory for coercing weights to double precision");
      return(NhlFATAL);
    }
    tmp_wgt[0] = *tmp1_wgt;

    NGCALLF(dcosweight,DCOSWEIGHT)(tmp_lat,&inlat,tmp_wgt);
  }
  else {
/*
 * wgt is not a scalar, so just point tmp_wgt at tmp1_wgt.
 */
    tmp_wgt = tmp1_wgt;
  }

/*
 * Allocate space for output array.
 */
  if(type_angmom == NCL_float) {
    angmom     = (void*)calloc(ntim,sizeof(float));
    tmp_angmom = (double*)calloc(1,sizeof(double));
    if(tmp_angmom == NULL || angmom == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"angmom_atm: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    angmom = (void*)calloc(ntim,sizeof(double));
    if(angmom == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"angmom_atm: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }

/*
 * Call function.
 */
  index_dp = 0;
  for( i = 0; i < ntim; i++ ) {
    if(type_u != NCL_double) {
/*
 * Coerce subsection of u (tmp_u) to double.
 */
      coerce_subset_input_double(u,tmp_u,index_dp,type_u,klevnlatnlon,
                                 0,NULL,NULL);
    }
    else {
/*
 * Point tmp_u to appropriate location in u.
 */
      tmp_u = &((double*)u)[index_dp];
    }

    if(type_angmom == NCL_double) tmp_angmom = &((double*)angmom)[i];

/*
 * Depending on whether dp is 1-dimensional or 3 (or 4) dimensional,
 * call a different Fortran routine.  First, if dp is 3 or dimensional,
 * we need to coerce it to double.
 */ 
    if(ndims_dp != 1) {
      if(type_dp != NCL_double) {
/*
 * Coerce subsection of dp (tmp_dp) to double.
 */
        coerce_subset_input_double(dp,tmp_dp,index_dp,type_dp,klevnlatnlon,
                                   0,NULL,NULL);
      }
      else {
/*
 * Point tmp_dp to appropriate location in dp.
 */
        tmp_dp = &((double*)dp)[index_dp];
      }

      NGCALLF(daamom3,DAAMOM3)(tmp_u,tmp_dp,tmp_lat,tmp_wgt,&inlon,&inlat,&iklev,
			       &missing_du.doubleval,tmp_angmom);
    }
    else {
      NGCALLF(daamom1,DAAMOM1)(tmp_u,tmp_dp,tmp_lat,tmp_wgt,&inlon,&inlat,&iklev,
			       &missing_du.doubleval,tmp_angmom);
    }
/*
 * Copy output values from temporary tmp_angmom to angmom.
 */
    if(type_angmom != NCL_double) {
      ((float*)angmom)[i] = (float)(*tmp_angmom);
    }
    index_dp += klevnlatnlon;
  }
/*
 * Free memory.
 */
  if(type_u   != NCL_double) NclFree(tmp_u);
  if(type_dp  != NCL_double) NclFree(tmp_dp);
  if(type_lat != NCL_double) NclFree(tmp_lat);
  if(tmp_wgt  != tmp1_wgt)   NclFree(tmp_wgt);
  if(type_wgt != NCL_double) NclFree(tmp1_wgt);
  if(type_angmom != NCL_double) NclFree(tmp_angmom);
/*
 * Return.
 */
  dsizes_angmom[0] = ntim;
  if(type_angmom == NCL_double) {
    return(NclReturnValue(angmom,1,dsizes_angmom,&missing_du,type_angmom,0));
  }
  else {
    return(NclReturnValue(angmom,1,dsizes_angmom,&missing_ru,type_angmom,0));
  }
}


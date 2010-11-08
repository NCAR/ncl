#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(mocloops,MOCLOOPS)(int *, int *, int *, int *, int *,
                                       double *, double *, int *, double *,
                                       double *, double *, double *, double *,
                                       double *, double *);

NhlErrorTypes moc_globe_atl_W( void )
{

/*
 * Input variables
 */
/*
 * Argument # 0
 */
  void *lat_aux_grid;
  double *tmp_lat_aux_grid;
  ng_size_t dsizes_lat_aux_grid[1];
  NclBasicDataTypes type_lat_aux_grid;
/*
 * Argument # 1
 */
  void *a_wvel;
  double *tmp_a_wvel;
  ng_size_t dsizes_a_wvel[3];
  int has_missing_a_wvel;
  NclScalar missing_a_wvel, missing_dbl_a_wvel;
  NclBasicDataTypes type_a_wvel;

/*
 * Argument # 2
 */
  void *a_bolus;
  double *tmp_a_bolus;
  ng_size_t dsizes_a_bolus[3];
  NclBasicDataTypes type_a_bolus;

/*
 * Argument # 3
 */
  void *a_submeso;
  double *tmp_a_submeso;
  ng_size_t dsizes_a_submeso[3];
  NclBasicDataTypes type_a_submeso;

/*
 * Argument # 4
 */
  void *tlat;
  double *tmp_tlat;
  ng_size_t dsizes_tlat[2];
  NclBasicDataTypes type_tlat;

/*
 * Argument # 5
 */
  int *rmlak;
  ng_size_t dsizes_rmlak[3];
/*
 * Return variable
 */
  void *tmp;
  double *dtmp1, *dtmp2, *dtmp3;
  int ndims_tmp;
  ng_size_t *dsizes_tmp;
  NclBasicDataTypes type_tmp;


/*
 * Various
 */
  ng_size_t nyaux, kdep, nlat, mlon, nlatmlon, kdepnlatmlon, kdepnyaux2;
  ng_size_t i, size_output;
  int nrx, inlat, imlon, ikdep, inyaux;
  int ret;

/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
/*
 * Get argument # 0
 */
  lat_aux_grid = (void*)NclGetArgValue(
           0,
           6,
           NULL,
           dsizes_lat_aux_grid,
           NULL,
           NULL,
           &type_lat_aux_grid,
           DONT_CARE);

/*
 * Get argument # 1
 */
  a_wvel = (void*)NclGetArgValue(
           1,
           6,
           NULL,
           dsizes_a_wvel,
           &missing_a_wvel,
           &has_missing_a_wvel,
           &type_a_wvel,
           DONT_CARE);

/*
 * Get argument # 2
 */
  a_bolus = (void*)NclGetArgValue(
           2,
           6,
           NULL,
           dsizes_a_bolus,
           NULL,
           NULL,
           &type_a_bolus,
           DONT_CARE);
  
/*
 * Get argument # 3
 */
  a_submeso = (void*)NclGetArgValue(
           3,
           6,
           NULL,
           dsizes_a_submeso,
           NULL,
           NULL,
           &type_a_submeso,
           DONT_CARE);

/*
 * Get argument # 4
 */
  tlat = (void*)NclGetArgValue(
           4,
           6,
           NULL,
           dsizes_tlat,
           NULL,
           NULL,
           &type_tlat,
           DONT_CARE);

/*
 * Get argument # 5
 */
  rmlak = (int*)NclGetArgValue(
           5,
           6,
           NULL,
           dsizes_rmlak,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

/*
 * Error checking
 */
  nyaux = dsizes_lat_aux_grid[0];
  kdep  = dsizes_a_wvel[0];
  nlat  = dsizes_a_wvel[1];
  mlon  = dsizes_a_wvel[2];
  kdepnyaux2   = 2 * kdep * nyaux;
  nlatmlon     = nlat * mlon;
  kdepnlatmlon = kdep * nlatmlon;

/*
 * Test dimension sizes.
 */
  if((mlon > INT_MAX) || (nlat > INT_MAX) ||
     (kdep > INT_MAX) || (nyaux > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"moc_globe_atl: one or more input dimension sizes are greater than INT_MAX");
    return(NhlFATAL);
  }
  imlon = (int) mlon;
  inlat = (int) nlat;
  ikdep = (int) kdep;
  inyaux = (int) nyaux;

  for(i = 0; i <= 2; i++) {
    if(dsizes_a_bolus[i] != dsizes_a_wvel[i] || 
       dsizes_a_bolus[i] != dsizes_a_submeso[i]) {
     NhlPError(NhlFATAL,NhlEUNKNOWN,"moc_globe_atl: a_wvel, a_submeso, and a_bolus must have the same dimensionality");
     return(NhlFATAL);
    }
  }

  if(dsizes_tlat[0] != nlat || dsizes_tlat[1] != mlon) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"moc_globe_atl: The dimensions of tlat must be nlat x mlon");
    return(NhlFATAL);
  }

  if(dsizes_rmlak[0] != 2 || 
     dsizes_rmlak[1] != nlat || dsizes_rmlak[2] != mlon) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"moc_globe_atl: The dimensions of rmlak must be 2 x nlat x mlon");
    return(NhlFATAL);
  }

/*
 * Coerce missing value to double if necessary.
 */
  coerce_missing(type_a_wvel,has_missing_a_wvel,&missing_a_wvel,
                 &missing_dbl_a_wvel,NULL);

/*
 * Coerce input arrays to double if necassary.
 */
  tmp_lat_aux_grid = coerce_input_double(lat_aux_grid,type_lat_aux_grid,nyaux,
					 0,NULL,NULL);
  if(tmp_lat_aux_grid == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"moc_globe_atl: Unable to allocate memory for coercing lat_aux_grid to double");
    return(NhlFATAL);
  }

  tmp_a_wvel = coerce_input_double(a_wvel,type_a_wvel,kdepnlatmlon,
                                   has_missing_a_wvel,&missing_a_wvel,
                                   &missing_dbl_a_wvel);

  if(tmp_a_wvel == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"moc_globe_atl: Unable to allocate memory for coercing a_wvel to double");
    return(NhlFATAL);
  }

  tmp_a_bolus = coerce_input_double(a_bolus,type_a_bolus,kdepnlatmlon,0,
                                    NULL,NULL);
  if(tmp_a_bolus == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"moc_globe_atl: Unable to allocate memory for coercing a_bolus to double");
    return(NhlFATAL);
  }

  tmp_a_submeso = coerce_input_double(a_submeso,type_a_submeso,kdepnlatmlon,
                                      0,NULL,NULL);
  if(tmp_a_submeso == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"moc_globe_atl: Unable to allocate memory for coercing a_submeso to double");
    return(NhlFATAL);
  }

  tmp_tlat = coerce_input_double(tlat,type_tlat,nlatmlon,0,NULL,NULL);
  if(tmp_tlat == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"moc_globe_atl: Unable to allocate memory for coercing tlat to double");
    return(NhlFATAL);
  }


/*
 * The output will be float unless any of the first four arguments
 * are double.
 */
  if(type_a_wvel == NCL_double || type_a_submeso == NCL_double ||
     type_a_bolus == NCL_double) {
    type_tmp = NCL_double;
  }
  else {
    type_tmp = NCL_float;
  }

/* 
 * Allocate space for output array.
 */
  size_output = 3 * kdepnyaux2;    /* 3 x 2 x kdep x nyaux */

  if(type_tmp != NCL_double) {
    tmp   = (void *)calloc(size_output, sizeof(float));
    dtmp1 = (double *)calloc(kdepnyaux2, sizeof(double));
    dtmp2 = (double *)calloc(kdepnyaux2, sizeof(double));
    dtmp3 = (double *)calloc(kdepnyaux2, sizeof(double));
    if(tmp == NULL || dtmp1 == NULL || dtmp2 == NULL ||
       dtmp3 == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"moc_globe_atl: Unable to allocate memory for temporary output arrays");
      return(NhlFATAL);
    }
  }
  else {
    tmp = (void *)calloc(size_output, sizeof(double));
    if(tmp == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"moc_globe_atl: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    dtmp1 = &((double*)tmp)[0];
    dtmp2 = &((double*)tmp)[kdepnyaux2];
    dtmp3 = &((double*)tmp)[2*kdepnyaux2];
  }

/* 
 * Allocate space for output dimension sizes and set them.
 */
  ndims_tmp  = 4;
  dsizes_tmp = (ng_size_t*)calloc(ndims_tmp,sizeof(ng_size_t));  
  if( dsizes_tmp == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"moc_globe_atl: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }
  dsizes_tmp[0] = 3; 
  dsizes_tmp[1] = 2;
  dsizes_tmp[2] = kdep;
  dsizes_tmp[3] = nyaux; 

/*
 * Call the Fortran routine.
 */
  nrx = 2;

  NGCALLF(mocloops,MOCLOOPS)(&inyaux, &imlon, &inlat, &ikdep, &nrx, tmp_tlat, 
                             tmp_lat_aux_grid, rmlak, tmp_a_wvel, tmp_a_bolus, 
                             tmp_a_submeso, &missing_dbl_a_wvel.doubleval, 
                             dtmp1, dtmp2, dtmp3);

  if(type_tmp != NCL_double) {
    coerce_output_float_only(tmp,dtmp1,kdepnyaux2,0);
    coerce_output_float_only(tmp,dtmp2,kdepnyaux2,kdepnyaux2);
    coerce_output_float_only(tmp,dtmp3,kdepnyaux2,2*kdepnyaux2);
  }

/*
 * Free unneeded memory.
 */
  if(type_lat_aux_grid != NCL_double) NclFree(tmp_lat_aux_grid);
  if(type_a_wvel    != NCL_double) NclFree(tmp_a_wvel);
  if(type_a_bolus   != NCL_double) NclFree(tmp_a_bolus);
  if(type_a_submeso != NCL_double) NclFree(tmp_a_submeso);
  if(type_tlat      != NCL_double) NclFree(tmp_tlat);
  if(type_tmp       != NCL_double) {
    NclFree(dtmp1);
    NclFree(dtmp2);
    NclFree(dtmp3);
  }

/*
 * Return value back to NCL script.
 */ 
  ret = NclReturnValue(tmp,ndims_tmp,dsizes_tmp,NULL,type_tmp,0);
  NclFree(dsizes_tmp);
  return(ret);
}

#include <stdio.h>
#include "wrapper.h"
#include "spei.h"

NhlErrorTypes spei_W( void )
{
/*
 * Input variables
 */
  void *precip;
  double *tmp_precip;
  int       ndims_precip;
  ng_size_t dsizes_precip[NCL_MAX_DIMENSIONS];
  int has_missing_precip;
  NclScalar missing_precip, missing_flt_precip, missing_dbl_precip;
  NclBasicDataTypes type_precip;

/*
 * Argument # 1
 */
  void *temp;
  double *tmp_temp;
  int       ndims_temp;
  ng_size_t dsizes_temp[NCL_MAX_DIMENSIONS];
  int has_missing_temp;
  NclScalar missing_temp, missing_flt_temp, missing_dbl_temp;
  NclBasicDataTypes type_temp;

/*
 * Argument # 2
 */
  void *lat;
  double *tmp_lat;
  int       ndims_lat;
  ng_size_t dsizes_lat[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_lat;

/*
 * Argument # 3
 */
  int *acumulated;
/*
 * Argument # 4
 */
  int *month;
/*
 * Argument # 5
 */
  int *year;
/*
 * Argument # 6
 */
  int *seasonality;
/*
 * Return variable
 */
  void *spei;
  double *tmp_spei;
  int has_missing_spei;
  ng_size_t *dsizes_spei;
  NclScalar missing_spei, missing_flt_spei, missing_dbl_spei;
  NclBasicDataTypes type_spei;

/*
 * Various
 */
  ng_size_t npts, acum_npts, index_precip, index_lat, index_spei;
  ng_size_t i, j, size_leftmost, size_output;
  int inpts, acum_inpts, nlat, ret;
  double *etpSeries, *balanceSeries, *acumSeries, *seasonSeries;

/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
/*
 * Get argument # 0
 */
  precip = (void*)NclGetArgValue(
           0,
           7,
           &ndims_precip,
           dsizes_precip,
           &missing_precip,
           &has_missing_precip,
           &type_precip,
           DONT_CARE);

/*
 * Check dimension sizes.
 */
  if(ndims_precip < 1) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"spei: The precip array must have at least one dimension");
    return(NhlFATAL);
  }

/*
 * Coerce missing value to double if necessary.
 */
  coerce_missing(type_precip,has_missing_precip,&missing_precip,
                 &missing_dbl_precip,&missing_flt_precip);

  npts = dsizes_precip[ndims_precip-1];
  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"spei: npts = %ld is greater than INT_MAX", npts);
    return(NhlFATAL);
  }
  inpts = (int) npts;

/*
 * Get argument # 1
 */
  temp = (void*)NclGetArgValue(
           1,
           7,
           &ndims_temp,
           dsizes_temp,
           &missing_temp,
           &has_missing_temp,
           &type_temp,
           DONT_CARE);

/*
 * Check dimension sizes.
 */
  if(ndims_temp != ndims_precip) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"spei: The temp and precip arrays must be the same dimensionality");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_temp; i++) {
    if(dsizes_precip[i] != dsizes_temp[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"spei: The temp and precip arrays must be the same dimensionality");
      return(NhlFATAL);
    }
  }
/*
 * Coerce missing value to double if necessary.
 */
  coerce_missing(type_temp,has_missing_temp,&missing_temp,
                 &missing_dbl_temp,&missing_flt_temp);

/*
 * Get argument # 2
 */
  lat = (void*)NclGetArgValue(
           2,
           7,
           &ndims_lat,
           dsizes_lat,
           NULL,
           NULL,
           &type_lat,
           DONT_CARE);

  nlat = dsizes_lat[ndims_lat-1];
  if(nlat != 1) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"spei: Can only handle one latitude value right now\n");
    return(NhlFATAL);
  }
/*
 * Get argument # 3
 */
  acumulated = (int*)NclGetArgValue(
           3,
           7,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
/*
 * Get argument # 4
 */
  month = (int*)NclGetArgValue(
           4,
           7,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
/*
 * Get argument # 5
 */
  year = (int*)NclGetArgValue(
           5,
           7,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
/*
 * Get argument # 6
 */
  seasonality = (int*)NclGetArgValue(
           6,
           7,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

/*
 * Calculate size of output array.
 */
  acum_npts = npts-(*acumulated)+1;
  if(acum_npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"spei: acumulated npts = %ld is greater than INT_MAX", acum_npts);
    return(NhlFATAL);
  }
  acum_inpts = (int) acum_npts;

/*
 * Calculate size of leftmost dimensions.
 */
  size_leftmost  = 1;
  for(i = 0; i < ndims_precip-1; i++) size_leftmost *= dsizes_precip[i];

/* 
 * Allocate space for coercing input arrays.  If any of the input
 * is already double, then we don't need to allocate space for
 * temporary arrays, because we'll just change the pointer into
 * the void array appropriately.
 */
  if(type_precip != NCL_double) {
    tmp_precip = (double *)calloc(npts,sizeof(double));
    if(tmp_precip == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"spei: Unable to allocate memory for coercing precip array to double");
      return(NhlFATAL);
    }
  }
/*
 * Allocate space for tmp_temp.
 */
  if(type_temp != NCL_double) {
    tmp_temp = (double *)calloc(npts,sizeof(double));
    if(tmp_temp == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"spei: Unable to allocate memory for coercing temp array to double");
      return(NhlFATAL);
    }
  }
/*
 * Allocate space for tmp_lat.
 */
  if(type_lat != NCL_double) {
    tmp_lat = (double *)calloc(nlat,sizeof(double));
    if(tmp_lat == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"spei: Unable to allocate memory for coercing latitude array to double");
      return(NhlFATAL);
    }
  }
/*
 * Allocate space for work arrays.
 */
  etpSeries     = (double *)calloc(NUMDATOSMAX,sizeof(double));
  balanceSeries = (double *)calloc(NUMDATOSMAX,sizeof(double));
  acumSeries    = (double *)calloc(NUMDATOSMAX,sizeof(double));
  seasonSeries  = (double *)calloc(NUMDATOSMAX,sizeof(double));
  if(etpSeries == NULL || balanceSeries == NULL || 
     acumSeries == NULL || seasonSeries == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"spei: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/* 
 * Allocate space for output array.
 */
  dsizes_spei  = (ng_size_t*)calloc(ndims_temp,sizeof(ng_size_t)); 
  if(dsizes_spei == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"spei: Unable to allocate memory for output array dimension sizes");
    return(NhlFATAL);
  }
  for( i = 0; i < (ndims_temp-1); i++ ) dsizes_spei[i] = dsizes_temp[i];
  dsizes_spei[ndims_temp-1] = acum_npts;
  size_output = size_leftmost * acum_npts;
  if(type_precip == NCL_double || type_temp == NCL_double) {
    spei = (void *)calloc(size_output, sizeof(double));
    if(spei == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"spei: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    type_spei = NCL_double;
    missing_spei.doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
  }
  else {
    spei     = (void *)calloc(size_output, sizeof(float));
    tmp_spei = (double *)calloc(acum_npts,sizeof(double));
    if(spei == NULL || tmp_spei == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"spei: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    type_spei = NCL_float;
    missing_spei.floatval = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
  }
/*
 * Initialize to all missing, because not every value will necessarily
 * have a calculation.
 */
  if(type_spei == NCL_double) {
    for( i = 0; i < size_output; i++ )  ((double*)spei)[i] = missing_spei.doubleval;
  }
  else {
    for( i = 0; i < size_output; i++ ) ((float*)spei)[i]   = missing_spei.floatval;
  }

/*
 * Loop across leftmost dimensions and call the Fortran routine for each
 * subsection of the input arrays.
 */
  index_precip = index_lat = index_spei = 0;

  for(i = 0; i < size_leftmost; i++) {
/*
 * Coerce subsection of precip (tmp_precip) to double if necessary.
 */
    if(type_precip != NCL_double) {
      coerce_subset_input_double(precip,tmp_precip,index_precip,type_precip,npts,0,NULL,NULL); 
    }
    else {
      tmp_precip = &((double*)precip)[index_precip];
    }
/*
 * Coerce subsection of temp (tmp_temp) to double if necessary.
 */
    if(type_temp != NCL_double) {
      coerce_subset_input_double(temp,tmp_temp,index_precip,type_temp,npts,0,NULL,NULL);
    }
    else {
      tmp_temp = &((double*)temp)[index_precip];
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

    /* Output array */
    if(type_spei == NCL_double) tmp_spei = &((double*)spei)[index_spei];
/*
 * Call the Fortran routine.
 */
    spei_func(tmp_precip, tmp_temp, inpts, 
              *tmp_lat, *acumulated, *month, 
              *year, *seasonality,&etpSeries[0], &balanceSeries[0],
              &acumSeries[0],&seasonSeries[0],&tmp_spei[0]);
/*
 * Coerce output back to float if necessary.
 */
    if(type_spei != NCL_double) {
      coerce_output_float_only(spei,tmp_spei,acum_npts,index_spei);
    }
    index_spei   += acum_npts;
    index_precip += npts;
    /* Only handle one latitude value right now */
    /*    index_lat    += nlat;*/
  }

/*
 * Free unneeded memory.
 */
  NclFree(etpSeries);
  NclFree(balanceSeries);
  NclFree(acumSeries);
  NclFree(seasonSeries);
  if(type_precip != NCL_double) NclFree(tmp_precip);
  if(type_temp   != NCL_double) NclFree(tmp_temp);
  if(type_lat    != NCL_double) NclFree(tmp_lat);
  if(type_spei   != NCL_double) NclFree(tmp_spei);
/*
 * Return value back to NCL script.
 */
  ret = NclReturnValue(spei,ndims_precip,dsizes_spei,&missing_spei,type_spei,0);
  NclFree(dsizes_spei);
  return(ret);
}

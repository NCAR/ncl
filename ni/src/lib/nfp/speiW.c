#include <stdio.h>
#include "wrapper.h"
#include "spei.h"

NhlErrorTypes speidx_W( void )
{
/*
 * Input variables
 */
  void *data;
  double *tmp_data;
  int       ndims_data;
  ng_size_t dsizes_data[NCL_MAX_DIMENSIONS];
  int has_missing_data;
  NclScalar missing_data, missing_flt_data, missing_dbl_data;
  NclBasicDataTypes type_data;

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
  int *lrun;
/*
 * Argument # 3
 */
  logical *opt;

/*
 * Argument # 4
 */
  int *dim;

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
  double *acumSeries, *seasonSeries;
  ng_size_t i, j, l, size_output;
  ng_size_t ilt, iln, iltln, ntim, acum_ntim, nlat, nlon, nrnt, nlatlon;
  int seasonality=12;  /* hard-coded for now. */
  int intim, num_rgt_dims, ret;
  ng_size_t index_input, index_lat, index_nrt, size_data, total_nr, total_nl;
  char grid_type[13];

/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
/*
 * Get argument # 0
 */
  data = (void*)NclGetArgValue(
           0,
           5,
           &ndims_data,
           dsizes_data,
           &missing_data,
           &has_missing_data,
           &type_data,
           DONT_CARE);

/*
 * Coerce missing value to double if necessary.
 */
  coerce_missing(type_data,has_missing_data,&missing_data,
                 &missing_dbl_data,&missing_flt_data);

  size_data = 1;
  for(i = 0; i < ndims_data; i++) size_data *= dsizes_data[i];

/*
 * Get argument # 1
 */
  lat = (void*)NclGetArgValue(
           1,
           5,
           &ndims_lat,
           dsizes_lat,
           NULL,
           NULL,
           &type_lat,
           DONT_CARE);

/*
 * Get argument # 2
 */
  lrun = (int*)NclGetArgValue(
           2,
           5,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
/*
 * Get argument # 3
 */
  opt = (logical*)NclGetArgValue(
           3,
           5,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

/*
 * Get argument # 4
 */
  dim = (int *)NclGetArgValue(4,5,NULL,NULL,NULL,NULL,NULL,0);

/*
 * Some error checking. Make sure input dimension is valid.
 */
  if(dim[0] < 0 || dim[0] >= ndims_data) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"speidx: Invalid dimension argument, can't continue");
    return(NhlFATAL);
  }

/*
 * Check size of time dimension.
 */
  ntim = dsizes_data[dim[0]];
  if(ntim > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"speidx: ntim = %ld is greater than INT_MAX", ntim);
    return(NhlFATAL);
  }
  intim = (int) ntim;


/*
 * Check dimension sizes. This code is a bit complicated given the possible
 * structure of the input data and lat arrays.
 *
 * Note: anything to the left of the "ntim" dimension is
 * considered to be a leftmost dimension that we have to loop across. 
 *
 * What we care about for error checking are the dimensions to the *right* of
 * ntim, which will tell us what kind of lat/lon grid we have.
 *
 * t(...,ntim)           - lat is a scalar
 *
 * t(...,ntim,ncol)      - the grid is unstructured (eg: spectral element), then lat(ncol)
 *
 * t(...,ntim,nlat,mlon) - the grid is rectilinear, then lat(nlat)
 *
 * t(...,ntim,nlat,mlon) - the grid is curvilinear, then lat(nlat,mlon) 
 */
  num_rgt_dims = ndims_data-dim[0];
/*
 * scalar case
 */
  if(num_rgt_dims == 1) {
    if(!is_scalar(ndims_lat,dsizes_lat)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"speidx: If data has no lat/lon dimensions, then lat must be a scalar. Check your data array and the 'dim' value.");
      return(NhlFATAL);
    }
    else {
      strcpy(grid_type,"scalar");
      nlat = nlon = 1;
    }
  }
/*
 * unstructured grid case
 */
  else if(num_rgt_dims == 2) {
    if(ndims_lat != 1 || (ndims_lat == 1 && dsizes_lat[0] != dsizes_data[dim[0]+1])) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"speidx: If data is on an unstructured grid, then lat must be 1D and the same size as rightmost dimension of data. Check your data array and the 'dim' value.");
      return(NhlFATAL);
    }
    else {
      strcpy(grid_type,"unstructured");
      nlat = dsizes_lat[0];
      nlon = 1;
    }
  }
/*
 * rectilinear or curvilinear grid case
 */
  else if(num_rgt_dims == 3) {
    if((ndims_lat == 1 && (dsizes_lat[0] != dsizes_data[dim[0]+1]))|| 
       (ndims_lat == 2 && (dsizes_lat[0] != dsizes_data[dim[0]+1]  || 
                           dsizes_lat[1] != dsizes_data[dim[0]+2]))||
       ndims_lat > 2) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"speidx: If data is on a rectilinear or curvilinear grid, then lat must either be a 1D array of size nlat or a 2D array of size nlat x mlon. Check your data array and the 'dim' value.");
      return(NhlFATAL);
    }
    else {
      if(ndims_lat == 1) {
        strcpy(grid_type,"rectilinear");
        nlat = dsizes_lat[0];
        nlon = dsizes_data[ndims_data-1];
      }
      else {
        strcpy(grid_type,"curvilinear");
        nlat = dsizes_lat[0];
        nlon = dsizes_lat[1];
      }
    }
  }
/*
 * There's a problem with the input data, lat, and/or dim variables.
 */
  else {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"speidx: the data and/or lat arrays don't appear to have the correct dimensionality, or else 'dim' has the wrong value.");
    return(NhlFATAL);
  }

/*
 * Calculate size of output array.
 */
  acum_ntim = ntim-(*lrun)+1;
  if(acum_ntim > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"speidx: accumulated # of time steps = %ld is greater than INT_MAX", acum_ntim);
    return(NhlFATAL);
  }

/*
 * Calculate size of rightmost and leftmost dimensions.
 */
  total_nr = nlat * nlon;
  total_nl = size_data / ntim / total_nr;

/*
 * Allocate space for coercing data and lat to double. We have to allocate space 
 * for tmp_data no matter what, because the output values may not be 
 * contiguous in memory.
 */
  tmp_data = (double *)calloc(ntim,sizeof(double));
  if(tmp_data == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"speidx: Unable to allocate memory for coercing data array to double");
    return(NhlFATAL);
  }

/*
 * Allocate space for tmp_lat.
 */
  if(type_lat != NCL_double) tmp_lat = (double *)calloc(1,sizeof(double));

/*
 * Allocate space for work arrays.
 */
  acumSeries    = (double *)calloc(ntim,sizeof(double));
  seasonSeries  = (double *)calloc(ntim,sizeof(double));
  if(acumSeries == NULL || seasonSeries == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"speidx: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/* 
 * Allocate space for output array. Note: the temporary "tmp_spei"
 * array will be ntim in size, but the output array will be 
 * acum_ntim.
 */
  size_output = (size_data / ntim) * acum_ntim;
  tmp_spei    = (double *)calloc(ntim,sizeof(double));
  if(tmp_spei == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"speidx: Unable to allocate memory for temporary output array");
    return(NhlFATAL);
  }
  if(type_data == NCL_double) {
    spei                   = (void *)calloc(size_output, sizeof(double));
    type_spei              = NCL_double;
    missing_spei.doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
  }
  else {
    spei                  = (void *)calloc(size_output, sizeof(float));
    type_spei             = NCL_float;
    missing_spei.floatval = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
  }
  if(spei == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"speidx: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * The output array is the same size as data, except with the time 
 * dimension replaced with accumulated time. 
 */
  dsizes_spei = (ng_size_t*)calloc(ndims_data,sizeof(ng_size_t)); 
  if(dsizes_spei == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"speidx: Unable to allocate memory for output array dimension sizes");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_data; i++) dsizes_spei[i] = dsizes_data[i];
  dsizes_spei[dim[0]] = acum_ntim;    /* replacing the time dimension. */

/*
 * Loop across nlat and nlon and pass in ntim subsections of data
 * and single latitude values to Fortran routine.
 */
  nlatlon    = nlat * nlon;
  nrnt       = total_nr * ntim;

  for(i = 0; i < total_nl; i++) {
    index_nrt = i*nrnt;
    for(ilt = 0; ilt < nlat; ilt++) {
/* 
 * A rectilinear grid is a special case as it has
 * lat values for the lat dimension.  Scalar
 * and unstructured grids have nlon=1, and 
 * curvilinear grids have nlat x nlon latitude values
 */
      if(!strcmp(grid_type,"rectilinear")) {
        if(type_lat != NCL_double) {
          coerce_subset_input_double(lat,tmp_lat,ilt,type_lat,1,0,NULL,NULL);
        }
        else {
          tmp_lat = &((double*)lat)[ilt];
        }
      }
      for(iln = 0; iln < nlon; iln++) {
        iltln       = (ilt*nlon)+iln;
        index_input = index_nrt + iltln;
/*
 * Coerce subsection of data (tmp_data) to double.
 */
        coerce_subset_input_double_step(data,tmp_data,index_input,nlatlon,type_data,
                                        ntim,0,NULL,NULL);
	for(j = 0; j < ntim; j++) tmp_spei[j] = acumSeries[j] = 0.0;
	for(j = 0; j < ntim; j++) {
	  for(l = 0; l < *lrun; l++) {
	    acumSeries[j-*lrun+1] += tmp_data[j-l];
	  }
	}

/*
 * For anything but a rectilinear grid (which we handled above), 
 * coerce subsection of lat (tmp_lat) to double if necessary.
 */
        if(strcmp(grid_type,"rectilinear")) {
          index_lat = ilt*nlon+iln;
          if(type_lat != NCL_double) {
            coerce_subset_input_double(lat,tmp_lat,index_lat,type_lat,1,0,NULL,NULL);
          }
          else {
            tmp_lat = &((double*)lat)[index_lat];
          }
        }
/*
 * Call the C routine.
 */
	spei_func(&acumSeries[0], acum_ntim, missing_dbl_data.doubleval,
		  seasonality, &tmp_spei[0], &seasonSeries[0]); 
/*
 * Coerce output back to appropriate location. Note that only acum_ntim
 * points are being copied.  The assumption is that the last 
 * (ntime-acum_ntim) points are all set to 0.0
 */
        coerce_output_float_or_double_step(spei,tmp_spei,type_spei,
					   acum_ntim,index_input,total_nr);
      }
    }
  }

/*
 * Free unneeded memory.
 */
  NclFree(acumSeries);
  NclFree(seasonSeries);
  NclFree(tmp_data);
  NclFree(tmp_spei);
  if(type_lat!= NCL_double) NclFree(tmp_lat);
/*
 * Return value back to NCL script.
 */
  ret = NclReturnValue(spei,ndims_data,dsizes_spei,&missing_spei,type_spei,0);
  NclFree(dsizes_spei);
  return(ret);
}

NhlErrorTypes spei_driver_W( void )
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
  logical *opt;

/*
 * Argument # 5
 */
  int *dim;

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
  double *etpSeries, *balanceSeries, *acumSeries, *seasonSeries;
  ng_size_t i, size_output;
  ng_size_t ilt, iln, iltln, ntim, acum_ntim, nlat, nlon, nrnt, nlatlon;
  int seasonality=12;  /* hard-coded for now. */
  int intim, num_rgt_dims, ret;
  ng_size_t index_input, index_lat, index_nrt, size_temp, total_nr, total_nl;
  char grid_type[13];

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
           6,
           &ndims_precip,
           dsizes_precip,
           &missing_precip,
           &has_missing_precip,
           &type_precip,
           DONT_CARE);

/*
 * Coerce missing value to double if necessary.
 */
  coerce_missing(type_precip,has_missing_precip,&missing_precip,
                 &missing_dbl_precip,&missing_flt_precip);

/*
 * Get argument # 1
 */
  temp = (void*)NclGetArgValue(
           1,
           6,
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
    NhlPError(NhlFATAL,NhlEUNKNOWN,"spei_driver: The temp and precip arrays must be the same dimensionality");
    return(NhlFATAL);
  }
  size_temp = 1;    /* calculate size of temp while we check dimension sizes */
  for(i = 0; i < ndims_temp; i++) {
    size_temp *= dsizes_temp[i];
    if(dsizes_precip[i] != dsizes_temp[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"spei_driver: The temp and precip arrays must be the same dimensionality");
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
           6,
           &ndims_lat,
           dsizes_lat,
           NULL,
           NULL,
           &type_lat,
           DONT_CARE);

/*
 * Get argument # 3
 */
  acumulated = (int*)NclGetArgValue(
           3,
           6,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
/*
 * Get argument # 4
 */
  opt = (logical*)NclGetArgValue(
           4,
           6,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

/*
 * Get argument # 5
 */
  dim = (int *)NclGetArgValue(5,6,NULL,NULL,NULL,NULL,NULL,0);

/*
 * Some error checking. Make sure input dimension is valid.
 */
  if(dim[0] < 0 || dim[0] >= ndims_precip) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"spei_driver: Invalid dimension argument, can't continue");
    return(NhlFATAL);
  }

/*
 * Check size of time dimension.
 */
  ntim = dsizes_precip[dim[0]];
  if(ntim > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"spei_driver: ntim = %ld is greater than INT_MAX", ntim);
    return(NhlFATAL);
  }
  intim = (int) ntim;


/*
 * Check dimension sizes. This code is a bit complicated given the possible
 * structure of the input temp and lat arrays.
 *
 * Note: anything to the left of the "ntim" dimension is
 * considered to be a leftmost dimension that we have to loop across. 
 *
 * What we care about for error checking are the dimensions to the *right* of
 * ntim, which will tell us what kind of lat/lon grid we have.
 *
 * t(...,ntim)           - lat is a scalar
 *
 * t(...,ntim,ncol)      - the grid is unstructured (eg: spectral element), then lat(ncol)
 *
 * t(...,ntim,nlat,mlon) - the grid is rectilinear, then lat(nlat)
 *
 * t(...,ntim,nlat,mlon) - the grid is curvilinear, then lat(nlat,mlon) 
 */
  num_rgt_dims = ndims_temp-dim[0];
/*
 * scalar case
 */
  if(num_rgt_dims == 1) {
    if(!is_scalar(ndims_lat,dsizes_lat)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"spei_driver: If temp has no lat/lon dimensions, then lat must be a scalar. Check your temp array and the 'dim' value.");
      return(NhlFATAL);
    }
    else {
      strcpy(grid_type,"scalar");
      nlat = nlon = 1;
    }
  }
/*
 * unstructured grid case
 */
  else if(num_rgt_dims == 2) {
    if(ndims_lat != 1 || (ndims_lat == 1 && dsizes_lat[0] != dsizes_temp[dim[0]+1])) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"spei_driver: If temp is on an unstructured grid, then lat must be 1D and the same size as rightmost dimension of temp. Check your temp array and the 'dim' value.");
      return(NhlFATAL);
    }
    else {
      strcpy(grid_type,"unstructured");
      nlat = dsizes_lat[0];
      nlon = 1;
    }
  }
/*
 * rectilinear or curvilinear grid case
 */
  else if(num_rgt_dims == 3) {
    if((ndims_lat == 1 && (dsizes_lat[0] != dsizes_temp[dim[0]+1]))|| 
       (ndims_lat == 2 && (dsizes_lat[0] != dsizes_temp[dim[0]+1]  || 
                           dsizes_lat[1] != dsizes_temp[dim[0]+2]))||
       ndims_lat > 2) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"spei_driver: If temp is on a rectilinear or curvilinear grid, then lat must either be a 1D array of size nlat or a 2D array of size nlat x mlon. Check your temp array and the 'dim' value.");
      return(NhlFATAL);
    }
    else {
      if(ndims_lat == 1) {
        strcpy(grid_type,"rectilinear");
        nlat = dsizes_lat[0];
        nlon = dsizes_temp[ndims_temp-1];
      }
      else {
        strcpy(grid_type,"curvilinear");
        nlat = dsizes_lat[0];
        nlon = dsizes_lat[1];
      }
    }
  }
/*
 * There's a problem with the input temp, lat, and/or dim variables.
 */
  else {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"spei_driver: the temp and/or lat arrays don't appear to have the correct dimensionality, or else 'dim' has the wrong value.");
    return(NhlFATAL);
  }

/*
 * Calculate size of output array.
 */
  acum_ntim = ntim-(*acumulated)+1;
  if(acum_ntim > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"spei_driver: acumulated # of time steps = %ld is greater than INT_MAX", acum_ntim);
    return(NhlFATAL);
  }

/*
 * Calculate size of rightmost and leftmost dimensions.
 */
  total_nr = nlat * nlon;
  total_nl = size_temp / ntim / total_nr;

/*
 * Allocate space for coercing temp, precip, and lat to double. We have to allocate space 
 * for tmp_precip and tmp_temp no matter what, because the output values may not be 
 * contiguous in memory.
 */
  tmp_precip = (double *)calloc(ntim,sizeof(double));
  if(tmp_precip == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"spei_driver: Unable to allocate memory for coercing precip array to double");
    return(NhlFATAL);
  }
/*
 * Allocate space for tmp_temp.
 */
  tmp_temp = (double *)calloc(ntim,sizeof(double));
  if(tmp_temp == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"spei_driver: Unable to allocate memory for coercing temp array to double");
    return(NhlFATAL);
  }
/*
 * Allocate space for tmp_lat.
 */
  if(type_lat != NCL_double) tmp_lat = (double *)calloc(1,sizeof(double));

/*
 * Allocate space for work arrays.
 */
  etpSeries     = (double *)calloc(ntim,sizeof(double));
  balanceSeries = (double *)calloc(ntim,sizeof(double));
  acumSeries    = (double *)calloc(ntim,sizeof(double));
  seasonSeries  = (double *)calloc(ntim,sizeof(double));
  if(etpSeries == NULL || balanceSeries == NULL || 
     acumSeries == NULL || seasonSeries == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"spei_driver: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/* 
 * Allocate space for output array. Note: the temporary "tmp_spei"
 * array will be ntim in size, but the output array will be 
 * acum_ntim.
 */
  size_output = (size_temp / ntim) * acum_ntim;
  tmp_spei    = (double *)calloc(ntim,sizeof(double));
  if(tmp_spei == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"spei_driver: Unable to allocate memory for temporary output array");
    return(NhlFATAL);
  }
  if(type_precip == NCL_double || type_temp == NCL_double) {
    spei                   = (void *)calloc(size_output, sizeof(double));
    type_spei              = NCL_double;
    missing_spei.doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
  }
  else {
    spei                  = (void *)calloc(size_output, sizeof(float));
    type_spei             = NCL_float;
    missing_spei.floatval = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
  }
  if(spei == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"spei_driver: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Initialize to all missing, because not every value will necessarily
 * have a calculation. COMMENTED OUT BECAUSE THE SPEI ROUTINE IS
 * SETTING THIS ARRAY TO 0.0
 */
/*
  if(type_spei == NCL_double) {
    for( i = 0; i < size_output; i++ )  ((double*)spei)[i] = missing_spei.doubleval;
  }
  else {
    for( i = 0; i < size_output; i++ ) ((float*)spei)[i]   = missing_spei.floatval;
  }
*/
/*
 * The output array is the same size as temp/precip, except with the time 
 * dimension replaced with acumulated time. 
 */
  dsizes_spei = (ng_size_t*)calloc(ndims_temp,sizeof(ng_size_t)); 
  if(dsizes_spei == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"spei_driver: Unable to allocate memory for output array dimension sizes");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_temp; i++) dsizes_spei[i] = dsizes_temp[i];
  dsizes_spei[dim[0]] = acum_ntim;    /* replacing the time dimension. */

/*
 * Loop across nlat and nlon and pass in ntim subsections of temp
 * and single latitude values to Fortran routine.
 */
  nlatlon    = nlat * nlon;
  nrnt       = total_nr * ntim;

  for(i = 0; i < total_nl; i++) {
    index_nrt = i*nrnt;
    for(ilt = 0; ilt < nlat; ilt++) {
/* 
 * A rectilinear grid is a special case as it has
 * lat values for the lat dimension.  Scalar
 * and unstructured grids have nlon=1, and 
 * curvilinear grids have nlat x nlon latitude values
 */
      if(!strcmp(grid_type,"rectilinear")) {
        if(type_lat != NCL_double) {
          coerce_subset_input_double(lat,tmp_lat,ilt,type_lat,1,0,NULL,NULL);
        }
        else {
          tmp_lat = &((double*)lat)[ilt];
        }
      }
      for(iln = 0; iln < nlon; iln++) {
        iltln       = (ilt*nlon)+iln;
        index_input = index_nrt + iltln;
/*
 * Coerce subsection of precip (tmp_precip) to double.
 */
        coerce_subset_input_double_step(precip,tmp_precip,index_input,nlatlon,type_precip,
                                        ntim,0,NULL,NULL);
/*
 * Coerce subsection of temp (tmp_temp) to double.
 */
        coerce_subset_input_double_step(temp,tmp_temp,index_input,nlatlon,type_temp,
                                        ntim,0,NULL,NULL);
/*
 * For anything but a rectilinear grid (which we handled above), 
 * coerce subsection of lat (tmp_lat) to double if necessary.
 */
        if(strcmp(grid_type,"rectilinear")) {
          index_lat = ilt*nlon+iln;
          if(type_lat != NCL_double) {
            coerce_subset_input_double(lat,tmp_lat,index_lat,type_lat,1,0,NULL,NULL);
          }
          else {
            tmp_lat = &((double*)lat)[index_lat];
          }
        }
/*
 * Call the C routine.
 */
	spei_driver(tmp_precip, tmp_temp, missing_dbl_temp.doubleval,
		    intim, *tmp_lat, *acumulated,seasonality,
		    &etpSeries[0],&balanceSeries[0],
		    &acumSeries[0],&seasonSeries[0],&tmp_spei[0]);
/*
 * Coerce output back to appropriate location. Note that only acum_ntim
 * points are being copied.  The assumption is that the last 
 * (ntime-acum_ntim) points are all set to 0.0
 */
        coerce_output_float_or_double_step(spei,tmp_spei,type_spei,
					   acum_ntim,index_input,total_nr);
      }
    }
  }

/*
 * Free unneeded memory.
 */
  NclFree(etpSeries);
  NclFree(balanceSeries);
  NclFree(acumSeries);
  NclFree(seasonSeries);
  NclFree(tmp_precip);
  NclFree(tmp_temp);
  NclFree(tmp_spei);
  if(type_lat!= NCL_double) NclFree(tmp_lat);
/*
 * Return value back to NCL script.
 */
  ret = NclReturnValue(spei,ndims_precip,dsizes_spei,&missing_spei,type_spei,0);
  NclFree(dsizes_spei);
  return(ret);
}

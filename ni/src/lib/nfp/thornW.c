#include <stdio.h>
#include "wrapper.h"
#include "spei.h"

extern void NGCALLF(thorn2,THORN2)(double *, int *, double *, 
                                   double *, double *, int *);

NhlErrorTypes thornthwaite_W( void )
{

/*
 * Input variables
 *
 * Argument # 0
 */
  void *temp;
  double *tmp_temp;
  int ndims_temp;
  ng_size_t dsizes_temp[NCL_MAX_DIMENSIONS], size_temp;
  int has_missing_temp;
  NclScalar missing_temp, missing_flt_temp, missing_dbl_temp;
  NclBasicDataTypes type_temp;

/*
 * Argument # 1
 */
  void *lat;
  double *tmp_lat;
  int ndims_lat;
  ng_size_t dsizes_lat[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_lat;

/*
 * Argument # 2
 */
  logical *opt;

/*
 * Argument # 3
 */
  int *dim;

/*
 * Return variable
 */
  void *pet;
  double *tmp_pet;
  ng_size_t *dsizes_pet, size_pet;
  NclScalar missing_pet;
  NclBasicDataTypes type_pet;

/*
 * Various
 */
  int intim, ret, num_rgt_dims, ier;
  ng_size_t i, ilt, iln, iltln, ntim, ntim_temp, nlat, nlon, nrnt, nrnp, nlatlon;
  ng_size_t index_temp, index_pet, index_lat, index_nrt, index_nrp, total_nr, total_nl, nt;
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
  temp = (void*)NclGetArgValue(
           0,
           4,
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
           4,
           &ndims_lat,
           dsizes_lat,
           NULL,
           NULL,
           &type_lat,
           DONT_CARE);

/*
 * Get argument # 2
 */
  opt = (logical*)NclGetArgValue(
           2,
           4,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

/*
 * Get argument # 3
 */
  dim = (int *)NclGetArgValue(3,4,NULL,NULL,NULL,NULL,NULL,DONT_CARE);

/*
 * Some error checking. Make sure input dimension is valid.
 */
  if(dim[0] < 0 || dim[0] >= ndims_temp) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"thornthwaite: Invalid dimension argument, can't continue");
    return(NhlFATAL);
  }

/*
 * Check size of time dimension.
 */
  ntim_temp = dsizes_temp[dim[0]];
  if(ntim_temp > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"thornthwaite: ntime = %ld is greater than INT_MAX", ntim_temp);
    return(NhlFATAL);
  }

/*
 * The if test below was added in V6.4.0 as a way of checking if ntim=1 (the 
 * Fortran expects ntim to be a multiple of 12). If ntim=1, then we will
 * propagate the scalar time value to an array of twelve elements, and
 * set nim=12.  See NCL-2336.
 */
  if( (ntim_temp!=1) && (ntim_temp%12) != 0) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"thornthwaite: ntime must be 1 or a multiple of 12");
    return(NhlFATAL);
  }
  if(ntim_temp == 1) {
    ntim = 12;
  }
  else {
    ntim = ntim_temp;
  }
  intim = (int) ntim;

/*
 * Check dimension sizes. This code is a bit complicated given the possible
 * structure of the input temp and lat arrays.
 *
 * Note: anything to the left of the "ntim_temp" dimension is
 * considered to be a leftmost dimension that we have to loop across. 
 *
 * What we care about for error checking are the dimensions to the *right* of
 * ntim_temp, which will tell us what kind of lat/lon grid we have.
 *
 * t(...,ntim_temp)           - lat is a scalar
 *
 * t(...,ntim_temp,ncol)      - the grid is unstructured (eg: spectral element), then lat(ncol)
 *
 * t(...,ntim_temp,nlat,mlon) - the grid is rectilinear, then lat(nlat)
 *
 * t(...,ntim_temp,nlat,mlon) - the grid is curvilinear, then lat(nlat,mlon) 
 */
  num_rgt_dims = ndims_temp-dim[0];
/*
 * scalar case
 */
  if(num_rgt_dims == 1) {
    if(!is_scalar(ndims_lat,dsizes_lat)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"thornthwaite: If temp has no lat/lon dimensions, then lat must be a scalar. Check your temp array and the 'dim' value.");
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
      NhlPError(NhlFATAL,NhlEUNKNOWN,"thornthwaite: If temp is on an unstructured grid, then lat must be 1D and the same size as rightmost dimension of temp. Check your temp array and the 'dim' value.");
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
      NhlPError(NhlFATAL,NhlEUNKNOWN,"thornthwaite: If temp is on a rectilinear or curvilinear grid, then lat must either be a 1D array of size nlat or a 2D array of size nlat x mlon. Check your temp array and the 'dim' value.");
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
    NhlPError(NhlFATAL,NhlEUNKNOWN,"thornthwaite: the temp and/or lat arrays don't appear to have the correct dimensionality, or else 'dim' has the wrong value.");
    return(NhlFATAL);
  }

/*
 * Calculate size of temp and output. They will be the same if ntim_temp is divisible by 12.
 */
  size_pet = size_temp = 1;
  dsizes_pet = (ng_size_t*)calloc(ndims_temp,sizeof(ng_size_t));
  for(i = 0; i < ndims_temp; i++) {
    dsizes_pet[i] = dsizes_temp[i];
    size_pet  *= dsizes_temp[i];
    size_temp *= dsizes_temp[i];
  }
  if(ntim_temp==1) {
    size_pet *= 12;
    dsizes_pet[dim[0]] = 12;
  }

/*
 * Calculate size of rightmost and leftmost dimensions.
 */
  total_nr = nlat * nlon;
  total_nl = size_temp / ntim_temp / total_nr;

/*
 * Allocate space for coercing temp and lat to double. We have to allocate space 
 * for tmp_temp no matter what, because the output values may not be 
 * contiguous in memory.
 */
  tmp_temp = (double *)calloc(ntim,sizeof(double));
  if(tmp_temp == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"thornthwaite: Unable to allocate memory for coercing input array to double");
    return(NhlFATAL);
  }
  if(type_lat != NCL_double) tmp_lat = (double *)calloc(1,sizeof(double));

/* 
 * Allocate space for output array.
 */
  tmp_pet = (double *)calloc(ntim,sizeof(double));
  if(tmp_pet == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"thornthwaite: Unable to allocate memory for temporary output array");
    return(NhlFATAL);
  }
  if(type_temp != NCL_double) {
    pet         = (void *)calloc(size_pet, sizeof(float));
    type_pet    = NCL_float;
    missing_pet = missing_flt_temp;
  }
  else {
    pet         = (void *)calloc(size_pet, sizeof(double));
    type_pet    = NCL_double;
    missing_pet = missing_dbl_temp;
  }
  if(pet == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"thornthwaite: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Loop across nlat and nlon and pass in ntim_temp subsections of temp
 * and single latitude values to Fortran routine.
 */
  nlatlon    = nlat * nlon;
  nrnt       = total_nr * ntim_temp;
  nrnp       = total_nr * ntim;

  for(i = 0; i < total_nl; i++) {
    index_nrt = i*nrnt;
    index_nrp = i*nrnp;
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
        iltln      = (ilt*nlon)+iln;
        index_temp = index_nrt + iltln;
        index_pet  = index_nrp + iltln;
/*
 * Coerce subsection of temp (tmp_temp) to double if necessary.
 */
        coerce_subset_input_double_step(temp,tmp_temp,index_temp,nlatlon,type_temp,
                                        ntim_temp,0,NULL,NULL);
        if(ntim_temp==1) {
          /* Copy just the first value, then we'll propagate this to the other 11 array values. */
          for(nt = 1; nt < ntim; nt++) {
            tmp_temp[nt] = tmp_temp[0];
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
 * Call the Fortran routine.
 */
        NGCALLF(thorn2,THORN2)(tmp_temp, &intim, &missing_dbl_temp.doubleval,
                               tmp_lat, tmp_pet, &ier);
/*
 * Coerce output back to appropriate location.
 */
        coerce_output_float_or_double_step(pet,tmp_pet,type_pet,ntim,index_pet,total_nr);
      }
    }
  }

/*
 * Free unneeded memory.
 */
  NclFree(tmp_temp);
  NclFree(tmp_pet);
  if(type_lat  != NCL_double) NclFree(tmp_lat);

/*
 * Return value back to NCL script.
 */
  ret = NclReturnValue(pet,ndims_temp,dsizes_pet,&missing_pet,type_pet,0);
  return(ret);
}

/*
 * This wrapper is an unadvertised one. The interface is the same as
 * "thornthwaite", except instead of calling the Fortran thorn2, it
 * calls a C version based on R's thornthwaite function. This C
 * function is used by the speidx wrapper.
 */

NhlErrorTypes thornthwaite_r_W( void )
{

/*
 * Input variables
 *
 * Argument # 0
 */
  void *temp;
  double *tmp_temp;
  int ndims_temp;
  ng_size_t dsizes_temp[NCL_MAX_DIMENSIONS], size_temp;
  int has_missing_temp;
  NclScalar missing_temp, missing_flt_temp, missing_dbl_temp;
  NclBasicDataTypes type_temp;

/*
 * Argument # 1
 */
  void *lat;
  double *tmp_lat;
  int ndims_lat;
  ng_size_t dsizes_lat[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_lat;

/*
 * Argument # 2
 */
  logical *opt;

/*
 * Argument # 3
 */
  int *dim;

/*
 * Return variable
 */
  void *pet;
  double *tmp_pet;
  ng_size_t *dsizes_pet, size_pet;
  NclScalar missing_pet;
  NclBasicDataTypes type_pet;

/*
 * Various
 */
  int intim, ret, num_rgt_dims;
  ng_size_t i, ilt, iln, iltln, ntim, ntim_temp, nlat, nlon, nrnt, nrnp, nlatlon;
  ng_size_t index_temp, index_pet, index_lat, index_nrt, index_nrp, total_nr, total_nl, nt;
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
  temp = (void*)NclGetArgValue(
           0,
           4,
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
           4,
           &ndims_lat,
           dsizes_lat,
           NULL,
           NULL,
           &type_lat,
           DONT_CARE);

/*
 * Get argument # 2
 */
  opt = (logical*)NclGetArgValue(
           2,
           4,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

/*
 * Get argument # 3
 */
  dim = (int *)NclGetArgValue(3,4,NULL,NULL,NULL,NULL,NULL,DONT_CARE);

/*
 * Some error checking. Make sure input dimension is valid.
 */
  if(dim[0] < 0 || dim[0] >= ndims_temp) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"thornthwaite_r: Invalid dimension argument, can't continue");
    return(NhlFATAL);
  }

/*
 * Check size of time dimension.
 */
  ntim_temp = dsizes_temp[dim[0]];
  if(ntim_temp > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"thornthwaite_r: ntim = %ld is greater than INT_MAX", ntim_temp);
    return(NhlFATAL);
  }

/*
 * The if test below was added in V6.4.0 as a way of checking if ntim=1 (the 
 * Fortran expects ntim to be a multiple of 12). If ntim=1, then we will
 * propagate the scalar time value to an array of twelve elements, and
 * set nim=12.  See NCL-2336.
 */
  if( (ntim_temp!=1) && (ntim_temp%12) != 0) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"thornthwaite_r: ntime must be 1 or a multiple of 12");
    return(NhlFATAL);
  }
  if(ntim_temp == 1) {
    ntim = 12;
  }
  else {
    ntim = ntim_temp;
  }
  intim = (int) ntim;

/*
 * Check dimension sizes. This code is a bit complicated given the possible
 * structure of the input temp and lat arrays.
 *
 * Note: anything to the left of the "ntim_temp" dimension is
 * considered to be a leftmost dimension that we have to loop across. 
 *
 * What we care about for error checking are the dimensions to the *right* of
 * ntim_temp, which will tell us what kind of lat/lon grid we have.
 *
 * t(...,ntim_temp)           - lat is a scalar
 *
 * t(...,ntim_temp,ncol)      - the grid is unstructured (eg: spectral element), then lat(ncol)
 *
 * t(...,ntim_temp,nlat,mlon) - the grid is rectilinear, then lat(nlat)
 *
 * t(...,ntim_temp,nlat,mlon) - the grid is curvilinear, then lat(nlat,mlon) 
 */
  num_rgt_dims = ndims_temp-dim[0];
/*
 * scalar case
 */
  if(num_rgt_dims == 1) {
    if(!is_scalar(ndims_lat,dsizes_lat)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"thornthwaite_r: If temp has no lat/lon dimensions, then lat must be a scalar. Check your temp array and the 'dim' value.");
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
      NhlPError(NhlFATAL,NhlEUNKNOWN,"thornthwaite_r: If temp is on an unstructured grid, then lat must be 1D and the same size as rightmost dimension of temp. Check your temp array and the 'dim' value.");
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
      NhlPError(NhlFATAL,NhlEUNKNOWN,"thornthwaite_r: If temp is on a rectilinear or curvilinear grid, then lat must either be a 1D array of size nlat or a 2D array of size nlat x mlon. Check your temp array and the 'dim' value.");
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
    NhlPError(NhlFATAL,NhlEUNKNOWN,"thornthwaite_r: the temp and/or lat arrays don't appear to have the correct dimensionality, or else 'dim' has the wrong value.");
    return(NhlFATAL);
  }

/*
 * Calculate size of temp and output. They will be the same if ntim_temp is divisible by 12.
 */
  size_pet = size_temp = 1;
  dsizes_pet = (ng_size_t*)calloc(ndims_temp,sizeof(ng_size_t));
  for(i = 0; i < ndims_temp; i++) {
    dsizes_pet[i] = dsizes_temp[i];
    size_pet  *= dsizes_temp[i];
    size_temp *= dsizes_temp[i];
  }
  if(ntim_temp==1) {
    size_pet *= 12;
    dsizes_pet[dim[0]] = 12;
  }

/*
 * Calculate size of rightmost and leftmost dimensions.
 */
  total_nr = nlat * nlon;
  total_nl = size_temp / ntim_temp / total_nr;

/*
 * Allocate space for coercing temp and lat to double. We have to allocate space 
 * for tmp_temp no matter what, because the output values may not be 
 * contiguous in memory.
 */
  tmp_temp = (double *)calloc(ntim,sizeof(double));
  if(tmp_temp == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"thornthwaite_r: Unable to allocate memory for coercing input array to double");
    return(NhlFATAL);
  }
  if(type_lat != NCL_double) tmp_lat = (double *)calloc(1,sizeof(double));

/* 
 * Allocate space for output array.
 */
  tmp_pet = (double *)calloc(ntim,sizeof(double));
  if(tmp_pet == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"thornthwaite_r: Unable to allocate memory for temporary output array");
    return(NhlFATAL);
  }
  if(type_temp != NCL_double) {
    pet         = (void *)calloc(size_pet, sizeof(float));
    type_pet    = NCL_float;
    missing_pet = missing_flt_temp;
  }
  else {
    pet         = (void *)calloc(size_pet, sizeof(double));
    type_pet    = NCL_double;
    missing_pet = missing_dbl_temp;
  }
  if(pet == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"thornthwaite_r: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Loop across nlat and nlon and pass in ntim_temp subsections of temp
 * and single latitude values to Fortran routine.
 */
  nlatlon    = nlat * nlon;
  nrnt       = total_nr * ntim_temp;
  nrnp       = total_nr * ntim;

  for(i = 0; i < total_nl; i++) {
    index_nrt = i*nrnt;
    index_nrp = i*nrnp;
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
        iltln      = (ilt*nlon)+iln;
        index_temp = index_nrt + iltln;
        index_pet  = index_nrp + iltln;
/*
 * Coerce subsection of temp (tmp_temp) to double if necessary.
 */
        coerce_subset_input_double_step(temp,tmp_temp,index_temp,nlatlon,type_temp,
                                        ntim_temp,0,NULL,NULL);
        if(ntim_temp==1) {
          /* Copy just the first value, then we'll propagate this to the other 11 array values. */
          for(nt = 1; nt < ntim; nt++) {
            tmp_temp[nt] = tmp_temp[0];
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
 * Call the C version of "R" routine.
 */
        thornthwaite(tmp_temp, intim, missing_dbl_temp.doubleval,*tmp_lat, tmp_pet);
/*
 * Coerce output back to appropriate location.
 */
        coerce_output_float_or_double_step(pet,tmp_pet,type_pet,ntim,index_pet,total_nr);
      }
    }
  }

/*
 * Free unneeded memory.
 */
  NclFree(tmp_temp);
  NclFree(tmp_pet);
  if(type_lat  != NCL_double) NclFree(tmp_lat);

/*
 * Return value back to NCL script.
 */
  ret = NclReturnValue(pet,ndims_temp,dsizes_pet,&missing_pet,type_pet,0);
  return(ret);
}

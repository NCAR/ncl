#include <stdio.h>
#include "wrapper.h"

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
  ng_size_t dsizes_temp[NCL_MAX_DIMENSIONS];
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
  NclScalar missing_pet;
  NclBasicDataTypes type_pet;

/*
 * Various
 */
  int intim, ret, ier, num_rgt_dims;
  ng_size_t i, ilt, iln, iltln, ntim, nlat, nlon, nrnt, nlatlon;
  ng_size_t index_temp, index_lat, index_nrt, size_temp, total_nr, total_nl;
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
  dim = (int *)NclGetArgValue(3,4,NULL,NULL,NULL,NULL,NULL,0);

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
  ntim = dsizes_temp[dim[0]];
  if(ntim > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"thornthwaite: ntim = %ld is greater than INT_MAX", ntim);
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
 * Calculate temp size (and hence output size).
 */
  size_temp = 1;
  for(i = 0; i < ndims_temp; i++) size_temp *= dsizes_temp[i];

/*
 * Calculate size of rightmost and leftmost dimensions.
 */
  total_nr = nlat * nlon;
  total_nl = size_temp / ntim / total_nr;

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
    pet         = (void *)calloc(size_temp, sizeof(float));
    type_pet    = NCL_float;
    missing_pet = missing_flt_temp;
  }
  else {
    pet         = (void *)calloc(size_temp, sizeof(double));
    type_pet    = NCL_double;
    missing_pet = missing_dbl_temp;
  }
  if(pet == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"thornthwaite: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

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
        iltln      = (ilt*nlon)+iln;
        index_temp = index_nrt + iltln;
/*
 * Coerce subsection of temp (tmp_temp) to double if necessary.
 */
        coerce_subset_input_double_step(temp,tmp_temp,index_temp,nlatlon,type_temp,
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
 * Call the Fortran routine.
 */
        NGCALLF(thorn2,THORN2)(tmp_temp, &intim, &missing_dbl_temp.doubleval,
                               tmp_lat, tmp_pet, &ier);
/*
 * Coerce output back to appropriate location.
 */
        coerce_output_float_or_double_step(pet,tmp_pet,type_pet,ntim,index_temp,total_nr);
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
  ret = NclReturnValue(pet,ndims_temp,dsizes_temp,&missing_pet,type_pet,0);
  return(ret);
}

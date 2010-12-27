#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(dzhgtdrv,DZHGTDRV)(int*,int*,int*,double*,double*,
                                       double*,double*,double*,int*,int*);

NhlErrorTypes dz_height_W( void )
{
/*
 * Input variables
 */
  void *z, *zsfc, *ztop;
  int *iopt;
  double *tmp_z = NULL;
  double *tmp_zsfc = NULL;
  double *tmp1_zsfc = NULL;
  double *tmp_ztop;
  int ndims_z;
  ng_size_t dsizes_z[NCL_MAX_DIMENSIONS];
  int ndims_zsfc;
  ng_size_t dsizes_zsfc[NCL_MAX_DIMENSIONS];
  int has_missing_zsfc, is_scalar_zsfc;
  NclScalar missing_zsfc, missing_dzsfc, missing_rzsfc;
  NclBasicDataTypes type_z, type_zsfc, type_ztop;
/*
 * Output variables
 */
  void *dz;
  double *tmp_dz = NULL;
  NclBasicDataTypes type_dz;
  NclScalar missing_dz;
/*
 * Various.
 */
  ng_size_t i, nlat, nlon, klvl, nlatnlon, klvlnlatnlon;
  ng_size_t size_z, index_zsfc, index_z, size_leftmost;
  int ier, der, ret;
  int inlon, inlat, iklvl;

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
          NULL,
          NULL,
          &type_z,
          DONT_CARE);

  zsfc = (void*)NclGetArgValue(
          1,
          4,
          &ndims_zsfc, 
          dsizes_zsfc,
          &missing_zsfc,
          &has_missing_zsfc,
          &type_zsfc,
          DONT_CARE);
    
  ztop = (void*)NclGetArgValue(
          2,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_ztop,
          DONT_CARE);

  iopt = (int*)NclGetArgValue(
          3,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);
/*
 * Check dimension sizes for z.
 */
  if(ndims_z < 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dz_height: The 'z' array must be at least 3-dimensional with rightmost dimensions lev x lat x lon");
    return(NhlFATAL);
  }
  klvl = dsizes_z[ndims_z-3];
  nlat = dsizes_z[ndims_z-2];
  nlon = dsizes_z[ndims_z-1];

/*
 * Check input dimension sizes.
 */
    if((nlon > INT_MAX) ||
       (nlat > INT_MAX) ||
       (klvl > INT_MAX)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dz_height: one or more input dimension sizes are greater than INT_MAX");
      return(NhlFATAL);
    }
    inlon = (int) nlon;
    inlat = (int) nlat;
    iklvl = (int) klvl;

/*
 * Check dimension sizes for zsfc. It must be a scalar, an nlat x nlon
 * array, or the same size as z.
 */
  is_scalar_zsfc = is_scalar(ndims_zsfc,dsizes_zsfc);

  der = 0;
  if(ndims_zsfc == 1 && !is_scalar_zsfc) {
    der = 1;
  }
  else if(ndims_zsfc == 2 && (dsizes_zsfc[0] != nlat || dsizes_zsfc[1] != nlon)) {
    der = 1;
  }
  else if(ndims_zsfc > 2) {
    if(ndims_zsfc != ndims_z) {
      der = 1;
    }
    else {
      i = 0;
      while(i < ndims_zsfc && !der) {
        if(dsizes_zsfc[i] != dsizes_z[i]) {
          der = 1;
        }
        i++;
      }
    }
  }
  if(der) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dz_height: The 'zsfc' array must be a scalar, an nlat x nlon array, or an array the same size as 'z'");
    return(NhlFATAL);
  }

/*
 * Calculate other dimension sizes.
 */
  nlatnlon      = nlat * nlon;
  klvlnlatnlon  = klvl * nlatnlon;
  size_leftmost = 1;
  for(i = 0; i < ndims_z-3; i++) size_leftmost *= dsizes_z[i];
  size_z = size_leftmost * klvlnlatnlon;

/*
 * Determine type of output.
 */
  if(type_z == NCL_double || type_zsfc == NCL_double || type_ztop == NCL_double) {
    type_dz = NCL_double;
  }
  else {
    type_dz = NCL_float;
  }

  if(has_missing_zsfc) {
    coerce_missing(type_zsfc,has_missing_zsfc,&missing_zsfc,
                   &missing_dzsfc,&missing_rzsfc);
  }
  else {
    missing_dzsfc.doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
    missing_rzsfc.floatval  = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;

  }

/*
 * Coerce input data to double or allocate space if necessary.
 */
  if(type_z != NCL_double) {
    if(ndims_z == 3) {
      tmp_z = coerce_input_double(z,type_z,klvlnlatnlon,0,NULL,NULL);
    }
    else {
      tmp_z = (double*)calloc(klvlnlatnlon,sizeof(double));
    }
    if(tmp_z == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dz_height: Unable to coerce 'z' to double precision");
      return(NhlFATAL);
    }
  }
  else {
    if(ndims_z == 3) {
      tmp_z = &((double*)z)[0];
    }
  }

/*
 * If zsfc is a scalar, it must be copied to an nlat x nlon array.
 */
  if(is_scalar_zsfc) {
    tmp1_zsfc = coerce_input_double(zsfc,type_zsfc,1,0,NULL,NULL);
    tmp_zsfc  = (double*)calloc(nlatnlon,sizeof(double));
    for(i = 0; i < nlatnlon; i++) tmp_zsfc[i] = *tmp1_zsfc;
  }
/*
 * Otherwise, see if need to coerce it.
 */
  else if(type_zsfc != NCL_double) {
    if(ndims_zsfc == 2) {
      tmp_zsfc = coerce_input_double(zsfc,type_zsfc,nlatnlon,0,NULL,NULL);
    }
    else {
      tmp_zsfc = (double*)calloc(nlatnlon,sizeof(double));
    }
    if(tmp_zsfc == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dz_height: Unable to coerce 'zsfc' to double precision");
      return(NhlFATAL);
    }
  }
  else {
    if(ndims_zsfc == 2) {
      tmp_zsfc = &((double*)zsfc)[0];
    }
  }

  tmp_ztop = coerce_input_double(ztop,type_ztop,1,0,NULL,NULL);
  if( tmp_ztop == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dz_height: Unable to coerce input to double precision");
    return(NhlFATAL);
  }

/*
 * Allocate space for output array.
 */
  if(type_dz == NCL_float) {
    dz     = (void*)calloc(size_z,sizeof(float));
    tmp_dz = (double*)calloc(klvlnlatnlon,sizeof(double));
    if(tmp_dz == NULL || dz == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dz_height: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    missing_dz = missing_rzsfc;
  }
  else {
    dz = (void*)calloc(size_z,sizeof(double));
    if(dz == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dz_height: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    missing_dz = missing_dzsfc;
  }

  index_z = index_zsfc = 0;
  for(i = 0; i < size_leftmost; i++ ) {
/*
 * Coerce subsection of z (tmp_z) to double, if necessary
 */
    if(ndims_z > 3) {
      if(type_z != NCL_double) {
        coerce_subset_input_double(z,tmp_z,index_z,type_z,klvlnlatnlon,
                                   0,NULL,NULL);
      }
      else {
/*
 * Point tmp_z to z.
 */
        tmp_z = &((double*)z)[index_z];
      }
    }

/*
 * Coerce subsection of zsfc (tmp_zsfc) to double, if necessary.
 */
    if(ndims_zsfc > 2) {
      if(type_zsfc != NCL_double) {
        coerce_subset_input_double(zsfc,tmp_zsfc,index_zsfc,type_zsfc,
                                   nlatnlon,0,NULL,NULL);
      }
      else {
/*
 * Point tmp_zsfc to zsfc.
 */
        tmp_zsfc = &((double*)zsfc)[index_zsfc];
      }
    }

/*
 * Point tmp_dz to dz if necessary.
 */
    if(type_dz == NCL_double) {
      tmp_dz = &((double*)dz)[index_z];
    }

/*
 * Call Fortran routine.
 */
    NGCALLF(dzhgtdrv,DZHGTDRV)(&inlon,&inlat,&iklvl,tmp_z,tmp_zsfc,
                               &missing_dzsfc.doubleval,tmp_ztop,tmp_dz,
                               iopt,&ier);
/*
 * Check for error returns.
 */
    if(ier == 1) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"dz_height: ztop is < 0 which is not allowed. Missing values will be returned.");
    }
    if(ier == 5) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"dz_height: z must be monotonically increasing or decreasing. Missing values will be returned.");
    }
    if(ier == 15) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"dz_height: zsfc and/or ztop contained missing values. Missing values will be returned.");
    }
/*
 * If float, copy output values from temporary tmp_dz to dz.
 */
    if(type_dz == NCL_float) {
      coerce_output_float_only(dz,tmp_dz,klvlnlatnlon,index_z);
    }
    index_z    += klvlnlatnlon;
    index_zsfc += nlatnlon;
  }

/*
 * Free memory.
 */
  if(is_scalar_zsfc) {
    if(type_zsfc != NCL_double) NclFree(tmp1_zsfc);
    NclFree(tmp_zsfc);
  }
  else {
    if(type_zsfc != NCL_double) NclFree(tmp_zsfc);
  }
  if(type_z    != NCL_double) NclFree(tmp_z);
  if(type_ztop != NCL_double) NclFree(tmp_ztop);
  if(type_dz   != NCL_double) NclFree(tmp_dz);

  ret = NclReturnValue(dz,ndims_z,dsizes_z,&missing_dz,type_dz,0);
  return(ret);
}


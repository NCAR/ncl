#include <stdio.h>

/*
 * The following are the required NCAR Graphics include files.
 * They should be located in ${NCARG_ROOT}/include.
 */
#include "wrapper.h"

extern void NGCALLF(preshybrid,PRESHYBRID)(double*,double*,double*,double*,
                                           int*,double*);

extern void NGCALLF(dpreshybrid,DRESPHYBRID)(double*,double*,double*,double*,
                                             int*,double*);

extern void NGCALLF(dphybrid,DPHYBRID)(double*,double*,double*,double*,
                                       int*,int*,int*,double*);

extern void NGCALLF(ddphybrid,DDPHYBRID)(double*,double*,double*,double*,
                                         int*,int*,int*,double*);

NhlErrorTypes pres_hybrid_W( void )
{
/*
 * Input variables
 */
  void *psfc, *p0, *hya, *hyb;
  double *tmp_psfc, *tmp_p0, *tmp_hya, *tmp_hyb;
  int ndims_psfc, dsizes_psfc[NCL_MAX_DIMENSIONS];
  int dsizes_hya[NCL_MAX_DIMENSIONS], dsizes_hyb[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_p0, type_psfc, type_hya, type_hyb;
/*
 * Output variables
 */
  void *phy;
  double *tmp_phy;
  int ndims_phy, *dsizes_phy;
  NclBasicDataTypes type_phy;
/*
 * Various.
 */
  int i, j, index_phy, klvl, size_leftmost, size_phy;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  psfc = (void*)NclGetArgValue(
          0,
          4,
          &ndims_psfc, 
          dsizes_psfc,
          NULL,
          NULL,
          &type_psfc,
          2);
  p0 = (void*)NclGetArgValue(
          1,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_p0,
          2);
  hya = (void*)NclGetArgValue(
          2,
          4,
          NULL,
          dsizes_hya,
          NULL,
          NULL,
          &type_hya,
          2);
  hyb = (void*)NclGetArgValue(
          3,
          4,
          NULL,
          dsizes_hyb,
          NULL,
          NULL,
          &type_hyb,
          2);
/*
 * Check dimensions.
 */
  klvl = dsizes_hya[0];
  if( dsizes_hyb[0] != klvl) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pres_hybrid: The 'hyb' array must be the same length as 'hya'");
    return(NhlFATAL);
  }
/*
 * Determine type of output.
 */
  if(type_psfc == NCL_double) {
    type_phy = NCL_double;
  }
  else {
    type_phy = NCL_float;
  }
/*
 * Calculate total size of output array.
 */
  if(ndims_psfc == 1 && dsizes_psfc[0] == 1) {
    ndims_phy = 1;
  }
  else {
    ndims_phy = ndims_psfc + 1;
  }

  dsizes_phy = (int*)calloc(ndims_phy,sizeof(int));  
  if( dsizes_phy == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pres_hybrid: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }

  size_leftmost = 1;
/*
 * psfc is not a scalar, so phy will be an (ndims_psfc+1)-D array of length 
 * N x klvl, where N represents the dimensions of psfc.
 */
  for( i = 0; i < ndims_psfc; i++ ) {
    size_leftmost *= dsizes_psfc[i];
    dsizes_phy[i] = dsizes_psfc[i];
  }
  size_phy = size_leftmost * klvl;
  dsizes_phy[ndims_phy-1] = klvl;
/*
 * Coerce data to double if necessary.
 */
  tmp_p0  = coerce_input_double(p0,type_p0,1,0,NULL,NULL);
  tmp_hya = coerce_input_double(hya,type_hya,klvl,0,NULL,NULL);
  tmp_hyb = coerce_input_double(hyb,type_hyb,klvl,0,NULL,NULL);
  if( tmp_p0 == NULL || tmp_hya == NULL || tmp_hyb == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pres_hybrid: Unable to coerce input to double precision");
    return(NhlFATAL);
  }

/*
 * Coerce psfc.
 */
  if(type_psfc != NCL_double) {
    tmp_psfc = (double*)calloc(1,sizeof(double));
    if( tmp_psfc == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pres_hybrid: Unable to allocate memory for coercing psfc array to double precision");
      return(NhlFATAL);
    }
  }
/*
 * Allocate space for output array.
 */
  if(type_phy == NCL_float) {
    phy     = (void*)calloc(size_phy,sizeof(float));
    tmp_phy = (double*)calloc(klvl,sizeof(double));
    if(tmp_phy == NULL || phy == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pres_hybrid: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    phy = (void*)calloc(size_phy,sizeof(double));
    if(phy == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pres_hybrid: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }

/*
 * Call function.
 */
  index_phy = 0;
  for( i = 0; i < size_leftmost; i++ ) {
    if(type_psfc != NCL_double) {
/*
 * Coerce subsection of psfc (tmp_psfc) to double.
 */
      coerce_subset_input_double(psfc,tmp_psfc,i,type_psfc,1,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_psfc to appropriate location in psfc.
 */
      tmp_psfc = &((double*)psfc)[i];
    }

    if(type_phy == NCL_double) tmp_phy = &((double*)phy)[index_phy];

    NGCALLF(preshybrid,PRESHYBRID)(tmp_p0,tmp_psfc,tmp_hya,tmp_hyb,&klvl,
                                   tmp_phy);
/*
 * Copy output values from temporary tmp_phy to phy.
 */
    if(type_phy != NCL_double) {
      for( j = 0; j < klvl; j++ ) {
        ((float*)phy)[index_phy+j] = (float)tmp_phy[j];
      }
    }
    index_phy += klvl;
  }
/*
 * Free memory.
 */
  if(type_psfc != NCL_double) NclFree(tmp_psfc);
  if(type_p0   != NCL_double) NclFree(tmp_p0);
  if(type_hya  != NCL_double) NclFree(tmp_hya);
  if(type_hyb  != NCL_double) NclFree(tmp_hyb);
  if(type_phy  != NCL_double) NclFree(tmp_phy);
/*
 * Return.
 */
  return(NclReturnValue(phy,ndims_phy,dsizes_phy,NULL,type_phy,0));
}



NhlErrorTypes dpres_hybrid_W( void )
{
/*
 * Input variables
 */
  void *psfc, *p0, *hya, *hyb;
  double *tmp_psfc, *tmp_p0, *tmp_hya, *tmp_hyb;
  int ndims_psfc, dsizes_psfc[NCL_MAX_DIMENSIONS];
  int dsizes_hya[NCL_MAX_DIMENSIONS], dsizes_hyb[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_p0, type_psfc, type_hya, type_hyb;
/*
 * Output variables
 */
  void *phy;
  double *tmp_phy;
  int ndims_phy, *dsizes_phy;
  NclBasicDataTypes type_phy;
/*
 * Various.
 */
  int i, j, index_phy, klvl, klvl1, size_leftmost, size_phy;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  psfc = (void*)NclGetArgValue(
          0,
          4,
          &ndims_psfc, 
          dsizes_psfc,
          NULL,
          NULL,
          &type_psfc,
          2);
  p0 = (void*)NclGetArgValue(
          1,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_p0,
          2);
  hya = (void*)NclGetArgValue(
          2,
          4,
          NULL,
          dsizes_hya,
          NULL,
          NULL,
          &type_hya,
          2);
  hyb = (void*)NclGetArgValue(
          3,
          4,
          NULL,
          dsizes_hyb,
          NULL,
          NULL,
          &type_hyb,
          2);
/*
 * Check dimensions.
 */
  klvl  = dsizes_hya[0];
  klvl1 = klvl-1;
  if( dsizes_hyb[0] != klvl) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dpres_hybrid: The 'hyb' array must be the same length as 'hya'");
    return(NhlFATAL);
  }
/*
 * Determine type of output.
 */
  if(type_psfc == NCL_double) {
    type_phy = NCL_double;
  }
  else {
    type_phy = NCL_float;
  }
/*
 * Calculate total size of output array.
 */
  if(ndims_psfc == 1 && dsizes_psfc[0] == 1) {
    ndims_phy = 1;
  }
  else {
    ndims_phy = ndims_psfc + 1;
  }

  dsizes_phy = (int*)calloc(ndims_phy,sizeof(int));  
  if( dsizes_phy == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dpres_hybrid: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }

  size_leftmost = 1;
/*
 * psfc is not a scalar, so phy will be an (ndims_psfc+1)-D array of length 
 * N x klvl1, where N represents the dimensions of psfc.
 */
  for( i = 0; i < ndims_psfc; i++ ) {
    size_leftmost *= dsizes_psfc[i];
    dsizes_phy[i] = dsizes_psfc[i];
  }
  size_phy = size_leftmost * klvl1;
  dsizes_phy[ndims_phy-1] = klvl1;
/*
 * Coerce data to double if necessary.
 */
  tmp_p0  = coerce_input_double(p0,type_p0,1,0,NULL,NULL);
  tmp_hya = coerce_input_double(hya,type_hya,klvl,0,NULL,NULL);
  tmp_hyb = coerce_input_double(hyb,type_hyb,klvl,0,NULL,NULL);
  if( tmp_hya == NULL || tmp_hyb == NULL || tmp_p0 == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dpres_hybrid: Unable to coerce input to double precision");
    return(NhlFATAL);
  }
/*
 * Coerce psfc.
 */
  if(type_psfc != NCL_double) {
    tmp_psfc = (double*)calloc(1,sizeof(double));
    if( tmp_psfc == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dpres_hybrid: Unable to allocate memory for coercing psfc array to double precision");
      return(NhlFATAL);
    }
  }
/*
 * Allocate space for output array.
 */
  if(type_phy == NCL_float) {
    phy     = (void*)calloc(size_phy,sizeof(float));
    tmp_phy = (double*)calloc(klvl1,sizeof(double));
    if(tmp_phy == NULL || phy == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dpres_hybrid: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    phy = (void*)calloc(size_phy,sizeof(double));
    if(phy == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dpres_hybrid: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }

/*
 * Call function.
 */
  index_phy = 0;
  for( i = 0; i < size_leftmost; i++ ) {
    if(type_psfc != NCL_double) {
/*
 * Coerce subsection of psfc (tmp_psfc) to double.
 */
      coerce_subset_input_double(psfc,tmp_psfc,i,type_psfc,1,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_psfc to appropriate location in psfc.
 */
      tmp_psfc = &((double*)psfc)[i];
    }

    if(type_phy == NCL_double) tmp_phy = &((double*)phy)[index_phy];

    NGCALLF(dpreshybrid,DPRESHYBRID)(tmp_p0,tmp_psfc,tmp_hya,tmp_hyb,&klvl,
                                     tmp_phy);
/*
 * Copy output values from temporary tmp_phy to phy.
 */
    if(type_phy != NCL_double) {
      for( j = 0; j < klvl1; j++ ) {
        ((float*)phy)[index_phy+j] = (float)tmp_phy[j];
      }
    }
    index_phy += klvl1;
  }
/*
 * Free memory.
 */
  if(type_psfc != NCL_double) NclFree(tmp_psfc);
  if(type_p0   != NCL_double) NclFree(tmp_p0);
  if(type_hya  != NCL_double) NclFree(tmp_hya);
  if(type_hyb  != NCL_double) NclFree(tmp_hyb);
  if(type_phy  != NCL_double) NclFree(tmp_phy);
/*
 * Return.
 */
  return(NclReturnValue(phy,ndims_phy,dsizes_phy,NULL,type_phy,0));
}

NhlErrorTypes pres_hybrid_ccm_W( void )
{
/*
 * Input variables
 */
  void *psfc, *p0, *hya, *hyb;
  double *tmp_psfc, *tmp_p0, *tmp_hya, *tmp_hyb;
  int ndims_psfc, dsizes_psfc[NCL_MAX_DIMENSIONS];
  int dsizes_hya[NCL_MAX_DIMENSIONS], dsizes_hyb[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_p0, type_psfc, type_hya, type_hyb;
/*
 * Output variables
 */
  void *phy;
  double *tmp_phy;
  int ndims_phy, *dsizes_phy;
  NclBasicDataTypes type_phy;
/*
 * Various.
 */
  int i, j, index_phy, nlat, nlon, klvl, klvlnlatnlon;
  int size_leftmost, size_phy;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  psfc = (void*)NclGetArgValue(
          0,
          4,
          &ndims_psfc, 
          dsizes_psfc,
          NULL,
          NULL,
          &type_psfc,
          2);
  p0 = (void*)NclGetArgValue(
          1,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_p0,
          2);
  hya = (void*)NclGetArgValue(
          2,
          4,
          NULL,
          dsizes_hya,
          NULL,
          NULL,
          &type_hya,
          2);
  hyb = (void*)NclGetArgValue(
          3,
          4,
          NULL,
          dsizes_hyb,
          NULL,
          NULL,
          &type_hyb,
          2);
/*
 * Check dimensions.
 */
  klvl = dsizes_hya[0];
  if( dsizes_hyb[0] != klvl) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pres_hybrid_ccm: The 'hyb' array must be the same length as 'hya'");
    return(NhlFATAL);
  }
  if(ndims_psfc < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pres_hybrid_ccm: The 'psfc' array must be at least two-dimensional");
    return(NhlFATAL);
  }
  nlat = dsizes_psfc[ndims_psfc-2];
  nlon = dsizes_psfc[ndims_psfc-1];
  klvlnlatnlon = klvl * nlat * nlon;
/*
 * Determine type of output.
 */
  if(type_psfc == NCL_double) {
    type_phy = NCL_double;
  }
  else {
    type_phy = NCL_float;
  }
/*
 * Calculate total size of output array.
 */
  ndims_phy = ndims_psfc + 1;

  dsizes_phy = (int*)calloc(ndims_phy,sizeof(int));  
  if( dsizes_phy == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pres_hybrid_ccm: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }

/*
 * Calculate dimension sizes of phy.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_psfc-2; i++ ) {
    size_leftmost *= dsizes_psfc[i];
    dsizes_phy[i] = dsizes_psfc[i];
  }
  size_phy = size_leftmost * klvlnlatnlon;
  dsizes_phy[ndims_psfc-2] = klvl;
  dsizes_phy[ndims_psfc-1] = nlat;
  dsizes_phy[ndims_psfc]   = nlon;
/*
 * Coerce data to double if necessary.
 */
  tmp_p0  = coerce_input_double(p0,type_p0,1,0,NULL,NULL);
  tmp_hya = coerce_input_double(hya,type_hya,klvl,0,NULL,NULL);
  tmp_hyb = coerce_input_double(hyb,type_hyb,klvl,0,NULL,NULL);
  if( tmp_hya == NULL || tmp_hyb == NULL || tmp_p0 == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pres_hybrid_ccm: Unable to coerce input to double precision");
    return(NhlFATAL);
  }

/*
 * Coerce psfc.
 */
  if(type_psfc != NCL_double) {
    tmp_psfc = (double*)calloc(nlat*nlon,sizeof(double));
    if( tmp_psfc == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pres_hybrid_ccm: Unable to allocate memory for coercing psfc array to double precision");
      return(NhlFATAL);
    }
  }

/*
 * Allocate space for output array.
 */
  if(type_phy == NCL_float) {
    phy     = (void*)calloc(size_phy,sizeof(float));
    tmp_phy = (double*)calloc(klvlnlatnlon,sizeof(double));
    if(tmp_phy == NULL || phy == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pres_hybrid_ccm: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    phy = (void*)calloc(size_phy,sizeof(double));
    if(phy == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pres_hybrid_ccm: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }

/*
 * Call function.
 */
  index_phy = 0;
  for( i = 0; i < size_leftmost; i++ ) {
    if(type_psfc != NCL_double) {
/*
 * Coerce subsection of psfc (tmp_psfc) to double.
 */
      coerce_subset_input_double(psfc,tmp_psfc,i,type_psfc,nlat*nlon,0,
                                 NULL,NULL);
    }
    else {
/*
 * Point tmp_psfc to appropriate location in psfc.
 */
      tmp_psfc = &((double*)psfc)[i];
    }

    if(type_phy == NCL_double) tmp_phy = &((double*)phy)[index_phy];

    NGCALLF(dphybrid,DPHYBRID)(tmp_p0,tmp_hya,tmp_hyb,tmp_psfc,&nlon,&nlat,
                               &klvl,tmp_phy);
/*
 * Copy output values from temporary tmp_phy to phy.
 */
    if(type_phy != NCL_double) {
      for( j = 0; j < klvlnlatnlon; j++ ) {
        ((float*)phy)[index_phy+j] = (float)tmp_phy[j];
      }
    }
    index_phy += klvlnlatnlon;
  }
/*
 * Free memory.
 */
  if(type_psfc != NCL_double) NclFree(tmp_psfc);
  if(type_p0   != NCL_double) NclFree(tmp_p0);
  if(type_hya  != NCL_double) NclFree(tmp_hya);
  if(type_hyb  != NCL_double) NclFree(tmp_hyb);
  if(type_phy  != NCL_double) NclFree(tmp_phy);
/*
 * Return.
 */
  return(NclReturnValue(phy,ndims_phy,dsizes_phy,NULL,type_phy,0));
}



NhlErrorTypes dpres_hybrid_ccm_W( void )
{
/*
 * Input variables
 */
  void *psfc, *p0, *hya, *hyb;
  double *tmp_psfc, *tmp_p0, *tmp_hya, *tmp_hyb;
  int ndims_psfc, dsizes_psfc[NCL_MAX_DIMENSIONS];
  int dsizes_hya[NCL_MAX_DIMENSIONS], dsizes_hyb[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_p0, type_psfc, type_hya, type_hyb;
/*
 * Output variables
 */
  void *phy;
  double *tmp_phy;
  int ndims_phy, *dsizes_phy;
  NclBasicDataTypes type_phy;
/*
 * Various.
 */
  int i, j, index_phy, nlat, nlon, klvl, klvl1, klvl1nlatnlon;
  int size_leftmost, size_phy;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  psfc = (void*)NclGetArgValue(
          0,
          4,
          &ndims_psfc, 
          dsizes_psfc,
          NULL,
          NULL,
          &type_psfc,
          2);
  p0 = (void*)NclGetArgValue(
          1,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_p0,
          2);
  hya = (void*)NclGetArgValue(
          2,
          4,
          NULL,
          dsizes_hya,
          NULL,
          NULL,
          &type_hya,
          2);
  hyb = (void*)NclGetArgValue(
          3,
          4,
          NULL,
          dsizes_hyb,
          NULL,
          NULL,
          &type_hyb,
          2);
/*
 * Check dimensions.
 */
  klvl = dsizes_hya[0];
  klvl1 = klvl - 1;
  if( dsizes_hyb[0] != klvl) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dpres_hybrid_ccm: The 'hyb' array must be the same length as 'hya'");
    return(NhlFATAL);
  }
  if(ndims_psfc < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dpres_hybrid_ccm: The 'psfc' array must be at least two-dimensional");
    return(NhlFATAL);
  }
  nlat = dsizes_psfc[ndims_psfc-2];
  nlon = dsizes_psfc[ndims_psfc-1];
  klvl1nlatnlon = klvl1 * nlat * nlon;
/*
 * Determine type of output.
 */
  if(type_psfc == NCL_double) {
    type_phy = NCL_double;
  }
  else {
    type_phy = NCL_float;
  }
/*
 * Calculate total size of output array.
 */
  ndims_phy = ndims_psfc + 1;

  dsizes_phy = (int*)calloc(ndims_phy,sizeof(int));  
  if( dsizes_phy == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dpres_hybrid_ccm: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }

/*
 * Calculate dimension sizes of phy.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_psfc-2; i++ ) {
    size_leftmost *= dsizes_psfc[i];
    dsizes_phy[i] = dsizes_psfc[i];
  }
  size_phy = size_leftmost * klvl1nlatnlon;
  dsizes_phy[ndims_psfc-2] = klvl1;
  dsizes_phy[ndims_psfc-1] = nlat;
  dsizes_phy[ndims_psfc]   = nlon;
/*
 * Coerce data to double if necessary.
 */
  tmp_p0  = coerce_input_double(p0,type_p0,1,0,NULL,NULL);
  tmp_hya = coerce_input_double(hya,type_hya,klvl,0,NULL,NULL);
  tmp_hyb = coerce_input_double(hyb,type_hyb,klvl,0,NULL,NULL);
  if( tmp_hya == NULL || tmp_hyb == NULL || tmp_p0 == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dpres_hybrid_ccm: Unable to coerce input to double precision");
    return(NhlFATAL);
  }

/*
 * Coerce psfc.
 */
  if(type_psfc != NCL_double) {
    tmp_psfc = (double*)calloc(nlat*nlon,sizeof(double));
    if( tmp_psfc == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dpres_hybrid_ccm: Unable to allocate memory for coercing psfc array to double precision");
      return(NhlFATAL);
    }
  }
/*
 * Allocate space for output array.
 */
  if(type_phy == NCL_float) {
    phy     = (void*)calloc(size_phy,sizeof(float));
    tmp_phy = (double*)calloc(klvl1nlatnlon,sizeof(double));
    if(tmp_phy == NULL || phy == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dpres_hybrid_ccm: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    phy = (void*)calloc(size_phy,sizeof(double));
    if(phy == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dpres_hybrid_ccm: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }

/*
 * Call function.
 */
  index_phy = 0;
  for( i = 0; i < size_leftmost; i++ ) {
    if(type_psfc != NCL_double) {
/*
 * Coerce subsection of psfc (tmp_psfc) to double.
 */
      coerce_subset_input_double(psfc,tmp_psfc,i,type_psfc,nlat*nlon,0,
                                 NULL,NULL);
    }
    else {
/*
 * Point tmp_psfc to appropriate location in psfc.
 */
      tmp_psfc = &((double*)psfc)[i];
    }

    if(type_phy == NCL_double) tmp_phy = &((double*)phy)[index_phy];

    NGCALLF(ddphybrid,DDPHYBRID)(tmp_p0,tmp_hya,tmp_hyb,tmp_psfc,&nlon,&nlat,
                                 &klvl,tmp_phy);
/*
 * Copy output values from temporary tmp_phy to phy.
 */
    if(type_phy != NCL_double) {
      for( j = 0; j < klvl1nlatnlon; j++ ) {
        ((float*)phy)[index_phy+j] = (float)tmp_phy[j];
      }
    }
    index_phy += klvl1nlatnlon;
  }
/*
 * Free memory.
 */
  if(type_psfc != NCL_double) NclFree(tmp_psfc);
  if(type_p0   != NCL_double) NclFree(tmp_p0);
  if(type_hya  != NCL_double) NclFree(tmp_hya);
  if(type_hyb  != NCL_double) NclFree(tmp_hyb);
  if(type_phy  != NCL_double) NclFree(tmp_phy);
/*
 * Return.
 */
  return(NclReturnValue(phy,ndims_phy,dsizes_phy,NULL,type_phy,0));
}

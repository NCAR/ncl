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

extern void NGCALLF(dh2sdrv,DH2SDRV)(double*,double*,double*,double*,
                                     double*,double*,double*,int*,double*,
                                     int*,int*);

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
      coerce_output_float_only(phy,tmp_phy,klvl,index_phy);
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
      coerce_output_float_only(phy,tmp_phy,klvl1,index_phy);
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
  int i, j, index_psfc, index_phy, nlat, nlon, klvl, nlatnlon, klvlnlatnlon;
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
  nlatnlon     = nlat * nlon;
  klvlnlatnlon = klvl * nlatnlon;
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
    tmp_psfc = (double*)calloc(nlatnlon,sizeof(double));
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
  index_psfc = index_phy = 0;
  for( i = 0; i < size_leftmost; i++ ) {
    if(type_psfc != NCL_double) {
/*
 * Coerce subsection of psfc (tmp_psfc) to double.
 */
      coerce_subset_input_double(psfc,tmp_psfc,index_psfc,type_psfc,
                                 nlatnlon,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_psfc to appropriate location in psfc.
 */
      tmp_psfc = &((double*)psfc)[index_psfc];
    }

    if(type_phy == NCL_double) tmp_phy = &((double*)phy)[index_phy];

    NGCALLF(dphybrid,DPHYBRID)(tmp_p0,tmp_hya,tmp_hyb,tmp_psfc,&nlon,&nlat,
                               &klvl,tmp_phy);
/*
 * Copy output values from temporary tmp_phy to phy.
 */
    if(type_phy != NCL_double) {
      coerce_output_float_only(phy,tmp_phy,klvlnlatnlon,index_phy);
    }
    index_psfc += nlatnlon;
    index_phy  += klvlnlatnlon;
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
  int i, j, nlat, nlon, klvl, klvl1, nlatnlon, klvl1nlatnlon;
  int index_psfc, index_phy, size_leftmost, size_phy;
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
  nlatnlon      = nlat * nlon;
  klvl1nlatnlon = klvl1 * nlatnlon;
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
    tmp_psfc = (double*)calloc(nlatnlon,sizeof(double));
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
  index_psfc = index_phy = 0;
  for( i = 0; i < size_leftmost; i++ ) {
    if(type_psfc != NCL_double) {
/*
 * Coerce subsection of psfc (tmp_psfc) to double.
 */
      coerce_subset_input_double(psfc,tmp_psfc,index_psfc,type_psfc,
                                 nlatnlon,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_psfc to appropriate location in psfc.
 */
      tmp_psfc = &((double*)psfc)[index_psfc];
    }

    if(type_phy == NCL_double) tmp_phy = &((double*)phy)[index_phy];

    NGCALLF(ddphybrid,DDPHYBRID)(tmp_p0,tmp_hya,tmp_hyb,tmp_psfc,&nlon,&nlat,
                                 &klvl,tmp_phy);
/*
 * Copy output values from temporary tmp_phy to phy.
 */
    if(type_phy != NCL_double) {
      coerce_output_float_only(phy,tmp_phy,klvl1nlatnlon,index_phy);
    }
    index_phy  += klvl1nlatnlon;
    index_psfc += nlatnlon;
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

NhlErrorTypes sigma2hybrid_W( void )
{
/*
 * Input variables
 */
  void *x, *sigma, *hya, *hyb, *p0, *psfc;
  int *intyp;
  double *tmp_x, *tmp_sigma, *tmp_hya, *tmp_hyb, *tmp_p0, *tmp_psfc;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS];
  int dsizes_sigma[1], dsizes_hya[1], dsizes_hyb[1];
  int ndims_psfc, dsizes_psfc[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_x, type_sigma, type_hya, type_hyb;
  NclBasicDataTypes type_p0, type_psfc;
/*
 * Output variables
 */
  void *xhybrid;
  double *tmp_xhybrid;
  int *dsizes_xhybrid;
  NclBasicDataTypes type_xhybrid;
/*
 * Various.
 */
  double *tmp_sigo;
  int i, scalar_psfc, nlvi, nlvo;
  int index_x, index_xhybrid, size_leftmost, size_xhybrid;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  x = (void*)NclGetArgValue(
          0,
          7,
          &ndims_x, 
          dsizes_x,
          NULL,
          NULL,
          &type_x,
          2);
  sigma = (void*)NclGetArgValue(
          1,
          7,
          NULL, 
          dsizes_sigma,
          NULL,
          NULL,
          &type_sigma,
          2);
  hya = (void*)NclGetArgValue(
          2,
          7,
          NULL,
          dsizes_hya,
          NULL,
          NULL,
          &type_hya,
          2);
  hyb = (void*)NclGetArgValue(
          3,
          7,
          NULL,
          dsizes_hyb,
          NULL,
          NULL,
          &type_hyb,
          2);
  p0 = (void*)NclGetArgValue(
          4,
          7,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_p0,
          2);
  psfc = (void*)NclGetArgValue(
          5,
          7,
          &ndims_psfc, 
          dsizes_psfc,
          NULL,
          NULL,
          &type_psfc,
          2);
  intyp = (int*)NclGetArgValue(
          6,
          7,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);
/*
 * Check dimensions. hya and hyb must be the same size, and the
 * rightmost dimension of x must be the same as the length of sigma.
 */
  nlvi = dsizes_x[ndims_x-1];
  nlvo = dsizes_hya[0];

  if( dsizes_sigma[0] != nlvi ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"sigma2hybrid: The rightmost dimension of 'x' must be the same length as 'sigma'");
    return(NhlFATAL);
  }

  if( dsizes_hyb[0] != nlvo ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"sigma2hybrid: 'hya' and 'hyb' must be the same length");
    return(NhlFATAL);
  }
/*
 * psfc must be the same as the leftmost N-1 dimensions of X. If x is
 * a 1D array, then psfc must be a scalar.
 */
  scalar_psfc = is_scalar(ndims_psfc,dsizes_psfc);
  if( (ndims_x == 1 && !scalar_psfc) ||
      (ndims_x  > 1 && ndims_psfc != (ndims_x-1)) ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"sigma2hybrid: If 'x' is one-dimensional, then 'psfc' must be a scalar; otherwise, the dimensions of 'psfc' must be equal to all but the rightmost dimension of 'x'");
    return(NhlFATAL);
  }
  if(!scalar_psfc) {
    for(i = 0; i <= ndims_psfc-1; i++) {
      if(dsizes_psfc[i] != dsizes_x[i]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"sigma2hybrid: If 'x' has more than one dimension, then the dimensions of 'psfc' must be equal to all but the rightmost dimension of 'x'");
        return(NhlFATAL);
      }
    }
  }
/*
 * Determine type of output.
 */
  if(type_x == NCL_double) {
    type_xhybrid = NCL_double;
  }
  else {
    type_xhybrid = NCL_float;
  }
/*
 * Calculate size of output array.
 *
 * xhybrid will be dimensioned N x nlvo, where N represents all but the
 * last dimension of x. 
 */
  dsizes_xhybrid = (int*)calloc(ndims_x,sizeof(int));  
  if( dsizes_xhybrid == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"sigma2hybrid: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }

  size_leftmost = 1;
  for( i = 0; i < ndims_x-1; i++ ) {
    size_leftmost *= dsizes_x[i];
    dsizes_xhybrid[i] = dsizes_x[i];
  }
  size_xhybrid = size_leftmost * nlvo;
  dsizes_xhybrid[ndims_x-1] = nlvo;
/*
 * Coerce data to double if necessary.
 */
  tmp_sigma= coerce_input_double(sigma,type_sigma,nlvi,0,NULL,NULL);
  tmp_hya  = coerce_input_double(hya,type_hya,nlvo,0,NULL,NULL);
  tmp_hyb  = coerce_input_double(hyb,type_hyb,nlvo,0,NULL,NULL);
  tmp_p0   = coerce_input_double(p0,type_p0,1,0,NULL,NULL);
  tmp_psfc = coerce_input_double(psfc,type_psfc,1,0,NULL,NULL);
  if( tmp_hya == NULL || tmp_hyb == NULL || tmp_p0 == NULL ||
      tmp_psfc == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"sigma2hybrid: Unable to coerce input to double precision");
    return(NhlFATAL);
  }

/*
 * Allocate space for output array.
 */
  if(type_xhybrid == NCL_float) {
    xhybrid     = (void*)calloc(size_xhybrid,sizeof(float));
    tmp_x       = (double*)calloc(nlvi,sizeof(double));
    tmp_sigo    = (double*)calloc(nlvo,sizeof(double));
    tmp_xhybrid = (double*)calloc(nlvo,sizeof(double));
    if(tmp_x == NULL || tmp_xhybrid == NULL || xhybrid == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"sigma2hybrid: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    tmp_sigo = (double*)calloc(nlvo,sizeof(double));
    xhybrid  = (void*)calloc(size_xhybrid,sizeof(double));
    if(tmp_sigo == NULL || xhybrid == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"sigma2hybrid: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }

/*
 * Loop through leftmost dimensions and call Fortran function.
 */
  index_x = index_xhybrid = 0;
  for( i = 0; i < size_leftmost; i++ ) {
    if(!scalar_psfc) {
      if(type_psfc != NCL_double) {
/*
 * Coerce subsection of psfc (tmp_psfc) to double.
 */
        coerce_subset_input_double(psfc,tmp_psfc,i,type_psfc,
                                   1,0,NULL,NULL);
      }
      else {
/*
 * Point tmp_psfc to appropriate location in psfc.
 */
        tmp_psfc = &((double*)psfc)[i];
      }
    }
    if(type_x != NCL_double) {
/*
 * Coerce subsection of x (tmp_x) to double.
 */
      coerce_subset_input_double(x,tmp_x,index_x,type_x,nlvi,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_x to appropriate location in x.
 */
      tmp_x = &((double*)x)[index_x];
    }
/*
 * Point temporary output array to appropriate location in xhybrid.
 */
    if(type_xhybrid == NCL_double) {
      tmp_xhybrid = &((double*)xhybrid)[index_xhybrid];
    }

    NGCALLF(dh2sdrv,DH2SDRV)(tmp_x,tmp_xhybrid,tmp_hya,tmp_hyb,tmp_p0,
                             tmp_sigma,tmp_sigo,intyp,tmp_psfc,&nlvi,&nlvo);
/*
 * Copy output values from temporary tmp_xhybrid to xhybrid.
 */
    if(type_xhybrid != NCL_double) {
      coerce_output_float_only(xhybrid,tmp_xhybrid,nlvo,index_xhybrid);
    }
    index_xhybrid += nlvo;
    index_x       += nlvi;
  }
/*
 * Free memory.
 */
  if(type_x       != NCL_double) NclFree(tmp_x);
  if(type_sigma   != NCL_double) NclFree(tmp_sigma);
  if(type_hya     != NCL_double) NclFree(tmp_hya);
  if(type_hyb     != NCL_double) NclFree(tmp_hyb);
  if(type_p0      != NCL_double) NclFree(tmp_p0);
  if(type_psfc    != NCL_double) NclFree(tmp_psfc);
  if(type_xhybrid != NCL_double) NclFree(tmp_xhybrid);
/*
 * Return.
 */
  return(NclReturnValue(xhybrid,ndims_x,dsizes_xhybrid,NULL,
                        type_xhybrid,0));
}




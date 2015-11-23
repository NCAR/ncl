#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(preshybrid,PRESHYBRID)(double*,double*,double*,double*,
                                           int*,double*);

extern void NGCALLF(dpreshybrid,DPRESPHYBRID)(double*,double*,double*,double*,
                                              int*,double*);

extern void NGCALLF(dphybrid,DPHYBRID)(double*,double*,double*,double*,
                                       int*,int*,int*,double*,double*);

extern void NGCALLF(ddphybrid,DDPHYBRID)(double*,double*,double*,double*,
                                         int*,int*,int*,double*,double*);
					
extern void NGCALLF(dphybridjra55,DPHYBRIDJRA55)(double *,double *,double *,int *,
						 int *,int *,double *,double *,int *,
						 double *);

extern void NGCALLF(dh2sdrv,DH2SDRV)(double*,double*,double*,double*,
                                     double*,double*,double*,int*,double*,
                                     int*,int*);

extern void NGCALLF(p2hyo,P2HYO)(double*,int*,int*,int*,double*,double*,
                                 double*,double*,double*,int*,double*,
                                 double*,int*,int*,int*);

extern void NGCALLF(dpresplvl,DPRESPLVL)(int *,double *,int *,int *,int *,
                                         double *,double *,double *,double *,
                                         int *,int *,int *);

NhlErrorTypes pres_hybrid_W( void )
{
/*
 * Input variables
 */
  void *psfc, *p0, *hya, *hyb;
  double *tmp_psfc = NULL;
  double *tmp_p0, *tmp_hya, *tmp_hyb;
  int ndims_psfc;
  ng_size_t dsizes_psfc[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_hya[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_hyb[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_p0, type_psfc, type_hya, type_hyb;
/*
 * Output variables
 */
  void *phy;
  double *tmp_phy = NULL;
  int ndims_phy;
  ng_size_t *dsizes_phy;
  NclBasicDataTypes type_phy;
/*
 * Various.
 */
  ng_size_t i, klvl, size_leftmost, size_phy, index_phy;
  int ret, iklvl;
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
          DONT_CARE);
  p0 = (void*)NclGetArgValue(
          1,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_p0,
          DONT_CARE);
  hya = (void*)NclGetArgValue(
          2,
          4,
          NULL,
          dsizes_hya,
          NULL,
          NULL,
          &type_hya,
          DONT_CARE);
  hyb = (void*)NclGetArgValue(
          3,
          4,
          NULL,
          dsizes_hyb,
          NULL,
          NULL,
          &type_hyb,
          DONT_CARE);
/*
 * Check dimensions.
 */
  klvl = dsizes_hya[0];
  if( dsizes_hyb[0] != klvl) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pres_hybrid: The 'hyb' array must be the same length as 'hya'");
    return(NhlFATAL);
  }
/*
 * Test input dimension sizes.
 */
  if(klvl > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pres_hybrid: klvl = %ld is greater than INT_MAX", klvl);
    return(NhlFATAL);
  }
  iklvl = (int) klvl;

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

  dsizes_phy = (ng_size_t*)calloc(ndims_phy,sizeof(ng_size_t));  
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


    NGCALLF(preshybrid,PRESHYBRID)(tmp_p0,tmp_psfc,tmp_hya,tmp_hyb,&iklvl,
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
  ret = NclReturnValue(phy,ndims_phy,dsizes_phy,NULL,type_phy,0);
  NclFree(dsizes_phy);
  return(ret);
}



NhlErrorTypes dpres_hybrid_W( void )
{
/*
 * Input variables
 */
  void *psfc, *p0, *hya, *hyb;
  double *tmp_psfc = NULL;
  double *tmp_p0, *tmp_hya, *tmp_hyb;
  int ndims_psfc;
  ng_size_t dsizes_psfc[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_hya[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_hyb[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_p0, type_psfc, type_hya, type_hyb;
/*
 * Output variables
 */
  void *phy;
  double *tmp_phy = NULL;
  int ndims_phy;
  ng_size_t *dsizes_phy;
  NclBasicDataTypes type_phy;
/*
 * Various.
 */
  ng_size_t i, klvl, klvl1, size_leftmost, size_phy, index_phy;
  int ret, iklvl;
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
          DONT_CARE);
  p0 = (void*)NclGetArgValue(
          1,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_p0,
          DONT_CARE);
  hya = (void*)NclGetArgValue(
          2,
          4,
          NULL,
          dsizes_hya,
          NULL,
          NULL,
          &type_hya,
          DONT_CARE);
  hyb = (void*)NclGetArgValue(
          3,
          4,
          NULL,
          dsizes_hyb,
          NULL,
          NULL,
          &type_hyb,
          DONT_CARE);
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
 * Test input dimension sizes.
 */
  if(klvl > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dpres_hybrid: klvl = %ld is greater than INT_MAX", klvl);
    return(NhlFATAL);
  }
  iklvl = (int) klvl;

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

  dsizes_phy = (ng_size_t*)calloc(ndims_phy,sizeof(ng_size_t));  
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

    NGCALLF(dpreshybrid,DPRESHYBRID)(tmp_p0,tmp_psfc,tmp_hya,tmp_hyb,&iklvl,
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
  ret = NclReturnValue(phy,ndims_phy,dsizes_phy,NULL,type_phy,0);
  NclFree(dsizes_phy);
  return(ret);
}

NhlErrorTypes pres_hybrid_ccm_W( void )
{
/*
 * Input variables
 */
  void *psfc, *p0, *hya, *hyb;
  double *tmp_psfc = NULL;
  double *tmp_p0, *tmp_hya, *tmp_hyb;
  int ndims_psfc;
  ng_size_t dsizes_psfc[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_hya[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_hyb[NCL_MAX_DIMENSIONS];
  int has_missing_psfc;
  NclBasicDataTypes type_p0, type_psfc, type_hya, type_hyb;
  NclScalar missing_psfc, missing_dpsfc, missing_rpsfc;
/*
 * Output variables
 */
  void *phy;
  double *tmp_phy = NULL;
  int ndims_phy;
  ng_size_t *dsizes_phy;
  NclBasicDataTypes type_phy;
/*
 * Various.
 */
  ng_size_t i, nlat, nlon, klvl, nlatnlon, klvlnlatnlon;
  ng_size_t size_leftmost, size_phy, index_psfc, index_phy;
  int ret, inlon, inlat, iklvl;
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
          &missing_psfc,
          &has_missing_psfc,
          &type_psfc,
          DONT_CARE);
  p0 = (void*)NclGetArgValue(
          1,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_p0,
          DONT_CARE);
  hya = (void*)NclGetArgValue(
          2,
          4,
          NULL,
          dsizes_hya,
          NULL,
          NULL,
          &type_hya,
          DONT_CARE);
  hyb = (void*)NclGetArgValue(
          3,
          4,
          NULL,
          dsizes_hyb,
          NULL,
          NULL,
          &type_hyb,
          DONT_CARE);
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
 * Test input dimension sizes.
 */
  if((nlon > INT_MAX) || (nlat > INT_MAX) || (klvl > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pres_hybrid_ccm: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  iklvl = (int) klvl;

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
 * Get double precision missing value, if any.
 */
  coerce_missing(type_psfc,has_missing_psfc,&missing_psfc,
                 &missing_dpsfc,&missing_rpsfc);
/*
 * Calculate total size of output array.
 */
  ndims_phy = ndims_psfc + 1;

  dsizes_phy = (ng_size_t*)calloc(ndims_phy,sizeof(ng_size_t));  
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

    NGCALLF(dphybrid,DPHYBRID)(tmp_p0,tmp_hya,tmp_hyb,tmp_psfc,&inlon,&inlat,
                               &iklvl,tmp_phy,&missing_dpsfc.doubleval);

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
  if(has_missing_psfc) {
    if(type_phy != NCL_double) {
      ret = NclReturnValue(phy,ndims_phy,dsizes_phy,&missing_rpsfc,type_phy,
                           0);
    }
    else {
      ret = NclReturnValue(phy,ndims_phy,dsizes_phy,&missing_dpsfc,type_phy,
                           0);
    }
  }
  else {
    ret = NclReturnValue(phy,ndims_phy,dsizes_phy,NULL,type_phy,0);
  }
  NclFree(dsizes_phy);
  return(ret);
}



NhlErrorTypes pres_hybrid_jra55_W( void )
{
/*
 * Input variables
 */
  void *psfc, *hya, *hyb;
  double *tmp_psfc = NULL;
  double *tmp_hya, *tmp_hyb;
  int ndims_psfc;
  ng_size_t dsizes_psfc[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_hya[1], dsizes_hyb[1];
  int has_missing_psfc;
  NclBasicDataTypes type_psfc, type_hya, type_hyb;
  NclScalar missing_psfc, missing_dpsfc, missing_rpsfc;
/*
 * Output variables
 */
  void *phy;
  double *tmp_phy = NULL;
  int ndims_phy;
  ng_size_t *dsizes_phy;
  NclBasicDataTypes type_phy;
  NclScalar missing_phy;
/*
 * Various.
 */
  double *tmp_pi;
  ng_size_t i, nlat, nlon, nlatnlon, iklvlnlatnlon, oklvlnlatnlon;
  ng_size_t size_leftmost, size_phy, index_psfc, index_phy;
  int ret, inlon, inlat, iklvl, oklvl;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  psfc = (void*)NclGetArgValue(
          0,
          3,
          &ndims_psfc, 
          dsizes_psfc,
          &missing_psfc,
          &has_missing_psfc,
          &type_psfc,
          DONT_CARE);

  hya = (void*)NclGetArgValue(
          1,
          3,
          NULL,
          dsizes_hya,
          NULL,
          NULL,
          &type_hya,
          DONT_CARE);

  hyb = (void*)NclGetArgValue(
          2,
          3,
          NULL,
          dsizes_hyb,
          NULL,
          NULL,
          &type_hyb,
          DONT_CARE);
/*
 * Check dimensions.
 */
  if( dsizes_hya[0] != dsizes_hyb[0]) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pres_hybrid_jra55: The 'hya/hyb' arrays must be the same length");
    return(NhlFATAL);
  }
  iklvl = dsizes_hya[0];
  oklvl = iklvl-1;

  if(ndims_psfc < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pres_hybrid_jra55: The 'psfc' array must be at least two-dimensional");
    return(NhlFATAL);
  }
  nlat = dsizes_psfc[ndims_psfc-2];
  nlon = dsizes_psfc[ndims_psfc-1];
  nlatnlon      = nlat * nlon;
  iklvlnlatnlon = iklvl * nlatnlon;
  oklvlnlatnlon = oklvl * nlatnlon;

/*
 * Test input dimension sizes.
 */
  if((nlon > INT_MAX) || (nlat > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pres_hybrid_jra55: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;

/*
 * Get double precision missing value, if any.
 */
  coerce_missing(type_psfc,has_missing_psfc,&missing_psfc,
                 &missing_dpsfc,&missing_rpsfc);
/*
 * Determine type of output.
 */
  if(type_psfc == NCL_double) type_phy = NCL_double;
  else                        type_phy = NCL_float;

/*
 * Calculate total size of output array.
 */
  ndims_phy = ndims_psfc + 1;

  dsizes_phy = (ng_size_t*)calloc(ndims_phy,sizeof(ng_size_t));  
  if( dsizes_phy == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pres_hybrid_jra55: Unable to allocate memory for holding dimension sizes");
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
  size_phy = size_leftmost * oklvlnlatnlon;
  dsizes_phy[ndims_psfc-2] = oklvl;
  dsizes_phy[ndims_psfc-1] = nlat;
  dsizes_phy[ndims_psfc]   = nlon;
/*
 * Coerce data to double if necessary.
 */
  tmp_hya = coerce_input_double(hya,type_hya,iklvl,0,NULL,NULL);
  tmp_hyb = coerce_input_double(hyb,type_hyb,iklvl,0,NULL,NULL);
  if( tmp_hya == NULL || tmp_hyb == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pres_hybrid_jra55: Unable to coerce hya/hyb to double precision");
    return(NhlFATAL);
  }

/*
 * Coerce psfc.
 */
  if(type_psfc != NCL_double) {
    tmp_psfc = (double*)calloc(nlatnlon,sizeof(double));
    if( tmp_psfc == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pres_hybrid_jra55: Unable to allocate memory for coercing psfc array to double precision");
      return(NhlFATAL);
    }
  }

/*
 * Allocate space for output array.
 */
  if(type_phy == NCL_float) {
    phy     = (void*)calloc(size_phy,sizeof(float));
    tmp_phy = (double*)calloc(oklvlnlatnlon,sizeof(double));
    if(tmp_phy == NULL || phy == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pres_hybrid_jra55: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    missing_phy = missing_rpsfc;
  }
  else {
    phy = (void*)calloc(size_phy,sizeof(double));
    if(phy == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pres_hybrid_jra55: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    missing_phy = missing_dpsfc;
  }

/*
 * Allocate space for work array.
 */
  tmp_pi = (double*)calloc(iklvlnlatnlon,sizeof(double));
  if(tmp_pi == NULL) {
  NhlPError(NhlFATAL,NhlEUNKNOWN,"pres_hybrid_jra55: Unable to allocate memory for work array");
  return(NhlFATAL);
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

    NGCALLF(dphybridjra55,DPHYBRIDJRA55)(tmp_hya,tmp_hyb,tmp_psfc,&inlon,&inlat,
					 &oklvl,tmp_phy,&missing_dpsfc.doubleval,
					 &iklvl,tmp_pi);
/*
 * Copy output values from temporary tmp_phy to phy.
 */
    if(type_phy != NCL_double) {
      coerce_output_float_only(phy,tmp_phy,oklvlnlatnlon,index_phy);
    }
    index_psfc += nlatnlon;
    index_phy  += oklvlnlatnlon;
  }
/*
 * Free memory.
 */
  NclFree(tmp_pi);
  if(type_psfc != NCL_double) NclFree(tmp_psfc);
  if(type_hya  != NCL_double) NclFree(tmp_hya);
  if(type_hyb  != NCL_double) NclFree(tmp_hyb);
  if(type_phy  != NCL_double) NclFree(tmp_phy);
/*
 * Return.
 */
  if(has_missing_psfc) {
    ret = NclReturnValue(phy,ndims_phy,dsizes_phy,&missing_phy,type_phy,0);
  }
  else {
    ret = NclReturnValue(phy,ndims_phy,dsizes_phy,NULL,type_phy,0);
  }
  NclFree(dsizes_phy);
  return(ret);
}


NhlErrorTypes dpres_hybrid_ccm_W( void )
{
/*
 * Input variables
 */
  void *psfc, *p0, *hya, *hyb;
  double *tmp_psfc = NULL;
  double *tmp_p0, *tmp_hya, *tmp_hyb;
  int ndims_psfc;
  ng_size_t dsizes_psfc[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_hya[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_hyb[NCL_MAX_DIMENSIONS];
  int has_missing_psfc;
  NclBasicDataTypes type_p0, type_psfc, type_hya, type_hyb;
  NclScalar missing_psfc, missing_dpsfc, missing_rpsfc;
/*
 * Output variables
 */
  void *phy;
  double *tmp_phy = NULL;
  int ndims_phy;
  ng_size_t *dsizes_phy;
  NclBasicDataTypes type_phy;
/*
 * Various.
 */
  ng_size_t i, nlat, nlon, klvl, klvl1, nlatnlon, klvl1nlatnlon;
  ng_size_t size_leftmost, size_phy, index_psfc, index_phy;
  int ret, iklvl, inlat, inlon;
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
          &missing_psfc,
          &has_missing_psfc,
          &type_psfc,
          DONT_CARE);
  p0 = (void*)NclGetArgValue(
          1,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_p0,
          DONT_CARE);
  hya = (void*)NclGetArgValue(
          2,
          4,
          NULL,
          dsizes_hya,
          NULL,
          NULL,
          &type_hya,
          DONT_CARE);
  hyb = (void*)NclGetArgValue(
          3,
          4,
          NULL,
          dsizes_hyb,
          NULL,
          NULL,
          &type_hyb,
          DONT_CARE);
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
 * Test input dimension sizes.
 */
  if((nlon > INT_MAX) || (nlat > INT_MAX) || (klvl > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dpres_hybrid_ccm: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  iklvl = (int) klvl;

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

  dsizes_phy = (ng_size_t*)calloc(ndims_phy,sizeof(ng_size_t));  
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
 * Get double precision missing value, if any.
 */
  coerce_missing(type_psfc,has_missing_psfc,&missing_psfc,
                 &missing_dpsfc,&missing_rpsfc);
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

    NGCALLF(ddphybrid,DDPHYBRID)(tmp_p0,tmp_hya,tmp_hyb,tmp_psfc,&inlon,&inlat,
                                 &iklvl,tmp_phy,&missing_dpsfc.doubleval);

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
  if(has_missing_psfc) {
    if(type_phy != NCL_double) {
      ret = NclReturnValue(phy,ndims_phy,dsizes_phy,&missing_rpsfc,type_phy,
                           0);
    }
    else {
      ret = NclReturnValue(phy,ndims_phy,dsizes_phy,&missing_dpsfc,type_phy,
                           0);
    }
  }
  else {
    ret = NclReturnValue(phy,ndims_phy,dsizes_phy,NULL,type_phy,0);
  }
  NclFree(dsizes_phy);
  return(ret);
}

NhlErrorTypes dpres_plevel_W( void )
{
/*
 * Input variables
 */
  void *plev, *psfc, *ptop;
  int *iopt;
  double *tmp_plev, *tmp_psfc, *tmp_ptop;
  int ndims_psfc;
  ng_size_t dsizes_psfc[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_plev[1];
  int has_missing_psfc, is_scalar_psfc;
  NclScalar missing_psfc, missing_dpsfc;
  NclBasicDataTypes type_plev, type_psfc, type_ptop;
/*
 * Output variables
 */
  void *dp;
  double *tmp_dp;
  int ndims_dp;
  ng_size_t *dsizes_dp;
  NclBasicDataTypes type_dp;
  NclScalar missing_dp;
/*
 * Various.
 */
  ng_size_t ntim, nlat, nlon, klvl;
  ng_size_t nlatnlon, klvlnlatnlon, ntimnlatnlon, ntimklvlnlatnlon;
  int intim, inlat, inlon, iklvl, kflag, ier, ret;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  plev = (void*)NclGetArgValue(
          0,
          4,
          NULL,
          dsizes_plev,
          NULL,
          NULL,
          &type_plev,
          DONT_CARE);

  psfc = (void*)NclGetArgValue(
          1,
          4,
          &ndims_psfc, 
          dsizes_psfc,
          &missing_psfc,
          &has_missing_psfc,
          &type_psfc,
          DONT_CARE);
    
  ptop = (void*)NclGetArgValue(
          2,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_ptop,
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
 * Check dimension sizes for psfc.
 */
  is_scalar_psfc = is_scalar(ndims_psfc,dsizes_psfc);

  if(ndims_psfc > 3 || (!is_scalar_psfc && ndims_psfc == 1)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dpres_plevel: The 'psfc' array must be a scalar or be a 2 or 3 dimensional array with rightmost dimensions lat x lon");
    return(NhlFATAL);
  }

/*
 * Get dimension sizes.
 */
  klvl = dsizes_plev[0];
  if(ndims_psfc == 1) {
    ntim = nlat = nlon = 1;
  }
  else if(ndims_psfc == 2) {
    ntim = 1;
    nlat = dsizes_psfc[ndims_psfc-2];
    nlon = dsizes_psfc[ndims_psfc-1];
  }
  else {
    ntim = dsizes_psfc[ndims_psfc-3];
    nlat = dsizes_psfc[ndims_psfc-2];
    nlon = dsizes_psfc[ndims_psfc-1];
  }

/*
 * Test input dimension sizes.
 */
  if((klvl > INT_MAX) || (ntim > INT_MAX) || (nlat > INT_MAX) ||
     (nlon > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dpres_plevel: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  iklvl = (int) klvl;
  intim = (int) ntim;
  inlat = (int) nlat;
  inlon = (int) nlon;

  nlatnlon         = nlat * nlon;
  ntimnlatnlon     = ntim * nlatnlon;
  klvlnlatnlon     = klvl * nlatnlon;
  ntimklvlnlatnlon = ntim * klvlnlatnlon;

/*
 * Determine type of output. It depends on the type of psfc only.
 */
  if(type_psfc == NCL_double) {
    type_dp = NCL_double;
  }
  else {
    type_dp = NCL_float;
  }

/*
 * Determine dimension size of output array.
 */
  if(is_scalar_psfc) {
    ndims_dp = 1;      /* will be length klvl */
  }
  else {
    ndims_dp = ndims_psfc + 1;
  }

  dsizes_dp = (ng_size_t*)calloc(ndims_dp,sizeof(ng_size_t));  
  if( dsizes_dp == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dpres_plevel: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }

  if(ndims_psfc == 1) {
    dsizes_dp[0] = klvl;
  }
  else if(ndims_psfc == 2) {
    dsizes_dp[0] = klvl;
    dsizes_dp[1] = nlat;
    dsizes_dp[2] = nlon;
  }
  else {
    dsizes_dp[0] = ntim;
    dsizes_dp[1] = klvl;
    dsizes_dp[2] = nlat;
    dsizes_dp[3] = nlon;
  }

/*
 * Set the input and output missing values.
 */
  if(has_missing_psfc) {
    coerce_missing(type_psfc,has_missing_psfc,&missing_psfc,
                   &missing_dpsfc,NULL);
  }
  else {
    if(type_dp == NCL_float) {
/*
 * These were set to 1e20 before default missing values were 
 * changed in V6.0.0.
 */
      missing_dpsfc.doubleval = (double)((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
    }
    else {
      missing_dpsfc.doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
    }
  }
  if(type_dp == NCL_float) {
    missing_dp.floatval = (float)missing_dpsfc.doubleval;
  }
  else {
    missing_dp.doubleval = missing_dpsfc.doubleval;
  }
/*
 * Coerce data to double if necessary.
 */
  tmp_psfc = coerce_input_double(psfc,type_psfc,ntimnlatnlon,0,NULL,NULL);
  tmp_plev = coerce_input_double(plev,type_plev,klvl,0,NULL,NULL);
  tmp_ptop = coerce_input_double(ptop,type_ptop,1,0,NULL,NULL);
  if( tmp_ptop == NULL || tmp_psfc == NULL || tmp_plev == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dpres_plevel: Unable to coerce input to double precision");
    return(NhlFATAL);
  }

/*
 * Allocate space for output array.
 */
  if(type_dp == NCL_float) {
    dp     = (void*)calloc(ntimklvlnlatnlon,sizeof(float));
    tmp_dp = (double*)calloc(ntimklvlnlatnlon,sizeof(double));
    if(tmp_dp == NULL || dp == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dpres_plevel: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    dp = (void*)calloc(ntimklvlnlatnlon,sizeof(double));
    tmp_dp = &((double*)dp)[0];
    if(dp == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dpres_plevel: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }

  NGCALLF(dpresplvl,DPRESPLVL)(&iklvl,tmp_plev,&intim,&inlat,&inlon,tmp_psfc,
                               &missing_dpsfc.doubleval,tmp_ptop,tmp_dp,
                               iopt,&kflag,&ier);
  if(ier < 0) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"dpres_plevel: At one or more grid points the sum of the layer thicknesses is not equal to (psfc-ptop). Are units of plev, psfc and ptop matching?");
  }
/*
 * Copy output values from temporary tmp_dp to dp.
 */
  if(type_dp != NCL_double) {
    coerce_output_float_only(dp,tmp_dp,ntimklvlnlatnlon,0);
  }

/*
 * Free memory.
 */
  if(type_psfc != NCL_double) NclFree(tmp_psfc);
  if(type_plev != NCL_double) NclFree(tmp_plev);
  if(type_ptop != NCL_double) NclFree(tmp_ptop);
  if(type_dp   != NCL_double) NclFree(tmp_dp);

/*
 * Return. kflag == 1 --> there are missing values in the output.
 */
  if(kflag == 1) {
    ret = NclReturnValue(dp,ndims_dp,dsizes_dp,&missing_dp,type_dp,0);
  }
  else {
    ret = NclReturnValue(dp,ndims_dp,dsizes_dp,NULL,type_dp,0);
  }
  NclFree(dsizes_dp);
  return(ret);
}

NhlErrorTypes sigma2hybrid_W( void )
{
/*
 * Input variables
 */
  void *x, *sigma, *hya, *hyb, *p0, *psfc;
  int *intyp;
  double *tmp_x = NULL;
  double *tmp_sigma, *tmp_hya, *tmp_hyb, *tmp_p0, *tmp_psfc;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_sigma[1];
  ng_size_t dsizes_hya[1];
  ng_size_t dsizes_hyb[1];
  int ndims_psfc;
  ng_size_t dsizes_psfc[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_x, type_sigma, type_hya, type_hyb;
  NclBasicDataTypes type_p0, type_psfc;
/*
 * Output variables
 */
  void *xhybrid;
  double *tmp_xhybrid = NULL;
  ng_size_t *dsizes_xhybrid;
  NclBasicDataTypes type_xhybrid;
/*
 * Various.
 */
  double *tmp_sigo;
  ng_size_t i, nlvi, nlvo, index_x, index_xhybrid;
  ng_size_t size_leftmost, size_xhybrid;
  int inlvi, inlvo, scalar_psfc, ret;
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
          DONT_CARE);
  sigma = (void*)NclGetArgValue(
          1,
          7,
          NULL, 
          dsizes_sigma,
          NULL,
          NULL,
          &type_sigma,
          DONT_CARE);
  hya = (void*)NclGetArgValue(
          2,
          7,
          NULL,
          dsizes_hya,
          NULL,
          NULL,
          &type_hya,
          DONT_CARE);
  hyb = (void*)NclGetArgValue(
          3,
          7,
          NULL,
          dsizes_hyb,
          NULL,
          NULL,
          &type_hyb,
          DONT_CARE);
  p0 = (void*)NclGetArgValue(
          4,
          7,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_p0,
          DONT_CARE);
  psfc = (void*)NclGetArgValue(
          5,
          7,
          &ndims_psfc, 
          dsizes_psfc,
          NULL,
          NULL,
          &type_psfc,
          DONT_CARE);
  intyp = (int*)NclGetArgValue(
          6,
          7,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);
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
 * Test input dimension sizes.
 */
  if((nlvi > INT_MAX) || (nlvo > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"sigma2hybrid: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlvi = (int) nlvi;
  inlvo = (int) nlvo;

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
  dsizes_xhybrid = (ng_size_t*)calloc(ndims_x,sizeof(ng_size_t));  
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
 *
 * psfc might be a scalar and it might be an array.
 * Just allocate one double for it.
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
  if(type_x != NCL_double) {
    tmp_x = (double*)calloc(nlvi,sizeof(double));
    if(tmp_x == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"sigma2hybrid: Unable to allocate memory for coercing x to double");
      return(NhlFATAL);
    }
  }

/*
 * Allocate space for output array.
 */
  if(type_xhybrid == NCL_float) {
    xhybrid     = (void*)calloc(size_xhybrid,sizeof(float));
    tmp_xhybrid = (double*)calloc(nlvo,sizeof(double));
    if(tmp_xhybrid == NULL || xhybrid == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"sigma2hybrid: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    xhybrid  = (void*)calloc(size_xhybrid,sizeof(double));
    if(xhybrid == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"sigma2hybrid: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  tmp_sigo = (double*)calloc(nlvo,sizeof(double));
  if(tmp_sigo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"sigma2hybrid: Unable to allocate memory for sigo array");
    return(NhlFATAL);
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
                             tmp_sigma,tmp_sigo,intyp,tmp_psfc,&inlvi,&inlvo);
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
  NclFree(tmp_sigo);

/*
 * Return.
 */
  ret = NclReturnValue(xhybrid,ndims_x,dsizes_xhybrid,NULL,
                        type_xhybrid,0);
  NclFree(dsizes_xhybrid);
  return(ret);
}


NhlErrorTypes pres2hybrid_W( void )
{
/*
 * Input variables
 */
  void *p, *ps, *p0, *xi, *hyao, *hybo;
  int *kflag;
  double *tmp_p, *tmp_p0, *tmp_hyao, *tmp_hybo;
  double *tmp_ps = NULL;
  double *tmp_xi = NULL;
  int ndims_ps;
  ng_size_t dsizes_ps[NCL_MAX_DIMENSIONS];
  int has_missing_xi, ndims_xi;
  ng_size_t dsizes_xi[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_p[1];
  ng_size_t dsizes_hyao[1];
  ng_size_t dsizes_hybo[1];
  NclBasicDataTypes type_p, type_p0, type_ps, type_xi, type_hyao, type_hybo;
  NclScalar missing_xi, missing_dxi;
/*
 * Output variables
 */
  void *xo;
  double *tmp_xo = NULL;
  ng_size_t *dsizes_xo;
  NclBasicDataTypes type_xo;
  NclScalar missing_xo;
/*
 * Various.
 */
  ng_size_t index_xi, index_xo, index_ps;
  ng_size_t i, size_leftmost, size_xo;
  ng_size_t nlat, nlon, nlevi, nlevo, nlat_nlon, nlat_nlon_nlevi, nlat_nlon_nlevo;
  int iflag, ret, ier, return_missing;
  int inlon, inlat, inlevi, inlevo;

/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  p = (void*)NclGetArgValue(
          0,
          7,
          NULL,
          dsizes_p,
          NULL,
          NULL,
          &type_p,
          DONT_CARE);
  ps = (void*)NclGetArgValue(
          1,
          7,
          &ndims_ps, 
          dsizes_ps,
          NULL,
          NULL,
          &type_ps,
          DONT_CARE);
  p0 = (void*)NclGetArgValue(
          2,
          7,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_p0,
          DONT_CARE);
  xi = (void*)NclGetArgValue(
          3,
          7,
          &ndims_xi, 
          dsizes_xi,
          &missing_xi,
          &has_missing_xi,
          &type_xi,
          DONT_CARE);
  hyao = (void*)NclGetArgValue(
          4,
          7,
          NULL,
          dsizes_hyao,
          NULL,
          NULL,
          &type_hyao,
          DONT_CARE);
  hybo = (void*)NclGetArgValue(
          5,
          7,
          NULL,
          dsizes_hybo,
          NULL,
          NULL,
          &type_hybo,
          DONT_CARE);

  kflag = (int*)NclGetArgValue(
          6,
          7,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);
/*
 * Check # of dimensions.
 */
  if(ndims_ps < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pres2hybrid: 'ps' must be at least two dimensions");
    return(NhlFATAL);
  }
  if(ndims_xi < 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pres2hybrid: 'xi' must be at least three dimensions");
    return(NhlFATAL);
  }
  if(ndims_xi != ndims_ps+1) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pres2hybrid: 'xi' must have one more dimension than 'ps'");
    return(NhlFATAL);
  }
/*
 * Check dimension sizes.
 */
  nlevi  = dsizes_p[0];
  nlevo = dsizes_hyao[0];
  nlat  = dsizes_ps[ndims_ps-2];
  nlon  = dsizes_ps[ndims_ps-1];
  nlat_nlon       = nlat*nlon;
  nlat_nlon_nlevi = nlat_nlon*nlevi;
  nlat_nlon_nlevo = nlat_nlon*nlevo;

/*
 * Test input dimension sizes.
 */
  if((nlon > INT_MAX) || (nlat > INT_MAX) || (nlevi > INT_MAX) || (nlevo > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pres2hybrid: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  inlevi = (int) nlevi;
  inlevo = (int) nlevo;

  if( dsizes_xi[ndims_xi-3] != nlevi || dsizes_xi[ndims_xi-2] != nlat || 
      dsizes_xi[ndims_xi-1] != nlon) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pres2hybrid: The three rightmost dimensions of 'xi' must be nlevi x nlat x nlon");
    return(NhlFATAL);
  }
  if( dsizes_hybo[0] != nlevo ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pres2hybrid: 'hyao' and 'hybo' must be the same length");
    return(NhlFATAL);
  }
  for(i=0; i <= ndims_ps-3; i++) {
    if(dsizes_ps[i] != dsizes_xi[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pres2hybrid: The leftmost n-2 dimensions of 'ps' and n-3 dimensions of 'xi' must be the same");
      return(NhlFATAL);
    }
  }

/*
 * Determine type of output.
 */
  if(type_ps == NCL_double || type_xi == NCL_double) {
    type_xo = NCL_double;
  }
  else {
    type_xo = NCL_float;
  }
/*
 * Get missing value of xi, in case we need to use it for setting
 * output values to missing. Otherwise, set the missing value to
 * the default for float or missing (it was 1.e20 before V6.0.0)
 *
 * This was added in version 4.3.0 of ncl to cover the extrapolation
 * algorithms Dennis added.
 */
  if(has_missing_xi) {
    coerce_missing(type_xi,has_missing_xi,&missing_xi,&missing_dxi,NULL);
  }
  else {
    if(type_xo == NCL_float) {
      missing_dxi.doubleval = (double)((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
    }
    else {
      missing_dxi.doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
    }
  }
  if(type_xo == NCL_float) {
    missing_xo.floatval = (float)missing_dxi.doubleval;
  }
  else {
    missing_xo.doubleval = missing_dxi.doubleval;
  }
/*
 * Calculate total size of output array.
 */
  dsizes_xo = (ng_size_t*)calloc(ndims_xi,sizeof(ng_size_t));  
  if( dsizes_xo == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pres2hybrid: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }

/*
 * Xo will have the same dimensions as xi, except the level dimension
 * is replaced by the dimension of hyao/hybo
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_xi-3; i++ ) {
    size_leftmost *= dsizes_xi[i];
    dsizes_xo[i] = dsizes_xi[i];
  }
  dsizes_xo[ndims_xi-3] = nlevo;
  dsizes_xo[ndims_xi-2] = nlat;
  dsizes_xo[ndims_xi-1] = nlon;
  size_xo = size_leftmost * nlat_nlon_nlevo;

/*
 * Coerce data to double if necessary.
 */
  tmp_p0   = coerce_input_double(p0,type_p0,1,0,NULL,NULL);
  tmp_p    = coerce_input_double(p,type_p,nlevi,0,NULL,NULL);
  tmp_hyao = coerce_input_double(hyao,type_hyao,nlevo,0,NULL,NULL);
  tmp_hybo = coerce_input_double(hybo,type_hybo,nlevo,0,NULL,NULL);
  if( tmp_p0==NULL || tmp_p==NULL || tmp_hyao==NULL || tmp_hybo==NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pres2hybrid: Unable to coerce input to double precision");
    return(NhlFATAL);
  }

/*
 * Create temp array for coercing subselections of ps in do loop later.
 */
  if(type_ps != NCL_double) {
    tmp_ps = (double*)calloc(nlat_nlon,sizeof(double));
    if( tmp_ps == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pres2hybrid: Unable to allocate memory for coercing ps array to double precision");
      return(NhlFATAL);
    }
  }
/*
 * Create temp array for coercing subselections of xi in do loop later.
 */
  if(type_xi != NCL_double) {
    tmp_xi = (double*)calloc(nlat_nlon_nlevi,sizeof(double));
    if( tmp_xi == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pres2hybrid: Unable to allocate memory for coercing xi array to double precision");
      return(NhlFATAL);
    }
  }
/*
 * Allocate space for output array.
 */
  if(type_xo == NCL_float) {
    xo     = (void*)calloc(size_xo,sizeof(float));
    tmp_xo = (double*)calloc(nlat_nlon_nlevo,sizeof(double));
    if(tmp_xo == NULL || xo == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pres2hybrid: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    xo = (void*)calloc(size_xo,sizeof(double));
    if(xo == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pres2hybrid: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }

/*
 * Call function.
 */
  index_xi = 0;
  index_xo = 0;
  index_ps = 0;
  for( i = 0; i < size_leftmost; i++ ) {
    if(type_ps != NCL_double) {
/*
 * Coerce subsection of ps (tmp_ps) to double.
 */
      coerce_subset_input_double(ps,tmp_ps,index_ps,type_ps,nlat_nlon,
                                 0,NULL,NULL);
    }
    else {
/*
 * Point tmp_ps to appropriate location in ps.
 */
      tmp_ps = &((double*)ps)[index_ps];
    }

    if(type_xi != NCL_double) {
/*
 * Coerce subsection of xi (tmp_xi) to double.
 */
      coerce_subset_input_double(xi,tmp_xi,index_xi,type_xi,nlat_nlon_nlevi,
                                 0,NULL,NULL);
    }
    else {
/*
 * Point tmp_xi to appropriate location in xi.
 */
      tmp_xi = &((double*)xi)[index_xi];
    }

    if(type_xo == NCL_double) tmp_xo = &((double*)xo)[index_xo];

    NGCALLF(p2hyo,P2HYO)(tmp_p,&inlon,&inlat,&inlevi,tmp_xi,tmp_ps,tmp_p0,
                         tmp_hyao,tmp_hybo,&inlevo,tmp_xo,
                         &missing_dxi.doubleval,&iflag,kflag,&ier);

/*
 * If iflag is 1, then this means there are missing values present in
 * the output, and hence we need to make sure the return value has a
 * missing value attached (later).
 */
    if(iflag == 1) {
      return_missing = 1;
    }
/*
 * Copy output values from temporary tmp_xo to xo.
 */
    if(type_xo != NCL_double) {
      coerce_output_float_only(xo,tmp_xo,nlat_nlon_nlevo,index_xo);
    }
    index_xi +=nlat_nlon_nlevi;
    index_xo +=nlat_nlon_nlevo;
    index_ps += nlat_nlon;
  }
/*
 * Free memory.
 */
  if(type_p    != NCL_double) NclFree(tmp_p);
  if(type_ps   != NCL_double) NclFree(tmp_ps);
  if(type_p0   != NCL_double) NclFree(tmp_p0);
  if(type_xi   != NCL_double) NclFree(tmp_xi);
  if(type_hyao != NCL_double) NclFree(tmp_hyao);
  if(type_hybo != NCL_double) NclFree(tmp_hybo);
  if(type_xo   != NCL_double) NclFree(tmp_xo);
/*
 * Return values to NCL script. First check if we also need to
 * set a _FillValue attribute for the return variable.
 */
  if(return_missing) {
    ret = NclReturnValue(xo,ndims_xi,dsizes_xo,&missing_xo,type_xo,0);
  }
  else {
    ret = NclReturnValue(xo,ndims_xi,dsizes_xo,NULL,type_xo,0);
  }

  NclFree(dsizes_xo);
  return(ret);
}

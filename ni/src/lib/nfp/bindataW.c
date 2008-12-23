#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(bindatasum3,BINDATASUM3)(int *, int *, double *,
                                             int *, double *, double *,
                                             int *, double *, double *,
                                             double *, double *);

NhlErrorTypes bin_sum_util_W( void )
{

/*
 * Input variables
 */
/*
 * Argument # 0
 */
  void *gbin;
  double *tmp_gbin;
  int dsizes_gbin[2];
  NclBasicDataTypes type_gbin;

/*
 * Argument # 1
 */
  int *gknt;
  int dsizes_gknt[2];

/*
 * Argument # 2
 */
  void *glon;
  double *tmp_glon;
  int dsizes_glon[1];
  NclBasicDataTypes type_glon;

/*
 * Argument # 3
 */
  void *glat;
  double *tmp_glat;
  int dsizes_glat[1];
  NclBasicDataTypes type_glat;

/*
 * Argument # 4
 */
  void *zlon;
  double *tmp_zlon;
  int dsizes_zlon[1];
  NclBasicDataTypes type_zlon;

/*
 * Argument # 5
 */
  void *zlat;
  double *tmp_zlat;
  int dsizes_zlat[1];
  NclBasicDataTypes type_zlat;

/*
 * Argument # 6
 */
  void *z;
  double *tmp_z;
  int dsizes_z[1];
  int has_missing_z;
  NclScalar missing_z, missing_dbl_z;
  NclBasicDataTypes type_z;

/*
 * Various
 */
  int i, nlat, mlon, nlatmlon, nz;

/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
/*
 * Get argument # 0
 */
  gbin = (void*)NclGetArgValue(
           0,
           7,
           NULL,
           dsizes_gbin,
           NULL,
           NULL,
           &type_gbin,
           2);

/* 
 * Make sure gbin is a float or double.
 */
  if(type_gbin != NCL_float && type_gbin != NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"bin_sum_util: gbin must be float or double");
    return(NhlFATAL);
  }
  nlat = dsizes_gbin[0];
  mlon = dsizes_gbin[1];
  nlatmlon = nlat * mlon;

/*
 * Get argument # 1
 */
  gknt = (int*)NclGetArgValue(
           1,
           7,
           NULL,
           dsizes_gknt,
           NULL,
           NULL,
           NULL,
           2);
  if(dsizes_gknt[0] != nlat || dsizes_gknt[1] != mlon) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"bin_sum_util: gknt must be the same size as gbin");
    return(NhlFATAL);
  }

/*
 * Get argument # 2
 */
  glon = (void*)NclGetArgValue(
           2,
           7,
           NULL,
           dsizes_glon,
           NULL,
           NULL,
           &type_glon,
           2);

  if(dsizes_glon[0] != mlon) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"bin_sum_util: The length of glon must be the same as the rightmost dimension of gbin");
    return(NhlFATAL);
  }

/*
 * Get argument # 3
 */
  glat = (void*)NclGetArgValue(
           3,
           7,
           NULL,
           dsizes_glat,
           NULL,
           NULL,
           &type_glat,
           2);

/*
 * Check dimension sizes.
 */
  if(dsizes_glat[0] != nlat) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"bin_sum_util: The length of glat must be the same as the leftmost dimension of gbin");
    return(NhlFATAL);
  }

/*
 * Get argument # 4
 */
  zlon = (void*)NclGetArgValue(
           4,
           7,
           NULL,
           dsizes_zlon,
           NULL,
           NULL,
           &type_zlon,
           2);

  nz = dsizes_zlon[0];

/*
 * Get argument # 5
 */
  zlat = (void*)NclGetArgValue(
           5,
           7,
           NULL,
           dsizes_zlat,
           NULL,
           NULL,
           &type_zlat,
           2);

  if(dsizes_zlat[0] != nz) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"bin_sum_util: zlat must be the same size as zlon");
    return(NhlFATAL);
  }

/*
 * Get argument # 6
 */
  z = (void*)NclGetArgValue(
           6,
           7,
           NULL,
           dsizes_z,
           &missing_z,
           &has_missing_z,
           &type_z,
           2);

  if(dsizes_z[0] != nz) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"bin_sum_util: z must be the same size as zlat and zlon");
    return(NhlFATAL);
  }

/*
 * Coerce missing value to double if necessary.
 */
  coerce_missing(type_z,has_missing_z,&missing_z,&missing_dbl_z,NULL);

/* 
 * Coerce input arrays to double, if necessary.
 */
  tmp_gbin = coerce_input_double(gbin,type_gbin,nlatmlon,0,NULL,NULL);
  if(tmp_gbin == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"bin_sum_util: Unable to allocate memory for coercing input array to double");
    return(NhlFATAL);
  }

  tmp_glon = coerce_input_double(glon,type_glon,mlon,0,NULL,NULL);
  if(tmp_glon == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"bin_sum_util: Unable to allocate memory for coercing input array to double");
    return(NhlFATAL);
  }

  tmp_glat = coerce_input_double(glat,type_glat,nlat,0,NULL,NULL);
  if(type_glat != NCL_double) {
    tmp_glat = (double *)calloc(nlat,sizeof(double));
    if(tmp_glat == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"bin_sum_util: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }

  tmp_zlon = coerce_input_double(zlon,type_zlon,nz,0,NULL,NULL);
  if(tmp_zlon == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"bin_sum_util: Unable to allocate memory for coercing input array to double");
    return(NhlFATAL);
  }

  tmp_zlat = coerce_input_double(zlat,type_zlat,nz,0,NULL,NULL);
  if(tmp_zlat == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"bin_sum_util: Unable to allocate memory for coercing input array to double");
    return(NhlFATAL);
  }

  tmp_z = coerce_input_double(z,type_z,nz,has_missing_z,&missing_z,
                              &missing_dbl_z);
  if(tmp_z == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"bin_sum_util: Unable to allocate memory for coercing input array to double");
    return(NhlFATAL);
  }

/*
 * Call the Fortran routine.
 */
  NGCALLF(bindatasum3,BINDATASUM3)(&mlon, &nlat, tmp_gbin, gknt, tmp_glon, 
                                   tmp_glat, &nz, tmp_zlon, tmp_zlat, 
                                   tmp_z, &missing_dbl_z.doubleval);
/*
 * Coerce gbin back to float if necessary.
 */
  if(type_gbin == NCL_float) {
    coerce_output_float_only(gbin,tmp_gbin,nlatmlon,0);
  }

/*
 * Free unneeded memory.
 */
  if(type_gbin != NCL_double) NclFree(tmp_gbin);
  if(type_glon != NCL_double) NclFree(tmp_glon);
  if(type_glat != NCL_double) NclFree(tmp_glat);
  if(type_zlon != NCL_double) NclFree(tmp_zlon);
  if(type_zlat != NCL_double) NclFree(tmp_zlat);
  if(type_z    != NCL_double) NclFree(tmp_z);

/*
 * This is a procedure, so just return no error code.
 */
  return(NhlNOERROR);
}

#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(bindatasum3,BINDATASUM3)(int *, int *, double *,
                                             int *, double *, double *,
                                             int *, double *, double *,
                                             double *, double *);

extern void NGCALLF(bindataavg,BINDATAAVG)(int *, double *, double *, 
                                           double *, double *, int *, int *, 
                                           double *, double *, double *, 
                                           int *, int *);

NhlErrorTypes bin_sum_W( void )
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
 * Get argument # 0. Note that this is an output variable, so the
 * last argument must be 1, not 2!
 */
  gbin = (void*)NclGetArgValue(
           0,
           7,
           NULL,
           dsizes_gbin,
           NULL,
           NULL,
           &type_gbin,
           1);

/* 
 * Make sure gbin is a float or double.
 */
  if(type_gbin != NCL_float && type_gbin != NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"bin_sum: gbin must be float or double");
    return(NhlFATAL);
  }
  nlat = dsizes_gbin[0];
  mlon = dsizes_gbin[1];
  nlatmlon = nlat * mlon;

/*
 * Get argument # 1. Note that this is an output variable, so the
 * last argument must be 1, not 2!
 */
  gknt = (int*)NclGetArgValue(
           1,
           7,
           NULL,
           dsizes_gknt,
           NULL,
           NULL,
           NULL,
           1);

  if(dsizes_gknt[0] != nlat || dsizes_gknt[1] != mlon) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"bin_sum: gknt must be the same size as gbin");
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
    NhlPError(NhlFATAL,NhlEUNKNOWN,"bin_sum: The length of glon must be the same as the rightmost dimension of gbin");
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
    NhlPError(NhlFATAL,NhlEUNKNOWN,"bin_sum: The length of glat must be the same as the leftmost dimension of gbin");
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
    NhlPError(NhlFATAL,NhlEUNKNOWN,"bin_sum: zlat must be the same size as zlon");
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
    NhlPError(NhlFATAL,NhlEUNKNOWN,"bin_sum: z must be the same size as zlat and zlon");
    return(NhlFATAL);
  }

/*
 * Coerce missing value to double if necessary.
 */
  coerce_missing(type_z,has_missing_z,&missing_z,&missing_dbl_z,NULL);

/*
 * Create temporary double array for output, if necessary. gbin is
 * an input/output variable, so make sure to carry the values over.
 */
  tmp_gbin = coerce_input_double(gbin,type_gbin,nlatmlon,0,NULL,NULL);
  if(tmp_gbin == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"bin_sum: Unable to allocate memory for coercing input array to double");
    return(NhlFATAL);
  }

/* 
 * Coerce other input arrays to double, if necessary.
 */
  tmp_glon = coerce_input_double(glon,type_glon,mlon,0,NULL,NULL);
  if(tmp_glon == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"bin_sum: Unable to allocate memory for coercing glon to double");
    return(NhlFATAL);
  }

  tmp_glat = coerce_input_double(glat,type_glat,nlat,0,NULL,NULL);
  if(tmp_glat == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"bin_sum: Unable to allocate memory for coercing glat to double");
    return(NhlFATAL);
  }

  tmp_zlon = coerce_input_double(zlon,type_zlon,nz,0,NULL,NULL);
  if(tmp_zlon == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"bin_sum: Unable to allocate memory for coercing zlon to double");
    return(NhlFATAL);
  }

  tmp_zlat = coerce_input_double(zlat,type_zlat,nz,0,NULL,NULL);
  if(tmp_zlat == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"bin_sum: Unable to allocate memory for coercing zlat to double");
    return(NhlFATAL);
  }

  tmp_z = coerce_input_double(z,type_z,nz,has_missing_z,&missing_z,
                              &missing_dbl_z);
  if(tmp_z == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"bin_sum: Unable to allocate memory for coercing z to double");
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

NhlErrorTypes bin_avg_W( void )
{

/*
 * Input variables
 */
/*
 * Argument # 0
 */
  void *zlon;
  double *tmp_zlon;
  int dsizes_zlon[1];
  NclBasicDataTypes type_zlon;

/*
 * Argument # 1
 */
  void *zlat;
  double *tmp_zlat;
  int dsizes_zlat[1];
  NclBasicDataTypes type_zlat;

/*
 * Argument # 2
 */
  void *z;
  double *tmp_z;
  int dsizes_z[1];
  int has_missing_z;
  NclScalar missing_z, missing_dbl_z;
  NclBasicDataTypes type_z;

/*
 * Argument # 3
 */
  void *glon;
  double *tmp_glon;
  int dsizes_glon[1];
  NclBasicDataTypes type_glon;

/*
 * Argument # 4
 */
  void *glat;
  double *tmp_glat;
  int dsizes_glat[1];
  NclBasicDataTypes type_glat;

/*
 * Argument # 5
 */
  logical *opt;

/*
 * Return variable
 */
  void *gbinknt;
  double *tmp_gbinknt;
  int ndims_gbinknt, dsizes_gbinknt[3];
  NclBasicDataTypes type_gbinknt;

/*
 * Various
 */
  int ret, ier, nz, mlon, nlat, size_output;

/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
/*
 * Get argument # 0
 */
  zlon = (void*)NclGetArgValue(
           0,
           6,
           NULL,
           dsizes_zlon,
           NULL,
           NULL,
           &type_zlon,
           2);

  nz = dsizes_zlon[0];
/*
 * Get argument # 1
 */
  zlat = (void*)NclGetArgValue(
           1,
           6,
           NULL,
           dsizes_zlat,
           NULL,
           NULL,
           &type_zlat,
           2);

  if(dsizes_zlat[0] != nz) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"bin_avg: zlat must be the same length as zlon");
    return(NhlFATAL);
  }

/*
 * Get argument # 2
 */
  z = (void*)NclGetArgValue(
           2,
           6,
           NULL,
           dsizes_z,
           &missing_z,
           &has_missing_z,
           &type_z,
           2);

  if(dsizes_z[0] != nz) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"bin_avg: z must be the same length as zlat and zlon");
    return(NhlFATAL);
  }

/*
 * Coerce missing value to double if necessary.
 */
  coerce_missing(type_z,has_missing_z,&missing_z,&missing_dbl_z,NULL);

/*
 * Get argument # 3
 */
  glon = (void*)NclGetArgValue(
           3,
           6,
           NULL,
           dsizes_glon,
           NULL,
           NULL,
           &type_glon,
           2);

  mlon = dsizes_glon[0];

/*
 * Get argument # 4
 */
  glat = (void*)NclGetArgValue(
           4,
           6,
           NULL,
           dsizes_glat,
           NULL,
           NULL,
           &type_glat,
           2);

  nlat = dsizes_glat[0];

/*
 * Get argument # 5
 */
  opt = (logical*)NclGetArgValue(
           5,
           6,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           2);

/*
 * The output type defaults to float, unless this input array is double.
 */
  if(type_zlon == NCL_double || type_zlat == NCL_double || 
     type_glon == NCL_double || type_glat == NCL_double ||
     type_z == NCL_double) {
    type_gbinknt = NCL_double;
  }
  else {
    type_gbinknt = NCL_float;
  }

/* 
 * Coerce input arrays to double if necessary.
 */
  tmp_zlon = coerce_input_double(zlon,type_zlon,nz,0,NULL,NULL);
  if(tmp_zlon == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"bin_avg: Unable to allocate memory for coercing input array to double");
    return(NhlFATAL);
  }

  tmp_zlat = coerce_input_double(zlat,type_zlat,nz,0,NULL,NULL);
  if(tmp_zlat == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"bin_avg: Unable to allocate memory for coercing input array to double");
    return(NhlFATAL);
  }

  tmp_z = coerce_input_double(z,type_z,nz,has_missing_z,&missing_z,
                              &missing_dbl_z);
  if(tmp_z == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"bin_avg: Unable to allocate memory for coercing input array to double");
    return(NhlFATAL);
  }

  tmp_glon = coerce_input_double(glon,type_glon,mlon,0,NULL,NULL);
  if(tmp_glon == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"bin_avg: Unable to allocate memory for coercing input array to double");
    return(NhlFATAL);
  }

  tmp_glat = coerce_input_double(glat,type_glat,nlat,0,NULL,NULL);
  if(tmp_glat == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"bin_avg: Unable to allocate memory for coercing input array to double");
    return(NhlFATAL);
  }

/*
 * Calculate size of output array.
 */
  size_output = 2 * nlat * mlon;

/* 
 * Allocate space for output array.
 */
  if(type_gbinknt != NCL_double) {
    gbinknt     = (void *)calloc(size_output, sizeof(float));
    tmp_gbinknt = (double *)calloc(size_output,sizeof(double));
    if(gbinknt == NULL || tmp_gbinknt == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"bin_avg: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    gbinknt = (void *)calloc(size_output, sizeof(double));
    if(gbinknt == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"bin_avg: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    tmp_gbinknt = (double *)gbinknt;
  }


/* 
 * Set dimension sizes.
 */
  ndims_gbinknt     = 3;
  dsizes_gbinknt[0] = 2;
  dsizes_gbinknt[1] = nlat;
  dsizes_gbinknt[2] = mlon;

/*
 * Call the Fortran routine.
 */
  NGCALLF(bindataavg,BINDATAAVG)(&nz, tmp_zlon, tmp_zlat, tmp_z, 
                                 &missing_dbl_z.doubleval, &mlon, &nlat, 
                                 tmp_glon, tmp_glat, tmp_gbinknt, opt, &ier);

/*
 * Coerce gbinknt back to float if necessary.
 */
  if(type_gbinknt == NCL_float) {
    coerce_output_float_only(gbinknt,tmp_gbinknt,size_output,0);
  }


/*
 * Free unneeded memory.
 */
  if(type_zlon != NCL_double) NclFree(tmp_zlon);
  if(type_zlat != NCL_double) NclFree(tmp_zlat);
  if(type_z    != NCL_double) NclFree(tmp_z);
  if(type_glon != NCL_double) NclFree(tmp_glon);
  if(type_glat != NCL_double) NclFree(tmp_glat);
  if(type_gbinknt != NCL_double) NclFree(tmp_gbinknt);

/*
 * Return value back to NCL script.
 */
  ret = NclReturnValue(gbinknt,ndims_gbinknt,dsizes_gbinknt,NULL,
                       type_gbinknt,0);

  return(ret);
}

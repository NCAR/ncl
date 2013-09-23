#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(spareapolyi,SPAREAPOLYI)(double *, double *, int *, double *, double *);

NhlErrorTypes area_poly_sphere_W( void )
{

/*
 * Input variables
 */
/*
 * Argument # 0
 */
  void *lat;
  double *tmp_lat;
  ng_size_t dsizes_lat[1];
  NclBasicDataTypes type_lat;

/*
 * Argument # 1
 */
  void *lon;
  double *tmp_lon;
  ng_size_t dsizes_lon[1];
  NclBasicDataTypes type_lon;

/*
 * Argument # 2
 */
  void *rsph;
  double *tmp_rsph;
  NclBasicDataTypes type_rsph;

/*
 * Return variable
 */
  void *parea;
  double *tmp_parea;
  int  ndims_parea;
  ng_size_t dsizes_parea[1];
  NclBasicDataTypes type_parea;

/*
 * Various
 */
  ng_size_t npts;
  int inpts, ret;

/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
/*
 * Get argument # 0
 */
  lat = (void*)NclGetArgValue(
           0,
           3,
           NULL,
           dsizes_lat,
           NULL,
           NULL,
           &type_lat,
           DONT_CARE);

  npts = dsizes_lat[0];
  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"area_poly_sphere: npts = %ld is greater than INT_MAX", npts);
    return(NhlFATAL);
  }
  inpts = (int) npts;
/*
 * Get argument # 1
 */
  lon = (void*)NclGetArgValue(
           1,
           3,
           NULL,
           dsizes_lon,
           NULL,
           NULL,
           &type_lon,
           DONT_CARE);
  
  if(dsizes_lon[0] != npts) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"area_poly_sphere: The #0 dimension of lon must be length npts");
    return(NhlFATAL);
  }
/*
 * Get argument # 2
 */
  rsph = (void*)NclGetArgValue(
           2,
           3,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_rsph,
           DONT_CARE);

/* 
 * Allocate space for coercing input arrays.  If any of the input
 * is already double, then we don't need to allocate space for
 * temporary arrays, because we'll just change the pointer into
 * the void array appropriately.
 */
  tmp_lat  = coerce_input_double(lat,type_lat,npts,0,NULL,NULL);
  if(tmp_lat == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"area_poly_sphere: Unable to allocate memory for coercing lat to double");
    return(NhlFATAL);
  }
  tmp_lon  = coerce_input_double(lon,type_lon,npts,0,NULL,NULL);
  if(tmp_lon == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"area_poly_sphere: Unable to allocate memory for coercing lon to double");
    return(NhlFATAL);
  }
  tmp_rsph = coerce_input_double(rsph,type_rsph,1,0,NULL,NULL);
  if(tmp_rsph == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"area_poly_sphere: Unable to allocate memory for coercing rsph to double");
    return(NhlFATAL);
  }

  if(type_lat == NCL_double || type_lon == NCL_double || type_rsph == NCL_double) {
    type_parea = NCL_double;
  }
  else {
    type_parea = NCL_float;
  }

/* 
 * Allocate space for output array.
 */
  if(type_parea != NCL_double) {
    parea     = (void *)calloc(1, sizeof(float));
    tmp_parea = (double *)calloc(1,sizeof(double));
    if(parea == NULL || tmp_parea == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"area_poly_sphere: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    parea = (void *)calloc(1, sizeof(double));
    if(parea == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"area_poly_sphere: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    tmp_parea = (double*)parea;
  }

/*
 * Call the Fortran routine.
 */
  NGCALLF(spareapolyi,SPAREAPOLYI)(tmp_lat, tmp_lon, &inpts, tmp_rsph, tmp_parea);

/*
 * Coerce as necessary
 */
  coerce_output_float_or_double(parea,tmp_parea,type_parea,1,0);

/*
 * Free unneeded memory.
 */
  if(type_lat   != NCL_double) NclFree(tmp_lat);
  if(type_lon   != NCL_double) NclFree(tmp_lon);
  if(type_rsph  != NCL_double) NclFree(tmp_rsph);
  if(type_parea != NCL_double) NclFree(tmp_parea);

/*
 * Return value back to NCL script. Output is a scalar.
 */
  ndims_parea     = 1;
  dsizes_parea[0] = 1;
  ret = NclReturnValue(parea,ndims_parea,dsizes_parea,NULL,type_parea,0);
  return(ret);
}

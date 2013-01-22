#include <string.h>
#include <stdio.h>
#include "wrapper.h"

NhlErrorTypes dsgrid2s_W( void )
{
  int ier = 0;
  float *x;
  ng_size_t dsizes_x[1];
  int has_missing_x;
  float *y;
  ng_size_t dsizes_y[1];
  int has_missing_y;
  float *z;
  int ndims_z;
  ng_size_t dsizes_z[NCL_MAX_DIMENSIONS];
  int has_missing_z;
  float *xo;
  ng_size_t dsizes_xo[1];
  int has_missing_xo;
  float *yo;
  ng_size_t dsizes_yo[1];
  int has_missing_yo;
  NclScalar missing_x, missing_y, missing_z, missing_xo, missing_yo;
  float *zo, *tmp_zo;
  int ndims_zo;
  ng_size_t *dsizes_zo;
  ng_size_t i, nzo, size_leftmost, size_z, size_zo;
  ng_size_t npts, nxo, nyo;
  int type_size_zo, inpts, inxo, inyo;
  ng_size_t index_z = 0, index_zo = 0;
  int ret;

/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 */
  x = (float*)NclGetArgValue(
                             0,
                             5,
                             NULL,
                             dsizes_x,
                             &missing_x,
                             &has_missing_x,
                             NULL,
                             DONT_CARE);
  y = (float*)NclGetArgValue(
                             1,
                             5,
                             NULL,
                             dsizes_y,
                             &missing_y,
                             &has_missing_y,
                             NULL,
                             DONT_CARE);
/*
 * Test the dimension sizes.
 */
  if(dsizes_x[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid2s: the length of x is > INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_x[0];
  inpts = (int) npts;

/*
 * Check dimension sizes for x and y.
 */
  if(dsizes_y[0] != npts) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid2s: x and y must be the same length");
    return(NhlFATAL);
  }

/*
 * Get z.
 */
  z = (float*)NclGetArgValue(
                             2,
                             5,
                             &ndims_z,
                             dsizes_z,
                             &missing_z,
                             &has_missing_z,
                             NULL,
                             DONT_CARE);

/*
 * Check rightmost dimension size for z.
 */
  if(dsizes_z[ndims_z-1] != npts) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid2s: the last (rightmost) dimension of z must be the same length as x and y");
    return(NhlFATAL);
  }

/*
 * Get rest of parameters.
 */

  xo = (float*)NclGetArgValue(
                              3,
                              5,
                              NULL,
                              dsizes_xo,
                              &missing_xo,
                              &has_missing_xo,
                              NULL,
                              DONT_CARE);
  yo = (float*)NclGetArgValue(
                              4,
                              5,
                              NULL,
                              dsizes_yo,
                              &missing_yo,
                              &has_missing_yo,
                              NULL,
                              DONT_CARE);
/*
 * Test the dimension sizes.
 */
  if( (dsizes_xo[0] > INT_MAX) || (dsizes_yo[0] > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid2s: the length of xo and/or yo is greater than INT_MAX");
    return(NhlFATAL);
  }
  nxo  = dsizes_xo[0];
  nyo  = dsizes_yo[0];
  inxo = (int) nxo;
  inyo = (int) nyo;
  nzo = nxo * nyo;

/*
 * Compute the total size of the input and the leftmost dimensions.
 */

  size_leftmost = 1;
  for( i = 0; i < ndims_z-1; i++ ) size_leftmost *= dsizes_z[i];
  size_z = size_leftmost * npts;

/*
 * Check for missing values. 
 */
  if(contains_missing_float(x,npts,has_missing_x,missing_x.floatval) ||
     contains_missing_float(y,npts,has_missing_y,missing_y.floatval) ||
     contains_missing_float(z,size_z,has_missing_z,missing_z.floatval) ||
     contains_missing_float(xo,nxo,has_missing_xo,missing_xo.floatval) ||
     contains_missing_float(yo,nyo,has_missing_yo,missing_yo.floatval)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid2s: the input arrays cannot contain any missing values" );
    return(NhlFATAL);
  }

/*
 * Calculate space for output array and its dimension sizes.
 */
  ndims_zo  = ndims_z + 1;
  size_zo   = size_leftmost * nzo;
  zo        = (float *) calloc(size_zo, sizeof(float));
  dsizes_zo = (ng_size_t *) calloc(ndims_zo, sizeof(ng_size_t));
  type_size_zo = nzo * sizeof(float);

  if(zo == NULL || dsizes_zo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "dsgrid2s: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_zo-2; i++ ) dsizes_zo[i] = dsizes_z[i];
  dsizes_zo[ndims_zo-2] = nxo;
  dsizes_zo[ndims_zo-1] = nyo;

/*
 * The following section loops through the leftmost dimensions and calls
 * the c_dsgrid2s function.
 */
  for( i = 0; i < size_leftmost; i++ ) {
    tmp_zo = c_dsgrid2s (inpts,x,y,&z[index_z],inxo,inyo,xo,yo,&ier);
    if(ier) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid2s: ier = %d", ier);
      NclFree(tmp_zo);
      return(NhlFATAL);
    }
    memcpy((void*)((char*)zo+i*type_size_zo),
           (void*)((char*)tmp_zo),type_size_zo);

    index_z  += npts;
    index_zo += nzo;
    NclFree(tmp_zo);
  }
  ret = NclReturnValue((void*)zo,ndims_zo,dsizes_zo,NULL,NCL_float,0);
  NclFree(dsizes_zo);
  return(ret);
}

NhlErrorTypes dsgrid2_W( void )
{
/* 
 * Input values
 */
  void *x, *y, *z;
  double *tmp_x, *tmp_y;
  double *tmp_z = NULL;
  ng_size_t dsizes_x[1];
  int has_missing_x;
  ng_size_t dsizes_y[1];
  int has_missing_y;
  int ndims_z;
  ng_size_t dsizes_z[NCL_MAX_DIMENSIONS];
  int has_missing_z;
  NclBasicDataTypes type_x, type_y, type_z;
  NclScalar missing_x, missing_y, missing_z;
  NclScalar missing_dx, missing_dy, missing_dz, missing_rz;
/*
 * Output grid to interpolate on.
 */
  void *xo, *yo;
  double *tmp_xo, *tmp_yo;
  ng_size_t dsizes_xo[1];
  int has_missing_xo;
  ng_size_t dsizes_yo[1];
  int has_missing_yo;
  NclBasicDataTypes type_xo, type_yo;
  NclScalar missing_xo, missing_yo, missing_dxo, missing_dyo;
/*
 * Output variables.
 */
  void *zo;
  double *tmp_zo;
  int ndims_zo;
  ng_size_t *dsizes_zo;
  NclBasicDataTypes type_zo;
  NclScalar missing_zo, missing_dzo;
/*
 * Various
 */
  ng_size_t i, nzo, size_leftmost, size_z, size_zo;
  ng_size_t npts, nxo, nyo;
  int inpts, inxo, inyo, ier = 0, nmiss = 0, ret;
  ng_size_t index_z = 0, index_zo = 0;

/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept numerics.
 */
  x = (void*)NclGetArgValue(
                             0,
                             5,
                             NULL,
                             dsizes_x,
                             &missing_x,
                             &has_missing_x,
                             &type_x,
                             DONT_CARE);
  y = (void*)NclGetArgValue(
                             1,
                             5,
                             NULL,
                             dsizes_y,
                             &missing_y,
                             &has_missing_y,
                             &type_y,
                             DONT_CARE);
/*
 * Test the dimension sizes.
 */
  if(dsizes_x[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid2: the length of x is > INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_x[0];
  inpts = (int) npts;

/*
 * Check dimension sizes for x and y.
 */
  if(dsizes_y[0] != npts) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid2: x and y must be the same length");
    return(NhlFATAL);
  }

/*
 * Get z.
 */
  z = (void*)NclGetArgValue(
                             2,
                             5,
                             &ndims_z,
                             dsizes_z,
                             &missing_z,
                             &has_missing_z,
                             &type_z,
                             DONT_CARE);
/*
 * Check rightmost dimension size for z.
 */
  if(dsizes_z[ndims_z-1] != npts) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid2: the last (rightmost) dimension of z must be the same length as x and y");
    return(NhlFATAL);
  }

/*
 * Get rest of parameters.
 */

  xo = (void*)NclGetArgValue(
                              3,
                              5,
                              NULL,
                              dsizes_xo,
                              &missing_xo,
                              &has_missing_xo,
                              &type_xo,
                              DONT_CARE);
  yo = (void*)NclGetArgValue(
                              4,
                              5,
                              NULL,
                              dsizes_yo,
                              &missing_yo,
                              &has_missing_yo,
                              &type_yo,
                              DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if( (dsizes_xo[0] > INT_MAX) || (dsizes_yo[0] > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid2: the length of xo and/or yo is greater than INT_MAX");
    return(NhlFATAL);
  }
  nxo  = dsizes_xo[0];
  nyo  = dsizes_yo[0];
  inxo = (int) nxo;
  inyo = (int) nyo;
  nzo = nxo * nyo;

/*
 * Compute the total size of the input and the leftmost dimensions.
 */

  size_leftmost = 1;
  for( i = 0; i < ndims_z-1; i++ ) size_leftmost *= dsizes_z[i];
  size_z = size_leftmost * npts;

/*
 * Coerce missing values.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,NULL);
  coerce_missing(type_y,has_missing_y,&missing_y,&missing_dy,NULL);
  coerce_missing(type_z,has_missing_z,&missing_z,&missing_dz,&missing_rz);
  coerce_missing(type_xo,has_missing_xo,&missing_xo,&missing_dxo,NULL);
  coerce_missing(type_yo,has_missing_yo,&missing_yo,&missing_dyo,NULL);
/*
 * Coerce input arrays to double, if necessary.
 */
  tmp_x  = coerce_input_double(x,type_x,npts,has_missing_x,&missing_x,
                               &missing_dx);
  tmp_y  = coerce_input_double(y,type_y,npts,has_missing_y,&missing_y,
                               &missing_dy);
  tmp_xo = coerce_input_double(xo,type_xo,nxo,has_missing_xo,&missing_xo,
                               &missing_dxo);
  tmp_yo = coerce_input_double(yo,type_yo,nyo,has_missing_yo,&missing_yo,
                               &missing_dyo);
/*
 * Allocate space for temporary input array. The temporary array
 * tmp_z is just big enough to hold a 1-dimensional subsection of the
 * z array. We only need to allocate space for it if the
 * input is not already double. Otherwise, we just have it point
 * to the appropriate locations in z.
 */
  if(type_z != NCL_double) {
    tmp_z = (double*)calloc(npts,sizeof(double));
    if(tmp_z == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid2: Unable to allocate memory for coercing z array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Check for missing values.
 */
  if(contains_missing(tmp_x,npts,has_missing_x,missing_dx.doubleval) ||
     contains_missing(tmp_y,npts,has_missing_y,missing_dy.doubleval) ||
     contains_missing(tmp_xo,nxo,has_missing_xo,missing_dxo.doubleval) ||
     contains_missing(tmp_yo,nyo,has_missing_yo,missing_dyo.doubleval)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid2: the input arrays cannot contain any missing values" );
    return(NhlFATAL);
  }

/*
 * Calculate space for output array and its dimension sizes.
 */
  ndims_zo  = ndims_z + 1;
  size_zo   = size_leftmost * nzo;
  dsizes_zo = (ng_size_t *) calloc(ndims_zo, sizeof(ng_size_t));

  if(type_z != NCL_double) {
    type_zo = NCL_float;
    zo = (void*)calloc(size_zo,sizeof(float));
  }
  else {
    type_zo = NCL_double;
    zo = (void*)calloc(size_zo,sizeof(double));
  }
  if(zo == NULL || dsizes_zo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "dsgrid2: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  if(has_missing_z) {
    if(type_zo == NCL_double) missing_zo = missing_dz;
    else                      missing_zo = missing_rz;
    missing_dzo = missing_dz;
  }

  for( i = 0; i < ndims_zo-2; i++ ) dsizes_zo[i] = dsizes_z[i];
  dsizes_zo[ndims_zo-2] = nxo;
  dsizes_zo[ndims_zo-1] = nyo;

/*
 * The following section loops through the leftmost dimensions and calls
 * the c_dsgrid2d function.
 */
  for( i = 0; i < size_leftmost; i++ ) {
    if(type_z != NCL_double) {
/*
 * Coerce npts subsection of z (tmp_z) to double.
 */
      coerce_subset_input_double(z,tmp_z,index_z,type_z,npts,has_missing_z,
                                 &missing_z,&missing_dz);
    }
    else {
/*
 * Point tmp_z to appropriate location in z.
 */
      tmp_z = &((double*)z)[index_z];
    }
/*
 * Check for missing values in z.
 */
    if(contains_missing(tmp_z,npts,has_missing_z,missing_dz.doubleval)) {
      nmiss++;
/*
 * Set all elements of this 2D grid to a missing value, if a missing
 * value exists.
 */
      set_subset_output_missing(zo,index_zo,type_zo,nzo,missing_dzo.doubleval);
    }
    else {
/*
 * Call c_dsgrid2d.
 */
      tmp_zo = c_dsgrid2d (inpts,tmp_x,tmp_y,tmp_z,inxo,inyo,tmp_xo,tmp_yo,&ier);
/*
 * Check for errors.
 */
      if(ier) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid2: ier = %d", ier);
/*
 * Free arrays
 */
        if(type_x  != NCL_double) NclFree(tmp_x);
        if(type_y  != NCL_double) NclFree(tmp_y);
        if(type_z  != NCL_double) NclFree(tmp_z);
        if(type_xo != NCL_double) NclFree(tmp_xo);
        if(type_yo != NCL_double) NclFree(tmp_yo);
        NclFree(tmp_zo);
        return(NhlFATAL);
      }
      coerce_output_float_or_double(zo,tmp_zo,type_zo,nzo,index_zo);
      index_z  += npts;
      index_zo += nzo;
      NclFree(tmp_zo);
    }
  }
/*
 * Check if z array had missing values. If so, print a warning message.
 */
  if(nmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"dsgrid2: %d 1-dimensional input arrays contained missing values. No interpolation performed on these arrays",nmiss);
  }
/*
 * Free arrays
 */
  if(type_x  != NCL_double) NclFree(tmp_x);
  if(type_y  != NCL_double) NclFree(tmp_y);
  if(type_z  != NCL_double) NclFree(tmp_z);
  if(type_xo != NCL_double) NclFree(tmp_xo);
  if(type_yo != NCL_double) NclFree(tmp_yo);

  if(has_missing_z) {
    ret = NclReturnValue(zo,ndims_zo,dsizes_zo,&missing_zo,type_zo,0);
  }
  else {
    ret = NclReturnValue(zo,ndims_zo,dsizes_zo,NULL,type_zo,0);
  }
  NclFree(dsizes_zo);
  return(ret);
}

NhlErrorTypes dsgrid2d_W( void )
{
  int ier = 0;
  double *x;
  ng_size_t dsizes_x[1];
  int has_missing_x;
  double *y;
  ng_size_t dsizes_y[1];
  int has_missing_y;
  double *z;
  int ndims_z;
  ng_size_t dsizes_z[NCL_MAX_DIMENSIONS];
  int has_missing_z;
  double *xo;
  ng_size_t dsizes_xo[1];
  int has_missing_xo;
  double *yo;
  ng_size_t dsizes_yo[1];
  int has_missing_yo;
  NclScalar missing_x, missing_y, missing_z, missing_xo, missing_yo;
  double *zo, *tmp_zo;
  int ndims_zo;
  ng_size_t *dsizes_zo;
  ng_size_t i, nzo, size_leftmost, size_z, size_zo;
  ng_size_t npts, nxo, nyo;
  int inpts, inxo, inyo, type_size_zo;
  ng_size_t index_z = 0, index_zo = 0;
  int ret;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 */
  x = (double*)NclGetArgValue(
                             0,
                             5,
                             NULL,
                             dsizes_x,
                             &missing_x,
                             &has_missing_x,
                             NULL,
                             DONT_CARE);
  y = (double*)NclGetArgValue(
                             1,
                             5,
                             NULL,
                             dsizes_y,
                             &missing_y,
                             &has_missing_y,
                             NULL,
                             DONT_CARE);
/*
 * Test the dimension sizes.
 */
  if(dsizes_x[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid2d: the length of x is > INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_x[0];
  inpts = (int) npts;

/*
 * Check dimension sizes for x and y.
 */
  if(dsizes_y[0] != npts) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid2d: x and y must be the same length");
    return(NhlFATAL);
  }

/*
 * Get z.
 */
  z = (double*)NclGetArgValue(
                             2,
                             5,
                             &ndims_z,
                             dsizes_z,
                             &missing_z,
                             &has_missing_z,
                             NULL,
                             DONT_CARE);

/*
 * Check rightmost dimension size for z.
 */
  if(dsizes_z[ndims_z-1] != npts) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid2d: the last (rightmost) dimension of z must be the same length as x and y");
    return(NhlFATAL);
  }

/*
 * Get rest of parameters.
 */

  xo = (double*)NclGetArgValue(
                               3,
                               5,
                               NULL,
                               dsizes_xo,
                               &missing_xo,
                               &has_missing_xo,
                               NULL,
                               DONT_CARE);
  yo = (double*)NclGetArgValue(
                               4,
                               5,
                               NULL,
                               dsizes_yo,
                               &missing_yo,
                               &has_missing_yo,
                               NULL,
                               DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if( (dsizes_xo[0] > INT_MAX) || (dsizes_yo[0] > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid2d: the length of xo and/or yo is greater than INT_MAX");
    return(NhlFATAL);
  }
  nxo  = dsizes_xo[0];
  nyo  = dsizes_yo[0];
  inxo = (int) nxo;
  inyo = (int) nyo;

  nzo = nxo * nyo;

/*
 * Compute the total size of the input and the leftmost dimensions.
 */

  size_leftmost = 1;
  for( i = 0; i < ndims_z-1; i++ ) size_leftmost *= dsizes_z[i];
  size_z = size_leftmost * npts;

/*
 * Check for missing values. 
 */
  if(contains_missing(x,npts,has_missing_x,missing_x.doubleval) ||
     contains_missing(y,npts,has_missing_y,missing_y.doubleval) ||
     contains_missing(z,size_z,has_missing_z,missing_z.doubleval) ||
     contains_missing(xo,nxo,has_missing_xo,missing_xo.doubleval) ||
     contains_missing(yo,nyo,has_missing_yo,missing_yo.doubleval)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid2d: the input arrays cannot contain any missing values" );
    return(NhlFATAL);
  }

/*
 * Calculate space for output array and its dimension sizes.
 */
  ndims_zo  = ndims_z + 1;
  size_zo   = size_leftmost * nzo;
  zo        = (double *) calloc(size_zo, sizeof(double));
  dsizes_zo = (ng_size_t *)  calloc(ndims_zo, sizeof(ng_size_t));
  type_size_zo = nzo * sizeof(double);

  if(zo == NULL || dsizes_zo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "dsgrid2d: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_zo-2; i++ ) dsizes_zo[i] = dsizes_z[i];
  dsizes_zo[ndims_zo-2] = nxo;
  dsizes_zo[ndims_zo-1] = nyo;

/*
 * The following section loops through the leftmost dimensions and calls
 * the c_dsgrid2d function.
 */
  for( i = 0; i < size_leftmost; i++ ) {
    tmp_zo = c_dsgrid2d (inpts,x,y,&z[index_z],inxo,inyo,xo,yo,&ier);
    if(ier) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid2d: ier = %d", ier);
      NclFree(tmp_zo);
      return(NhlFATAL);
    }
    memcpy((void*)((char*)zo+i*type_size_zo),
           (void*)((char*)tmp_zo),type_size_zo);

    index_z  += npts;
    index_zo += nzo;
    NclFree(tmp_zo);
  }
  ret = NclReturnValue((void*)zo,ndims_zo,dsizes_zo,NULL,NCL_double,0);
  NclFree(dsizes_zo);
  return(ret);
}

NhlErrorTypes dsgrid3s_W( void )
{
  int ier = 0;
  float *x;
  ng_size_t dsizes_x[1];
  int has_missing_x;
  float *y;
  ng_size_t dsizes_y[1];
  int has_missing_y;
  float *z;
  ng_size_t dsizes_z[1];
  int has_missing_z;
  float *u;
  int ndims_u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  int has_missing_u;
  float *xo;
  ng_size_t dsizes_xo[1];
  int has_missing_xo;
  float *yo;
  ng_size_t dsizes_yo[1];
  int has_missing_yo;
  float *zo;
  ng_size_t dsizes_zo[1];
  int has_missing_zo;
  NclScalar missing_x, missing_y, missing_z, missing_u;
  NclScalar missing_xo, missing_yo, missing_zo;
  float *uo, *tmp_uo;
  int ndims_uo;
  ng_size_t *dsizes_uo;
  ng_size_t i, npts, nxo, nyo, nzo, nuo;
  int inpts, inxo, inyo, inzo, ret;
  ng_size_t size_leftmost, size_u, size_uo;
  int type_size_uo;
  ng_size_t index_u = 0, index_uo = 0;

/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 */
  x = (float*)NclGetArgValue(
                             0,
                             7,
                             NULL,
                             dsizes_x,
                             &missing_x,
                             &has_missing_x,
                             NULL,
                             DONT_CARE);
  y = (float*)NclGetArgValue(
                             1,
                             7,
                             NULL,
                             dsizes_y,
                             &missing_y,
                             &has_missing_y,
                             NULL,
                             DONT_CARE);
  z = (float*)NclGetArgValue(
                             2,
                             7,
                             NULL,
                             dsizes_z,
                             &missing_z,
                             &has_missing_z,
                             NULL,
                             DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if(dsizes_x[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid3s: the length of x is > INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_x[0];
  inpts = (int) npts;

/*
 * Check dimension sizes for x, y, and z.
 */
  if(dsizes_y[0] != npts || dsizes_z[0] != npts) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid3s: x, y, and z must be the same length");
    return(NhlFATAL);
  }

/*
 * Get u.
 */
  u = (float*)NclGetArgValue(
                             3,
                             7,
                             &ndims_u,
                             dsizes_u,
                             &missing_u,
                             &has_missing_u,
                             NULL,
                             DONT_CARE);

/*
 * Check rightmost dimension size for u.
 */
  if(dsizes_u[ndims_u-1] != npts) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid3s: the last (rightmost) dimension of u must be the same length as x, y, and z");
    return(NhlFATAL);
  }

/*
 * Compute the total size of the input and the leftmost dimensions.
 */

  size_leftmost = 1;
  for( i = 0; i < ndims_u-1; i++ ) size_leftmost *= dsizes_u[i];
  size_u = size_leftmost * npts;

/*
 * Get rest of parameters.
 */

  xo = (float*)NclGetArgValue(
                             4,
                             7,
                             NULL,
                             dsizes_xo,
                             &missing_xo,
                             &has_missing_xo,
                             NULL,
                             DONT_CARE);
  yo = (float*)NclGetArgValue(
                             5,
                             7,
                             NULL,
                             dsizes_yo,
                             &missing_yo,
                             &has_missing_yo,
                             NULL,
                             DONT_CARE);
  zo = (float*)NclGetArgValue(
                             6,
                             7,
                             NULL,
                             dsizes_zo,
                             &missing_zo,
                             &has_missing_zo,
                             NULL,
                             DONT_CARE);
/*
 * Test the dimension sizes.
 */
  if( (dsizes_xo[0] > INT_MAX) || (dsizes_yo[0] > INT_MAX) ||
      (dsizes_zo[0] > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid3s: the length of xo, yo, and/or zo is greater than INT_MAX");
    return(NhlFATAL);
  }
  nxo  = dsizes_xo[0];
  nyo  = dsizes_yo[0];
  nzo  = dsizes_zo[0];
  inxo = (int) nxo;
  inyo = (int) nyo;
  inzo = (int) nzo;
  nuo = nxo * nyo * nzo;

/*
 * Check for missing values. 
 */
  if(contains_missing_float(x,npts,has_missing_x,missing_x.floatval) ||
     contains_missing_float(y,npts,has_missing_y,missing_y.floatval) ||
     contains_missing_float(z,npts,has_missing_z,missing_z.floatval) ||
     contains_missing_float(u,size_u,has_missing_u,missing_u.floatval) ||
     contains_missing_float(xo,nxo,has_missing_xo,missing_xo.floatval) ||
     contains_missing_float(yo,nyo,has_missing_yo,missing_yo.floatval) ||
     contains_missing_float(zo,nzo,has_missing_zo,missing_zo.floatval)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid3s: the input arrays cannot contain any missing values" );
    return(NhlFATAL);
  }

/*
 * Calculate space for output array and its dimension sizes.
 */
  ndims_uo  = ndims_u + 2;
  size_uo   = size_leftmost * nuo;
  uo        = (float *) calloc(size_uo, sizeof(float));
  dsizes_uo = (ng_size_t *) calloc(ndims_uo, sizeof(ng_size_t));
  type_size_uo = nuo * sizeof(float);

  if(uo == NULL || dsizes_uo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "dsgrid3s: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_uo-3; i++ ) dsizes_uo[i] = dsizes_u[i];
  dsizes_uo[ndims_uo-3] = nxo;
  dsizes_uo[ndims_uo-2] = nyo;
  dsizes_uo[ndims_uo-1] = nzo;

/*
 * The following section loops through the leftmost dimensions and calls
 * the c_dsgrid3s function.
 */
  for( i = 0; i < size_leftmost; i++ ) {
    tmp_uo = c_dsgrid3s(inpts,x,y,z,&u[index_u],inxo,inyo,inzo,xo,yo,zo,&ier);
    if(ier) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid3s: ier = %d", ier);
      NclFree(tmp_uo);
      return(NhlFATAL);
    }
    memcpy((void*)((char*)uo+i*type_size_uo),
           (void*)((char*)tmp_uo),type_size_uo);

    index_u  += npts;
    index_uo += nuo;
    NclFree(tmp_uo);
  }
  ret = NclReturnValue((void*)uo,ndims_uo,dsizes_uo,NULL,NCL_float,0);
  NclFree(dsizes_uo);
  return(ret);
}


NhlErrorTypes dsgrid3_W( void )
{
/* 
 * Input values
 */
  void *x, *y, *z, *u;
  double *tmp_x, *tmp_y, *tmp_z;
  double *tmp_u = NULL;
  ng_size_t dsizes_x[1];
  int has_missing_x;
  ng_size_t dsizes_y[1];
  int has_missing_y;
  ng_size_t dsizes_z[1];
  int has_missing_z;
  int ndims_u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  int has_missing_u;
  NclBasicDataTypes type_x, type_y, type_z, type_u;
  NclScalar missing_x, missing_y, missing_z, missing_u;
  NclScalar missing_dx, missing_dy, missing_dz, missing_du, missing_ru;
/*
 * Output grid to interpolate on.
 */
  void *xo, *yo, *zo;
  double *tmp_xo, *tmp_yo, *tmp_zo;
  ng_size_t dsizes_xo[1];
  int has_missing_xo;
  ng_size_t dsizes_yo[1];
  int has_missing_yo;
  ng_size_t dsizes_zo[1];
  int has_missing_zo;
  NclBasicDataTypes type_xo, type_yo, type_zo;
  NclScalar missing_xo, missing_yo, missing_zo;
  NclScalar missing_dxo, missing_dyo, missing_dzo;
/*
 * Output variables.
 */
  void *uo;
  double *tmp_uo;
  int ndims_uo;
  ng_size_t *dsizes_uo;
  NclBasicDataTypes type_uo;
  NclScalar missing_uo, missing_duo;
/*
 * Various
 */
  ng_size_t i, nuo, size_leftmost, size_u, size_uo;
  ng_size_t index_u = 0, index_uo = 0;
  ng_size_t npts, nxo, nyo, nzo;
  int inpts, inxo, inyo, inzo, ier = 0, nmiss = 0, ret;

/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept numerics.
 */
  x = (void*)NclGetArgValue(
                             0,
                             7,
                             NULL,
                             dsizes_x,
                             &missing_x,
                             &has_missing_x,
                             &type_x,
                             DONT_CARE);
  y = (void*)NclGetArgValue(
                             1,
                             7,
                             NULL,
                             dsizes_y,
                             &missing_y,
                             &has_missing_y,
                             &type_y,
                             DONT_CARE);
  z = (void*)NclGetArgValue(
                             2,
                             7,
                             NULL,
                             dsizes_z,
                             &missing_z,
                             &has_missing_z,
                             &type_z,
                             DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if(dsizes_x[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid3: the length of x is > INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_x[0];
  inpts = (int) npts;

/*
 * Check dimension sizes for x, y, and z.
 */
  if(dsizes_y[0] != npts || dsizes_z[0] != npts) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid3: x, y, and z must be the same length");
    return(NhlFATAL);
  }

/*
 * Get u.
 */
  u = (void*)NclGetArgValue(
                             3,
                             7,
                             &ndims_u,
                             dsizes_u,
                             &missing_u,
                             &has_missing_u,
                             &type_u,
                             DONT_CARE);

/*
 * Check rightmost dimension size for u.
 */
  if(dsizes_u[ndims_u-1] != npts) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid3: the last (rightmost) dimension of u must be the same length as x, y, and z");
    return(NhlFATAL);
  }

/*
 * Get rest of parameters.
 */

  xo = (void*)NclGetArgValue(
                             4,
                             7,
                             NULL,
                             dsizes_xo,
                             &missing_xo,
                             &has_missing_xo,
                             &type_xo,
                             DONT_CARE);
  yo = (void*)NclGetArgValue(
                             5,
                             7,
                             NULL,
                             dsizes_yo,
                             &missing_yo,
                             &has_missing_yo,
                             &type_yo,
                             DONT_CARE);
  zo = (void*)NclGetArgValue(
                             6,
                             7,
                             NULL,
                             dsizes_zo,
                             &missing_zo,
                             &has_missing_zo,
                             &type_zo,
                             DONT_CARE);
/*
 * Test the dimension sizes.
 */
  if( (dsizes_xo[0] > INT_MAX) || (dsizes_yo[0] > INT_MAX) ||
      (dsizes_zo[0] > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid3: the length of xo, yo, and/or zo is greater than INT_MAX");
    return(NhlFATAL);
  }
  nxo  = dsizes_xo[0];
  nyo  = dsizes_yo[0];
  nzo  = dsizes_zo[0];
  inxo = (int) nxo;
  inyo = (int) nyo;
  inzo = (int) nzo;
  nuo = nxo * nyo * nzo;

/*
 * Compute the total size of the input and the leftmost dimensions.
 */

  size_leftmost = 1;
  for( i = 0; i < ndims_u-1; i++ ) size_leftmost *= dsizes_u[i];
  size_u = size_leftmost * npts;

/*
 * Coerce missing values.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,NULL);
  coerce_missing(type_y,has_missing_y,&missing_y,&missing_dy,NULL);
  coerce_missing(type_z,has_missing_z,&missing_z,&missing_dz,NULL);
  coerce_missing(type_u,has_missing_u,&missing_u,&missing_du,&missing_ru);
  coerce_missing(type_xo,has_missing_xo,&missing_xo,&missing_dxo,NULL);
  coerce_missing(type_yo,has_missing_yo,&missing_yo,&missing_dyo,NULL);
  coerce_missing(type_zo,has_missing_zo,&missing_zo,&missing_dzo,NULL);
/*
 * Coerce input arrays to double, if necessary.
 */
  tmp_x  = coerce_input_double(x,type_x,npts,has_missing_x,&missing_x,
                               &missing_dx);
  tmp_y  = coerce_input_double(y,type_y,npts,has_missing_y,&missing_y,
                               &missing_dy);
  tmp_z  = coerce_input_double(z,type_z,npts,has_missing_z,&missing_z,
                               &missing_dz);
  tmp_xo = coerce_input_double(xo,type_xo,nxo,has_missing_xo,&missing_xo,
                               &missing_dxo);
  tmp_yo = coerce_input_double(yo,type_yo,nyo,has_missing_yo,&missing_yo,
                               &missing_dyo);
  tmp_zo = coerce_input_double(zo,type_zo,nzo,has_missing_zo,&missing_zo,
                               &missing_dzo);
/*
 * Allocate space for temporary input array. The temporary array
 * tmp_u is just big enough to hold a 1-dimensional subsection of the
 * u array. We only need to allocate space for it if the
 * input is not already double. Otherwise, we just have it point
 * to the appropriate locations in u.
 */
  if(type_u != NCL_double) {
    tmp_u = (double*)calloc(npts,sizeof(double));
    if(tmp_u == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid3: Unable to allocate memory for coercing u array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Check for missing values. 
 */ 
  if(contains_missing(tmp_x,npts,has_missing_x,missing_dx.doubleval) ||
     contains_missing(tmp_y,npts,has_missing_y,missing_dy.doubleval) ||
     contains_missing(tmp_z,npts,has_missing_z,missing_dz.doubleval) ||
     contains_missing(tmp_xo,nxo,has_missing_xo,missing_dxo.doubleval) ||
     contains_missing(tmp_yo,nyo,has_missing_yo,missing_dyo.doubleval) ||
     contains_missing(tmp_zo,nzo,has_missing_zo,missing_dzo.doubleval)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid3: the input arrays cannot contain any missing values" );
    return(NhlFATAL);
  }

/*
 * Calculate space for output array and its dimension sizes.
 */
  ndims_uo  = ndims_u + 2;
  size_uo   = size_leftmost * nuo;
  dsizes_uo = (ng_size_t *) calloc(ndims_uo, sizeof(ng_size_t));

  if(type_u != NCL_double) {
    type_uo = NCL_float;
    uo = (void*)calloc(size_uo,sizeof(float));
  }
  else {
    type_uo = NCL_double;
    uo = (void*)calloc(size_uo,sizeof(double));
  }
  if(uo == NULL || dsizes_uo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "dsgrid3: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  if(has_missing_u) {
    if(type_uo == NCL_double) missing_uo = missing_du;
    else                      missing_uo = missing_ru;
    missing_duo = missing_du;
  }

  for( i = 0; i < ndims_uo-3; i++ ) dsizes_uo[i] = dsizes_u[i];
  dsizes_uo[ndims_uo-3] = nxo;
  dsizes_uo[ndims_uo-2] = nyo;
  dsizes_uo[ndims_uo-1] = nzo;

/*
 * The following section loops through the leftmost dimensions and calls
 * the c_dsgrid3d function.
 */
  for( i = 0; i < size_leftmost; i++ ) {
    if(type_u != NCL_double) {
/*
 * Coerce npts subsection of u (tmp_u) to double.
 */
      coerce_subset_input_double(u,tmp_u,index_u,type_u,npts,has_missing_u,
                                 &missing_u,&missing_du);
    }
    else {
/*
 * Point tmp_u to appropriate location in u.
 */
      tmp_u = &((double*)u)[index_u];
    }
/*
 * Check for missing values in u.
 */
    if(contains_missing(tmp_u,npts,has_missing_u,missing_du.doubleval)) {
      nmiss++;
/*
 * Set all elements of this 2D grid to a missing value, if a missing
 * value exists.
 */
      set_subset_output_missing(uo,index_uo,type_uo,nuo,missing_duo.doubleval);
    }
    else {
/*
 * Call c_dsgrid3d.
 */
      tmp_uo = c_dsgrid3d(inpts,tmp_x,tmp_y,tmp_z,tmp_u,inxo,inyo,inzo,
                          tmp_xo,tmp_yo,tmp_zo,&ier);
/*
 * Check for errors.
 */
      if(ier) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid3: ier = %d", ier);
/*
 * Free arrays
 */
        if(type_x  != NCL_double) NclFree(tmp_x);
        if(type_y  != NCL_double) NclFree(tmp_y);
        if(type_z  != NCL_double) NclFree(tmp_z);
        if(type_u  != NCL_double) NclFree(tmp_u);
        if(type_xo != NCL_double) NclFree(tmp_xo);
        if(type_yo != NCL_double) NclFree(tmp_yo);
        if(type_zo != NCL_double) NclFree(tmp_zo);
        NclFree(tmp_uo);
        return(NhlFATAL);
      }
      coerce_output_float_or_double(uo,tmp_uo,type_uo,nuo,index_uo);
      index_u  += npts;
      index_uo += nuo;
      NclFree(tmp_uo);
    }
  }
/*
 * Check if u array had missing values. If so, print a warning message.
 */
  if(nmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"dsgrid2: %d 1-dimensional input arrays contained missing values. No interpolation performed on these arrays",nmiss);
  }
/*
 * Free arrays
 */
  if(type_x  != NCL_double) NclFree(tmp_x);
  if(type_y  != NCL_double) NclFree(tmp_y);
  if(type_z  != NCL_double) NclFree(tmp_z);
  if(type_u  != NCL_double) NclFree(tmp_u);
  if(type_xo != NCL_double) NclFree(tmp_xo);
  if(type_yo != NCL_double) NclFree(tmp_yo);
  if(type_zo != NCL_double) NclFree(tmp_zo);

  if(has_missing_u) {
    ret = NclReturnValue(uo,ndims_uo,dsizes_uo,&missing_uo,type_uo,0);
  }
  else {
    ret = NclReturnValue(uo,ndims_uo,dsizes_uo,NULL,type_uo,0);
  }
  NclFree(dsizes_uo);
  return(ret);
}


NhlErrorTypes dsgrid3d_W( void )
{
  int ier = 0;
  double *x;
  ng_size_t dsizes_x[1];
  int has_missing_x;
  double *y;
  ng_size_t dsizes_y[1];
  int has_missing_y;
  double *z;
  ng_size_t dsizes_z[1];
  int has_missing_z;
  double *u;
  int ndims_u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  int has_missing_u;
  double *xo;
  ng_size_t dsizes_xo[1];
  int has_missing_xo;
  double *yo;
  ng_size_t dsizes_yo[1];
  int has_missing_yo;
  double *zo;
  ng_size_t dsizes_zo[1];
  int has_missing_zo;
  NclScalar missing_x, missing_y, missing_z, missing_u;
  NclScalar missing_xo, missing_yo, missing_zo;
  double *uo, *tmp_uo;
  int ndims_uo;
  ng_size_t *dsizes_uo;
  ng_size_t i, nuo;
  ng_size_t npts, nxo, nyo, nzo;
  int inpts, inxo, inyo, inzo, type_size_uo;
  ng_size_t size_leftmost, size_u, size_uo;
  int ret;
  ng_size_t index_u = 0, index_uo = 0;

/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 */
  x = (double*)NclGetArgValue(
                             0,
                             7,
                             NULL,
                             dsizes_x,
                             &missing_x,
                             &has_missing_x,
                             NULL,
                             DONT_CARE);
  y = (double*)NclGetArgValue(
                             1,
                             7,
                             NULL,
                             dsizes_y,
                             &missing_y,
                             &has_missing_y,
                             NULL,
                             DONT_CARE);
  z = (double*)NclGetArgValue(
                             2,
                             7,
                             NULL,
                             dsizes_z,
                             &missing_z,
                             &has_missing_z,
                             NULL,
                             DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if(dsizes_x[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid3d: the length of x is > INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_x[0];
  inpts = (int) npts;

/*
 * Check dimension sizes for x, y, and z.
 */
  if(dsizes_y[0] != npts || dsizes_z[0] != npts) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid3d: x, y, and z must be the same length");
    return(NhlFATAL);
  }

/*
 * Get u.
 */
  u = (double*)NclGetArgValue(
                             3,
                             7,
                             &ndims_u,
                             dsizes_u,
                             &missing_u,
                             &has_missing_u,
                             NULL,
                             DONT_CARE);
/*
 * Check rightmost dimension size for u.
 */
  if(dsizes_u[ndims_u-1] != npts) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid3: the last (rightmost) dimension of u must be the same length as x, y, and z");
    return(NhlFATAL);
  }

/*
 * Compute the total size of the input and the leftmost dimensions.
 */

  size_leftmost = 1;
  for( i = 0; i < ndims_u-1; i++ ) size_leftmost *= dsizes_u[i];
  size_u = size_leftmost * npts;

/*
 * Get rest of parameters.
 */

  xo = (double*)NclGetArgValue(
                             4,
                             7,
                             NULL,
                             dsizes_xo,
                             &missing_xo,
                             &has_missing_xo,
                             NULL,
                             DONT_CARE);
  yo = (double*)NclGetArgValue(
                             5,
                             7,
                             NULL,
                             dsizes_yo,
                             &missing_yo,
                             &has_missing_yo,
                             NULL,
                             DONT_CARE);

  zo = (double*)NclGetArgValue(
                             6,
                             7,
                             NULL,
                             dsizes_zo,
                             &missing_zo,
                             &has_missing_zo,
                             NULL,
                             DONT_CARE);
/*
 * Test the dimension sizes.
 */
  if( (dsizes_xo[0] > INT_MAX) || (dsizes_yo[0] > INT_MAX) ||
      (dsizes_zo[0] > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid3d: the length of xo, yo, and/or zo is greater than INT_MAX");
    return(NhlFATAL);
  }
  nxo  = dsizes_xo[0];
  nyo  = dsizes_yo[0];
  nzo  = dsizes_zo[0];
  inxo = (int) nxo;
  inyo = (int) nyo;
  inzo = (int) nzo;
  nuo = nxo * nyo * nzo;

/*
 * Check for missing values. 
 */
  if(contains_missing(x,npts,has_missing_x,missing_x.doubleval) ||
     contains_missing(y,npts,has_missing_y,missing_y.doubleval) ||
     contains_missing(z,npts,has_missing_z,missing_z.doubleval) ||
     contains_missing(u,size_u,has_missing_u,missing_u.doubleval) ||
     contains_missing(xo,nxo,has_missing_xo,missing_xo.doubleval) ||
     contains_missing(yo,nyo,has_missing_yo,missing_yo.doubleval) ||
     contains_missing(zo,nzo,has_missing_zo,missing_zo.doubleval)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid3d: the input arrays cannot contain any missing values" );
    return(NhlFATAL);
  }

/*
 * Calculate space for output array and its dimension sizes.
 */
  ndims_uo  = ndims_u + 2;
  size_uo   = size_leftmost * nuo;
  uo        = (double *) calloc(size_uo, sizeof(double));
  dsizes_uo = (ng_size_t *) calloc(ndims_uo, sizeof(ng_size_t));
  type_size_uo = nuo * sizeof(double);

  if(uo == NULL || dsizes_uo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "dsgrid3d: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_uo-3; i++ ) dsizes_uo[i] = dsizes_u[i];
  dsizes_uo[ndims_uo-3] = nxo;
  dsizes_uo[ndims_uo-2] = nyo;
  dsizes_uo[ndims_uo-1] = nzo;

/*
 * The following section loops through the leftmost dimensions and calls
 * the c_dsgrid3d function.
 */
  for( i = 0; i < size_leftmost; i++ ) {
    tmp_uo = c_dsgrid3d(inpts,x,y,z,&u[index_u],inxo,inyo,inzo,xo,yo,zo,&ier);
    if(ier) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid3d: ier = %d", ier);
      NclFree(tmp_uo);
      return(NhlFATAL);
    }
    memcpy((void*)((char*)uo+i*type_size_uo),
           (void*)((char*)tmp_uo),type_size_uo);

    index_u  += npts;
    index_uo += nuo;
    NclFree(tmp_uo);
  }
  ret = NclReturnValue((void*)uo,ndims_uo,dsizes_uo,NULL,NCL_double,0);
  NclFree(dsizes_uo);
  return(ret);
}


NhlErrorTypes dssetp_W(void)
{

  char  *arg1, *cval;
  int   numpi, numpf, numpc, i;

/*
 *  List the integer and float parameter names.  To add new ones,
 *  all that needs to be done is add the names to this list.
 */
  char *params_i[] = {"shd", "SHD"};
  char *params_f[] = {"dmv", "dmx", "exp", "DMV", "DMX", "EXP"};
  char *params_c[] = {"erf", "ERF"};

/*
 * Input array variables
 */
  NrmQuark *pname;
  int ndims_pname;
  ng_size_t dsizes_pname[NCL_MAX_DIMENSIONS];
  void *pvalue;
  int ndims_pvalue;
  ng_size_t dsizes_pvalue[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_pname, type_pvalue;

/*
 * Retrieve argument #1
 */
  pname = (NrmQuark *) NclGetArgValue(
          0,
          2,
          &ndims_pname,
          dsizes_pname,
          NULL,
          NULL,
          &type_pname,
          DONT_CARE);

/*
 * Check number of dimensions for argument #1.
 */
  if(ndims_pname != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "dssetp: Argument #1 has the wrong number of dimensions.");
    return(NhlFATAL);
  }
  arg1 = NrmQuarkToString(*pname);
 
/*
 *  Check to see if the parameter name is valid.
 */
  numpi = sizeof(params_i)/sizeof(void *);
  numpf = sizeof(params_f)/sizeof(void *);
  numpc = sizeof(params_c)/sizeof(void *);
  for (i = 0; i < numpi; i++) {
    if (!strncmp(arg1, params_i[i], strlen(params_i[i]))) {
      goto OK_NAME;
    }
  }
  for (i = 0; i < numpf; i++) {
    if (!strncmp(arg1, params_f[i], strlen(params_f[i]))) {
      goto OK_NAME;
    }
  }
  for (i = 0; i < numpc; i++) {
    if (!strncmp(arg1, params_c[i], strlen(params_c[i]))) {
      goto OK_NAME;
    }
  }
  NhlPError(NhlFATAL, NhlEUNKNOWN, "dssetp: unrecognized parameter name");
  return(NhlFATAL);

/*
 * Retrieve argument #2
 */
OK_NAME: pvalue = (void *) NclGetArgValue(
           1,
           2,
           &ndims_pvalue,
           dsizes_pvalue,
           NULL,
           NULL,
           &type_pvalue,
           DONT_CARE);

/*
 *  Process the parameter if it has an integer value.
 */
  if (type_pvalue == NCL_int) {
    for (i = 0; i < numpi; i++) {
      if (!strncmp(arg1, params_i[i], strlen(params_i[i]))) {
        c_dsseti(arg1, *((int *) pvalue));
        return(NhlNOERROR);
      }
    }
    NhlPError(NhlFATAL, NhlEUNKNOWN, "The specified value for the parameter "
              "has an invalid type");
    return(NhlFATAL);
  }
  else if (type_pvalue == NCL_float || type_pvalue == NCL_double) {

/*
 *  Process the parameter if it has a float value or double value.
 */
    for (i = 0; i < numpf; i++) {
      if (!strncmp(arg1, params_f[i], strlen(params_f[i]))) {
        if (type_pvalue == NCL_float) {
          c_dssetr(arg1, *((float *) pvalue));
          return(NhlNOERROR);
        }
        else if (type_pvalue == NCL_double) {
          c_dssetrd(arg1, *((double *) pvalue));
          return(NhlNOERROR);
        }
      }
    }
    NhlPError(NhlFATAL, NhlEUNKNOWN, "The specified value for the parameter "
              "has an invalid type");
    return(NhlFATAL);
  }
  else if (type_pvalue == NCL_string) {

/*
 *  Process the parameter if it has a string value.
 */
    for (i = 0; i < numpc; i++) {
      if (!strncmp(arg1, params_c[i], strlen(params_c[i]))) {
        cval = NrmQuarkToString( *((NrmQuark *) pvalue));
        c_dssetc(arg1, cval);
        return(NhlNOERROR);
      }
    }
    NhlPError(NhlFATAL, NhlEUNKNOWN, "The specified value for the parameter "
              "has an invalid type");
    return(NhlFATAL);
  }
  return(NhlNOERROR);
}

NhlErrorTypes dsgetp_W(void)
{
/*
 *  Get values for dsgrid parameters.
 */

  char  *arg1, *cval;
  int   numpi, numpf, numpc, i;
  NrmQuark *qvalue;

/*
 *  List the integer and float parameter names.  To add new ones,
 *  all that needs to be done is add the names to this list.
 */
  char *params_i[] = {"shd", "SHD"};
  char *params_f[] = {"dmv", "dmx", "exp", "DMV", "DMX", "EXP"};
  char *params_c[] = {"erf", "ERF"};

/*
 * Input array variable
 */
  NrmQuark *pname;
  int ndims_pname;
  ng_size_t dsizes_pname[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_pname;
  float *fval;
  int *ival;
  ng_size_t ret_size = 1; 

/*
 * Retrieve argument #1
 */
  pname = (NrmQuark *) NclGetArgValue(
          0,
          1,
          &ndims_pname,
          dsizes_pname,
          NULL,
          NULL,
          &type_pname,
          DONT_CARE);

/*
 * Check number of dimensions for argument #1.
 */
  if(ndims_pname != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "dsgetp: Argument #1 has the wrong number of dimensions.");
    return(NhlFATAL);
  }
  arg1 = NrmQuarkToString(*pname);

/*
 *  Check to see if the parameter name is valid.
 */
  numpi = sizeof(params_i)/sizeof(void *);
  numpf = sizeof(params_f)/sizeof(void *);
  numpc = sizeof(params_c)/sizeof(void *);
  for (i = 0; i < numpi; i++) {
    if (!strncmp(arg1, params_i[i], strlen(params_i[i]))) {
      goto OK_NAME;
    }
  }
  for (i = 0; i < numpf; i++) {
    if (!strncmp(arg1, params_f[i], strlen(params_f[i]))) {
      goto OK_NAME;
    }
  }
  for (i = 0; i < numpc; i++) {
    if (!strncmp(arg1, params_c[i], strlen(params_c[i]))) {
      goto OK_NAME;
    }
  }
  NhlPError(NhlFATAL, NhlEUNKNOWN, "dssetp: unrecognized parameter name");
  return(NhlFATAL);

/*
 *  Process the parameter if it has an integer value.
 */
OK_NAME:  for (i = 0; i < numpi; i++) {
    if (!strncmp(arg1, params_i[i], strlen(params_i[i]))) {
      ival = (int *) calloc(1,sizeof(int));
      c_dsgeti(arg1, ival);
      return(NclReturnValue( (void *) ival, 1, &ret_size, NULL, NCL_int, 0));
    }
  }

/*
 *  Process the parameter if it has a float value.
 */
  for (i = 0; i < numpf; i++) {
    if (!strncmp(arg1, params_f[i], strlen(params_f[i]))) {
      fval = (float *) calloc(1,sizeof(float));
      c_dsgetr(arg1, fval);
      return(NclReturnValue((void *) fval, 1, &ret_size, NULL, NCL_float, 0));
    }
  }

/*
 *  Process the parameter if it has a string value.
 */
  for (i = 0; i < numpc; i++) {
    if (!strncmp(arg1, params_c[i], strlen(params_c[i]))) {
      cval = (char *) calloc(100,sizeof(char));
      if (cval == NULL) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
             "dsgetp: unable to allocate memory for return string");
        return(NhlFATAL);
      }
      c_dsgetc(arg1, cval);
      qvalue = (NrmQuark *) calloc(1,sizeof(NrmQuark));
      *qvalue = NrmStringToQuark(cval);
      return(NclReturnValue((void *) qvalue, 1, &ret_size, NULL,NCL_string, 1));
    }
  }
  return(NhlNOERROR);
}


NhlErrorTypes dspnt2s_W( void )
{
  float *x;
  ng_size_t dsizes_x[1];
  int has_missing_x;
  float *y;
  ng_size_t dsizes_y[1];
  int has_missing_y;
  float *z;
  int ndims_z;
  ng_size_t dsizes_z[NCL_MAX_DIMENSIONS];
  int has_missing_z;
  float *xo;
  ng_size_t dsizes_xo[1];
  int has_missing_xo;
  float *yo;
  ng_size_t dsizes_yo[1];
  int has_missing_yo;
  float *zo;
  int ndims_zo;
  ng_size_t dsizes_zo[NCL_MAX_DIMENSIONS];
  NclScalar missing_x, missing_y, missing_z, missing_xo, missing_yo;
  ng_size_t i, size_leftmost, size_z, index_z, index_zo;
  ng_size_t npts, nptso;
  int inpts, inptso, ier;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 */
  x = (float*)NclGetArgValue(
                             0,
                             6,
                             NULL,
                             dsizes_x,
                             &missing_x,
                             &has_missing_x,
                             NULL,
                             DONT_CARE);
  y = (float*)NclGetArgValue(
                             1,
                             6,
                             NULL,
                             dsizes_y,
                             &missing_y,
                             &has_missing_y,
                             NULL,
                             DONT_CARE);
  z = (float*)NclGetArgValue(
                             2,
                             6,
                             &ndims_z,
                             dsizes_z,
                             &missing_z,
                             &has_missing_z,
                             NULL,
                             DONT_CARE);

  xo = (float*)NclGetArgValue(
                             3,
                             6,
                             NULL,
                             dsizes_xo,
                             &missing_xo,
                             &has_missing_xo,
                             NULL,
                             DONT_CARE);
  yo = (float*)NclGetArgValue(
                             4,
                             6,
                             NULL,
                             dsizes_yo,
                             &missing_yo,
                             &has_missing_yo,
                             NULL,
                             DONT_CARE);
  zo = (float*)NclGetArgValue(
                             5,
                             6,
                             &ndims_zo,
                             dsizes_zo,
                             NULL,
                             NULL,
                             NULL,
                             DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if( (dsizes_x[0] > INT_MAX) || (dsizes_xo[0] > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt2s: the length of x and/or xo is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts   = dsizes_x[0];
  nptso  = dsizes_xo[0];
  inpts  = (int) npts;
  inptso = (int) nptso;

/*
 * Make sure z and zo have the same number of dimensions.
 */
  if(ndims_z != ndims_zo) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt2s: z and zo must have the same number of dimensions");
    return(NhlFATAL);
  }
/*
 * Calculate leftmost dimensions of z/zo, and make sure they are
 * the same.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_z-1; i++ ) {
    if(dsizes_z[i] != dsizes_zo[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt2s: z and zo must have the same leftmost dimension sizes");
      return(NhlFATAL);
    }
    size_leftmost *= dsizes_z[i];
  }
  size_z = size_leftmost * npts;

/*
 * Check dimension sizes.
 */
   if(dsizes_y[0]  == npts  && dsizes_z[ndims_z-1]   == npts &&
      dsizes_yo[0] == nptso && dsizes_zo[ndims_zo-1] == nptso) {
/*
 * Check for missing values. 
 */
     if(contains_missing_float(x,npts,has_missing_x,missing_x.floatval) || 
        contains_missing_float(y,npts,has_missing_y,missing_y.floatval) ||
        contains_missing_float(z,size_z,has_missing_z,missing_z.floatval) ||
        contains_missing_float(xo,nptso,has_missing_xo,missing_xo.floatval) ||
        contains_missing_float(yo,nptso,has_missing_yo,missing_yo.floatval)) {
       NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt2s: the input arrays cannot contain any missing values" );
       return(NhlFATAL);
     }
/*
 * The following section loops through the leftmost dimensions and calls
 * the c_dspnt2s function.
 */
     index_z = index_zo = 0;
     for( i = 0; i < size_leftmost; i++ ) {
       c_dspnt2s(inpts,x,y,&z[index_z],inptso,xo,yo,&zo[index_zo],&ier);
       if(ier) {
         NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt2s: ier = %d", ier);
         return(NhlFATAL);
       }
       index_z  += npts;
       index_zo += nptso;
     }
   }
   else {
     NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt2s: the dimension sizes of parameters x and y, and xo and yo must be identical");
     return(NhlFATAL);
   }
   return(NhlNOERROR);
}


NhlErrorTypes dspnt2d_W( void )
{
  double *x;
  ng_size_t dsizes_x[1];
  int has_missing_x;
  double *y;
  ng_size_t dsizes_y[1];
  int has_missing_y;
  double *z;
  int ndims_z;
  ng_size_t dsizes_z[NCL_MAX_DIMENSIONS];
  int has_missing_z;
  double *xo;
  ng_size_t dsizes_xo[1];
  int has_missing_xo;
  double *yo;
  ng_size_t dsizes_yo[1];
  int has_missing_yo;
  double *zo;
  int ndims_zo;
  ng_size_t dsizes_zo[NCL_MAX_DIMENSIONS];
  NclScalar missing_x, missing_y, missing_z, missing_xo, missing_yo;
  int ier = 0;
  ng_size_t index_z, index_zo;
  ng_size_t i, size_leftmost, size_z;
  ng_size_t npts, nptso;
  int inpts, inptso;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 */
  x = (double*)NclGetArgValue(
                             0,
                             6,
                             NULL,
                             dsizes_x,
                             &missing_x,
                             &has_missing_x,
                             NULL,
                             DONT_CARE);
  y = (double*)NclGetArgValue(
                             1,
                             6,
                             NULL,
                             dsizes_y,
                             &missing_y,
                             &has_missing_y,
                             NULL,
                             DONT_CARE);
  z = (double*)NclGetArgValue(
                             2,
                             6,
                             &ndims_z,
                             dsizes_z,
                             &missing_z,
                             &has_missing_z,
                             NULL,
                             DONT_CARE);

  xo = (double*)NclGetArgValue(
                             3,
                             6,
                             NULL,
                             dsizes_xo,
                             &missing_xo,
                             &has_missing_xo,
                             NULL,
                             DONT_CARE);
  yo = (double*)NclGetArgValue(
                             4,
                             6,
                             NULL,
                             dsizes_yo,
                             &missing_yo,
                             &has_missing_yo,
                             NULL,
                             DONT_CARE);
  zo = (double*)NclGetArgValue(
                             5,
                             6,
                             &ndims_zo,
                             dsizes_zo,
                             NULL,
                             NULL,
                             NULL,
                             DONT_CARE);
/*
 * Test the dimension sizes.
 */
  if( (dsizes_x[0] > INT_MAX) || (dsizes_xo[0] > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt2d: the length of x and/or xo is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts   = dsizes_x[0];
  nptso  = dsizes_xo[0];
  inpts  = (int) npts;
  inptso = (int) nptso;

/*
 * Make sure z and zo have the same number of dimensions.
 */
  if(ndims_z != ndims_zo) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt2d: z and zo must have the same number of dimensions");
    return(NhlFATAL);
  }
/*
 * Calculate leftmost dimensions of z/zo, and make sure they are
 * the same.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_z-1; i++ ) {
    if(dsizes_z[i] != dsizes_zo[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt2s: z and zo must have the same leftmost dimension sizes");
      return(NhlFATAL);
    }
    size_leftmost *= dsizes_z[i];
  }
  size_z = size_leftmost * npts;

/*
 * Check dimension sizes.
 */
   if(dsizes_y[0]  == npts  && dsizes_z[ndims_z-1]   == npts &&
      dsizes_yo[0] == nptso && dsizes_zo[ndims_zo-1] == nptso) {
/*
 * Check for missing values. 
 */
     if(contains_missing(x,npts,has_missing_x,missing_x.doubleval) || 
        contains_missing(y,npts,has_missing_y,missing_y.doubleval) ||
        contains_missing(z,size_z,has_missing_z,missing_z.doubleval) ||
        contains_missing(xo,nptso,has_missing_xo,missing_xo.doubleval) ||
        contains_missing(yo,nptso,has_missing_yo,missing_yo.doubleval)) {
       NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt2d: the input arrays cannot contain any missing values" );
       return(NhlFATAL);
     }
/*
 * The following section loops through the leftmost dimensions and calls
 * the c_dspnt2d function.
 */
     index_z = index_zo = 0;
     for( i = 0; i < size_leftmost; i++ ) {
       c_dspnt2d(inpts,x,y,&z[index_z],inptso,xo,yo,&zo[index_zo],&ier);
       if(ier) {
         NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt2d: ier = %d", ier);
         return(NhlFATAL);
       }
       index_z  += npts;
       index_zo += nptso;
     }
   }
   else {
     NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt2d: the dimension sizes of parameters x, y, and z, and xo and yo must be identical");
     return(NhlFATAL);
   }
   return(NhlNOERROR);
}


NhlErrorTypes dspnt2_W( void )
{
  void *x, *y, *z;
  ng_size_t dsizes_x[1];
  int has_missing_x;
  ng_size_t dsizes_y[1];
  int has_missing_y;
  int ndims_z;
  ng_size_t dsizes_z[NCL_MAX_DIMENSIONS];
  int has_missing_z;
  void *xo, *yo, *zo;
  ng_size_t dsizes_xo[1];
  int has_missing_xo;
  ng_size_t dsizes_yo[1];
  int has_missing_yo;
  ng_size_t *dsizes_zo;
  NclScalar missing_x, missing_y, missing_z, missing_xo, missing_yo;
  NclScalar missing_dx, missing_dy, missing_dz, missing_dxo, missing_dyo;
  NclBasicDataTypes type_x, type_y, type_z, type_xo, type_yo, type_zo;
/*
 * Temporary arrays.
 */
  double *tmp_x, *tmp_y, *tmp_xo, *tmp_yo;
  double *tmp_z = NULL;
  double *tmp_zo = NULL;
  ng_size_t i, npts, nptso, size_z, size_zo, size_leftmost;
  int inpts, inptso, ier = 0, ret;
  ng_size_t index_z, index_zo;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  x = (void*)NclGetArgValue(
                            0,
                            5,
                            NULL,
                            dsizes_x,
                            &missing_x,
                            &has_missing_x,
                            &type_x,
                            DONT_CARE);
  y = (void*)NclGetArgValue(
                            1,
                            5,
                            NULL,
                            dsizes_y,
                            &missing_y,
                            &has_missing_y,
                            &type_y,
                            DONT_CARE);
  z = (void*)NclGetArgValue(
                            2,
                            5,
                            &ndims_z,
                            dsizes_z,
                            &missing_z,
                            &has_missing_z,
                            &type_z,
                            DONT_CARE);
  
  xo = (void*)NclGetArgValue(
                             3,
                             5,
                             NULL,
                             dsizes_xo,
                             &missing_xo,
                             &has_missing_xo,
                             &type_xo,
                             DONT_CARE);
  yo = (void*)NclGetArgValue(
                             4,
                             5,
                             NULL,
                             dsizes_yo,
                             &missing_yo,
                             &has_missing_yo,
                             &type_yo,
                             DONT_CARE);
/*
 * Test the dimension sizes.
 */
  if( (dsizes_x[0] > INT_MAX) || (dsizes_xo[0] > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt2: the length of x and/or xo is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts   = dsizes_x[0];
  nptso  = dsizes_xo[0];
  inpts  = (int) npts;
  inptso = (int) nptso;

/*
 * Check dimensions.
 */
  if(dsizes_y[0] != npts || dsizes_z[ndims_z-1] != npts) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt2: the rightmost dimension of z must be the same as the dimension of x and y");
     return(NhlFATAL);
  }

  if(dsizes_yo[0] != nptso) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt2: xo and yo must be the same length");
     return(NhlFATAL);
  }

/*
 * Calculate size of z and zo.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_z-1; i++ ) size_leftmost *= dsizes_z[i];
  size_z  = size_leftmost * npts;
  size_zo = size_leftmost * nptso;

  
/*
 * Coerce missing values.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,NULL);
  coerce_missing(type_y,has_missing_y,&missing_y,&missing_dy,NULL);
  coerce_missing(type_z,has_missing_z,&missing_z,&missing_dz,NULL);
  coerce_missing(type_xo,has_missing_xo,&missing_xo,&missing_dxo,NULL);
  coerce_missing(type_yo,has_missing_yo,&missing_yo,&missing_dyo,NULL);

/*
 * Allocate space for temporary z array. The temporary array
 * tmp_z is just big enough to hold a 1-dimensional subsection of the
 * z array. We only need to allocate space for it if the
 * input is not already double. Otherwise, we just have it point
 * to the appropriate locations in z later.
 */
  if(type_z != NCL_double) {
    tmp_z = (double*)calloc(npts,sizeof(double));
    if(tmp_z == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt2: Unable to allocate memory for coercing z array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Coerce other arrays to double, if necessary.
 */
  tmp_x  = coerce_input_double(x,type_x,npts,has_missing_x,&missing_x,
                               &missing_dx);
  tmp_y  = coerce_input_double(y,type_y,npts,has_missing_y,&missing_y,
                               &missing_dy);
  tmp_xo = coerce_input_double(xo,type_xo,nptso,has_missing_xo,&missing_xo,
                               &missing_dxo);
  tmp_yo = coerce_input_double(yo,type_yo,nptso,has_missing_yo,&missing_yo,
                               &missing_dyo);

  if(tmp_x == NULL || tmp_y == NULL || tmp_xo == NULL || tmp_yo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt2: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Check for missing values. 
 */
  if(contains_missing(tmp_x,npts,has_missing_x,missing_dx.doubleval) ||
     contains_missing(tmp_y,npts,has_missing_y,missing_dy.doubleval) ||
     contains_missing(tmp_xo,nptso,has_missing_xo,missing_dxo.doubleval) ||
     contains_missing(tmp_yo,nptso,has_missing_yo,missing_dyo.doubleval)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt2: the input arrays cannot contain any missing values" );
    return(NhlFATAL);
  }

/*
 * Allocate space for temporary output array. We only need to allocate
 * space for it if the input is not already double. Otherwise, we just
 * have it point to the appropriate location in zo.
 */
  if( type_x != NCL_double &&  type_y != NCL_double && type_z != NCL_double &&
     type_xo != NCL_double && type_yo != NCL_double) {
    type_zo = NCL_float;
    zo     = (void*)calloc(size_zo,sizeof(float));
    tmp_zo = (double*)calloc(nptso,sizeof(double));
    if(tmp_zo == NULL || zo == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt2: Unable to allocate memory for coercing zo array to double precision");
      return(NhlFATAL);
    }
  }
  else {
    type_zo = NCL_double;
    zo = (void*)calloc(size_zo,sizeof(double));
    if(zo == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt2: Unable to allocate memory for coercing zo array to double precision");
      return(NhlFATAL);
    }
  }
/*
 * Allocate array to hold dimension sizes for zo, and fill it up.
 */
  dsizes_zo = (ng_size_t *)calloc(ndims_z, sizeof(ng_size_t));
  if(dsizes_zo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt2: Unable to allocate memory dimension size array");
    return(NhlFATAL);
  }

  for( i = 0; i <= ndims_z-2; i++ ) dsizes_zo[i] = dsizes_z[i];
  dsizes_zo[ndims_z-1] = nptso;

/*
 * The following section calls the c_dspnt2d function.
 * Loop through the left most dimensions and call c_dspnt2d for
 * each one. 
 */
  index_z = index_zo = 0;

  for(i = 0; i < size_leftmost; i++) {
    if(type_z != NCL_double) {
/*
 * Coerce npts subsection of z (tmp_z) to double.
 */
      coerce_subset_input_double(z,tmp_z,index_z,type_z,npts,has_missing_z,
                                 &missing_z,&missing_dz);
    }
    else {
/*
 * Point tmp_z to appropriate location in z.
 */
      tmp_z = &((double*)z)[index_z];
    }
    if(type_zo == NCL_double) {
/*
 * Point tmp_zo to appropriate location in zo.
 */
      tmp_zo = &((double*)zo)[index_zo];
    }
/*
 * Check for missing values.
 */
    if(contains_missing(tmp_z,npts,has_missing_z,missing_dz.doubleval)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt2: the input arrays cannot contain any missing values" );
      return(NhlFATAL);
    }
/*
 * Call c_dspnt2d.
 */
    c_dspnt2d(inpts,tmp_x,tmp_y,tmp_z,inptso,tmp_xo,tmp_yo,tmp_zo,&ier);
/*
 * Check for errors.
 */
    if(ier) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt2: ier = %d", ier);
      return(NhlFATAL);
    }
/*
 * Coerce output back to float if necessary.
 */
    if(type_zo != NCL_double) {
      coerce_output_float_only(zo,tmp_zo,inptso,index_zo);
    }

    index_z  += npts;
    index_zo += nptso;
  }


/*
 * Free arrays
 */
  if(type_x  != NCL_double) NclFree(tmp_x);
  if(type_y  != NCL_double) NclFree(tmp_y);
  if(type_z  != NCL_double) NclFree(tmp_z);
  if(type_xo != NCL_double) NclFree(tmp_xo);
  if(type_yo != NCL_double) NclFree(tmp_yo);
  if(type_zo != NCL_double) NclFree(tmp_zo);

  ret = NclReturnValue(zo,ndims_z,dsizes_zo,NULL,type_zo,0);
  NclFree(dsizes_zo);
  return(ret);
}

NhlErrorTypes dspnt3s_W( void )
{
  float *x;
  ng_size_t dsizes_x[1];
  int has_missing_x;
  float *y;
  ng_size_t dsizes_y[1];
  int has_missing_y;
  float *z;
  ng_size_t dsizes_z[1];
  int has_missing_z;
  float *u;
  int ndims_u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  int has_missing_u;
  float *xo;
  ng_size_t dsizes_xo[1];
  int has_missing_xo;
  float *yo;
  ng_size_t dsizes_yo[1];
  int has_missing_yo;
  float *zo;
  ng_size_t dsizes_zo[1];
  int has_missing_zo;
  float *uo;
  int ndims_uo;
  ng_size_t dsizes_uo[NCL_MAX_DIMENSIONS];
  NclScalar missing_x, missing_y, missing_z, missing_u;
  NclScalar missing_xo, missing_yo, missing_zo;
  ng_size_t i, size_leftmost, size_u;
  ng_size_t npts, nptso, index_u, index_uo;
  int inpts, inptso, ier = 0;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 */
  x = (float*)NclGetArgValue(
                             0,
                             8,
                             NULL,
                             dsizes_x,
                             &missing_x,
                             &has_missing_x,
                             NULL,
                             DONT_CARE);
  y = (float*)NclGetArgValue(
                             1,
                             8,
                             NULL,
                             dsizes_y,
                             &missing_y,
                             &has_missing_y,
                             NULL,
                             DONT_CARE);
  z = (float*)NclGetArgValue(
                             2,
                             8,
                             NULL,
                             dsizes_z,
                             &missing_z,
                             &has_missing_z,
                             NULL,
                             DONT_CARE);

  u = (float*)NclGetArgValue(
                             3,
                             8,
                             &ndims_u,
                             dsizes_u,
                             &missing_u,
                             &has_missing_u,
                             NULL,
                             DONT_CARE);

  xo = (float*)NclGetArgValue(
                             4,
                             8,
                             NULL,
                             dsizes_xo,
                             &missing_xo,
                             &has_missing_xo,
                             NULL,
                             DONT_CARE);
  yo = (float*)NclGetArgValue(
                             5,
                             8,
                             NULL,
                             dsizes_yo,
                             &missing_yo,
                             &has_missing_yo,
                             NULL,
                             DONT_CARE);
  zo = (float*)NclGetArgValue(
                             6,
                             8,
                             NULL,
                             dsizes_zo,
                             &missing_zo,
                             &has_missing_zo,
                             NULL,
                             DONT_CARE);

  uo = (float*)NclGetArgValue(
                             7,
                             8,
                             &ndims_uo,
                             dsizes_uo,
                             NULL,
                             NULL,
                             NULL,
                             DONT_CARE);
/*
 * Test the dimension sizes.
 */
  if( (dsizes_x[0] > INT_MAX) || (dsizes_xo[0] > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt3s: the length of x and/or xo is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts   = dsizes_x[0];
  nptso  = dsizes_xo[0];
  inpts  = (int) npts;
  inptso = (int) nptso;

/*
 * Make sure u and uo have the same number of dimensions.
 */
  if(ndims_u != ndims_uo) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt3s: u and uo must have the same number of dimensions");
    return(NhlFATAL);
  }
/*
 * Calculate leftmost dimensions of u/uo, and make sure they are
 * the same.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_u-1; i++ ) {
    if(dsizes_u[i] != dsizes_uo[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt3s: u and uo must have the same leftmost dimension sizes");
      return(NhlFATAL);
    }
    size_leftmost *= dsizes_u[i];
  }
  size_u = size_leftmost * npts;

/*
 * Check dimension sizes.
 */
   if(dsizes_y[0]  == npts  && dsizes_z[0]  == npts  && 
      dsizes_yo[0] == nptso && dsizes_zo[0] == nptso &&
      dsizes_u[ndims_u-1] == npts && dsizes_uo[ndims_uo-1] == nptso) {
/*
 * Check for missing values. 
 */
     if(contains_missing_float(x,npts,has_missing_x,missing_x.floatval) ||
        contains_missing_float(y,npts,has_missing_y,missing_y.floatval) ||
        contains_missing_float(z,npts,has_missing_z,missing_z.floatval) ||
        contains_missing_float(u,size_u,has_missing_u,missing_u.floatval) ||
        contains_missing_float(xo,nptso,has_missing_xo,missing_xo.floatval) ||
        contains_missing_float(yo,nptso,has_missing_yo,missing_yo.floatval) ||
        contains_missing_float(zo,nptso,has_missing_zo,missing_zo.floatval)) {
       NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt3s: the input arrays cannot contain any missing values" );
       return(NhlFATAL);
     }
/*
 * The following section calls the c_dspnt3s function.
 */
     index_u = index_uo = 0;
     for( i = 0; i < size_leftmost; i++ ) {
       c_dspnt3s(inpts,x,y,z,&u[index_u],inptso,xo,yo,zo,&uo[index_uo],&ier);
       if(ier) {
         NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt3s: ier = %d", ier);
         return(NhlFATAL);
       }
       index_u  += npts;
       index_uo += nptso;
     }
   }
   else {
     NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt3s: the dimension sizes of parameters x, y, and z, and xo and yo must be identical");
     return(NhlFATAL);
   }
   return(NhlNOERROR);
}


NhlErrorTypes dspnt3d_W( void )
{
  double *x;
  ng_size_t dsizes_x[1];
  int has_missing_x;
  double *y;
  ng_size_t dsizes_y[1];
  int has_missing_y;
  double *z;
  ng_size_t dsizes_z[1];
  int has_missing_z;
  double *u;
  int ndims_u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  int has_missing_u;
  double *xo;
  ng_size_t dsizes_xo[1];
  int has_missing_xo;
  double *yo;
  ng_size_t dsizes_yo[1];
  int has_missing_yo;
  double *zo;
  ng_size_t dsizes_zo[1];
  int has_missing_zo;
  double *uo;
  int ndims_uo;
  ng_size_t dsizes_uo[NCL_MAX_DIMENSIONS];
  NclScalar missing_x, missing_y, missing_z, missing_u;
  NclScalar missing_xo, missing_yo, missing_zo;
  int ier = 0;
  ng_size_t index_u, index_uo;
  ng_size_t i, npts, nptso, size_leftmost, size_u;
  int inpts, inptso;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 */
  x = (double*)NclGetArgValue(
                             0,
                             8,
                             NULL,
                             dsizes_x,
                             &missing_x,
                             &has_missing_x,
                             NULL,
                             DONT_CARE);
  y = (double*)NclGetArgValue(
                             1,
                             8,
                             NULL,
                             dsizes_y,
                             &missing_y,
                             &has_missing_y,
                             NULL,
                             DONT_CARE);
  z = (double*)NclGetArgValue(
                             2,
                             8,
                             NULL,
                             dsizes_z,
                             &missing_z,
                             &has_missing_z,
                             NULL,
                             DONT_CARE);

  u = (double*)NclGetArgValue(
                             3,
                             8,
                             &ndims_u,
                             dsizes_u,
                             &missing_u,
                             &has_missing_u,
                             NULL,
                             DONT_CARE);

  xo = (double*)NclGetArgValue(
                             4,
                             8,
                             NULL,
                             dsizes_xo,
                             &missing_xo,
                             &has_missing_xo,
                             NULL,
                             DONT_CARE);
  yo = (double*)NclGetArgValue(
                             5,
                             8,
                             NULL,
                             dsizes_yo,
                             &missing_yo,
                             &has_missing_yo,
                             NULL,
                             DONT_CARE);
  zo = (double*)NclGetArgValue(
                             6,
                             8,
                             NULL,
                             dsizes_zo,
                             &missing_zo,
                             &has_missing_zo,
                             NULL,
                             DONT_CARE);

  uo = (double*)NclGetArgValue(
                             7,
                             8,
                             &ndims_uo,
                             dsizes_uo,
                             NULL,
                             NULL,
                             NULL,
                             DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if( (dsizes_x[0] > INT_MAX) || (dsizes_xo[0] > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt3d: the length of x and/or xo is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts   = dsizes_x[0];
  nptso  = dsizes_xo[0];
  inpts  = (int) npts;
  inptso = (int) nptso;

/*
 * Make sure u and uo have the same number of dimensions.
 */
  if(ndims_u != ndims_uo) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt3d: u and uo must have the same number of dimensions");
    return(NhlFATAL);
  }
/*
 * Calculate leftmost dimensions of u/uo, and make sure they are
 * the same.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_u-1; i++ ) {
    if(dsizes_u[i] != dsizes_uo[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt3d: u and uo must have the same lefmost dimension sizes");
      return(NhlFATAL);
    }
    size_leftmost *= dsizes_u[i];
  }
  size_u = size_leftmost * npts;

/*
 * Check dimension sizes.
 */
   if(dsizes_y[0]  == npts  && dsizes_z[0]  == npts  && 
      dsizes_yo[0] == nptso && dsizes_zo[0] == nptso &&
      dsizes_u[ndims_u-1] == npts && dsizes_uo[ndims_uo-1] == nptso) {
/*
 * Check for missing values. 
 */
     if(contains_missing(x,npts,has_missing_x,missing_x.doubleval) ||
        contains_missing(y,npts,has_missing_y,missing_y.doubleval) ||
        contains_missing(z,npts,has_missing_z,missing_z.doubleval) ||
        contains_missing(u,size_u,has_missing_u,missing_u.doubleval) ||
        contains_missing(xo,nptso,has_missing_xo,missing_xo.doubleval) ||
        contains_missing(yo,nptso,has_missing_yo,missing_yo.doubleval) ||
        contains_missing(zo,nptso,has_missing_zo,missing_zo.doubleval)) {
       NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt3d: the input arrays cannot contain any missing values" );
       return(NhlFATAL);
     }
/*
 * The following section calls the c_dspnt3d function.
 */
     index_u = index_uo = 0;
     for( i = 0; i < size_leftmost; i++ ) {
       c_dspnt3d(inpts,x,y,z,&u[index_u],inptso,xo,yo,zo,&uo[index_uo],&ier);
       if(ier) {
         NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt3d: ier = %d", ier);
         return(NhlFATAL);
       }
       index_u  += npts;
       index_uo += nptso;
     }
   }
   else {
     NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt3d: the dimension sizes of parameters x, y, and z, and xo and yo must be identical");
     return(NhlFATAL);
   }
   return(NhlNOERROR);
}

NhlErrorTypes dspnt3_W( void )
{
  void *x, *y, *z, *u;
  ng_size_t dsizes_x[1];
  int has_missing_x;
  ng_size_t dsizes_y[1];
  int has_missing_y;
  ng_size_t dsizes_z[1];
  int has_missing_z;
  int ndims_u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  int has_missing_u;
  void *xo, *yo, *zo, *uo;
  ng_size_t dsizes_xo[1];
  int has_missing_xo;
  ng_size_t dsizes_yo[1];
  int has_missing_yo;
  ng_size_t dsizes_zo[1];
  int has_missing_zo;
  ng_size_t *dsizes_uo;
  NclScalar missing_x, missing_y, missing_z, missing_u;
  NclScalar missing_xo, missing_yo, missing_zo;
  NclScalar missing_dx, missing_dy, missing_dz, missing_du;
  NclScalar missing_dxo, missing_dyo, missing_dzo;
  NclBasicDataTypes type_x, type_y, type_z, type_u;
  NclBasicDataTypes type_xo, type_yo, type_zo, type_uo;
/*
 * Temporary arrays.
 */
  double *tmp_x, *tmp_y, *tmp_z, *tmp_xo, *tmp_yo, *tmp_zo;
  double *tmp_u = NULL;
  double *tmp_uo = NULL;
  ng_size_t i, npts, nptso, size_u, size_uo, size_leftmost;
  int inpts, inptso;
  int ier = 0, ret;
  ng_size_t index_u, index_uo;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  x = (void*)NclGetArgValue(
                            0,
                            7,
                            NULL,
                            dsizes_x,
                            &missing_x,
                            &has_missing_x,
                            &type_x,
                            DONT_CARE);

  y = (void*)NclGetArgValue(
                            1,
                            7,
                            NULL,
                            dsizes_y,
                            &missing_y,
                            &has_missing_y,
                            &type_y,
                            DONT_CARE);

  z = (void*)NclGetArgValue(
                            2,
                            7,
                            NULL,
                            dsizes_z,
                            &missing_z,
                            &has_missing_z,
                            &type_z,
                            DONT_CARE);
  
  u = (void*)NclGetArgValue(
                            3,
                            7,
                            &ndims_u,
                            dsizes_u,
                            &missing_u,
                            &has_missing_u,
                            &type_u,
                            DONT_CARE);
  
  xo = (void*)NclGetArgValue(
                             4,
                             7,
                             NULL,
                             dsizes_xo,
                             &missing_xo,
                             &has_missing_xo,
                             &type_xo,
                             DONT_CARE);
  yo = (void*)NclGetArgValue(
                             5,
                             7,
                             NULL,
                             dsizes_yo,
                             &missing_yo,
                             &has_missing_yo,
                             &type_yo,
                             DONT_CARE);

  zo = (void*)NclGetArgValue(
                             6,
                             7,
                             NULL,
                             dsizes_zo,
                             &missing_zo,
                             &has_missing_zo,
                             &type_zo,
                             DONT_CARE);
/*
 * Test the dimension sizes.
 */
  if( (dsizes_x[0] > INT_MAX) || (dsizes_xo[0] > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt3: the length of x and/or xo is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts   = dsizes_x[0];
  nptso  = dsizes_xo[0];
  inpts  = (int) npts;
  inptso = (int) nptso;

/*
 * Check dimensions.
 */
  if(dsizes_y[0] != npts || dsizes_z[0] != npts || dsizes_u[ndims_u-1] != npts) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt3: the rightmost dimension of u must be the same as the dimension of x, y, and z");
     return(NhlFATAL);
  }

  if(dsizes_yo[0] != nptso || dsizes_zo[0] != nptso) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt3: xo, yo, and zo must be the same length");
     return(NhlFATAL);
  }

/*
 * Calculate size of u and uo.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_u-1; i++ ) size_leftmost *= dsizes_u[i];
  size_u  = size_leftmost * npts;
  size_uo = size_leftmost * nptso;

/*
 * Coerce missing values.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,NULL);
  coerce_missing(type_y,has_missing_y,&missing_y,&missing_dy,NULL);
  coerce_missing(type_z,has_missing_z,&missing_z,&missing_dz,NULL);
  coerce_missing(type_u,has_missing_u,&missing_u,&missing_du,NULL);
  coerce_missing(type_xo,has_missing_xo,&missing_xo,&missing_dxo,NULL);
  coerce_missing(type_yo,has_missing_yo,&missing_yo,&missing_dyo,NULL);
  coerce_missing(type_zo,has_missing_zo,&missing_zo,&missing_dzo,NULL);

/*
 * Allocate space for temporary u array. The temporary array
 * tmp_u is just big enough to hold a 1-dimensional subsection of the
 * u array. We only need to allocate space for it if the
 * input is not already double. Otherwise, we just have it point
 * to the appropriate locations in u later.
 */
  if(type_u != NCL_double) {
    tmp_u = (double*)calloc(npts,sizeof(double));
    if(tmp_u == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt3: Unable to allocate memory for coercing u array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Coerce other arrays to double, if necessary.
 */
  tmp_x  = coerce_input_double(x,type_x,npts,has_missing_x,&missing_x,
                               &missing_dx);
  tmp_y  = coerce_input_double(y,type_y,npts,has_missing_y,&missing_y,
                               &missing_dy);
  tmp_z  = coerce_input_double(z,type_z,npts,has_missing_z,&missing_z,
                               &missing_dz);
  tmp_xo = coerce_input_double(xo,type_xo,nptso,has_missing_xo,&missing_xo,
                               &missing_dxo);
  tmp_yo = coerce_input_double(yo,type_yo,nptso,has_missing_yo,&missing_yo,
                               &missing_dyo);
  tmp_zo = coerce_input_double(zo,type_zo,nptso,has_missing_zo,&missing_zo,
                               &missing_dzo);

  if(tmp_x == NULL  || tmp_y == NULL  || tmp_z == NULL ||
     tmp_xo == NULL || tmp_yo == NULL || tmp_zo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt3: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Check for missing values. 
 */
  if(contains_missing(tmp_x,npts,has_missing_x,missing_dx.doubleval) ||
     contains_missing(tmp_y,npts,has_missing_y,missing_dy.doubleval) ||
     contains_missing(tmp_z,npts,has_missing_z,missing_dz.doubleval) ||
     contains_missing(tmp_xo,nptso,has_missing_xo,missing_dxo.doubleval) ||
     contains_missing(tmp_yo,nptso,has_missing_yo,missing_dyo.doubleval) ||
     contains_missing(tmp_zo,nptso,has_missing_zo,missing_dzo.doubleval)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt3: the input arrays cannot contain any missing values" );
    return(NhlFATAL);
  }

/*
 * Allocate space for temporary output array. We only need to allocate
 * space for it if the input is not already double. Otherwise, we just
 * have it point to the appropriate location in zo.
 */
  if(type_x  != NCL_double && type_y  != NCL_double && 
     type_z  != NCL_double && type_u  != NCL_double &&
     type_xo != NCL_double && type_yo != NCL_double && type_zo !=NCL_double) {
    type_uo = NCL_float;
    uo     = (void*)calloc(size_uo,sizeof(float));
    tmp_uo = (double*)calloc(nptso,sizeof(double));
    if(tmp_uo == NULL || uo == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt3: Unable to allocate memory for coercing uo array to double precision");
      return(NhlFATAL);
    }
  }
  else {
    type_uo = NCL_double;
    uo = (void*)calloc(size_uo,sizeof(double));
    if(uo == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt3: Unable to allocate memory for coercing uo array to double precision");
      return(NhlFATAL);
    }
  }
/*
 * Allocate array to hold dimension sizes for uo, and fill it up.
 */
  dsizes_uo = (ng_size_t *)calloc(ndims_u, sizeof(ng_size_t));
  if(dsizes_uo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt3: Unable to allocate memory dimension size array");
    return(NhlFATAL);
  }

  for( i = 0; i <= ndims_u-2; i++ ) dsizes_uo[i] = dsizes_u[i];
  dsizes_uo[ndims_u-1] = nptso;

/*
 * The following section calls the c_dspnt3d function.
 * Loop through the left most dimensions and call c_dspnt3d for
 * each one. 
 */
  index_u = index_uo = 0;

  for(i = 0; i < size_leftmost; i++) {
    if(type_u != NCL_double) {
/*
 * Coerce npts subsection of u (tmp_u) to double.
 */
      coerce_subset_input_double(u,tmp_u,index_u,type_u,npts,has_missing_u,
                                 &missing_u,&missing_du);
    }
    else {
/*
 * Point tmp_u to appropriate location in u.
 */
      tmp_u = &((double*)u)[index_u];
    }
    if(type_uo == NCL_double) {
/*
 * Point tmp_uo to appropriate location in uo.
 */
      tmp_uo = &((double*)uo)[index_uo];
    }
/*
 * Check for missing values.
 */
    if(contains_missing(tmp_u,npts,has_missing_u,missing_du.doubleval)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt3: the input arrays cannot contain any missing values" );
      return(NhlFATAL);
    }
/*
 * Call c_dspnt3d.
 */
    c_dspnt3d(inpts,tmp_x,tmp_y,tmp_z,tmp_u,inptso,tmp_xo,tmp_yo,tmp_zo,
              tmp_uo,&ier);

/*
 * Check for errors.
 */
    if(ier) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt3: ier = %d", ier);
      return(NhlFATAL);
    }
/*
 * Coerce output back to float if necessary.
 */
    if(type_uo != NCL_double) {
      coerce_output_float_only(uo,tmp_uo,nptso,index_uo);
    }
    index_u  += npts;
    index_uo += nptso;
  }

/*
 * Free arrays
 */
  if(type_x  != NCL_double) NclFree(tmp_x);
  if(type_y  != NCL_double) NclFree(tmp_y);
  if(type_z  != NCL_double) NclFree(tmp_z);
  if(type_u  != NCL_double) NclFree(tmp_u);
  if(type_xo != NCL_double) NclFree(tmp_xo);
  if(type_yo != NCL_double) NclFree(tmp_yo);
  if(type_zo != NCL_double) NclFree(tmp_zo);
  if(type_uo != NCL_double) NclFree(tmp_uo);

  ret = NclReturnValue(uo,ndims_u,dsizes_uo,NULL,type_uo,0);
  NclFree(dsizes_uo);
  return(ret);
  
}


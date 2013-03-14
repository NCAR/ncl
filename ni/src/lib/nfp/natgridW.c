#include <string.h>
#include <stdio.h>
#include "wrapper.h"

extern void drwsrfc (int WKID, int nx, int ny, float *x, float *y, float *z,
                     float s1, float s2, float s3, int *iwk);
extern void drwvctc (int WKID, int lx, int ly, float *u, float *v);

extern void drwconc (int WKID, int i, int j, float *z);

NhlErrorTypes natgrids_W( void )
{
  int ier = 0;
  float *x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  float *y;
  ng_size_t dsizes_y[NCL_MAX_DIMENSIONS];
  int has_missing_y;
  float *z;
  int ndims_z;
  ng_size_t dsizes_z[NCL_MAX_DIMENSIONS];
  int has_missing_z;
  float *xo;
  ng_size_t dsizes_xo[NCL_MAX_DIMENSIONS];
  int has_missing_xo;
  float *yo;
  ng_size_t dsizes_yo[NCL_MAX_DIMENSIONS];
  int has_missing_yo;
  NclScalar missing_x, missing_y, missing_z, missing_xo, missing_yo;
  float *zo, *zo_tmp;
  int ndims_zo;
  ng_size_t *dsizes_zo;
  ng_size_t i, j, npts, nxo, nyo, nzo, size_leftmost, size_input, size_output;
  int inpts, inxo, inyo, ret;
  ng_size_t index_in = 0, index_out = 0;

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
 * Check dimension sizes for x and y.
 */
  if(dsizes_x[0] != dsizes_y[0]) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"natgrids: x and y must be the same length");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes. 
 */
  if(dsizes_x[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"natgrids: the length of x is > INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_x[0];
  inpts = (int) npts;

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
    NhlPError(NhlFATAL,NhlEUNKNOWN,"natgrids: the last (rightmost) dimension of z must be the same length as x and y");
    return(NhlFATAL);
  }

/*
 * Compute the total size of the input and the leftmost dimensions.
 */

  size_leftmost = 1;
  for( i = 0; i < ndims_z-1; i++ ) size_leftmost *= dsizes_z[i];
  size_input = size_leftmost * npts;

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
 * Test dimension sizes. 
 */
  if((dsizes_xo[0] > INT_MAX) || (dsizes_yo[0] > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"natgrids: the length of xo and/or yo is > INT_MAX");
    return(NhlFATAL);
  }
  nxo  = dsizes_xo[0];
  nyo  = dsizes_yo[0];
  inxo = (int) nxo;
  inyo = (int) nyo;

  nzo = nxo * nyo;

/*
 * Check for missing values. 
 */
  if(contains_missing_float(x,npts,has_missing_x,missing_x.floatval)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"natgrids: x cannot contain any missing values" );
    return(NhlFATAL);
  }
  if(contains_missing_float(y,npts,has_missing_y,missing_y.floatval)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"natgrids: y cannot contain any missing values" );
    return(NhlFATAL);
  }
  if(contains_missing_float(z,size_input,has_missing_z,missing_z.floatval)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"natgrids: z cannot contain any missing values" );
    return(NhlFATAL);
  }
  if(contains_missing_float(xo,nxo,has_missing_xo,missing_xo.floatval)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"natgrids: xo cannot contain any missing values" );
    return(NhlFATAL);
  }
  if(contains_missing_float(yo,nyo,has_missing_yo,missing_yo.floatval)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"natgrids: yo cannot contain any missing values" );
    return(NhlFATAL);
  }

/*
 * Calculate space for output array and its dimension sizes.
 */
  ndims_zo    = ndims_z + 1;
  size_output = size_leftmost * nzo;
  zo          = (float *) calloc(size_output, sizeof(float));
  dsizes_zo   = (ng_size_t *) calloc(ndims_zo, sizeof(ng_size_t));

  if(zo == NULL || dsizes_zo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "natgrids: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_zo-2; i++ ) dsizes_zo[i] = dsizes_z[i];
  dsizes_zo[ndims_zo-2] = nxo;
  dsizes_zo[ndims_zo-1] = nyo;

/*
 * The following section loops through the leftmost dimensions and calls
 * the c_natgrids function.
 */
  for( i = 0; i < size_leftmost; i++ ) {
    zo_tmp = c_natgrids (inpts,x,y,&z[index_in],inxo,inyo,xo,yo,&ier);

    if(!ier || (ier >= 4 && ier <= 6)) {
      if(ier) {
        NhlPError(NhlWARNING,NhlEUNKNOWN,"natgrids: ier = %d", ier);
      }
      for (j = 0; j < nzo; j++) {
        zo[index_out+j] = zo_tmp[j];
      }    
    }
    else {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"natgrids: ier = %d", ier);
      free(zo_tmp);
      return(NhlFATAL);
    }
    index_in  += npts;
    index_out += nzo;
    free(zo_tmp);
  }
  ret = NclReturnValue((void*)zo,ndims_zo,dsizes_zo,NULL,NCL_float,0);
  NclFree(dsizes_zo);
  return(ret);
}

NhlErrorTypes natgridd_W( void )
{
  int ier = 0;
  double *x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  double *y;
  ng_size_t dsizes_y[NCL_MAX_DIMENSIONS];
  int has_missing_y;
  double *z;
  int ndims_z;
  ng_size_t dsizes_z[NCL_MAX_DIMENSIONS];
  int has_missing_z;
  double *xo;
  ng_size_t dsizes_xo[NCL_MAX_DIMENSIONS];
  int has_missing_xo;
  double *yo;
  ng_size_t dsizes_yo[NCL_MAX_DIMENSIONS];
  int has_missing_yo;
  NclScalar missing_x, missing_y, missing_z, missing_xo, missing_yo;
  double *zo, *zo_tmp;
  int ndims_zo;
  ng_size_t *dsizes_zo;
  ng_size_t i, j, npts, nxo, nyo, nzo, size_leftmost, size_input, size_output;
  int inpts, inxo, inyo, ret;
  ng_size_t index_in = 0, index_out = 0;

/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept doubles.
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
 * Check dimension sizes for x and y.
 */
  if(dsizes_x[0] != dsizes_y[0]) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"natgridd: x and y must be the same length");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes. 
 */
  if(dsizes_x[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"natgridd: the length of x is > INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_x[0];
  inpts = (int) npts;

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
    NhlPError(NhlFATAL,NhlEUNKNOWN,"natgridd: the last (rightmost) dimension of z must be the same length as x and y");
    return(NhlFATAL);
  }

/*
 * Compute the total size of the input and the leftmost dimensions.
 */

  size_leftmost = 1;
  for( i = 0; i < ndims_z-1; i++ ) size_leftmost *= dsizes_z[i];
  size_input = size_leftmost * npts;

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
 * Test dimension sizes. 
 */
  if((dsizes_xo[0] > INT_MAX) || (dsizes_yo[0] > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"natgridd: the length of xo and/or yo is > INT_MAX");
    return(NhlFATAL);
  }
  nxo  = dsizes_xo[0];
  nyo  = dsizes_yo[0];
  inxo = (int) nxo;
  inyo = (int) nyo;

  nzo = nxo * nyo;

/*
 * Check for missing values. 
 */
  if(contains_missing(x,npts,has_missing_x,missing_x.doubleval)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"natgridd: x cannot contain any missing values" );
    return(NhlFATAL);
  }
  if(contains_missing(y,npts,has_missing_y,missing_y.doubleval)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"natgridd: y cannot contain any missing values" );
    return(NhlFATAL);
  }
  if(contains_missing(z,size_input,has_missing_z,missing_z.doubleval)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"natgridd: z cannot contain any missing values" );
    return(NhlFATAL);
  }
  if(contains_missing(xo,nxo,has_missing_xo,missing_xo.doubleval)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"natgridd: xo cannot contain any missing values" );
    return(NhlFATAL);
  }
  if(contains_missing(yo,nyo,has_missing_yo,missing_yo.doubleval)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"natgridd: yo cannot contain any missing values" );
    return(NhlFATAL);
  }

/*
 * Calculate space for output array and its dimension sizes.
 */
  ndims_zo    = ndims_z + 1;
  size_output = size_leftmost * nzo;
  zo          = (double *) calloc(size_output, sizeof(double));
  dsizes_zo   = (ng_size_t *) calloc(ndims_zo, sizeof(ng_size_t));

  if(zo == NULL || dsizes_zo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "natgridd: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_zo-2; i++ ) dsizes_zo[i] = dsizes_z[i];
  dsizes_zo[ndims_zo-2] = nxo;
  dsizes_zo[ndims_zo-1] = nyo;

/*
 * The following section loops through the leftmost dimensions and calls
 * the c_natgridd function.
 */
  for( i = 0; i < size_leftmost; i++ ) {
    zo_tmp = c_natgridd (inpts,x,y,&z[index_in],inxo,inyo,xo,yo,&ier);

    if(!ier || (ier >= 4 && ier <= 6)) {
      if(ier) {
        NhlPError(NhlWARNING,NhlEUNKNOWN,"natgridd: ier = %d", ier);
      }
      for (j = 0; j < nzo; j++) {
        zo[index_out+j] = zo_tmp[j];
      }    
    }
    else {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"natgridd: ier = %d", ier);
      free(zo_tmp);
      return(NhlFATAL);
    }
    index_in  += npts;
    index_out += nzo;
    free(zo_tmp);
  }
  ret = NclReturnValue((void*)zo,ndims_zo,dsizes_zo,NULL,NCL_double,0);
  NclFree(dsizes_zo);
  return(ret);
}

NhlErrorTypes natgrid_W( void )
{
/* 
 * Input values
 */
  void *x, *y, *z;
  double *tmp_x, *tmp_y;
  double *tmp_z = NULL;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  ng_size_t dsizes_y[NCL_MAX_DIMENSIONS];
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
  ng_size_t dsizes_xo[NCL_MAX_DIMENSIONS];
  int has_missing_xo;
  ng_size_t dsizes_yo[NCL_MAX_DIMENSIONS];
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
  ng_size_t i, npts, nxo, nyo, nzo, size_leftmost, size_input, size_output;
  int inpts, inxo, inyo;
  int ier = 0, nmiss = 0, ret; 
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
 * Check dimension sizes for x and y.
 */
  if(dsizes_x[0] != dsizes_y[0]) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"natgrid: x and y must be the same length");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes. 
 */
  if(dsizes_x[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"natgrid: the length of x is > INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_x[0];
  inpts = (int) npts;

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
    NhlPError(NhlFATAL,NhlEUNKNOWN,"natgrid: the last (rightmost) dimension of z must be the same length as x and y");
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
 * Test dimension sizes. 
 */
  if((dsizes_xo[0] > INT_MAX) || (dsizes_yo[0] > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"natgrid: the length of xo and/or yo is > INT_MAX");
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
  size_input = size_leftmost * npts;

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
      NhlPError(NhlFATAL,NhlEUNKNOWN,"natgrid: Unable to allocate memory for coercing z array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Check for missing values.
 */
  if(contains_missing(tmp_x,npts,has_missing_x,missing_dx.doubleval)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"natgrid: x cannot contain any missing values" );
    return(NhlFATAL);
  }
  if(contains_missing(tmp_y,npts,has_missing_y,missing_dy.doubleval)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"natgrid: y cannot contain any missing values" );
    return(NhlFATAL);
  }
  if(contains_missing(tmp_xo,nxo,has_missing_xo,missing_dxo.doubleval)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"natgrid: xo cannot contain any missing values" );
    return(NhlFATAL);
  }
  if(contains_missing(tmp_yo,nyo,has_missing_yo,missing_dyo.doubleval)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"natgrid: yo cannot contain any missing values" );
    return(NhlFATAL);
  }

/*
 * Calculate space for output array and its dimension sizes.
 */
  ndims_zo    = ndims_z + 1;
  size_output = size_leftmost * nzo;
  dsizes_zo   = (ng_size_t *) calloc(ndims_zo, sizeof(ng_size_t));

  if(type_z != NCL_double) {
    type_zo = NCL_float;
    zo = (void*)calloc(size_output,sizeof(float));
  }
  else {
    type_zo = NCL_double;
    zo = (void*)calloc(size_output,sizeof(double));
  }
  if(zo == NULL || dsizes_zo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "natgrid: Unable to allocate memory for output array");
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
 * the c_natgridd function.
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
 * Call c_natgridd.
 */
      tmp_zo = c_natgridd (inpts,tmp_x,tmp_y,tmp_z,inxo,inyo,tmp_xo,tmp_yo,&ier);
/*
 * Check for errors.
 */
      if(!ier || (ier >= 4 && ier <= 6)) {
        if(ier) {
          NhlPError(NhlWARNING,NhlEUNKNOWN,"natgrid: ier = %d", ier);
        }
	coerce_output_float_or_double(zo,tmp_zo,type_zo,nzo,index_zo);
        index_z  += npts;
        index_zo += nzo;
        NclFree(tmp_zo);
      }
      else {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"natgrid: ier = %d", ier);
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
    }
  }
/*
 * Check if z array had missing values. If so, print a warning message.
 */
  if(nmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"natgrid: %d 1-dimensional input array contained missing values. No interpolation performed on these arrays",nmiss);
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

NhlErrorTypes nnsetp_W(void)
{

  char  *arg1, *cval;
  int   numpi, numpf, numpc, i;

/*
 *  List the integer and float parameter names.  To add new ones,
 *  all that needs to be done is add the names to this list.
 */
  char *params_i[] = {"adf", "asc", "dup", "ext", "igr", "non", "rad", 
                      "sdi", "upd", "mdm", "ADF", "ASC", "DUP", "EXT", 
                      "IGR", "NON", "RAD", "SDI", "UPD", "MDM"};
/*
 *  The parameters "xas", "yas", and "zas" are not in the following
 *  list, since they are for retrieval only.
 */
  char *params_f[] = {"bI", "bJ", "hor", "magx", "magy", "magz",
                      "nul", "ver", "Bi", "Bj", "HOR", "MAGX", "MAGY", 
                      "MAGZ", "NUL", "VER", "bi", "bj", "BI", "BJ"};
  char *params_c[] = {"alg", "ALG", "erf", "ERF"};

/*
 * Input array variables
 */
  NrmQuark *pname;
  ng_size_t dsizes_pname[NCL_MAX_DIMENSIONS];
  void *pvalue;
  ng_size_t dsizes_pvalue[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_pvalue;

/*
 * Retrieve argument #1
 */
  pname = (NrmQuark *) NclGetArgValue(
          0,
          2,
          NULL,
          dsizes_pname,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

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
  NhlPError(NhlFATAL, NhlEUNKNOWN, "nnsetp: unrecognized parameter name");
  return(NhlFATAL);

/*
 * Retrieve argument #2
 */
OK_NAME:  pvalue = (void *) NclGetArgValue(
           1,
           2,
           NULL,
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
        c_nnseti(arg1, *((int *) pvalue));
        return(NhlNOERROR);
      }
    }
    NhlPError(NhlFATAL, NhlEUNKNOWN, "The specified value for the parameter "
              "has an incorrect type");
    return(NhlFATAL);
  }
  else if (type_pvalue == NCL_float || type_pvalue == NCL_double) {

/*
 *  Process the parameter if it has a float value or double value.
 */
    for (i = 0; i < numpf; i++) {
      if (!strncmp(arg1, params_f[i], strlen(params_f[i]))) {
        if (type_pvalue == NCL_float) {
          c_nnsetr(arg1, *((float *) pvalue));
          return(NhlNOERROR);
        }
        else if (type_pvalue == NCL_double) {
          c_nnsetrd(arg1, *((double *) pvalue));
          return(NhlNOERROR);
        }
      }
    }
    NhlPError(NhlFATAL, NhlEUNKNOWN, "The specified value for the parameter "
              "has an incorrect type");
    return(NhlFATAL);
  }
  else if (type_pvalue == NCL_string) {

/*
 *  Process the parameter if it has a string value.
 */
    for (i = 0; i < numpc; i++) {
      if (!strncmp(arg1, params_c[i], strlen(params_c[i]))) {
        cval = NrmQuarkToString( *((NrmQuark *) pvalue));
        c_nnsetc(arg1, cval);
        return(NhlNOERROR);
      }
    }
    NhlPError(NhlFATAL, NhlEUNKNOWN, "The specified value for the parameter "
              "has an incorrect type");
    return(NhlFATAL);
  }
  return(NhlNOERROR);
}

NhlErrorTypes nngetp_W(void)
{
/*
 *  Get values for fitpack parameters.
 */

  char  *arg1, *cval;
  int   numpi, numpf, numpc, i;
  NrmQuark qvalue;

/*
 *  List the integer and float parameter names.  To add new ones,
 *  all that needs to be done is add the names to this list.
 */
  char *params_i[] = {"adf", "asc", "dup", "ext", "igr", "non", "rad", 
                      "sdi", "upd", "mdm", "ADF", "ASC", "DUP", "EXT", "IGR", 
                      "NON", "RAD", "SDI", "UPD", "MDM"};
  char *params_f[] = {"bI", "bJ", "hor", "magx", "magy", "magz",
                      "nul", "ver", "xas", "yas", "zas", "Bi", "Bj", 
                      "HOR", "MAGX", "MAGY", "MAGZ", "NUL", "VER", 
                      "bi", "bj", "BI", "BJ", "XAS", "YAS", "ZAS"};
  char *params_c[] = {"alg", "ALG", "erf", "ERF"};

/*
 * Input array variable
 */
  NrmQuark *pname;
  ng_size_t dsizes_pname[NCL_MAX_DIMENSIONS];
  float *fval;
  int *ival;
  ng_size_t ret_size = 1;     

/*
 * Retrieve argument #1
 */
  pname = (NrmQuark *) NclGetArgValue(
          0,
          1,
          NULL,
          dsizes_pname,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

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
  NhlPError(NhlFATAL, NhlEUNKNOWN, "nnsetp: unrecognized parameter name");
  return(NhlFATAL);

/*
 *  Process the parameter if it has an integer value.
 */
OK_NAME:  for (i = 0; i < numpi; i++) {
    if (!strncmp(arg1, params_i[i], strlen(params_i[i]))) {
      ival = (int *) calloc(1,sizeof(int));
      c_nngeti(arg1, ival);
      return(NclReturnValue( (void *) ival, 1, &ret_size, NULL, NCL_int, 0));
    }
  }

/*
 *  Process the parameter if it has a float value.
 */
  for (i = 0; i < numpf; i++) {
    if (!strncmp(arg1, params_f[i], strlen(params_f[i]))) {
      fval = (float *) calloc(1,sizeof(float));
      c_nngetr(arg1, fval);
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
             "nngetp: unable to allocate memory for return string");
        return(NhlFATAL);
      }
      c_nngetc(arg1, cval);
      qvalue = NrmStringToQuark(cval);
      free(cval);
      return(NclReturnValue((void *) (&qvalue), 1, &ret_size, NULL,NCL_string, 1));
    }
  }
  return(NhlNOERROR);
}


NhlErrorTypes nngetaspects_W( void )
{
  int ier = 0;
  int *i;
  int *j;
  ng_size_t dsizes[1];
  float *rtmp;
/*
 * Retrieve parameters
 */
/*
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 */
  i = (int*)NclGetArgValue(0,2,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  j = (int*)NclGetArgValue(1,2,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  rtmp = (float*)NclMalloc(sizeof(float));
/*
 * The following section allocates the work memory and calls the
 * c_nngetaspects function.
 */
  c_nngetaspects(*i,*j,rtmp,&ier);
  if(!ier || (ier >= 4 && ier <= 6)) {
        dsizes[0] = 1;
        if(ier) {
          NhlPError(NhlWARNING,NhlEUNKNOWN,"nngetaspects: ier = %d", ier);
        }
        return(NclReturnValue((void*)rtmp,1,dsizes,NULL,NCL_float,0));
  }
  else {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"nngetaspects: ier = %d", ier);
        return(NhlFATAL);
  }
}


NhlErrorTypes nngetaspectd_W( void )
{
  int ier = 0;
  int *i;
  int *j;
  ng_size_t dsizes[1];
  double *dtmp;
/*
 * Retrieve parameters
 */
/*
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 */
  i = (int*)NclGetArgValue(0,2,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  j = (int*)NclGetArgValue(1,2,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  dtmp = (double*)NclMalloc(sizeof(double));
/*
 * The following section allocates the work memory and calls the
 * c_nngetaspectd function.
 */
  c_nngetaspectd(*i,*j,dtmp,&ier);
  if(!ier || (ier >= 4 && ier <= 6)) {
        dsizes[0] = 1;
        if(ier) {
          NhlPError(NhlWARNING,NhlEUNKNOWN,"nngetaspectd: ier = %d", ier);
        }
        return(NclReturnValue((void*)dtmp,1,dsizes,NULL,NCL_double,0));
  }
  else {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"nngetaspectd: ier = %d", ier);
        return(NhlFATAL);
  }
}


NhlErrorTypes nngetslopes_W( void )
{
  int ier = 0;
  int *i;
  int *j;
  ng_size_t dsizes[1];
  float *rtmp;
/*
 * Retrieve parameters
 */
/*
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 */
  i = (int*)NclGetArgValue(0,2,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  j = (int*)NclGetArgValue(1,2,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  rtmp = (float*)NclMalloc(sizeof(float));
/*
 * The following section allocates the work memory and calls the
 * c_nngetslopes function.
 */
  c_nngetslopes(*i,*j,rtmp,&ier);
  if(!ier || (ier >= 4 && ier <= 6)) {
        dsizes[0] = 1;
        if(ier) {
          NhlPError(NhlWARNING,NhlEUNKNOWN,"nngetslopes: ier = %d", ier);
        }
        return(NclReturnValue((void*)rtmp,1,dsizes,NULL,NCL_float,0));
  }
  else {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"nngetslopes: ier = %d", ier);
        return(NhlFATAL);
  }
}


NhlErrorTypes nngetsloped_W( void )
{
  int ier = 0;
  int *i;
  int *j;
  ng_size_t dsizes[1];
  double *dtmp;
/*
 * Retrieve parameters
 */
/*
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 */
  i = (int*)NclGetArgValue(0,2,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  j = (int*)NclGetArgValue(1,2,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  dtmp = (double*)NclMalloc(sizeof(double));
/*
 * The following section allocates the work memory and calls the
 * c_nngetsloped function.
 */
  c_nngetsloped(*i,*j,dtmp,&ier);
  if(!ier || (ier >= 4 && ier <= 6)) {
        dsizes[0] = 1;
        if(ier) {
          NhlPError(NhlWARNING,NhlEUNKNOWN,"nngetsloped: ier = %d", ier);
        }
        return(NclReturnValue((void*)dtmp,1,dsizes,NULL,NCL_double,0));
  }
  else {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"nngetsloped: ier = %d", ier);
        return(NhlFATAL);
  }
}


NhlErrorTypes drwsrfc_W( void )
{
  float *x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  float *y;
  ng_size_t dsizes_y[NCL_MAX_DIMENSIONS];
  float *z;
  ng_size_t dsizes_z[NCL_MAX_DIMENSIONS];
  float *x1,*y1,*z1;
  int *gkswid;
  int *iwk;
/*
 * Retrieve parameters
 */
/*
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 */
  gkswid = (int*)NclGetArgValue(0,7,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  x = (float*)NclGetArgValue(1,7, NULL, dsizes_x, NULL,NULL,NULL,DONT_CARE);
  y = (float*)NclGetArgValue(2,7, NULL, dsizes_y, NULL,NULL,NULL,DONT_CARE);
  z = (float*)NclGetArgValue(3,7, NULL, dsizes_z, NULL,NULL,NULL,DONT_CARE);
  x1 = (float*)NclGetArgValue(4,7,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  y1 = (float*)NclGetArgValue(5,7,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  z1 = (float*)NclGetArgValue(6,7,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
/*
 * This is the only dimension size check needed since the function
 * is registered to only accept single dimension parameters.
 */
   if((dsizes_x[0] == dsizes_y[0])&&(dsizes_x[0] == dsizes_z[0])) {
/*
 * The following section allocates the work memory and calls the
 * drwsrfc function.
 */
         iwk = (int*)NclMalloc(2*dsizes_x[0]*dsizes_y[0]*sizeof(int));
         if( iwk == NULL ) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"drwsrfc: Unable to allocate memory for work array");
           return(NhlFATAL);
         }
         drwsrfc(*gkswid,dsizes_x[0],dsizes_y[0],x,y,z,*x1,*y1,*z1,iwk);
  }
  else {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"drwsrfc: the dimension sizes of parameters x, y and z must be identical");
        return(NhlFATAL);
  }
   return(NhlNOERROR);
  
}

NhlErrorTypes drwvctc_W( void )
{
  float *u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  float *v;
  ng_size_t dsizes_v[NCL_MAX_DIMENSIONS];
  int *gkswid;
/*
 * Retrieve parameters
 */
/*
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 */
  gkswid = (int*)NclGetArgValue(0,3,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  u = (float*)NclGetArgValue(1,3, NULL, dsizes_u, NULL,NULL,NULL,DONT_CARE);
  v = (float*)NclGetArgValue(2,3, NULL, dsizes_v, NULL,NULL,NULL,DONT_CARE);
/*
 * This is the only dimension size check needed since the function
 * is registered to only accept single dimension parameters.
 */
   if((dsizes_u[0] == dsizes_v[0])&&(dsizes_u[1] == dsizes_v[1])) {
/*
 * The following section allocates the work memory and calls the
 * drwvctc function.
 */
         drwvctc(*gkswid,dsizes_u[0],dsizes_u[1],u,v);
  }
  else {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"drwvctc: the dimension sizes of parameters u and v must be identical");
        return(NhlFATAL);
  }
   return(NhlNOERROR);
}


NhlErrorTypes drwconc_W( void )
{
  float *z;
  ng_size_t dsizes_z[NCL_MAX_DIMENSIONS];
  int *gkswid;
/*
 * Retrieve parameters
 */
/*
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 */
  gkswid = (int*)NclGetArgValue(0,2,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  z = (float*)NclGetArgValue(1,2, NULL, dsizes_z, NULL,NULL,NULL,DONT_CARE);
/*
 * This is the only dimension size check needed since the function
 * is registered to only accept single dimension parameters.
 */
/*
 * The following section allocates the work memory and calls the
 * drwconc function.
 */
  drwconc(*gkswid,dsizes_z[0],dsizes_z[1],z);
  return(NhlNOERROR);
}


NhlErrorTypes nnpntinits_W( void )
{
  float *x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  float *y;
  ng_size_t dsizes_y[NCL_MAX_DIMENSIONS];
  int has_missing_y;
  float *z;
  ng_size_t dsizes_z[NCL_MAX_DIMENSIONS];
  int has_missing_z;
  NclScalar missing_x, missing_y, missing_z;
  int i;
  ng_size_t npts;
/*
 * Retrieve parameters
 */
  x = (float*)NclGetArgValue(
                                                         0,
                                                         3,
                                                         NULL,
                                                         dsizes_x,
                                                         &missing_x,
                                                         &has_missing_x,
                                                         NULL,
                                                         DONT_CARE);
  y = (float*)NclGetArgValue(
                                                         1,
                                                         3,
                                                         NULL,
                                                         dsizes_y,
                                                         &missing_y,
                                                         &has_missing_y,
                                                         NULL,
                                                         DONT_CARE);
  z = (float*)NclGetArgValue(
                                                         2,
                                                         3,
                                                         NULL,
                                                         dsizes_z,
                                                         &missing_z,
                                                         &has_missing_z,
                                                         NULL,
                                                         DONT_CARE);

/*
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 */
  if(dsizes_x[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"nnpntinits: the length of x is > INT_MAX");
    return(NhlFATAL);
  }
  npts = (int) dsizes_x[0];

  if((dsizes_x[0] == dsizes_y[0])&&(dsizes_x[0] == dsizes_z[0])) {
/*
 * Check for missing values. 
 */
         if(has_missing_x) {
           for( i = 0; i < dsizes_x[0]; i++ ) {
                 if(x[i] == missing_x.floatval) {
                   NhlPError(NhlFATAL,NhlEUNKNOWN,"nnpntinits: x cannot contain any missing values" );
                   return(NhlFATAL);
                 }
           }
         }
         if(has_missing_y) {
           for( i = 0; i < dsizes_y[0]; i++ ) {
                 if(y[i] == missing_y.floatval) {
                   NhlPError(NhlFATAL,NhlEUNKNOWN,"nnpntinits: y cannot contain any missing values" );
                   return(NhlFATAL);
                 }
           }
         }
         if(has_missing_z) {
           for( i = 0; i < dsizes_z[0]; i++ ) {
                 if(z[i] == missing_z.floatval) {
                   NhlPError(NhlFATAL,NhlEUNKNOWN,"nnpntinits: z cannot contain any missing values" );
                   return(NhlFATAL);
                 }
           }
         }
/*
 * The following section allocates the work memory and calls the
 * c_nnpntinits function.
 */
         c_nnpntinits(npts,x,y,z);
         return(NhlNOERROR);
   }
  else {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"nnpntinits: the dimension sizes of parameters x, y and z must be identical");
        return(NhlFATAL);
  }
}


NhlErrorTypes nnpntinitd_W( void )
{
  double *x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  double *y;
  ng_size_t dsizes_y[NCL_MAX_DIMENSIONS];
  int has_missing_y;
  double *z;
  ng_size_t dsizes_z[NCL_MAX_DIMENSIONS];
  int has_missing_z;
  NclScalar missing_x, missing_y, missing_z;
  int i, npts;
/*
 * Retrieve parameters
 */
  x = (double*)NclGetArgValue(
                                                         0,
                                                         3,
                                                         NULL,
                                                         dsizes_x,
                                                         &missing_x,
                                                         &has_missing_x,
                                                         NULL,
                                                         DONT_CARE);
  y = (double*)NclGetArgValue(
                                                         1,
                                                         3,
                                                         NULL,
                                                         dsizes_y,
                                                         &missing_y,
                                                         &has_missing_y,
                                                         NULL,
                                                         DONT_CARE);
  z = (double*)NclGetArgValue(
                                                         2,
                                                         3,
                                                         NULL,
                                                         dsizes_z,
                                                         &missing_z,
                                                         &has_missing_z,
                                                         NULL,
                                                         DONT_CARE);

/*
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 */
  if(dsizes_x[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"nnpntinitd: the length of x is > INT_MAX");
    return(NhlFATAL);
  }
  npts = (int) dsizes_x[0];

   if((dsizes_x[0] == dsizes_y[0])&&(dsizes_x[0] == dsizes_z[0])) {
/*
 * Check for missing values. 
 */
         if(has_missing_x) {
           for( i = 0; i < dsizes_x[0]; i++ ) {
                 if(x[i] == missing_x.doubleval) {
                   NhlPError(NhlFATAL,NhlEUNKNOWN,"nnpntinitd: x cannot contain any missing values" );
                   return(NhlFATAL);
                 }
           }
         }
         if(has_missing_y) {
           for( i = 0; i < dsizes_y[0]; i++ ) {
                 if(y[i] == missing_y.doubleval) {
                   NhlPError(NhlFATAL,NhlEUNKNOWN,"nnpntinitd: y cannot contain any missing values" );
                   return(NhlFATAL);
                 }
           }
         }
         if(has_missing_z) {
           for( i = 0; i < dsizes_z[0]; i++ ) {
                 if(z[i] == missing_z.doubleval) {
                   NhlPError(NhlFATAL,NhlEUNKNOWN,"nnpntinitd: z cannot contain any missing values" );
                   return(NhlFATAL);
                 }
           }
         }
/*
 * The following section allocates the work memory and calls the
 * c_nnpntinitd function.
 */
         c_nnpntinitd(npts,x,y,z);
         return(NhlNOERROR);
   }
  else {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"nnpntinitd: the dimension sizes of parameters x, y and z must be identical");
        return(NhlFATAL);
  }
}


NhlErrorTypes nnpntinit_W( void )
{
  void *x, *y, *z;
  double *tmp_x, *tmp_y, *tmp_z;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  ng_size_t dsizes_y[NCL_MAX_DIMENSIONS];
  int has_missing_y;
  ng_size_t dsizes_z[NCL_MAX_DIMENSIONS];
  int has_missing_z;
  NclScalar missing_x, missing_y, missing_z;
  NclScalar missing_dx, missing_dy, missing_dz;
  NclBasicDataTypes type_x, type_y, type_z;
  int npts;
/*
 * Retrieve parameters
 */
  x = (void*)NclGetArgValue(
                            0,
                            3,
                            NULL,
                            dsizes_x,
                            &missing_x,
                            &has_missing_x,
                            &type_x,
                            DONT_CARE);
  y = (void*)NclGetArgValue(
                            1,
                            3,
                            NULL,
                            dsizes_y,
                            &missing_y,
                            &has_missing_y,
                            &type_y,
                            DONT_CARE);
  z = (void*)NclGetArgValue(
                            2,
                            3,
                            NULL,
                            dsizes_z,
                            &missing_z,
                            &has_missing_z,
                            &type_z,
                            DONT_CARE);

/*
 * Check sizes.
 */
  if(dsizes_x[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"nnpntinit: the length of x is > INT_MAX");
    return(NhlFATAL);
  }
  npts = (int) dsizes_x[0];

  if(dsizes_y[0] != npts || dsizes_z[0] != npts) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"nnpntinit: the dimension sizes of parameters x, y and z must be identical");
    return(NhlFATAL);
  }
/*
 * Coerce missing values.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,NULL);
  coerce_missing(type_y,has_missing_y,&missing_y,&missing_dy,NULL);
  coerce_missing(type_z,has_missing_z,&missing_z,&missing_dz,NULL);
/*
 * Coerce input to double if necessary.
 */
  tmp_x = coerce_input_double(x,type_x,npts,0,NULL,NULL);
  tmp_y = coerce_input_double(y,type_y,npts,0,NULL,NULL);
  tmp_z = coerce_input_double(z,type_z,npts,0,NULL,NULL);

  if(tmp_x == NULL || tmp_y == NULL || tmp_z == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "nnpntinit: Unable to allocate memory for coercing input to double");
    return(NhlFATAL);
  }
/*
 * Check for missing values. 
 */
  if(contains_missing(tmp_x,npts,has_missing_x,missing_dx.doubleval)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"nnpntinit: x cannot contain any missing values" );
    return(NhlFATAL);
  }
  if(contains_missing(tmp_y,dsizes_y[0],has_missing_y,missing_dy.doubleval)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"nnpntinit: y cannot contain any missing values" );
    return(NhlFATAL);
  }
  if(contains_missing(tmp_z,dsizes_z[0],has_missing_z,missing_dz.doubleval)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"nnpntinit: z cannot contain any missing values" );
    return(NhlFATAL);
  }
/*
 * The following section allocates the work memory and calls the
 * c_nnpntinitd function.
 */
   c_nnpntinitd(npts,tmp_x,tmp_y,tmp_z);

/*
 * Free memory.
 */
   if(type_x   != NCL_double) NclFree(tmp_x);
   if(type_y   != NCL_double) NclFree(tmp_y);
   if(type_z   != NCL_double) NclFree(tmp_z);
   return(NhlNOERROR);
}


NhlErrorTypes nnpnts_W( void )
{
  float *x;
  float *y;
  float *z;
  ng_size_t dsizes_x[1];
  ng_size_t dsizes_y[1];
  ng_size_t i, npts;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 */
  x = (float*)NclGetArgValue(0,2,NULL,dsizes_x,NULL,NULL,NULL,DONT_CARE);
  y = (float*)NclGetArgValue(1,2,NULL,dsizes_y,NULL,NULL,NULL,DONT_CARE);

  npts = dsizes_x[0];
  if(dsizes_y[0] != npts) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"nnpnts: the dimension sizes of parameters x and y must be identical");
    return(NhlFATAL);
  }

/*
 * The following section allocates the output memory and calls the
 * c_nnpnts function.
 */
  z = (float*)calloc(npts,sizeof(float));
  for(i = 0; i < npts; i++ ) {
    c_nnpnts(x[i],y[i],&z[i]);
  }
  return(NclReturnValue((void*)z,1,dsizes_x,NULL,NCL_float,0));
}


NhlErrorTypes nnpntd_W( void )
{
  double *x;
  double *y;
  double *z;
  ng_size_t dsizes[1];
/*
 * Retrieve parameters
 */
/*
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 */
  x = (double*)NclGetArgValue(0,2,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  y = (double*)NclGetArgValue(1,2,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  z = (double*)NclMalloc(sizeof(double));
/*
 * The following section allocates the work memory and calls the
 * c_nnpnts function.
 */
  c_nnpntd(*x,*y,z);
  dsizes[0] = 1;
  return(NclReturnValue((void*)z,1,dsizes,NULL,NCL_double,0));
}



NhlErrorTypes nnpnt_W( void )
{
  void *x, *y, *z;
  double *tmp_x = NULL;
  double *tmp_y = NULL;
  double *tmp_z = NULL;
  ng_size_t dsizes_x[1];
  ng_size_t dsizes_y[1];
  NclBasicDataTypes type_x, type_y, type_z;
  int i, npts;

/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 */
  x = (void*)NclGetArgValue(0,2,NULL,dsizes_x,NULL,NULL,&type_x,DONT_CARE);
  y = (void*)NclGetArgValue(1,2,NULL,dsizes_y,NULL,NULL,&type_y,DONT_CARE);

  if(dsizes_x[0] != dsizes_y[0]) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"nnpnt: the dimension sizes of parameters x and y must be identical");
    return(NhlFATAL);
  }
  npts = dsizes_x[0];

  if(type_x == NCL_double || type_y == NCL_double) {
    type_z = NCL_double;
    z      = (void*)calloc(npts,sizeof(double));
  }
  else {
    type_z = NCL_float;
    z      = (void*)calloc(npts,sizeof(float));
    tmp_z  = (double*)calloc(1,sizeof(double));
  }
  if(z == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "nnpnt: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Set up up variables to hold double input if necessary.
 */
  if(type_x != NCL_double) tmp_x  = (double*)calloc(1,sizeof(double));
  if(type_y != NCL_double) tmp_y  = (double*)calloc(1,sizeof(double));

/*
 * The following section calls the c_nnpntd function and coerces the
 * value back to a float if necessary.
 */

  for(i = 0; i < npts; i++) { 
    if(type_x != NCL_double) {
/*
 * Coerce npts subsection of x (tmp_x) to double.
 */
      coerce_subset_input_double(x,tmp_x,i,type_x,1,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_x to appropriate location in x.
 */
      tmp_x = &((double*)x)[i];
    }

    if(type_y != NCL_double) {
/*
 * Coerce npts subsection of y (tmp_y) to double.
 */
      coerce_subset_input_double(y,tmp_y,i,type_y,1,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_y to appropriate location in y.
 */
      tmp_y = &((double*)y)[i];
    }

    if(type_z == NCL_double) {
      tmp_z = &((double*)z)[i];
    }
    c_nnpntd(*tmp_x,*tmp_y,tmp_z);

    if(type_z != NCL_double) coerce_output_float_only(z,tmp_z,1,i);
  }

  if(type_x != NCL_double) NclFree(tmp_x);
  if(type_y != NCL_double) NclFree(tmp_y);
  if(type_z != NCL_double) NclFree(tmp_z);

  return(NclReturnValue(z,1,dsizes_x,NULL,type_z,0));
}



NhlErrorTypes nnpntend_W( void )
{
/*
 * The following section allocates the work memory and calls the
 * c_nnpntend function.
 */
  c_nnpntend();
  return(NhlNOERROR);
}


NhlErrorTypes nnpntendd_W( void )
{
/*
 * The following section allocates the work memory and calls the
 * c_nnpntend function.
 */
  c_nnpntendd();
  return(NhlNOERROR);
}

NhlErrorTypes nngetwts_W( void )
{
/* 
 * Input values
 */
  int *numw, *nbrs;
  float *wts, *xe, *ye, *ze;

/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept numerics.
 */
  numw = (int*)NclGetArgValue(
                              0,
                              6,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              1);
  nbrs = (int*)NclGetArgValue(
                              1,
                              6,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              1);
  wts = (float*)NclGetArgValue(
                              2,
                              6,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              1);
  xe = (float*)NclGetArgValue(
                              3,
                              6,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              1);
  ye = (float*)NclGetArgValue(
                              4,
                              6,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              1);
  ze = (float*)NclGetArgValue(
                              5,
                              6,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              1);
/*
 * Call c_nngetwts.
 */
  c_nngetwts(numw,nbrs,wts,xe,ye,ze);

  return(NhlNOERROR);
}


NhlErrorTypes nngetwtsd_W( void )
{
/* 
 * Input values
 */
  int *numw, *nbrs;
  double *wts, *xe, *ye, *ze;

/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept numerics.
 */
  numw = (int*)NclGetArgValue(
                              0,
                              6,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              1);
  nbrs = (int*)NclGetArgValue(
                              1,
                              6,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              1);
  wts = (double*)NclGetArgValue(
                              2,
                              6,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              1);
  xe = (double*)NclGetArgValue(
                              3,
                              6,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              1);
  ye = (double*)NclGetArgValue(
                              4,
                              6,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              1);
  ze = (double*)NclGetArgValue(
                              5,
                              6,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              1);
/*
 * Call c_nngetwtsd.
 */
  c_nngetwtsd(numw,nbrs,wts,xe,ye,ze);

  return(NhlNOERROR);
}


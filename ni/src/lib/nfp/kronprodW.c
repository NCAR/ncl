#include <stdio.h>
#include "wrapper.h"

/*
 * This NCL wrapper calculates a Kronecker product, given two
 * 2D arrays (x and y) of arbitrary size, nx1 x nx2 and ny1 x ny2.
 */
NhlErrorTypes kron_product_W( void )
{

/*
 * Input variables
 */
/*
 * Argument # 0
 */
  void *x;
  double *tmp_x;
  ng_size_t dsizes_x[2];
  int has_missing_x;
  NclScalar missing_x, missing_flt_x, missing_dbl_x;
  NclBasicDataTypes type_x;

/*
 * Argument # 1
 */
  void *y;
  double *tmp_y;
  ng_size_t dsizes_y[2];
  int has_missing_y;
  NclScalar missing_y, missing_flt_y, missing_dbl_y;
  NclBasicDataTypes type_y;

/*
 * Return variable
 */
  void *kprod;
  double *tmp_kprod;
  ng_size_t *dsizes_kprod;
  NclScalar missing_kprod, missing_dbl_kprod;
  NclBasicDataTypes type_kprod;

/*
 * Various
 */
  ng_size_t nx1, nx2, nx1nx2, ny1, ny2, ny1ny2;
  ng_size_t i, j, k, l, ix, iy, ikp, size_output;
  int ret;

/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
/*
 * Get argument # 0
 */
  x = (void*)NclGetArgValue(
           0,
           2,
           NULL,
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);

  nx1 = dsizes_x[0];
  nx2 = dsizes_x[1];
  nx1nx2 = nx1 * nx2;

/*
 * Get argument # 1
 */
  y = (void*)NclGetArgValue(
           1,
           2,
           NULL,
           dsizes_y,
           &missing_y,
           &has_missing_y,
           &type_y,
           DONT_CARE);
/*
 * Coerce missing values to double if necessary.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,
                 &missing_dbl_x,&missing_flt_x);

  coerce_missing(type_y,has_missing_y,&missing_y,
                 &missing_dbl_y,&missing_flt_y);

  ny1 = dsizes_y[0];
  ny2 = dsizes_y[1];
  ny1ny2 = ny1 * ny2;

/*
 * Coerce x and y to double if necessary.
 */
  tmp_x = coerce_input_double(x,type_x,nx1nx2,has_missing_x,&missing_x,
                              &missing_dbl_x);
  tmp_y = coerce_input_double(y,type_y,ny1ny2,has_missing_y,&missing_y,
                              &missing_dbl_y);
  if(tmp_x == NULL || tmp_y == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"kron_product: Unable to allocate memory for coercing input arrays to double");
      return(NhlFATAL);
    }

  if(type_x == NCL_double || type_y == NCL_double) {
    type_kprod = NCL_double;
  }
  else {
    type_kprod = NCL_float;
  }

/*
 * Calculate size of output array.
 */
  size_output = nx1nx2* ny1ny2;

/* 
 * Allocate space for output array.
 */
  if(type_kprod != NCL_double) {
    kprod     = (void *)calloc(size_output, sizeof(float));
    tmp_kprod = (double *)calloc(size_output,sizeof(double));
    if(kprod == NULL || tmp_kprod == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"kron_product: Unable to allocate memory for temporary output array");
      return(NhlFATAL);
    }
  }
  else {
    kprod = (void *)calloc(size_output, sizeof(double));
    if(kprod == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"kron_product: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    tmp_kprod = &((double*)kprod)[0];
  }
  if(has_missing_x) {
    if(type_kprod == NCL_double) missing_kprod = missing_dbl_x;
    else                         missing_kprod = missing_flt_x;
    missing_dbl_kprod = missing_dbl_x;
  }
  else if(has_missing_y) {
    if(type_kprod == NCL_double) missing_kprod = missing_dbl_y;
    else                         missing_kprod = missing_flt_y;
    missing_dbl_kprod = missing_dbl_y;
  }
/* 
 * Allocate space for output dimension sizes and set them.
 */
  dsizes_kprod = (ng_size_t*)calloc(2,sizeof(ng_size_t));  
  if( dsizes_kprod == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"kron_product: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }
  dsizes_kprod[0] = nx1 * ny1;
  dsizes_kprod[1] = nx2 * ny2;

/*
 * Loop through the four dimemsions and calculate the
 * kronecker product.
 */
  ikp = 0;
  for(i = 0; i < nx1; i++ ) {
    for(k = 0; k < ny1; k++ ) {
      for(j = 0; j < nx2; j++ ) {
        ix = i*nx2 + j;
        for(l = 0; l < ny2; l++ ) {
          iy = k*ny2 + l;
          if( (has_missing_x && tmp_x[ix] == missing_dbl_x.doubleval) ||
              (has_missing_y && tmp_y[iy] == missing_dbl_y.doubleval)) {
            tmp_kprod[ikp] = missing_dbl_kprod.doubleval;
          }
          else{
            tmp_kprod[ikp] = tmp_x[ix] * tmp_y[iy];
          }
          ikp++;
        }
      }
    }
  }

/*
 * Coerce output back to float if necessary.
 */
  if(type_kprod == NCL_float) {
    coerce_output_float_only(kprod,tmp_kprod,size_output,0);
  }

/*
 * Free unneeded memory.
 */
  if(type_x != NCL_double)     NclFree(tmp_x);
  if(type_y != NCL_double)     NclFree(tmp_y);
  if(type_kprod != NCL_double) NclFree(tmp_kprod);

/*
 * Return value back to NCL script.
 */
  if(has_missing_x || has_missing_y) {
    ret = NclReturnValue(kprod,2,dsizes_kprod,&missing_kprod,type_kprod,0);
  }
  else {
    ret = NclReturnValue(kprod,2,dsizes_kprod,NULL,type_kprod,0);
  }
    
  NclFree(dsizes_kprod);
  return(ret);
}

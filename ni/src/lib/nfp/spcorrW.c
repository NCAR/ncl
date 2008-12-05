#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(spcorr,SPCORR)(double *, double *, int *, int *, double *);

NhlErrorTypes spcorr_W( void )
{

/*
 * Input variables
 */
/*
 * Argument # 0
 */
  void *x;
  double *tmp_x;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_x;

/*
 * Argument # 1
 */
  void *y;
  double *tmp_y;
  int ndims_y, dsizes_y[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_y;

/*
 * Return variable
 */
  void *spc;
  double tmp_spc;
  int ndims_spc, *dsizes_spc;
  NclBasicDataTypes type_spc;

/*
 * Various
 */
  int i, n, index_x, iwrite, size_spc, ret;

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
           &ndims_x,
           dsizes_x,
           NULL,
           NULL,
           &type_x,
           2);

  n = dsizes_x[ndims_x-1];
/*
 * Get argument # 1
 */
  y = (void*)NclGetArgValue(
           1,
           2,
           &ndims_y,
           dsizes_y,
           NULL,
           NULL,
           &type_y,
           2);

/*
 * Check dimension sizes.
 */
  if(ndims_y != ndims_x) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"spcorr: The x and y arrays must be the same dimensionality");
    return(NhlFATAL);
  }

  for(i = 0; i < ndims_x; i++) {
    if(dsizes_y[i] != dsizes_x[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"spcorr: The leftmost dimensions of x and y must be the same");
      return(NhlFATAL);
    }
  }

/*
 * Allocate space for output dimension sizes and set them.
 */
  if(ndims_x == 1) ndims_spc = 1;
  else             ndims_spc = ndims_x-1;

  dsizes_spc = (int*)calloc(ndims_spc,sizeof(int));  
  if( dsizes_spc == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"spcorr: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }

  size_spc = 1;
  if(ndims_x == 1) {
    dsizes_spc[0] = 1;
  }
  else {
    for(i = 0; i < ndims_spc; i++) {
      size_spc *= dsizes_x[i];
      dsizes_spc[i] = dsizes_x[i];
    }
  }

/*
 * The output type defaults to float, unless the input arrays are double.
 */
  type_spc = NCL_float;

/* 
 * Allocate space for coercing input arrays.  If any of the input
 * is already double, then we don't need to allocate space for
 * temporary arrays, because we'll just change the pointer into
 * the void array appropriately.
 */
/*
 * Allocate space for tmp_x.
 */
  if(type_x != NCL_double) {
    tmp_x = (double *)calloc(n,sizeof(double));
    if(tmp_x == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"spcorr: Unable to allocate memory for coercing x to double");
      return(NhlFATAL);
    }
  }
  else {
    type_spc = NCL_double;
  }
/*
 * Allocate space for tmp_y.
 */
  if(type_y != NCL_double) {
    tmp_y = (double *)calloc(n,sizeof(double));
    if(tmp_y == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"spcorr: Unable to allocate memory for coercing y to double");
      return(NhlFATAL);
    }
  }
  else {
    type_spc = NCL_double;
  }

/* 
 * Allocate space for output array.
 */
  if(type_spc != NCL_double) spc = (void *)calloc(size_spc, sizeof(float));
  else                       spc = (void *)calloc(size_spc, sizeof(double));

  if(spc == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"spcorr: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Loop across leftmost dimensions and call the Fortran routine for each
 * subsection of the input arrays.
 */
  index_x = 0;
  iwrite = 0;
  for(i = 0; i < size_spc; i++) {
/*
 * Coerce subsection of x (tmp_x) to double if necessary.
 */
    if(type_x != NCL_double) {
      coerce_subset_input_double(x,tmp_x,index_x,type_x,n,0,NULL,NULL);
    }
    else {
      tmp_x = &((double*)x)[index_x];
    }

/*
 * Coerce subsection of y (tmp_y) to double if necessary.
 */
    if(type_y != NCL_double) {
      coerce_subset_input_double(y,tmp_y,index_x,type_y,n,0,NULL,NULL);
    }
    else {
      tmp_y = &((double*)y)[index_x];
    }

/*
 * Call the Fortran routine.
 */
    NGCALLF(spcorr,SPCORR)(tmp_x, tmp_y, &n, &iwrite, &tmp_spc);

/*
 * Coerce output back to float if necessary.
 */
    coerce_output_float_or_double(spc,&tmp_spc,type_spc,1,i);

    index_x += n;
  }

/*
 * Free unneeded memory.
 */
  if(type_x != NCL_double) NclFree(tmp_x);
  if(type_y != NCL_double) NclFree(tmp_y);

/*
 * Return value back to NCL script.
 */
  ret = NclReturnValue(spc,ndims_spc,dsizes_spc,NULL,type_spc,0);
  NclFree(dsizes_spc);
  return(ret);
}

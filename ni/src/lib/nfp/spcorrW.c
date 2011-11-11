#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(spcorrz,SPCORRZ)(double *, double *, int *, int *, 
                                     double *,int *, double *, int *,
                                     double *, int *);

NhlErrorTypes spcorr_W( void )
{

/*
 * Input variables
 */
/*
 * Argument # 0
 */
  void *x;
  double *tmp_x = NULL;
  int has_missing_x, ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  NclScalar missing_x, missing_dx;
  NclBasicDataTypes type_x;

/*
 * Argument # 1
 */
  void *y;
  double *tmp_y = NULL;
  int has_missing_y, ndims_y;
  ng_size_t dsizes_y[NCL_MAX_DIMENSIONS];
  NclScalar missing_y, missing_dy;
  NclBasicDataTypes type_y;

/*
 * Return variable
 */
  void *spc;
  double tmp_spc;
  int ndims_spc;
  ng_size_t *dsizes_spc;
  int has_missing_spc;
  NclScalar missing_spc;
  NclBasicDataTypes type_spc;

/*
 * Various
 */
  ng_size_t i, n, index_x, size_spc;
  int iwrite;
  int ret, in, inmsg;

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
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);

  n = dsizes_x[ndims_x-1];
  if(n > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"spcorr: n = %ld is greater than INT_MAX", n);
    return(NhlFATAL);
  }
  in = (int) n;

/*
 * Get argument # 1
 */
  y = (void*)NclGetArgValue(
           1,
           2,
           &ndims_y,
           dsizes_y,
           &missing_y,
           &has_missing_y,
           &type_y,
           DONT_CARE);

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

  dsizes_spc = (ng_size_t*)calloc(ndims_spc,sizeof(ng_size_t));  
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
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,NULL);
  coerce_missing(type_y,has_missing_y,&missing_y,&missing_dy,NULL);

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
  has_missing_spc = 0;
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
    inmsg = 0;
    NGCALLF(spcorrz,SPCORRZ)(tmp_x, tmp_y, &in, &iwrite, &tmp_spc,
                             &has_missing_x,&missing_dx.doubleval,
                             &has_missing_y,&missing_dy.doubleval,
                             &inmsg);
    if(in == inmsg) {
/*
 * All input pairs had an X or Y missing value, so set output to 
 * missing.
 */
      if(!has_missing_spc) {
        has_missing_spc = 1;
        if(type_spc == NCL_float) {
          missing_spc = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis;
        }
        else {
          missing_spc = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis;
        }
      }
      if(type_spc == NCL_float) {
        tmp_spc = (double)missing_spc.floatval;
      }
      else {
        tmp_spc = missing_spc.doubleval;
      }
    }
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
  if(!has_missing_spc) {
    ret = NclReturnValue(spc,ndims_spc,dsizes_spc,NULL,type_spc,0);
  }
  else {
    ret = NclReturnValue(spc,ndims_spc,dsizes_spc,&missing_spc,type_spc,0);
  }
  NclFree(dsizes_spc);
  return(ret);
}

NhlErrorTypes spcorr_n_W( void )
{
/*
 * Input variables
 */
/*
 * Argument # 0
 */
  void *x;
  double *tmp_x = NULL;
  int has_missing_x, ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  NclScalar missing_x, missing_dx;
  NclBasicDataTypes type_x;

/*
 * Argument # 1
 */
  void *y;
  double *tmp_y = NULL;
  int has_missing_y, ndims_y;
  ng_size_t dsizes_y[NCL_MAX_DIMENSIONS];
  NclScalar missing_y, missing_dy;
  NclBasicDataTypes type_y;

/*
 * Argument # 2
 */
  int *dim;

/*
 * Return variable
 */
  void *spc;
  double tmp_spc;
  int ndims_spc;
  ng_size_t *dsizes_spc;
  int has_missing_spc;
  NclScalar missing_spc;
  NclBasicDataTypes type_spc;

/*
 * Various
 */
  ng_size_t i, j, n, index_x, index_s, index_nr, index_nrn;
  ng_size_t size_leftmost, size_rightmost, size_spc, size_rl;
  int iwrite;
  int ret, in, inmsg;

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
           3,
           &ndims_x,
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);

/*
 * Get argument # 1
 */
  y = (void*)NclGetArgValue(
           1,
           3,
           &ndims_y,
           dsizes_y,
           &missing_y,
           &has_missing_y,
           &type_y,
           DONT_CARE);

  dim = (int*)NclGetArgValue(
          2,
          3,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

/*
 * Check dimension sizes.
 */
  if(ndims_y != ndims_x) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"spcorr_n: The x and y arrays must be the same dimensionality");
    return(NhlFATAL);
  }

  for(i = 0; i < ndims_x; i++) {
    if(dsizes_y[i] != dsizes_x[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"spcorr_n: The leftmost dimensions of x and y must be the same");
      return(NhlFATAL);
    }
  }

/*
 * Make sure "dim" is a valid dimension.
 */
  if (*dim < 0 || *dim >= ndims_y) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"spcorr_n: Invalid dimension index");
    return(NhlFATAL);
  }

/*
 * Get dimension we're going to do calculation across.
 */
  n = dsizes_x[*dim];
  if(n > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"spcorr_n: n = %ld is greater than INT_MAX", n);
    return(NhlFATAL);
  }
  in = (int) n;

/*
 * Allocate space for output dimension sizes and set them.
 */
  if(ndims_x == 1) ndims_spc = 1;
  else             ndims_spc = ndims_x-1;

  dsizes_spc = (ng_size_t*)calloc(ndims_spc,sizeof(ng_size_t));  
  if( dsizes_spc == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"spcorr_n: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }

/*
 * Compute total size of output array, plus leftmost/rightmost dimensions.
 *
 * If ndims_x == 1, then really, spcorr_n shouldn't be used.
 */
  size_rightmost = size_leftmost = 1;
  if(ndims_x > 1) {
    for( i = 0; i < *dim; i++ ) {
      size_leftmost *= dsizes_x[i];
      dsizes_spc[i] = dsizes_x[i];
    }
    for( i = *dim+1; i < ndims_x; i++ ) {
      size_rightmost *= dsizes_x[i];
      dsizes_spc[i-1] = dsizes_x[i];
    }
  }
  else {
    dsizes_spc[0] = 1;
  }

  size_rl = size_leftmost * size_rightmost;
  size_spc = size_rl * n;

/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,NULL);
  coerce_missing(type_y,has_missing_y,&missing_y,&missing_dy,NULL);

/*
 * Allocate space for tmp_x and tmp_y.
 */
  tmp_x = (double *)calloc(n,sizeof(double));
  tmp_y = (double *)calloc(n,sizeof(double));
  if(tmp_x == NULL || tmp_y == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"spcorr_n: Unable to allocate memory for coercing x and y to double");
    return(NhlFATAL);
  }

/* 
 * Allocate space for output array.
 */
  if(type_x == NCL_double || type_y == NCL_double) {
    type_spc = NCL_double;
    spc      = (void *)calloc(size_spc, sizeof(double));
  }
  else {
    type_spc = NCL_float;
    spc      = (void *)calloc(size_spc, sizeof(float));
  }
  if(spc == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"spcorr_n: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Loop across leftmost and rightmost dimensions and call the 
 * Fortran routine for each subsection of the input arrays.
 */
  iwrite = has_missing_spc = 0;
  for( i = 0; i < size_leftmost; i++ ) {
    index_nr  = i*size_rightmost;
    index_nrn = index_nr * n;
    for( j = 0; j < size_rightmost; j++ ) {
      index_x = index_nrn + j;
      index_s = index_nr + j;
/*
 * Coerce subsection of x/y (tmp_x/tmp_y) to double.
 */
      coerce_subset_input_double_step(x,tmp_x,index_x,size_rightmost,
				      type_x,n,0,NULL,NULL);
      coerce_subset_input_double_step(y,tmp_y,index_x,size_rightmost,
				      type_y,n,0,NULL,NULL);
/*
 * Call the Fortran routine.
 */
      inmsg = 0;
      NGCALLF(spcorrz,SPCORRZ)(tmp_x, tmp_y, &in, &iwrite, &tmp_spc,
			       &has_missing_x,&missing_dx.doubleval,
			       &has_missing_y,&missing_dy.doubleval,
			       &inmsg);
      if(in == inmsg) {
/*
 * All input pairs had an X or Y missing value, so set output to 
 * missing.
 */
	if(!has_missing_spc) {
	  has_missing_spc = 1;
	  if(type_spc == NCL_float) {
	    missing_spc = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis;
	  }
	  else {
	    missing_spc = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis;
	  }
	}
	if(type_spc == NCL_float) {
	  tmp_spc = (double)missing_spc.floatval;
	}
	else {
	  tmp_spc = missing_spc.doubleval;
	}
      }
/*
 * Coerce output back to float if necessary.
 */
      coerce_output_float_or_double(spc,&tmp_spc,type_spc,1,index_s);
    }
  }
/*
 * Free unneeded memory.
 */
  NclFree(tmp_x);
  NclFree(tmp_y);

/*
 * Return value back to NCL script.
 */
  if(!has_missing_spc) {
    ret = NclReturnValue(spc,ndims_spc,dsizes_spc,NULL,type_spc,0);
  }
  else {
    ret = NclReturnValue(spc,ndims_spc,dsizes_spc,&missing_spc,type_spc,0);
  }
  NclFree(dsizes_spc);
  return(ret);
}

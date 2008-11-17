#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(spctimcross2,SPCTIMCROSS2)(int *, int *, int *, double *,
                                               double *, double *, int *,
                                               int *, double *, int *,
                                               double *, int *);

NhlErrorTypes space_time_cross_segment_W( void )
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
  int has_missing_x;
  NclScalar missing_x, missing_flt_x, missing_dbl_x;
  NclBasicDataTypes type_x;

/*
 * Argument # 1
 */
  void *y;
  double *tmp_y;
  int ndims_y, dsizes_y[NCL_MAX_DIMENSIONS];
  int has_missing_y;
  NclScalar missing_y, missing_flt_y, missing_dbl_y;
  NclBasicDataTypes type_y;

/*
 * Argument # 2
 */
  int *opt;
/*
 * Return variable
 */
  void *stc;
  double *tmp_stc;
  int *dsizes_stc, has_missing_stc;
  NclScalar missing_stc, missing_flt_stc, missing_dbl_stc;
  NclBasicDataTypes type_stc;

/*
 * Various
 */
  int nt, nm, nl, ntml, nt2p1, nlp1, lsave1, lsave2;
  int index_x, index_stc;
  double *wsave1, *wsave2;
  int i, ndims_leftmost, size_leftmost, size_stc, size_output, ret;

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
           2);

/*
 * Check dimension sizes.
 */
  if(ndims_x < 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"space_time_cross_segment: The x array must have at least 3 dimensions");
    return(NhlFATAL);
  }

/*
 * Coerce missing value to double if necessary.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dbl_x,
                 &missing_flt_x);

  nt = dsizes_x[ndims_x-3];
  nm = dsizes_x[ndims_x-2];
  nl = dsizes_x[ndims_x-1];
  ntml = nt * nm * nl;

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
           2);

/*
 * Check dimension sizes.
 */
  if(ndims_y != ndims_x) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"space_time_cross_segment: The x and y arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_x; i++) {
    if(dsizes_y[i] != dsizes_x[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"space_time_cross_segment: The x and y arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }

/*
 * Coerce missing value to double if necessary.
 */
  coerce_missing(type_y,has_missing_y,&missing_y,&missing_dbl_y,
                 &missing_flt_y);

/*
 * Get argument # 2
 */
  opt = (int*)NclGetArgValue(
           2,
           3,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           2);

/*
 * Calculate size of leftmost dimensions.
 */
  size_leftmost  = 1;
  ndims_leftmost = ndims_x-3;
  for(i = 0; i < ndims_leftmost; i++) size_leftmost *= dsizes_x[i];

/*
 * The output type defaults to float, unless this input array is double.
 */
  type_stc = NCL_float;

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
    tmp_x = (double *)calloc(ntml,sizeof(double));
    if(tmp_x == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"space_time_cross_segment: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_stc = NCL_double;
  }
/*
 * Allocate space for tmp_y.
 */
  if(type_y != NCL_double) {
    tmp_y = (double *)calloc(ntml,sizeof(double));
    if(tmp_y == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"space_time_cross_segment: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_stc = NCL_double;
  }

/*
 * Calculate size of output array.
 */
  nlp1  = nl+1;
  nt2p1 = nt/2+1;

  size_stc    = nlp1 * nt2p1 * 16;
  size_output = size_leftmost * size_stc;

/* 
 * Allocate space for output array.
 */
  if(type_stc != NCL_double) {
    stc     = (void *)calloc(size_output, sizeof(float));
    tmp_stc = (double *)calloc(size_stc,sizeof(double));
    if(tmp_stc == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"space_time_cross_segment: Unable to allocate memory for temporary output array");
      return(NhlFATAL);
    }
  }
  else {
    stc = (void *)calloc(size_output, sizeof(double));
  }
  if(stc == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"space_time_cross_segment: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
  if(has_missing_x) {
    if(type_stc == NCL_double) missing_stc = missing_dbl_x;
    else                       missing_stc = missing_flt_x;
    missing_dbl_stc = missing_dbl_x;
  }

/* 
 * Allocate space for output dimension sizes and set them.
 */
  dsizes_stc = (int*)calloc(ndims_x,sizeof(int));  
  if( dsizes_stc == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"space_time_cross_segment: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_x-3; i++) dsizes_stc[i] = dsizes_x[i];
  dsizes_stc[ndims_x-3] = 16;
  dsizes_stc[ndims_x-2] = nt2p1;
  dsizes_stc[ndims_x-1] = nlp1;

/* 
 * Allocate space for work arrays.
 */
  lsave1 = 4*nl + 15;
  lsave2 = 4*nt + 15;
  wsave1 = (double *)calloc(lsave1, sizeof(double));
  wsave2 = (double *)calloc(lsave2, sizeof(double));
  if(wsave1 == NULL || wsave2 == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"space_time_cross_segment: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Loop across leftmost dimensions and call the Fortran routine for each
 * subsection of the input arrays.
 */
  index_x = index_stc = 0;

  for(i = 0; i < size_leftmost; i++) {
/*
 * Coerce subsection of x (tmp_x) to double if necessary.
 */
    if(type_x != NCL_double) {
      coerce_subset_input_double(x,tmp_x,index_x,type_x,ntml,0,NULL,NULL);
    }
    else {
      tmp_x = &((double*)x)[index_x];
    }

/*
 * Coerce subsection of y (tmp_y) to double if necessary.
 */
    if(type_y != NCL_double) {
      coerce_subset_input_double(y,tmp_y,index_x,type_y,ntml,0,NULL,NULL);
    }
    else {
      tmp_y = &((double*)y)[index_x];
    }

/*
 * Point temporary output array to void output array if appropriate.
 */
    if(type_stc == NCL_double) tmp_stc = &((double*)stc)[index_stc];

/*
 * Call the Fortran routine.
 */
    NGCALLF(spctimcross2,SPCTIMCROSS2)(&nl, &nm, &nt, tmp_x, tmp_y, tmp_stc, 
                                       &nlp1, &nt2p1, wsave1, &lsave1,
                                       wsave2,&lsave2);

/*
 * Coerce output back to float if necessary.
 */
    if(type_stc == NCL_float) {
      coerce_output_float_only(stc,tmp_stc,size_stc,index_stc);
    }
    index_x   += ntml;
    index_stc += size_stc;
  }

/*
 * Free unneeded memory.
 */
  if(type_x   != NCL_double) NclFree(tmp_x);
  if(type_y   != NCL_double) NclFree(tmp_y);
  if(type_stc != NCL_double) NclFree(tmp_stc);
  NclFree(wsave1);
  NclFree(wsave2);

/*
 * Return value back to NCL script.
 */
  if(type_stc != NCL_double) {
    ret = NclReturnValue(stc,ndims_x,dsizes_stc,&missing_flt_stc,type_stc,0);
  }
  else {
    ret = NclReturnValue(stc,ndims_x,dsizes_stc,&missing_dbl_stc,type_stc,0);
  }
  NclFree(dsizes_stc);
  return(ret);
}


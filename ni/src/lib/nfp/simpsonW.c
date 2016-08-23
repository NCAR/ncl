#include <stdio.h>
#include <math.h>
#include "wrapper.h"

extern void NGCALLF(dsimpeq,DSIMPEQ)(int*,double*,double*,double*);
extern void NGCALLF(dsimpne,DSIMPNE)(int*,double*,double*,double*,double*);

NhlErrorTypes simpeq_W( void )
{
/*
 * Input array variables
 */
  void *f, *x;
  double *tmp_f = NULL;
  double *tmp_x = NULL;
  int ndims_f;
  ng_size_t dsizes_f[NCL_MAX_DIMENSIONS];
  int has_missing_f;
  int nmiss, found_missing;
  NclScalar missing_f, missing_df, missing_rf;
  NclBasicDataTypes type_f, type_x;

/*
 * Output array variables
 */
  void *simpeq;
  double *tmp_simpeq = NULL;
  NclBasicDataTypes type_simpeq;
  int ndims_simpeq;
  ng_size_t *dsizes_simpeq;
  NclScalar missing_simpeq;

/*
 * Declare various variables for random purposes.
 */
  ng_size_t i, index_f, npts, size_leftmost;
  int inpts, ret;

/*
 * Retrieve arguments.
 */
  f = (void*)NclGetArgValue(
          0,
          2,
          &ndims_f,
          dsizes_f,
          &missing_f,
          &has_missing_f,
          &type_f,
          DONT_CARE);

  npts = dsizes_f[ndims_f-1];

/*
 * Test dimension sizes.
 */
  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"simpeq: npts is greater than INT_MAX");
    return(NhlFATAL);
  }
  inpts = (int) npts;

  x = (void*)NclGetArgValue(
          1,
          2,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_x,
          DONT_CARE);

/*
 * Compute size of the output array (size_leftmost).
 */
  if(ndims_f > 1) {
    ndims_simpeq = ndims_f-1;
  }
  else {
    ndims_simpeq = 1;
  }
  dsizes_simpeq = (ng_size_t*)calloc(ndims_simpeq,sizeof(ng_size_t));
  size_leftmost = 1;
  if(ndims_simpeq > 1) {
    for( i = 0; i < ndims_f-1; i++ ) {
      dsizes_simpeq[i] = dsizes_f[i];
      size_leftmost    *= dsizes_f[i];
    }
  }
  else {
    dsizes_simpeq[0] = 1;
  }

/*
 * Coerce missing values to double if necessary.
 */
  coerce_missing(type_f,has_missing_f,&missing_f,&missing_df,&missing_rf);

/*
 * Create space for output array.
 */
  if(type_f != NCL_double) {
    type_simpeq = NCL_float;
    simpeq      = (void*)calloc(size_leftmost,sizeof(float));
    tmp_f       = (double*)calloc(npts,sizeof(double));
    tmp_simpeq  = (double*)calloc(1,sizeof(double));
    missing_simpeq.floatval = missing_rf.floatval;
    if(tmp_f == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"simpeq: Unable to allocate memory for temporary array");
      return(NhlFATAL);
    }
  }
  else {
    type_simpeq = NCL_double;
    simpeq      = (void*)calloc(size_leftmost,sizeof(double));
    missing_simpeq.doubleval = missing_df.doubleval;
  }

  if( simpeq == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"simpeq: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Coerce scalar x to double.
 */
  tmp_x = coerce_input_double(x,type_x,1,0,NULL,NULL);

/*
 * Call the Fortran version of this routine.
 */

  nmiss = index_f = 0;
  for( i = 0; i < size_leftmost; i++ ) {
    if(type_f != NCL_double) {
/*
 * Coerce npts subsection of f (tmp_f) to double.
 */
      coerce_subset_input_double(f,tmp_f,index_f,type_f,npts,has_missing_f,
                                 &missing_f,&missing_df);
    }
    else {
/*
 * Point tmp_f/tmp_simpeq to appropriate location in f/simpeq.
 */
      tmp_f = &((double*)f)[index_f];
    }
    if(type_simpeq == NCL_double) {
      tmp_simpeq = &((double*)simpeq)[i];
    }

/*
 * If any missing values are present, put missing values in this
 * particular value of s and continue.
 */
    found_missing = contains_missing(tmp_f,npts,has_missing_f,
                                     missing_df.doubleval);
    if(found_missing) {
      nmiss++;
      set_subset_output_missing(simpeq,i,type_simpeq,1,
                                missing_df.doubleval);
    }
    else {
      NGCALLF(dsimpeq,DSIMPEQ)(&inpts,tmp_x,tmp_f,tmp_simpeq);

      if(type_simpeq != NCL_double) {
        coerce_output_float_only(simpeq,tmp_simpeq,1,i);
      }
    }
    index_f += npts;
  }

/*
 * Free memory.
 */
  if(type_x != NCL_double) NclFree(tmp_x);
  if(type_f != NCL_double) {
    NclFree(tmp_simpeq);
    NclFree(tmp_f);
  }

  if(nmiss) {
    ret = NclReturnValue(simpeq,ndims_simpeq,dsizes_simpeq,
                          &missing_simpeq,type_simpeq,0);
  }
  else {
    ret = NclReturnValue(simpeq,ndims_simpeq,dsizes_simpeq,NULL,
                          type_simpeq,0);
  }
  NclFree(dsizes_simpeq);
  return(ret);
}

NhlErrorTypes simpne_W( void )
{
/*
 * Input array variables
 */
  void *x, *y;
  double *tmp_x = NULL;
  double *tmp_y = NULL;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  int ndims_y;
  ng_size_t dsizes_y[NCL_MAX_DIMENSIONS];
  int has_missing_y;
  int nmiss, nvalid, found_missing_x;
  NclScalar missing_x, missing_dx, missing_rx;
  NclScalar missing_y, missing_dy, missing_ry;
  NclBasicDataTypes type_x, type_y;

/*
 * Output array variables
 */
  void *simpne;
  double *tmp_simpne = NULL;
  NclBasicDataTypes type_simpne;
  int ndims_simpne;
  ng_size_t *dsizes_simpne;
  NclScalar missing_simpne;

/*
 * Declare various variables for random purposes.
 */
  ng_size_t i, index_y, npts, size_leftmost;
  int inpts, ret;

/*
 * Retrieve arguments.
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
 * Check dimensions of X and Y. They must either be the same size,
 * or X can be 1D and equal in length to the rightmost dimension of Y.
 */
  if(ndims_x == ndims_y) {
    for(i = 0; i < ndims_x; i++) {
      if(dsizes_x[i] != dsizes_y[i]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"simpne: If the input arrays have the same number of dimensions, they must be the same size.");
        return(NhlFATAL);
      }
    }
  }
  else {
    if( ndims_x != 1 ||
       (ndims_x == 1 && dsizes_x[0] != dsizes_y[ndims_y-1])) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"simpne: x must be the same size as y, or a 1D array equal in length to y's rightmost dimension.");
      return(NhlFATAL);
    }
  }

  npts = dsizes_y[ndims_y-1];

/*
 * Test dimension sizes.
 */
  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"simpne: npts is greater than INT_MAX");
    return(NhlFATAL);
  }
  inpts = (int) npts;

/*
 * Compute size of the output array (size_leftmost).
 */
  if(ndims_y > 1) {
    ndims_simpne = ndims_y-1;
  }
  else {
    ndims_simpne = 1;
  }
  dsizes_simpne = (ng_size_t*)calloc(ndims_simpne,sizeof(ng_size_t));
  size_leftmost = 1;
  if(ndims_simpne > 1) {
    for( i = 0; i < ndims_y-1; i++ ) {
      dsizes_simpne[i] = dsizes_y[i];
      size_leftmost *= dsizes_y[i];
    }
  }
  else {
    dsizes_simpne[0] = 1;
  }

/*
 * Coerce missing values to double if necessary.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
  coerce_missing(type_y,has_missing_y,&missing_y,&missing_dy,&missing_ry);

/*
 * Create space for output array.
 */
  type_simpne = NCL_float;
  if(type_x == NCL_double) {
    type_simpne = NCL_double;
  }
  else {
    tmp_x = (double*)calloc(npts,sizeof(double));
    if(tmp_x == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"simpne: Unable to allocate memory for temporary array");
      return(NhlFATAL);
    }
  }

  if(type_y == NCL_double) {
    type_simpne = NCL_double;
  }
  else {
    tmp_y = (double*)calloc(npts,sizeof(double));
    if(tmp_y == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"simpne: Unable to allocate memory for temporary array");
      return(NhlFATAL);
    }
  }
  if(type_simpne == NCL_double) {
    simpne                   = (void*)calloc(size_leftmost,sizeof(double));
    missing_simpne.doubleval = missing_dy.doubleval;
  }
  else {
    simpne                  = (void*)calloc(size_leftmost,sizeof(float));
    tmp_simpne              = (double*)calloc(1,sizeof(double));
    missing_simpne.floatval = missing_ry.floatval;
  }

  if( simpne == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"simpne: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * If X is 1D, convert it to double here, and don't worry about it
 * inside the loop. If it is n-D, then we'll convert each chunk of
 * it inside the loop.
 */
  if(ndims_x == 1) {
    if(type_x != NCL_double) {
/*
 * Coerce npts subsection of x (tmp_x) to double.
 */
      coerce_subset_input_double(x,tmp_x,0,type_x,npts,has_missing_x,
                                 &missing_x,&missing_dx);
    }
    else {
/*
 * Point tmp_x to x.
 */
      tmp_x = &((double*)x)[0];
    }
  }

/*
 * Call the Fortran version of this routine.
 */
  nmiss = index_y = 0;
  for( i = 0; i < size_leftmost; i++ ) {
/*
 * Coerce subset of x to double if necessary. 
 */
    if(ndims_x > 1) {
      if(type_x != NCL_double) {
        coerce_subset_input_double(x,tmp_x,index_y,type_x,npts,has_missing_x,
                                   &missing_x,&missing_dx);
      }
      else {
        tmp_x = &((double*)x)[index_y];
      }
    }

/*
 * Coerce subset of y to double if necessary. 
 */
    if(type_y != NCL_double) {
      coerce_subset_input_double(y,tmp_y,index_y,type_y,npts,has_missing_y,
                                 &missing_y,&missing_dy);
    }
    else {
      tmp_y = &((double*)y)[index_y];
    }

/*
 * Point tmp_simpne to appropriate location in simpne if necessary.
 */
    if(type_simpne == NCL_double) {
      tmp_simpne = &((double*)simpne)[i];
    }

/*
 * x should not have missing values. If it does, we put a missing
 * value in the place of the output value.
 */
    found_missing_x = contains_missing(tmp_x,npts,has_missing_x,
                                       missing_dx.doubleval);
    if(found_missing_x) {
      nmiss++;
      set_subset_output_missing(simpne,i,type_simpne,1,missing_dy.doubleval);
    }
    else {
      NGCALLF(dsimpne,DSIMPNE)(&inpts,tmp_x,tmp_y,&missing_dy.doubleval,
                               tmp_simpne);

      if(type_simpne != NCL_double) {
        coerce_output_float_only(simpne,tmp_simpne,1,i);
      }
    }
    index_y += npts;
  }
  nvalid = size_leftmost - nmiss;
  if(nvalid < 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"simpne: Must have three or more non-missing values.");
    return(NhlFATAL);
  }
/*
 * Free memory.
 */
  if(type_x != NCL_double) NclFree(tmp_x);
  if(type_y != NCL_double) NclFree(tmp_y);
  if(type_simpne != NCL_double) {
    NclFree(tmp_simpne);
  }

  if(nmiss) {
    ret = NclReturnValue(simpne,ndims_simpne,dsizes_simpne,
                          &missing_simpne,type_simpne,0);
  }
  else {
    ret = NclReturnValue(simpne,ndims_simpne,dsizes_simpne,NULL,
                          type_simpne,0);
  }
  NclFree(dsizes_simpne);
  return(ret);
}

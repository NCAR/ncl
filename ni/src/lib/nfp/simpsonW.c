#include <stdio.h>
#include <math.h>
#include "wrapper.h"

extern void NGCALLF(dsimpn,DSIMPN)(int*,double*,double*,double*);

NhlErrorTypes simpson_W( void )
{
/*
 * Input array variables
 */
  void *f, *x;
  double *tmp_f, *tmp_x;
  int ndims_f, dsizes_f[NCL_MAX_DIMENSIONS], has_missing_f;
  int nmiss, found_missing;
  NclScalar missing_f, missing_df, missing_rf;
  NclBasicDataTypes type_f, type_x;

/*
 * Output array variables
 */
  void *simpson;
  double *tmp_simpson;
  NclBasicDataTypes type_simpson;
  int ndims_simpson, *dsizes_simpson;
  NclScalar missing_simpson;

/*
 * Declare various variables for random purposes.
 */
  int i, index_f, npts, size_leftmost;

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
          2);

  npts = dsizes_f[ndims_f-1];

  x = (void*)NclGetArgValue(
          0,
          2,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_x,
          2);

/*
 * Compute size of the output array (size_leftmost).
 */
  if(ndims_f > 1) {
    ndims_simpson = ndims_f-1;
  }
  else {
    ndims_simpson = 1;
  }
  dsizes_simpson = (int*)calloc(ndims_simpson,sizeof(int));
  size_leftmost = 1;
  if(ndims_simpson > 1) {
    for( i = 0; i < ndims_f-1; i++ ) {
      dsizes_simpson[i] = dsizes_f[i];
      size_leftmost    *= dsizes_f[i];
    }
  }
  else {
    dsizes_simpson[0] = 1;
  }

/*
 * Coerce missing values to double if necessary.
 */
  coerce_missing(type_f,has_missing_f,&missing_f,&missing_df,&missing_rf);

/*
 * Create space for output array.
 */
  if(type_f != NCL_double) {
    type_simpson = NCL_float;
    simpson      = (void*)calloc(size_leftmost,sizeof(float));
    tmp_f        = (double*)calloc(npts,sizeof(double));
    tmp_simpson  = (double*)calloc(1,sizeof(double));
    missing_simpson.floatval = missing_rf.floatval;
  }
  else {
    type_simpson = NCL_double;
    simpson      = (void*)calloc(size_leftmost,sizeof(double));
    missing_simpson.doubleval = missing_df.doubleval;
    if(tmp_f == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"simpson: Unable to allocate memory for temporary array");
      return(NhlFATAL);
    }
  }

  if( simpson == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"simpson: Unable to allocate memory for output array");
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
 * Point tmp_f/tmp_simpson to appropriate location in f/simpson.
 */
      tmp_f = &((double*)f)[index_f];
    }
    if(type_simpson == NCL_double) {
      tmp_simpson = &((double*)simpson)[i];
    }

/*
 * If any missing values are present, put missing values in this
 * particular value of s and continue.
 */
    found_missing = contains_missing(tmp_f,npts,has_missing_f,
                                     missing_df.doubleval);
    if(found_missing) {
      nmiss++;
      set_subset_output_missing(simpson,i,type_simpson,1,
                                missing_df.doubleval);
    }
    else {
      NGCALLF(dsimpn,DSIMPN)(&npts,tmp_x,tmp_f,tmp_simpson);

      if(type_simpson != NCL_double) {
        coerce_output_float_only(simpson,tmp_simpson,1,i);
      }
    }
    index_f += npts;
  }

/*
 * Free memory.
 */
  if(type_x != NCL_double) clFree(tmp_x);
  if(type_f != NCL_double) {
    NclFree(tmp_simpson);
    NclFree(tmp_f);
  }

  if(nmiss) {
    return(NclReturnValue(simpson,ndims_simpson,dsizes_simpson,
                          &missing_simpson,type_simpson,0));
  }
  else {
    return(NclReturnValue(simpson,ndims_simpson,dsizes_simpson,NULL,
                          type_simpson,0));
  }
}

#include <stdio.h>
#include <math.h>
#include "wrapper.h"

extern void NGCALLF(wksmooth121,WKSMOOTH121)(double*,int*,int*,double*,
                                             double*);

NhlErrorTypes wk_smooth121_W( void )
{
/*
 * Input array variables
 */
  void *x;
  double *tmp_x = NULL;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclBasicDataTypes type_x;
  NclScalar missing_x, missing_dx, missing_rx;
/*
 * Work array.
 */
  double *work;
/*
 * Declare various variables for random purposes.
 */
  ng_size_t i, index_x, npts, total_leftmost, total_size_x;
  int inpts;

/*
 * Retrieve arguments.
 */
  x = (void*)NclGetArgValue(
          0,
          1,
          &ndims_x,
          dsizes_x,
          &missing_x,
          &has_missing_x,
          &type_x,
          1);

  if( type_x != NCL_float && type_x != NCL_double ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wk_smooth121: The type of the input/output array must be float or double");
    return(NhlFATAL);
  }

/*
 * Test dimension size.
 */
  npts = dsizes_x[ndims_x-1];
  
  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wk_smooth121: npts = %ld is greater than INT_MAX", npts);
    return(NhlFATAL);
  }
  inpts = (int) npts;
/*
 * Compute the total size of the output array (minus the last dimension).
 */
  total_leftmost = 1;
  for( i = 0; i < ndims_x-1; i++ ) total_leftmost *= dsizes_x[i];
  total_size_x = total_leftmost * npts;
/*
 * Coerce the missing value.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Create a temporary array to hold subarrays of x.
 */
  if(type_x == NCL_float) {
    tmp_x = (double*)calloc(npts,sizeof(double));
    if( tmp_x == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wk_smooth121: Unable to allocate memory for coercing x array to double precision");
      return(NhlFATAL);
    }
  }

/*
 * Allocate space for work array.
 */
  work = (double *)calloc(npts,sizeof(double));
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wk_smooth121: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  
/*
 * Call the Fortran version of this routine.
 */
  index_x = 0;
  for( i = 0; i < total_leftmost; i++ ) {
/*
 * Coerce subsection of x (tmp_x) to double.
 */
    if(type_x != NCL_double) {
      coerce_subset_input_double(x,tmp_x,index_x,type_x,npts,0,NULL,NULL);
    }
    else {
      tmp_x = &((double*)x)[index_x];
    }

    NGCALLF(wksmooth121,WKSMOOTH121)(tmp_x,&inpts,&inpts,&missing_dx.doubleval,
				     work);

    if(type_x != NCL_double) {
      coerce_output_float_only(x,tmp_x,npts,index_x);
    }
    index_x += npts;
  }
/*
 * Free memory.
 */
  NclFree(work);
  if(type_x != NCL_double) NclFree(tmp_x);

  return(NhlNOERROR);
}

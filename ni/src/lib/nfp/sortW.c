#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "wrapper.h"

extern void NGCALLF(dpsortdriver,DPSORTDRIVER)(double*, int*, int*, int*,
                           int*);

NhlErrorTypes dim_pqsort_W( void )
{
/*
 * Input array variables
 */
  void *x;
  int *kflag;
  double *tmp_x;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  NclScalar missing_x, missing_dx, missing_rx;
  NclBasicDataTypes type_x;
/*
 * Output array variables
 */
  int *iperm;
/*
 * various
 */
  int i, j, index_x, total_elements, ier = 0, ndim;
/*
 * Retrieve parameter.
 */
  x = (void*)NclGetArgValue(
           0,
           2,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           2);

  kflag = (void*)NclGetArgValue(
           1,
           2,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           2);
/*
 * Input array must be integer, float, or double.
 */
  if(type_x != NCL_int && type_x != NCL_float && type_x != NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_pqsort: The input array must be of type integer, float, or double");
    return(NhlFATAL);
  }
/*
 * Check kflag.
 */
  if (*kflag < -2 || *kflag > 2 || *kflag == 0) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_pqsort: kflag must be 2, 1, -1, or -2.");
    return(NhlFATAL);
  }

/*
 * Compute the total number of elements in output and input.
 */
  total_elements = 1;
  for(i = 0; i < ndims_x-1; i++) {
    total_elements *= dsizes_x[i];
  }    

  ndim = dsizes_x[ndims_x-1];
/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Allocate space for in/output arrays.
 */
  if(type_x != NCL_double) {
    tmp_x     = (double*)calloc(ndim,sizeof(double));
    if(tmp_x == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_pqsort: Unable to allocate memory for input array");
      return(NhlFATAL);
    }
  }
  iperm = (int*)calloc(ndim*total_elements,sizeof(int));
  if (iperm == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_pqsort: Unable to allocate memory for output array" );
    return(NhlFATAL);
  }

/*
 * Call the f77 double version of 'pqsort' with the full argument list.
 */
  index_x = 0;
  for(i = 0; i < total_elements; i++) {
    if(type_x != NCL_double) {
/*
 * Coerce subsection of x (tmp_x) to double.
 */
      coerce_subset_input_double(x,tmp_x,index_x,type_x,ndim,0,NULL,NULL);
    }
    else {
      tmp_x = &((double*)x)[index_x];
    }

    NGCALLF(dpsortdriver,DPSORTDRIVER)(tmp_x,&ndim,&iperm[index_x],kflag,&ier);

    if((*kflag ==2 || *kflag == -2) && type_x != NCL_double) {
      if(type_x == NCL_int) {
        for(j = 0; j < ndim; j++) {
          ((int*)x)[index_x+j] = (int)(tmp_x[j]);
        }
      }
      else {
        coerce_output_float_only(x,tmp_x,ndim,index_x);
      }
    }
    index_x += ndim;
  }
/*
 * Free unneeded memory.
 */
  if(type_x != NCL_double) NclFree(tmp_x);
/*
 * Return.
 */
  return(NclReturnValue(iperm,ndims_x,dsizes_x,NULL,NCL_int,0));
}



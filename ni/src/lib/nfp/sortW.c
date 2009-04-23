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
           DONT_CARE);

  kflag = (void*)NclGetArgValue(
           1,
           2,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
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
 * Compute the total number of elements in output and input, minus
 * the last dimension.
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


NhlErrorTypes dim_pqsort_n_W( void )
{
/*
 * Input array variables
 */
  void *x;
  int *kflag;
  int *dims;
  double *tmp_x;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  NclScalar missing_x, missing_dx, missing_rx;
  NclBasicDataTypes type_x;
/*
 * Output array variables
 */
  int *iperm, *tmp_iperm;
/*
 * various
 */
  int i, j, k, index_x, total_nl, total_nr, total_elements, ier = 0, ndim;
/*
 * Retrieve parameter.
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

  kflag = (int*)NclGetArgValue(
           1,
           3,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

  dims = (int*)NclGetArgValue(
           2,
           3,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
/*
 * Some error checking. Make sure input dimension is valid.
 */
  if(*dims < 0 || *dims >= ndims_x) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_pqsort_n: Invalid dimension argument, can't continue");
    return(NhlFATAL);
  }


/*
 * Input array must be integer, float, or double.
 */
  if(type_x != NCL_int && type_x != NCL_float && type_x != NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_pqsort_n: The input array must be of type integer, float, or double");
    return(NhlFATAL);
  }
/*
 * Check kflag.
 */
  if (*kflag < -2 || *kflag > 2 || *kflag == 0) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_pqsort_n: kflag must be 2, 1, -1, or -2.");
    return(NhlFATAL);
  }

/*
 * Compute the total number of elements in output and input, minus
 * the dims-th argument.
 */
  total_nl = total_nr = total_elements = 1;
  for(i = 0; i < *dims;   i++)       total_nl *= dsizes_x[i];
  for(i = *dims+1; i < ndims_x; i++) total_nr *= dsizes_x[i];
  total_elements = total_nr * total_nl;

  ndim = dsizes_x[*dims];
/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Allocate space for in/output arrays.
 */
  tmp_x = (double*)calloc(ndim,sizeof(double));
  if(tmp_x == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_pqsort_n: Unable to allocate memory for input array");
    return(NhlFATAL);
  }
  tmp_iperm = (int*)calloc(ndim,sizeof(int));
  iperm     = (int*)calloc(ndim*total_elements,sizeof(int));
  if (iperm == NULL || tmp_iperm == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_pqsort_n: Unable to allocate memory for output array" );
    return(NhlFATAL);
  }

/*
 * Call the f77 double version of 'pqsort' with the full argument list.
 */
  for(i = 0; i < total_nl; i++) {
    for(j = 0; j < total_nr; j++) {
      for(k = 0; k < ndim; k++) {
        index_x = (i*total_nr*ndim) + (k*total_nr) + j;
/*
 * Coerce subsection of x (tmp_x) to double.
 */
	coerce_subset_input_double(x,&tmp_x[k],index_x,type_x,1,0,NULL,NULL);
      }
      
      NGCALLF(dpsortdriver,DPSORTDRIVER)(tmp_x,&ndim,tmp_iperm,kflag,&ier);

      for(k = 0; k < ndim; k++) {
	index_x = (i*total_nr*ndim) + (k*total_nr) + j;

	iperm[index_x] = tmp_iperm[k]; /* Output permutation vector */

	if(*kflag ==2 || *kflag == -2) {
/*
 * Need to sort the original x array as well.
 */
	  if(type_x == NCL_int) {
	    ((int*)x)[index_x] = (int)(tmp_x[k]);
	  }
	  else {
	    coerce_output_float_or_double(x,&tmp_x[k],type_x,1,index_x);
	  }
	}
      }
    }
  }
/*
 * Free unneeded memory.
 */
  NclFree(tmp_x);
/*
 * Return.
 */
  return(NclReturnValue(iperm,ndims_x,dsizes_x,NULL,NCL_int,0));
}



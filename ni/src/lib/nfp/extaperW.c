#include <stdio.h>
#include <string.h>
/*
* The following are the required NCAR Graphics include files.
* They should be located in ${NCARG_ROOT}/include
*/
#include "wrapper.h"

extern void NGCALLF(dexptapersh,DEXPTAPERSH)(int*,int*,double*,double*,
                                             int*,int*,int*);

NhlErrorTypes exp_tapershC_W( void )
{
/*
 * Input array variables
 */
  void *ab;
  double *tmp_a, *tmp_b;
  void *new_ab;
  int ndims_ab, dsizes_ab[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_ab, type_new_ab;
  int *iwave, *noption;
/*
 * various
 */
  int i, j, total_leftmost, total_size_ab, total_size_ab2;
  int start, index_ab, nm, nb, mb, ier;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  ab = (void*)NclGetArgValue(
          0,
          3,
          &ndims_ab, 
          dsizes_ab,
          NULL,
          NULL,
          &type_ab,
          2);
/*
 * The grid coming in must be at least 3-dimensional.
 */
  if(ndims_ab < 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"exp_tapershC: The input array must be at least 3-dimensional");
    return(NhlFATAL);
  }
  if(dsizes_ab[0] != 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"exp_tapershC: The first dimension of the input array must be 2");
    return(NhlFATAL);
  }

/*
 * Get iwave and noption.
 */
  iwave = (int*)NclGetArgValue(
       1,
       3,
       NULL,
       NULL,
       NULL,
       NULL,
       NULL,
       2);

  noption = (int*)NclGetArgValue(
       2,
       3,
       NULL,
       NULL,
       NULL,
       NULL,
       NULL,
       2);
  
/*
 * Hard-code noption to 10 for now.
 */
  *noption = 10;

/*
 * Compute the total number of elements in our array.
 */
  nb  = dsizes_ab[ndims_ab-2];
  mb  = dsizes_ab[ndims_ab-1];
  nm = nb * mb;

  total_leftmost = 1;
  for(i = 1; i < ndims_ab-2; total_leftmost*=dsizes_ab[i],i++);

  total_size_ab  = total_leftmost*nm;
  total_size_ab2 = 2*total_size_ab;
/*
 * Create space for temporary a and b arrays.
 */
  tmp_a = (double*)calloc(nm,sizeof(double));
  tmp_b = (double*)calloc(nm,sizeof(double));
  if( tmp_a == NULL || tmp_b == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"exp_tapershC: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
/*
 * Allocate space for output array.
 */
  if(type_ab != NCL_double) {
    type_new_ab = NCL_float;
    new_ab = (void*)calloc(total_size_ab2,sizeof(float));
  }
  else {
    type_new_ab = NCL_double;
    new_ab = (void*)calloc(total_size_ab2,sizeof(double));
  }
  if( new_ab == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"exp_tapershC: Unable to allocate memory for coercing ab array to double precision");
    return(NhlFATAL);
  }
/*
 * Loop through total_leftmost and call Fortran function
 */
  index_ab = 0;
  start    = total_leftmost*nm;

  for(i = 0; i < total_leftmost; i++) {
/*
 * Coerce subsection of ab to temporary a and b.
 */
    coerce_subset_input_double(ab,tmp_a,index_ab,type_ab,nm,0,NULL,NULL);
    coerce_subset_input_double(ab,tmp_b,start+index_ab,type_ab,nm,0,
                               NULL,NULL);

    NGCALLF(dexptapersh,DEXPTAPERSH)(&mb,&nb,tmp_a,tmp_b,iwave,noption,&ier);
/*
 * Copy a and b arrays back into new_ab array.
 */
    coerce_output_float_or_double(new_ab,tmp_a,type_ab,nm,index_ab);
    coerce_output_float_or_double(new_ab,tmp_b,type_ab,nm,index_ab+start);
    index_ab += nm;
  }

/*
 * Free work arrays.
 */ 
  NclFree(tmp_a);
  NclFree(tmp_b);
/*
 * Return values. 
 */
  return(NclReturnValue(new_ab,ndims_ab,dsizes_ab,NULL,type_new_ab,0));
}


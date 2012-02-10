#include <stdio.h>
#include <string.h>
#include "wrapper.h"

extern void NGCALLF(drhombtrunc,DRHOMBTRUNC)(int*,int*,double*,double*,
                                             int*);
extern void NGCALLF(dtritrunc,DTRITRUNC)(int*,int*,int*,double*,double*);

NhlErrorTypes rhomb_trunC_W( void )
{
/*
 * Input array variables
 */
  void *ab;
  double *tmp_a, *tmp_b;
  void *new_ab;
  int ndims_ab;
  ng_size_t dsizes_ab[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_ab, type_new_ab;
  ng_size_t nt, m, n, nm, total_size_ab, total_size_ab2;
  int *T;
/*
 * various
 */
  ng_size_t i;
  ng_size_t index_nm; 
  ng_size_t start;
  int im, in;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  ab = (void*)NclGetArgValue(
          0,
          2,
          &ndims_ab, 
          dsizes_ab,
          NULL,
          NULL,
          &type_ab,
          DONT_CARE);
/*
 * Get T
 */
  T = (int*)NclGetArgValue(
       1,
       2,
       NULL,
       NULL,
       NULL,
       NULL,
       NULL,
       DONT_CARE);
/*
 * The grid coming in must be at least 3-dimensional.
 */
  if(ndims_ab < 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rhomb_trunC: The input array must be at least 3-dimensional");
    return(NhlFATAL);
  }
  if(dsizes_ab[0] != 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rhomb_trunC: The first dimension of the input array must be 2");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */
  m  = dsizes_ab[ndims_ab-2];
  n  = dsizes_ab[ndims_ab-1];
  nm = n * m;

/*
 * Test input dimension sizes.
 */
  if((m > INT_MAX) || (n > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rhomb_trunC: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  im = (int) m;
  in = (int) n;

  nt = 1;
  for(i = 1; i < ndims_ab-2; nt*=dsizes_ab[i],i++);

  total_size_ab  = nt*nm;
  total_size_ab2 = 2*total_size_ab;
/*
 * Create space for temporary a and b arrays.
 */
  tmp_a = (double*)calloc(nm,sizeof(double));
  tmp_b = (double*)calloc(nm,sizeof(double));
  if( tmp_a == NULL || tmp_b == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rhomb_trunC: Unable to allocate memory for work arrays");
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
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rhomb_trunC: Unable to allocate memory for coercing ab array to double precision");
    return(NhlFATAL);
  }
/*
 * Loop through nt and call Fortran function
 */
  index_nm = 0;
  start    = nt*nm;

  for(i = 0; i < nt; i++) {
/*
 * Coerce subsection of ab to temporary a and b.
 */
    coerce_subset_input_double(ab,tmp_a,index_nm,type_ab,nm,0,NULL,NULL);
    coerce_subset_input_double(ab,tmp_b,start+index_nm,type_ab,nm,0,
                               NULL,NULL);

    NGCALLF(drhombtrunc,DRHOMBTRUNC)(&in,&im,tmp_a,tmp_b,T);

/*
 * Copy a and b arrays back into new_ab array.
 */
    coerce_output_float_or_double(new_ab,tmp_a,type_ab,nm,index_nm);
    coerce_output_float_or_double(new_ab,tmp_b,type_ab,nm,index_nm+start);
    index_nm += nm;
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


NhlErrorTypes tri_trunC_W( void )
{
/*
 * Input array variables
 */
  void *ab;
  double *tmp_a, *tmp_b;
  void *new_ab;
  int ndims_ab;
  ng_size_t dsizes_ab[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_ab, type_new_ab;
  ng_size_t nt, m, n, nm, total_size_ab, total_size_ab2;
  int *T;
/*
 * various
 */
  ng_size_t i;
  ng_size_t index_nm;
  ng_size_t start;
  int im, in;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  ab = (void*)NclGetArgValue(
          0,
          2,
          &ndims_ab, 
          dsizes_ab,
          NULL,
          NULL,
          &type_ab,
          DONT_CARE);
/*
 * Get T
 */
  T = (int*)NclGetArgValue(
       1,
       2,
       NULL,
       NULL,
       NULL,
       NULL,
       NULL,
       DONT_CARE);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if(ndims_ab < 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tri_trunC: The input array must be at least 3-dimensional");
    return(NhlFATAL);
  }
  if(dsizes_ab[0] != 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tri_trunC: The first dimension of the input array must be 2");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */
  m = dsizes_ab[ndims_ab-2];
  n = dsizes_ab[ndims_ab-1];
  nm = n * m;
  if(n != m) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tri_trunC: the last two dimensions of ab must be the same size");
    return(NhlFATAL);
  }

/*
 * Test input dimension sizes.
 */
  if((m > INT_MAX) || (n > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tri_trunC: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  im = (int) m;
  in = (int) n;

  nt = 1;
  for(i = 1; i < ndims_ab-2; nt*=dsizes_ab[i],i++);

  total_size_ab  = nt*nm;
  total_size_ab2 = 2*total_size_ab;
/*
 * Create space for temporary a and b arrays.
 */
  tmp_a = (double*)calloc(nm,sizeof(double));
  tmp_b = (double*)calloc(nm,sizeof(double));
  if( tmp_a == NULL || tmp_b == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tri_trunC: Unable to allocate memory for work arrays");
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
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tri_trunC: Unable to allocate memory for coercing ab array to double precision");
    return(NhlFATAL);
  }
/*
 * Loop through nt and call Fortran function
 */
  index_nm = 0;
  start    = nt*nm;

  for(i = 0; i < nt; i++) {
/*
 * Coerce subsection of ab to temporary a and b.
 */
    coerce_subset_input_double(ab,tmp_a,index_nm,type_ab,nm,0,NULL,NULL);
    coerce_subset_input_double(ab,tmp_b,start+index_nm,type_ab,nm,0,
                               NULL,NULL);

    NGCALLF(dtritrunc,DTRITRUNC)(&in, T, &im, tmp_a, tmp_b);

/*
 * Copy a and b arrays back into new_ab array.
 */
    coerce_output_float_or_double(new_ab,tmp_a,type_ab,nm,index_nm);
    coerce_output_float_or_double(new_ab,tmp_b,type_ab,nm,index_nm+start);
    index_nm += nm;
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

NhlErrorTypes rhomb_trunc_W( void )
{
/*
 * Input array variables
 */
  void *a, *b;
  double *tmp_a = NULL;
  double *tmp_b = NULL;
  int ndims_a;
  ng_size_t dsizes_a[NCL_MAX_DIMENSIONS];
  int ndims_b;
  ng_size_t dsizes_b[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_a, type_b;
  ng_size_t nt, m, n, nm, total_size_ab;
  int *T;
/*
 * various
 */
  ng_size_t i;
  ng_size_t index_nm;
  int im, in;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  a = (void*)NclGetArgValue(
          0,
          3,
          &ndims_a, 
          dsizes_a,
          NULL,
          NULL,
          &type_a,
          DONT_CARE);

  b = (void*)NclGetArgValue(
          1,
          3,
          &ndims_b,
          dsizes_b,
          NULL,
          NULL,
          &type_b,
          DONT_CARE);
/*
 * Get T
 */
  T = (int*)NclGetArgValue(
       2,
       3,
       NULL,
       NULL,
       NULL,
       NULL,
       NULL,
       DONT_CARE);
/*
 * The grids coming in must be at least 2-dimensional.
 */
  if(ndims_a != ndims_b || ndims_a < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rhomb_trunc: The input arrays must have the same number of dimensions and be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_a; i++ ) {
    if(dsizes_a[i] != dsizes_b[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rhomb_trunc: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Compute the total number of elements in our array.
 */
  m = dsizes_a[ndims_a-2];
  n = dsizes_a[ndims_a-1];
  nm = n * m;

/*
 * Test input dimension sizes.
 */
  if((m > INT_MAX) || (n > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rhomb_trunc: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  im = (int) m;
  in = (int) n;


  nt = 1;
  for(i = 0; i < ndims_a-2; nt*=dsizes_a[i],i++);
  total_size_ab = nt*nm;
/*
 * a and b must be float or double.
 */
  if((type_a != NCL_float && type_a != NCL_double) ||
     (type_b != NCL_float && type_b != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rhomb_trunc: a and b must be of type float or double");
    return(NhlFATAL);
  }
/*
 * Create temporary a and b arrays.
 */
  if(type_a != NCL_double) {
    tmp_a = (double*)calloc(nm,sizeof(double));
    if( tmp_a == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rhomb_trunc: Unable to allocate memory for coercing a array to double precision");
      return(NhlFATAL);
    }
  }
  if(type_b != NCL_double) {
    tmp_b = (double*)calloc(nm,sizeof(double));
    if( tmp_b == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rhomb_trunc: Unable to allocate memory for coercing b array to double precision");
      return(NhlFATAL);
    }
  }
/*
 * Call Fortran function.
 */
  index_nm = 0;
  for(i = 0; i < nt; i++) {
/*
 * Coerce subsection of a and b to temporary a and b.
 */
    if(type_a != NCL_double) {
      coerce_subset_input_double(a,tmp_a,index_nm,type_a,nm,0,NULL,NULL);
    }
    else {
      tmp_a  = &((double*)a)[index_nm];
    }
    if(type_b != NCL_double) {
      coerce_subset_input_double(b,tmp_b,index_nm,type_b,nm,0,NULL,NULL);
    }
    else {
      tmp_b  = &((double*)b)[index_nm];
    }

    NGCALLF(drhombtrunc,DRHOMBTRUNC)(&in,&im,tmp_a,tmp_b,T);

    if(type_a != NCL_double) {
      coerce_output_float_only(a,tmp_a,nm,index_nm);
    }
    if(type_b != NCL_double) {
      coerce_output_float_only(b,tmp_b,nm,index_nm);
    }
    index_nm += nm;
  }

/*
 * Free work arrays.
 */ 
  if(type_a != NCL_double) NclFree(tmp_a);
  if(type_b != NCL_double) NclFree(tmp_b);
/*
 * Return
 */
  return(NhlNOERROR);
}


NhlErrorTypes tri_trunc_W( void )
{
/*
 * Input array variables
 */
  void *a, *b;
  double *tmp_a = NULL;
  double *tmp_b = NULL;
  int ndims_a;
  ng_size_t dsizes_a[NCL_MAX_DIMENSIONS];
  int ndims_b;
  ng_size_t dsizes_b[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_a, type_b;
  ng_size_t nt, m, n, nm, total_size_ab;
  int *T;
/*
 * various
 */
  ng_size_t i;
  ng_size_t index_nm;
  int im, in;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  a = (void*)NclGetArgValue(
          0,
          3,
          &ndims_a, 
          dsizes_a,
          NULL,
          NULL,
          &type_a,
          DONT_CARE);

  b = (void*)NclGetArgValue(
          1,
          3,
          &ndims_b,
          dsizes_b,
          NULL,
          NULL,
          &type_b,
          DONT_CARE);
/*
 * Get T
 */
  T = (int*)NclGetArgValue(
       2,
       3,
       NULL,
       NULL,
       NULL,
       NULL,
       NULL,
       DONT_CARE);
/*
 * The grids coming in must be at least 2-dimensional.
 */
  if(ndims_a != ndims_b || ndims_a < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tri_trunc: The input arrays must have the same number of dimensions and be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_a; i++ ) {
    if(dsizes_a[i] != dsizes_b[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"tri_trunc: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Compute the total number of elements in our array.
 */
  m = dsizes_a[ndims_a-2];
  n = dsizes_a[ndims_a-1];
  if(n != m) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tri_trunc: the last two dimensions of ab must be the same size");
    return(NhlFATAL);
  }
  nm = n * m;

/*
 * Test input dimension sizes.
 */
  if((m > INT_MAX) || (n > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tri_trunc: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  im = (int) m;
  in = (int) n;

  nt = 1;
  for(i = 0; i < ndims_a-2; nt*=dsizes_a[i],i++);
  total_size_ab = nt*nm;
/*
 * a and b must be float or double.
 */
  if((type_a != NCL_float && type_a != NCL_double) ||
     (type_b != NCL_float && type_b != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tri_trunc: a and b must be of type float or double");
    return(NhlFATAL);
  }
/*
 * Create temporary a and b arrays.
 */
  if(type_a != NCL_double) {
    tmp_a = (double*)calloc(nm,sizeof(double));
    if( tmp_a == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"tri_trunc: Unable to allocate memory for coercing a array to double precision");
      return(NhlFATAL);
    }
  }
  if(type_b != NCL_double) {
    tmp_b = (double*)calloc(nm,sizeof(double));
    if( tmp_b == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"tri_trunc: Unable to allocate memory for coercing b array to double precision");
      return(NhlFATAL);
    }
  }
/*
 * Call Fortran function.
 */
  index_nm = 0;
  for(i = 0; i < nt; i++) {
/*
 * Coerce subsection of a and b to temporary a and b.
 */
    if(type_a != NCL_double) {
      coerce_subset_input_double(a,tmp_a,index_nm,type_a,nm,0,NULL,NULL);
    }
    else {
      tmp_a  = &((double*)a)[index_nm];
    }
    if(type_b != NCL_double) {
      coerce_subset_input_double(b,tmp_b,index_nm,type_b,nm,0,NULL,NULL);
    }
    else {
      tmp_b  = &((double*)b)[index_nm];
    }

    NGCALLF(dtritrunc,DTRITRUNC)(&in, T, &im, tmp_a, tmp_b);

    if(type_a != NCL_double) coerce_output_float_only(a,tmp_a,nm,index_nm);
    if(type_b != NCL_double) coerce_output_float_only(b,tmp_b,nm,index_nm);
    index_nm += nm;
  }

/*
 * Free work arrays.
 */ 
  if(type_a != NCL_double) NclFree(tmp_a);
  if(type_b != NCL_double) NclFree(tmp_b);
/*
 * Return
 */
  return(NhlNOERROR);
}



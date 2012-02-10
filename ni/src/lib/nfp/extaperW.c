#include <stdio.h>
#include <string.h>
#include "wrapper.h"

extern void NGCALLF(dexptaper,DEXPTAPER)(double*,int*,double*,int*,int*);

extern void NGCALLF(dexptapersh,DEXPTAPERSH)(int*,int*,double*,double*,
                                             double*,int*,int*);

NhlErrorTypes exp_tapershC_W( void )
{
/*
 * Input array variables
 */
  void *ab, *new_ab, *n0;
  double *tmp_a, *tmp_b, *tmp_n0;
  int ndims_ab;
  ng_size_t dsizes_ab[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_ab, type_new_ab, type_n0;
  int *rate;
/*
 * various
 */
  ng_size_t i, start, nm, index_ab, total_leftmost, total_size_ab, total_size_ab2;
  int nb, mb, ier;
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
          DONT_CARE);
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
 * Get n0 and rate.
 */
  n0 = (void*)NclGetArgValue(
       1,
       3,
       NULL,
       NULL,
       NULL,
       NULL,
       &type_n0,
       DONT_CARE);

  rate = (int*)NclGetArgValue(
       2,
       3,
       NULL,
       NULL,
       NULL,
       NULL,
       NULL,
       DONT_CARE);
  
/*
 * Compute the total number of elements in our array.
 */
  if( (dsizes_ab[ndims_ab-2] > INT_MAX) || (dsizes_ab[ndims_ab-1] > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"exp_tapershC: One of the rightmost two dimensions of the input array is greater than INT_MAX");
    return(NhlFATAL);
  }
  nb  = (int) dsizes_ab[ndims_ab-2];
  mb  = (int) dsizes_ab[ndims_ab-1];
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
 * Coerce n0 to double.
 */
  tmp_n0 = coerce_input_double(n0,type_n0,1,0,NULL,NULL);

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

    NGCALLF(dexptapersh,DEXPTAPERSH)(&mb,&nb,tmp_a,tmp_b,tmp_n0,rate,&ier);
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


NhlErrorTypes exp_tapersh_W( void )
{
/*
 * Input array variables
 */
  void *a, *b, *n0;
  double *tmp_a = NULL;
  double *tmp_b = NULL;
  double *tmp_n0;
  int ndims_a;
  ng_size_t dsizes_a[NCL_MAX_DIMENSIONS];
  int ndims_b;
  ng_size_t dsizes_b[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_a, type_b, type_n0;
  int *rate;
/*
 * various
 */
  ng_size_t i, total_leftmost, total_size_ab, index_ab, nm;
  int mb, nb, ier;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  a = (void*)NclGetArgValue(
          0,
          4,
          &ndims_a, 
          dsizes_a,
          NULL,
          NULL,
          &type_a,
          DONT_CARE);

  b = (void*)NclGetArgValue(
          1,
          4,
          &ndims_b, 
          dsizes_b,
          NULL,
          NULL,
          &type_b,
          DONT_CARE);
/*
 * The grids coming in must be at least 2-dimensional.
 */
  if(ndims_a != ndims_b || ndims_a < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"exp_tapersh: The input arrays must have the same number of dimensions and be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_a; i++ ) {
    if(dsizes_a[i] != dsizes_b[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"exp_tapersh: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }

/*
 * Get n0 and rate.
 */
  n0 = (void*)NclGetArgValue(
       1,
       3,
       NULL,
       NULL,
       NULL,
       NULL,
       &type_n0,
       DONT_CARE);

  rate = (int*)NclGetArgValue(
       2,
       3,
       NULL,
       NULL,
       NULL,
       NULL,
       NULL,
       DONT_CARE);
  
/*
 * Compute the total number of elements in our array.
 */
  nb  = dsizes_a[ndims_a-2];
  mb  = dsizes_a[ndims_a-1];
  nm = nb * mb;

  total_leftmost = 1;
  for(i = 0; i < ndims_a-2; total_leftmost*=dsizes_a[i],i++);

  total_size_ab  = total_leftmost*nm;

/*
 * a and b must be float or double.
 */
  if((type_a != NCL_float && type_a != NCL_double) ||
     (type_b != NCL_float && type_b != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"exp_tapersh: a and b must be of type float or double");
    return(NhlFATAL);
  }
/*
 * Create space for temporary a and b arrays.
 */
  if(type_a != NCL_double) {
    tmp_a = (double*)calloc(nm,sizeof(double));
    if( tmp_a == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"exp_tapersh: Unable to allocate memory for coercing a array to double precision");
      return(NhlFATAL);
    }
  }
  if(type_b != NCL_double) {
    tmp_b = (double*)calloc(nm,sizeof(double));
    if( tmp_b == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"exp_tapersh: Unable to allocate memory for coercing b array to double precision");
      return(NhlFATAL);
    }
  }
/*
 * Coerce n0 to double.
 */
  tmp_n0 = coerce_input_double(n0,type_n0,1,0,NULL,NULL);

/*
 * Loop through total_leftmost and call Fortran function
 */
  index_ab = 0;

  for(i = 0; i < total_leftmost; i++) {
/*
 * Coerce subsection of a and b to temporary a and b.
 */
    if(type_a != NCL_double) {
      coerce_subset_input_double(a,tmp_a,index_ab,type_a,nm,0,NULL,NULL);
    }
    else {
      tmp_a  = &((double*)a)[index_ab];
    }
    if(type_b != NCL_double) {
      coerce_subset_input_double(b,tmp_b,index_ab,type_b,nm,0,NULL,NULL);
    }
    else {
      tmp_b  = &((double*)b)[index_ab];
    }

    NGCALLF(dexptapersh,DEXPTAPERSH)(&mb,&nb,tmp_a,tmp_b,tmp_n0,rate,&ier);

    if(type_a != NCL_double) {
      coerce_output_float_only(a,tmp_a,nm,index_ab);
    }
    if(type_b != NCL_double) {
      coerce_output_float_only(b,tmp_b,nm,index_ab);
    }

    index_ab += nm;
  }

/*
 * Free work arrays.
 */ 
  if(type_a != NCL_double) NclFree(tmp_a);
  if(type_b != NCL_double) NclFree(tmp_b);

/*
 * Return values. 
 */
  return(NhlNOERROR);
}


NhlErrorTypes exp_tapersh_wgts_W( void )
{
/*
 * Input array variables
 */
  void *nwgt_tmp;
  ng_size_t *nwgt;
  int inwgt, *rate;
  void *n0;
  double *tmp_n0;
  NclBasicDataTypes type_n0;
/*
 * Error return.
 */
  int ier;
/*
 * Return variables. 
 */
  void *s;
  double *tmp_s;
  ng_size_t dsizes_s[1];
  NclBasicDataTypes type_s, type_nwgt;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care sout its value.
 */
  nwgt_tmp = (void*)NclGetArgValue(
          0,
          3,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_nwgt,
          DONT_CARE);

/*
 * Check the input dimension size.
 */
  nwgt = get_dimensions(nwgt_tmp,1,type_nwgt,"exp_tapersh_wgts");
  if(nwgt == NULL) 
    return(NhlFATAL);

  if(*nwgt > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"exp_taper_wgts: nwgt is greater than INT_MAX");
    return(NhlFATAL);
  }
  inwgt = (int) *nwgt;

/*
 * Get n0 and rate.
 */
  n0 = (void*)NclGetArgValue(
       1,
       3,
       NULL,
       NULL,
       NULL,
       NULL,
       &type_n0,
       DONT_CARE);

  rate = (int*)NclGetArgValue(
       2,
       3,
       NULL,
       NULL,
       NULL,
       NULL,
       NULL,
       DONT_CARE);

/*
 * Allocate space for output array.
 */
  if(type_n0 != NCL_double) {
    type_s = NCL_float;
    s      = (void*)calloc(*nwgt,sizeof(float));
    tmp_s  = (double*)calloc(*nwgt,sizeof(double));

    if(s == NULL || tmp_s == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"exp_tapersh_wgts: Unable to allocate memory for return array");
      return(NhlFATAL);
    }
  }
  else {
    type_s = NCL_double;
    s      = (void*)calloc(*nwgt,sizeof(double));
    tmp_s  = &((double*)s)[0];

    if( s == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"exp_tapersh_wgts: Unable to allocate memory for return array");
      return(NhlFATAL);
    }
  }
/*
 * Coerce n0 to double.
 */
  tmp_n0 = coerce_input_double(n0,type_n0,1,0,NULL,NULL);

/*
 * Call Fortran routine.
 */
  NGCALLF(dexptaper,DEXPTAPER)(tmp_n0,rate,tmp_s,&inwgt,&ier);

/*
 * Coerce back to float if necessary and free array.
 */
  if(type_s != NCL_double) {
    coerce_output_float_only(s,tmp_s,*nwgt,0);
    NclFree(tmp_s);
  }

/*
 * Return values. 
 */
  dsizes_s[0] = *nwgt;
  NclFree(nwgt);
  return(NclReturnValue(s,1,dsizes_s,NULL,type_s,0));
}


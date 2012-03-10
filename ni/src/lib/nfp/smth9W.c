#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(dsmth9,DSMTH9)(double *,double *,int *,int *,double *,
                                   double *,double *,int *,int *);

NhlErrorTypes smth9_W( void )
{
/*
 * Input variables
 */
  void *x, *p, *q;
  double *tmp_x, *tmp_p, *tmp_q;
  logical *lwrap;
  int has_missing_x;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  NclScalar missing_x, missing_dx, missing_rx;
  NclBasicDataTypes type_x, type_p, type_q;
/*
 * Output variables
 */
  void *smth;
  NclBasicDataTypes type_smth;
/*
 * Various
 */
  double *work;
  ng_size_t total_size_x, ni, nj, ninj, lwork, i, nt, index_x;
  int ini, inj, ier;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  x = (void*)NclGetArgValue(
          0,
          4,
          &ndims_x,
          dsizes_x,
          &missing_x,
          &has_missing_x,
          &type_x,
          DONT_CARE);
  p = (void*)NclGetArgValue(
          1,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_p,
          DONT_CARE);

  q = (void*)NclGetArgValue(
          2,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_q,
          DONT_CARE);

  lwrap = (logical*)NclGetArgValue(
          3,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_x < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"smth9: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }

  nj = dsizes_x[ndims_x-2];
  ni = dsizes_x[ndims_x-1];
  ninj = ni * nj;

/*
 * Test input dimension sizes.
 */
  if((ni > INT_MAX) || (nj > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"smth9: ni and/or nj is greater than INT_MAX");
    return(NhlFATAL);
  }
  ini = (int) ni;
  inj = (int) nj;

/*
 * Compute the total number of elements in our array.
 */
  nt = 1;
  for(i = 0; i < ndims_x-2; i++) nt *= dsizes_x[i];

  total_size_x = nt * ninj;

/*
 * Check that input array has a missing value set.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
  if(!has_missing_x) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"smth9: No missing values are being set.\nDefault missing values will be used.\nBe careful of results.");
  }
/*
 * Create a temporary array to hold subarrays of x.
 */
  tmp_x = (double*)calloc(ninj,sizeof(double));
  if( tmp_x == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"smth9: Unable to allocate memory for coercing x array to double precision");
    return(NhlFATAL);
  }
/*
 * Coerce p and q to double precision if necessary.
 */
  tmp_p = coerce_input_double(p,type_p,1,0,NULL,NULL);
  tmp_q = coerce_input_double(q,type_p,1,0,NULL,NULL);

  if(tmp_p == NULL || tmp_q == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"smth9: Unable to coerce p/q to double precision");
    return(NhlFATAL);
  }
  
/*
 * Allocate space for output.
 */
  if(type_x != NCL_double && type_p != NCL_double && type_q != NCL_double) {
    type_smth = NCL_float;
    smth = (void*)calloc(total_size_x,sizeof(float));
  }
  else {
    type_smth = NCL_double;
    smth = (void*)calloc(total_size_x,sizeof(double));
  }
  if( smth == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"smth9: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Allocate space for work array.
 */
  lwork = ninj;
  work  = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"smth9: Unable to allocate memory for work array");
    return(NhlFATAL);
  }

/*
 * Call Fortran routine.
 */
  index_x = 0;
  
  for(i = 0; i < nt; i++ ) {
/*
 * Coerce subsection of x (tmp_x) to double.
 */
    coerce_subset_input_double(x,tmp_x,index_x,type_x,ninj,0,NULL,NULL);

    NGCALLF(dsmth9,DSMTH9)(tmp_x,work,&ini,&inj,tmp_p,tmp_q,
			   &missing_dx.doubleval,lwrap,&ier);

    coerce_output_float_or_double(smth,tmp_x,type_smth,ninj,index_x);

    index_x += ninj;
  }

/*
 * free memory.
 */
  NclFree(work);
  NclFree(tmp_x);
  if(type_p != NCL_double) NclFree(tmp_p);
  if(type_q != NCL_double) NclFree(tmp_q);

/*
 * Return values. 
 */
  if(type_smth != NCL_double) {
    return(NclReturnValue(smth,ndims_x,dsizes_x,&missing_rx,
                          NCL_float,0));
  }
  else {
    return(NclReturnValue(smth,ndims_x,dsizes_x,&missing_dx,
                          NCL_double,0));
  }
}

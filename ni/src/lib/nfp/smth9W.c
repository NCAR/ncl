#include <stdio.h>

/*
 * The following are the required NCAR Graphics include files.
 * They should be located in ${NCARG_ROOT}/include.
 */
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
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS];
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
  int total_size_x, ni, nj, ninj, lwork, i, j, index_x, nt, ier;
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
          2);
  p = (void*)NclGetArgValue(
          1,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_p,
          2);

  q = (void*)NclGetArgValue(
          2,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_q,
          2);

  lwrap = (logical*)NclGetArgValue(
          3,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);
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
 * Space for p.
 */
  tmp_p = (double*)calloc(1,sizeof(double));
  tmp_q = (double*)calloc(1,sizeof(double));
  if(tmp_p == NULL || tmp_q == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"smth9: Unable to allocate memory for coercing p/q to double precision");
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
  
  coerce_subset_input_double(p,tmp_p,0,type_p,1,0,NULL,NULL);
  coerce_subset_input_double(q,tmp_q,0,type_p,1,0,NULL,NULL);

  for(i = 0; i < nt; i++ ) {
/*
 * Coerce subsection of x (tmp_x) to double.
 */
    coerce_subset_input_double(x,tmp_x,index_x,type_x,ninj,0,NULL,NULL);

    NGCALLF(dsmth9,DSMTH9)(tmp_x,work,&ni,&nj,tmp_p,tmp_q,
                           &missing_dx.doubleval,lwrap,&ier);

    for(j = 0; j < ninj; j++) {
      if(type_smth != NCL_double) {
        ((float*)smth)[index_x+j] = (float)(tmp_x[j]);
      }
      else {
        ((double*)smth)[index_x+j] = tmp_x[j];
      }
    }
    index_x += ninj;
  }

/*
 * free memory.
 */
  NclFree(work);
  NclFree(tmp_x);
  NclFree(tmp_p);
  NclFree(tmp_q);

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

#include <stdio.h>
#include <math.h>
#include "wrapper.h"

extern void NGCALLF(vors,VORS)(int *, int *, double *, double *, double *, 
                               double *, int *);

NhlErrorTypes eof_varimax_W( void )
{
/*
 * Input array variables
 */
  void *evec;
  double *devec;
  int ndims_evec, dsizes_evec[NCL_MAX_DIMENSIONS], has_missing_evec;
  NclScalar missing_evec, missing_devec;
  NclBasicDataTypes type_evec;
  int nvar, nfac, ldevec, total_size_evec;
/*
 * Work array variables.
 */
  double *a, *b, *w;
/*
 * Output array variable
 */
  void  *evec_out;
  int i;
  NclBasicDataTypes type_evec_out;
/*
 * Retrieve parameters
 */
  evec = (void*)NclGetArgValue(
           0,
           1,
           &ndims_evec, 
           dsizes_evec,
           &missing_evec,
           &has_missing_evec,
           &type_evec,
           2);

/*
 * Check dimensions.
 */
  if( ndims_evec < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eof_varimax: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }

/*
 * Calculate size of output array.
 */
  nfac = dsizes_evec[0];

  nvar = 1;
  for( i = 1; i <= ndims_evec-1; i++ ) nvar *= dsizes_evec[i];
  ldevec = nvar;

  if( nvar < 1 || nfac < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eof_varimax: The dimensions of the input array must both be at least 1");
    return(NhlFATAL);
  }
  total_size_evec = nvar * nfac;

/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_evec,has_missing_evec,&missing_evec,
                 &missing_devec,NULL);
/*
 * Coerce evec to double no matter what, since we need to make a copy of
 * the input array anyway.
 */
  devec = (double*)calloc(total_size_evec,sizeof(double));
  if( devec == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eof_varimax: Unable to allocate memory for coercing evec array to double precision");
    return(NhlFATAL);
  }
  coerce_subset_input_double(evec,devec,0,type_evec,total_size_evec,
                             has_missing_evec,&missing_evec,&missing_devec);
/*
 * Check for a missing value.
 */
  if(contains_missing(devec,total_size_evec,has_missing_evec,
                      missing_devec.doubleval)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eof_varimax: The input array contains missing values.");
    return(NhlFATAL);
  }

/*
 * Allocate space for output array.
 */
  if(type_evec != NCL_double) {
    type_evec_out = NCL_float;
    evec_out      = (void*)calloc(total_size_evec,sizeof(float));
  }
  else {
/*
 * We've already allocated a double precision output variable, so
 * just point to it.
 */ 
    type_evec_out = NCL_double;
    evec_out      = (void*)devec;
  }
/*
 * Allocate memory for work arrays.
 */
  a = (double *)calloc(nvar,sizeof(double));
  b = (double *)calloc(nvar,sizeof(double));
  w = (double *)calloc(nvar,sizeof(double));
  if( a == NULL || b == NULL || w == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eof_varimax: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
/*
 * Call the Fortran 77 version of 'vors' with the full argument list.
 */
  NGCALLF(vors,VORS)(&nvar, &nfac, devec, a, b, w, &ldevec);

/*
 * Free unneeded memory.
 */
  NclFree(w);
  NclFree(a);
  NclFree(b);

  if(type_evec_out == NCL_float) {
    coerce_output_float_only(evec_out,devec,total_size_evec,0);
    NclFree(devec);
  }

  return(NclReturnValue(evec_out,ndims_evec,dsizes_evec,NULL,
                        type_evec_out,0));
}

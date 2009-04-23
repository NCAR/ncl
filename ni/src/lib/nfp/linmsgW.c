#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(dlinmsg,DLINMSG)(double *,int *,double *,int *, int *);

NhlErrorTypes linmsg_W( void )
{
/*
 * Input variables
 */
  void *x;
  double *tmp_x;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  NclScalar missing_x, missing_dx, missing_rx;
  int *opt, dsizes_opt[NCL_MAX_DIMENSIONS]; 
  NclBasicDataTypes type_x;
  int mflag, nptcrt;
/*
 * Output variables.
 */
  void *xlinmsg;
/*
 * Other variables
 */
  int i, j, index_x, total_size_x, total_size_x1, npts;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
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

  opt = (int*)NclGetArgValue(
          1,
          2,
          NULL,
          dsizes_opt,
          NULL,
          NULL,
          NULL,
          DONT_CARE);
/*
 * Compute the total number of elements in our x array.
 */
  npts = dsizes_x[ndims_x-1];
  total_size_x1 = 1;
  for( i = 0; i < ndims_x-1; i++ ) total_size_x1 *= dsizes_x[i];

  total_size_x = total_size_x1 * npts;
/*
 * Check "opt".  If it is a scalar, then set mflag equal to it. If it
 * has two elements, then the first element is mflag, and the second
 * element is nptcrt.
 */
  mflag = opt[0];
  if(dsizes_opt[0] >= 2) {
    nptcrt = opt[1];
  }
  else {
    nptcrt = npts;
  }
/*
 * coerce missing values.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Coerce data to double, even if it is already double. We need to
 * make a copy of the input array to keep it from getting modified.
 */
  tmp_x = (double*)calloc(npts,sizeof(double));
  if( tmp_x == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linmsg: Unable to allocate memory for coercing x array to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate space for output array
 */
  if(type_x != NCL_double) {
    xlinmsg = (void*)calloc(total_size_x,sizeof(float));
  }
  else {
    xlinmsg = (void*)calloc(total_size_x,sizeof(double));
  }
  if( xlinmsg == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linmsg: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Call Fortran function.
 */
  index_x = 0;
  for( i = 0; i < total_size_x1; i++ ) {
/*
 * Coerce subsection of x (tmp_x) to double.
 */
    coerce_subset_input_double(x,tmp_x,index_x,type_x,npts,0,NULL,NULL);

    NGCALLF(dlinmsg,DLINMSG)(tmp_x,&npts,&missing_dx.doubleval,&mflag,
                             &nptcrt);
    coerce_output_float_or_double(xlinmsg,tmp_x,type_x,npts,index_x);
    index_x += npts;
  }
/*
 * Free temp array.
 */
  NclFree(tmp_x);

  if(type_x != NCL_double) {
/*
 * Return float values with missing value set.
 */
    return(NclReturnValue(xlinmsg,ndims_x,dsizes_x,&missing_rx,NCL_float,0));
  }
  else {
/*
 * Return double values with missing value set.
 */
    return(NclReturnValue(xlinmsg,ndims_x,dsizes_x,&missing_dx,NCL_double,0));
  }
}

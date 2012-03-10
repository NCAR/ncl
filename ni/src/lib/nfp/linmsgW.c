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
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclScalar missing_x, missing_dx, missing_rx;
  int *opt;
  ng_size_t dsizes_opt[NCL_MAX_DIMENSIONS]; 
  NclBasicDataTypes type_x;
  int mflag, nptcrt;
/*
 * Output variables.
 */
  void *xlinmsg;
/*
 * Other variables
 */
  int inpts, inptcrt;
  ng_size_t i, index_x, total_size_x, total_size_x1, npts;
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
 * Test input dimension sizes.
 */
  if((npts > INT_MAX) || (nptcrt > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linmsg: npts and/or nptcrt is greater than INT_MAX");
    return(NhlFATAL);
  }
  inpts = (int) npts;
  inptcrt = (int) nptcrt;

/*
 * Coerce missing values.
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

    NGCALLF(dlinmsg,DLINMSG)(tmp_x,&inpts,&missing_dx.doubleval,&mflag,
                             &inptcrt);

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

NhlErrorTypes linmsg_n_W( void )
{
/*
 * Input variables
 */
  void *x;
  double *tmp_x;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclScalar missing_x, missing_dx, missing_rx, missing;
  int *dim, *opt;
  ng_size_t dsizes_opt[NCL_MAX_DIMENSIONS]; 
  NclBasicDataTypes type_x, type_xlinmsg;
  int mflag, nptcrt;
/*
 * Output variables.
 */
  void *xlinmsg;
/*
 * Other variables
 */
  ng_size_t i, j, nl, nr, nrnxi, index_nri, index_x, total_size_x, npts;
  int inpts, inptcrt;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
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

  opt = (int*)NclGetArgValue(
          1,
          3,
          NULL,
          dsizes_opt,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

  dim = (int*)NclGetArgValue(
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
  if(*dim < 0 || *dim >= ndims_x) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linmsg_n: Invalid dimension to do interpolation on, can't continue");
    return(NhlFATAL);
  }

  npts = dsizes_x[*dim];

/*
 * Calculate size of leftmost dimensions (nl) up to the dim-th
 *   dimension.
 * Calculate size of rightmost dimensions (nr) from the
 *   dim-th dimension.
 *
 * The dimension to do the interpolation across is "dim".
 */
  nl = nr = 1;
  if(ndims_x > 1) {
    for(i = 0; i < *dim ; i++) {
      nl = nl*dsizes_x[i];
    }
    for(i = *dim+1; i < ndims_x; i++) {
      nr = nr*dsizes_x[i];
    }
  }

  total_size_x  = nr * nl * npts;

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
 * Test input dimension sizes.
 */
  if((npts > INT_MAX) || (nptcrt > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linmsg_n: npts and/or nptcrt is greater than INT_MAX");
    return(NhlFATAL);
  }
  inpts = (int) npts;
  inptcrt = (int) nptcrt;


/*
 * Coerce missing values.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Coerce data to double, even if it is already double. We need to
 * make a copy of the input array to keep it from getting modified.
 */
  tmp_x = (double*)calloc(npts,sizeof(double));
  if( tmp_x == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linmsg_n: Unable to allocate memory for coercing x array to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate space for output array
 */
  if(type_x != NCL_double) {
    xlinmsg = (void*)calloc(total_size_x,sizeof(float));
    missing = missing_rx;
    type_xlinmsg = NCL_float;
  }
  else {
    xlinmsg = (void*)calloc(total_size_x,sizeof(double));
    missing = missing_dx;
    type_xlinmsg = NCL_double;
  }
  if( xlinmsg == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linmsg_n: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Call Fortran function.
 */
  nrnxi = nr * npts;
  for( i = 0; i < nl; i++ ) {
    index_nri = i*nrnxi;
    for( j = 0; j < nr; j++ ) {
      index_x = index_nri+j;
/*
 * Coerce subsection of x (tmp_x) to double.
 */
      coerce_subset_input_double_step(x,tmp_x,index_x,nr,type_x,
                                      npts,0,NULL,NULL);

      NGCALLF(dlinmsg,DLINMSG)(tmp_x,&inpts,&missing_dx.doubleval,&mflag,
                               &inptcrt);

      coerce_output_float_or_double_step(xlinmsg,tmp_x,type_x,npts,index_x,
                                         nr);
    }
  }
/*
 * Free temp array.
 */
  NclFree(tmp_x);

/*
 * Return float values with missing value set.
 */
  return(NclReturnValue(xlinmsg,ndims_x,dsizes_x,&missing,type_xlinmsg,0));
}

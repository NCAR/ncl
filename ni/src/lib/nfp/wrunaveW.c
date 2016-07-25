#include <stdio.h>
#include <math.h>
#include "wrapper.h"

extern void NGCALLF(dwgtrunave,DWGTRUNAVE)(double*,int*,double*,int*,int*,
                                           double*,double*,int*,int*);

extern void NGCALLF(drunave,DRUNAVE)(double*,int*,int*,int*,double*,double*,
                                     int*,int*);

NhlErrorTypes wgt_runave_W( void )
{
/*
 * Input array variables
 */
  void *x, *wgt;
  double *tmp_x, *tmp_wgt;
  int *kopt;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  int ndims_wgt;
  ng_size_t dsizes_wgt[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_x, type_wgt;
  NclScalar missing_x, missing_dx, missing_rx;
/*
 * Output variable.
 */
  void *wrunave;
/*
 * Work array.
 */
  ng_size_t lwork;
  double *work;
/*
 * Declare various variables for random purposes.
 */
  int ier, inpts, inwgt, ilwork;
  ng_size_t i, index_x, npts, nwgt, total_leftmost, total_size_x;

/*
 * Retrieve arguments.
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

  wgt = (void*)NclGetArgValue(
          1,
          3,
          &ndims_wgt,
          dsizes_wgt,
          NULL,
          NULL,
          &type_wgt,
          DONT_CARE);

  kopt = (int*)NclGetArgValue(
          2,
          3,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);
/*
 * Check input dimension sizes.
 */
  npts = dsizes_x[ndims_x-1];
  nwgt = dsizes_wgt[0];
  if( nwgt > npts ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_runave: The length of wgt must be less than or equal to the rightmost dimension of x");
    return(NhlFATAL);
  }
/*
 * Compute the total size of the output array (minus the last dimension).
 */
  total_leftmost = 1;
  for( i = 0; i < ndims_x-1; i++ ) total_leftmost *= dsizes_x[i];
  total_size_x = total_leftmost * npts;
/*
 * Coerce the missing value.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Create a temporary array to hold subarrays of x.
 */
  tmp_x = (double*)calloc(npts,sizeof(double));
  if( tmp_x == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_runave: Unable to allocate memory for coercing x array to double precision");
    return(NhlFATAL);
  }
/*
 * Coerce wgt array to double if necessary.
 */
  tmp_wgt = coerce_input_double(wgt,type_wgt,nwgt,0,NULL,NULL);
/*
 * Allocate space for output array.
 */
  if(type_x != NCL_double) {
    wrunave = (void*)calloc(total_size_x,sizeof(float));
  }
  else {
    wrunave = (void*)calloc(total_size_x,sizeof(double));
  }
  if( wrunave == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_runave: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  lwork = npts+2*(nwgt/2);
  
  if((npts > INT_MAX) || (nwgt > INT_MAX) || (lwork > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_runave: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inpts = (int) npts;
  inwgt = (int) nwgt;
  ilwork = (int) lwork;

/*
 * Allocate space for work array.
 */
  work = (double *)calloc(lwork,sizeof(double));
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_runave: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  
/*
 * Call the Fortran version of this routine.
 */
  index_x = 0;
  for( i = 0; i < total_leftmost; i++ ) {
/*
 * Coerce subsection of x (tmp_x) to double.
 */
    coerce_subset_input_double(x,tmp_x,index_x,type_x,npts,0,NULL,NULL);

    NGCALLF(dwgtrunave,DWGTRUNAVE)(tmp_x,&inpts,tmp_wgt,&inwgt,kopt,
				   &missing_dx.doubleval,work,&ilwork,&ier);

    coerce_output_float_or_double(wrunave,tmp_x,type_x,npts,index_x);

    index_x += npts;
  }
/*
 * Free memory.
 */
  NclFree(work);
  NclFree(tmp_x);
  if(type_wgt != NCL_double) NclFree(tmp_wgt);
        
  if(type_x != NCL_double) {
/*
 * Return float values with missing value set.
 */
    return(NclReturnValue(wrunave,ndims_x,dsizes_x,&missing_rx,NCL_float,0));
  }
  else {
/*
 * Return double values with missing value set.
 */
    return(NclReturnValue(wrunave,ndims_x,dsizes_x,&missing_dx,NCL_double,0));
  }
}


NhlErrorTypes wgt_runave_n_W( void )
{
/*
 * Input array variables
 */
  void *x, *wgt;
  double *tmp_x, *tmp_wgt;
  int *kopt, *dim;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  int ndims_wgt;
  ng_size_t dsizes_wgt[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_x, type_wgt;
  NclScalar missing_x, missing_dx, missing_rx;
/*
 * Output variable.
 */
  void *wrunave;
/*
 * Work array.
 */
  ng_size_t lwork;
  double *work;
/*
 * Declare various variables for random purposes.
 */
  int ier, inpts, inwgt, ilwork;
  ng_size_t i, j, index_x, index_nrnpts, npts, nwgt;
  ng_size_t total_leftmost, total_rightmost, total_size_x;

/*
 * Retrieve arguments.
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

  wgt = (void*)NclGetArgValue(
          1,
          4,
          &ndims_wgt,
          dsizes_wgt,
          NULL,
          NULL,
          &type_wgt,
          DONT_CARE);

  kopt = (int*)NclGetArgValue(
          2,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

  dim = (int*)NclGetArgValue(
          3,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

/*
 * Make sure "dim" is a valid dimension.
 */
  if (*dim < 0 || *dim >= ndims_x) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_runave_n: Invalid dimension index for calculating the weighted running average");
    return(NhlFATAL);
  }

/*
 * Check input dimension sizes.
 */
  npts = dsizes_x[*dim];
  nwgt = dsizes_wgt[0];
  if( nwgt > npts ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_runave_n: The length of wgt must be less than or equal to the %d-th dimension of x",*dim);
    return(NhlFATAL);
  }
/*
 * Compute the total size of the output array (minus the last dimension).
 */
  total_rightmost = total_leftmost = 1;
  for( i =      0; i < *dim;    i++ ) total_leftmost  *= dsizes_x[i];
  for( i = *dim+1; i < ndims_x; i++ ) total_rightmost *= dsizes_x[i];

  total_size_x = total_leftmost * total_rightmost * npts;

/*
 * Coerce the missing value.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Create a temporary array to hold subarrays of x.
 */
  tmp_x = (double*)calloc(npts,sizeof(double));
  if( tmp_x == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_runave_n: Unable to allocate memory for coercing x array to double precision");
    return(NhlFATAL);
  }
/*
 * Coerce wgt array to double if necessary.
 */
  tmp_wgt = coerce_input_double(wgt,type_wgt,nwgt,0,NULL,NULL);
/*
 * Allocate space for output array.
 */
  if(type_x != NCL_double) {
    wrunave = (void*)calloc(total_size_x,sizeof(float));
  }
  else {
    wrunave = (void*)calloc(total_size_x,sizeof(double));
  }
  if( wrunave == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_runave_n: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  lwork = npts+2*(nwgt/2);
  if((npts > INT_MAX) || (nwgt > INT_MAX) || (lwork > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_runave_n: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inpts = (int) npts;
  inwgt = (int) nwgt;
  ilwork = (int) lwork;

/*
 * Allocate space for work array.
 */
  work = (double *)calloc(lwork,sizeof(double));
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_runave_n: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  
/*
 * Call the Fortran version of this routine.
 */
  for( i = 0; i < total_leftmost; i++ ) {
    index_nrnpts = i * total_rightmost * npts;
    for( j = 0; j < total_rightmost; j++ ) {
      index_x = index_nrnpts + j;
/*
 * Coerce subsection of x (tmp_x) to double.
 */
      coerce_subset_input_double_step(x,tmp_x,index_x,total_rightmost,type_x,
                                      npts,0,NULL,NULL);

      NGCALLF(dwgtrunave,DWGTRUNAVE)(tmp_x,&inpts,tmp_wgt,&inwgt,kopt,
                                       &missing_dx.doubleval,work,&ilwork,&ier);

      coerce_output_float_or_double_step(wrunave,tmp_x,type_x,npts,index_x,
                                         total_rightmost);
    }
  }
/*
 * Free memory.
 */
  NclFree(work);
  NclFree(tmp_x);
  if(type_wgt != NCL_double) NclFree(tmp_wgt);
        
  if(type_x != NCL_double) {
/*
 * Return float values with missing value set.
 */
    return(NclReturnValue(wrunave,ndims_x,dsizes_x,&missing_rx,NCL_float,0));
  }
  else {
/*
 * Return double values with missing value set.
 */
    return(NclReturnValue(wrunave,ndims_x,dsizes_x,&missing_dx,NCL_double,0));
  }
}


NhlErrorTypes runave_W( void )
{
/*
 * Input array variables
 */
  void *x;
  double *tmp_x;
  int *nave, *kopt;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclBasicDataTypes type_x;
  NclScalar missing_x, missing_dx, missing_rx;
/*
 * Output variable.
 */
  void *runave;
/*
 * Work array.
 */
  ng_size_t lwork;
  double *work;
/*
 * Declare various variables for random purposes.
 */
  int ier, inpts, ilwork;
  ng_size_t i, index_x, npts, total_size_x, total_leftmost;

/*
 * Retrieve arguments.
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

  nave = (int*)NclGetArgValue(
          1,
          3,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

  kopt = (int*)NclGetArgValue(
          2,
          3,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);
/*
 * x and wgt must have the same leftmost dimensions.
 */
  npts = dsizes_x[ndims_x-1];

  if( *nave > npts ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"runave: nave must be less than or equal to the last dimension of x");
    return(NhlFATAL);
  }
/*
 * Compute the total size of the output array (minus the last dimension).
 */
  total_leftmost = 1;
  for( i = 0; i < ndims_x-1; i++ ) total_leftmost *= dsizes_x[i];
  total_size_x = total_leftmost * npts;
/*
 * Coerce the missing value.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Create a temporary array to hold subarrays of x.
 */
  tmp_x = (double*)calloc(npts,sizeof(double));
  if( tmp_x == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"runave: Unable to allocate memory for coercing input array to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate space for output array.
 */
  if(type_x != NCL_double) {
    runave = (void*)calloc(total_size_x,sizeof(float));
  }
  else {
    runave = (void*)calloc(total_size_x,sizeof(double));
  }
  if( runave == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"runave: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  lwork = npts+2*(*nave/2);
  
  if((npts > INT_MAX) || (lwork > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"runave: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inpts = (int) npts;
  ilwork = (int) lwork;

/*
 * Allocate space for work array.
 */
  work = (double *)calloc(lwork,sizeof(double));
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"runave: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  
/*
 * Call the Fortran version of this routine.
 *
 */
  index_x = 0;
  for( i = 0; i < total_leftmost; i++ ) {
/*
 * Coerce subsection of x (tmp_x) to double.
 */
    coerce_subset_input_double(x,tmp_x,index_x,type_x,npts,0,NULL,NULL);

    NGCALLF(drunave,DRUNAVE)(tmp_x,&inpts,nave,kopt,&missing_dx.doubleval,
			     work,&ilwork,&ier);

    coerce_output_float_or_double(runave,tmp_x,type_x,npts,index_x);

    index_x += npts;
  }
/*
 * Free work array. 
 */
  NclFree(work);
  NclFree(tmp_x);
        
  if(type_x != NCL_double) {
/*
 * Return float values with missing value set.
 */
    return(NclReturnValue(runave,ndims_x,dsizes_x,&missing_rx,NCL_float,0));
  }
  else {
/*
 * Return double values with missing value set.
 */
    return(NclReturnValue(runave,ndims_x,dsizes_x,&missing_dx,NCL_double,0));
  }
}

NhlErrorTypes runave_n_W( void )
{
/*
 * Input array variables
 */
  void *x;
  double *tmp_x;
  int *nave, *kopt, *dim;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclBasicDataTypes type_x;
  NclScalar missing_x, missing_dx, missing_rx;
/*
 * Output variable.
 */
  void *runave;
/*
 * Work array.
 */
  ng_size_t lwork;
  double *work;
/*
 * Declare various variables for random purposes.
 */
  int ier, inpts, ilwork;
  ng_size_t i, j, index_x, index_nrnpts, npts;
  ng_size_t total_size_x, total_leftmost, total_rightmost;

/*
 * Retrieve arguments.
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

  nave = (int*)NclGetArgValue(
          1,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

  kopt = (int*)NclGetArgValue(
          2,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

  dim = (int*)NclGetArgValue(
          3,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

/*
 * Make sure "dim" is a valid dimension.
 */
  if (*dim < 0 || *dim >= ndims_x) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"runave_n: Invalid dimension index for calculating the running average");
    return(NhlFATAL);
  }

/*
 * Check the dimensions.
 */
  npts = dsizes_x[*dim];

  if( *nave > npts ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"runave_n: nave must be less than or equal to the last dimension of x");
    return(NhlFATAL);
  }
/*
 * Compute the total size of the output array (minus the dim-th dimension).
 */
  total_rightmost = total_leftmost = 1;
  for( i =      0; i < *dim;    i++ ) total_leftmost  *= dsizes_x[i];
  for( i = *dim+1; i < ndims_x; i++ ) total_rightmost *= dsizes_x[i];

  total_size_x = total_leftmost * total_rightmost * npts;

/*
 * Coerce the missing value.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Create a temporary array to hold subarrays of x.
 */
  tmp_x = (double*)calloc(npts,sizeof(double));
  if( tmp_x == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"runave_n: Unable to allocate memory for coercing input array to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate space for output array.
 */
  if(type_x != NCL_double) {
    runave = (void*)calloc(total_size_x,sizeof(float));
  }
  else {
    runave = (void*)calloc(total_size_x,sizeof(double));
  }
  if( runave == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"runave_n: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Test dimension sizes.
 */
  lwork = npts+2*(*nave/2);
  
  if((npts > INT_MAX) || (lwork > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"runave_n: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inpts = (int) npts;
  ilwork = (int) lwork;

/*
 * Allocate space for work array.
 */
  work = (double *)calloc(lwork,sizeof(double));
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"runave_n: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  
/*
 * Call the Fortran version of this routine.
 *
 */
  for( i = 0; i < total_leftmost; i++ ) {
    index_nrnpts = i * total_rightmost * npts;
    for( j = 0; j < total_rightmost; j++ ) {
      index_x = index_nrnpts + j;
/*
 * Coerce subsection of x (tmp_x) to double.
 */
      coerce_subset_input_double_step(x,tmp_x,index_x,total_rightmost,type_x,
                                 npts,0,NULL,NULL);

      NGCALLF(drunave,DRUNAVE)(tmp_x,&inpts,nave,kopt,&missing_dx.doubleval,
			       work,&ilwork,&ier);

      coerce_output_float_or_double_step(runave,tmp_x,type_x,npts,index_x,
                                         total_rightmost);
    }
  }
/*
 * Free work array. 
 */
  NclFree(work);
  NclFree(tmp_x);
        
  if(type_x != NCL_double) {
/*
 * Return float values with missing value set.
 */
    return(NclReturnValue(runave,ndims_x,dsizes_x,&missing_rx,NCL_float,0));
  }
  else {
/*
 * Return double values with missing value set.
 */
    return(NclReturnValue(runave,ndims_x,dsizes_x,&missing_dx,NCL_double,0));
  }
}


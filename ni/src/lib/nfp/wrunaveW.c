#include <stdio.h>

/*
 * The following are the required NCAR Graphics include files.
 * They should be located in ${NCARG_ROOT}/include.
 */
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/ncl/defs.h>
#include "Symbol.h"
#include "NclMdInc.h"
#include <ncarg/ncl/NclDataDefs.h>
#include <ncarg/ncl/NclBuiltInSupport.h>
#include <math.h>
#include <ncarg/gks.h>

NhlErrorTypes wgt_runave_W( void )
{
/*
 * Input array variables
 */
  float *x, *x2, *wgt;
  int *kopt;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  int ndims_wgt, dsizes_wgt[NCL_MAX_DIMENSIONS];
  NclScalar missing_x;
/*
 * Work array.
 */
  int lwork;
  float *work;
/*
 * Declare various variables for random purposes.
 */
  int i, j, npts, nwgt, ier, size_x;
  float xmsg;

/*
 * Retrieve arguments.
 */
  x = (float*)NclGetArgValue(
          0,
          3,
          &ndims_x,
          dsizes_x,
		  &missing_x,
		  &has_missing_x,
          NULL,
          2);

  wgt = (float*)NclGetArgValue(
          1,
          3,
          &ndims_wgt,
          dsizes_wgt,
          NULL,
          NULL,
          NULL,
          2);

  kopt = (int*)NclGetArgValue(
          2,
          3,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);
/*
 * Test for missing values.
 */
  if( has_missing_x ) {
	xmsg = missing_x.floatval;
  }
  else {
	xmsg = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
  }
/*
 * wgt must be one-dimensional
 */
  if( ndims_wgt != 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_runave: The input array wgt must be one-dimensional"); 
    return(NhlFATAL);
  }
  npts = dsizes_x[ndims_x-1];
  nwgt = dsizes_wgt[0];
  if( nwgt > npts ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_runave: The length of wgt must be less than or equal to the last dimension of x");
    return(NhlFATAL);
  }
/*
 * Compute the total size of the output array (minus the last dimension).
 */
  size_x = 1;
  for( i = 0; i < ndims_x-1; i++ ) {
    size_x *= dsizes_x[i];
  }
/*
 * Allocate space for work and output array.
 */
  lwork = npts+2*(nwgt/2);
  work = (float *)NclMalloc(lwork*sizeof(float));
  x2   = (float *)NclMalloc( size_x*npts*sizeof(float));
  if( x2 == NULL || work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_runave: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  
/*
 * Call the Fortran version of this routine.
 *
 */
  for( i = 0; i < size_x*npts; i++ ) {
	x2[i] = x[i];
  }

  j = 0;
  for( i = 0; i < size_x; i++ ) {
	NGCALLF(wgtrunave, WGTRUNAVE)(&x2[j],&npts,wgt,&nwgt,kopt,&xmsg,
									work,&lwork,&ier);
	j += npts;
  }
  free(work);
	
  if( has_missing_x ) {
	return(NclReturnValue((void*)x2,ndims_x,dsizes_x,&missing_x,NCL_float,0));
  }
  else {
	return(NclReturnValue((void*)x2,ndims_x,dsizes_x,NULL,NCL_float,0));
  }
}


NhlErrorTypes runave_W( void )
{
/*
 * Input array variables
 */
  float *x, *x2;
  int *nave, *kopt;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  NclScalar missing_x;
/*
 * Work array.
 */
  int lwork;
  float *work;
/*
 * Declare various variables for random purposes.
 */
  int i, j, k, npts, ier, size_x;
  float xmsg;

/*
 * Retrieve arguments.
 */
  x = (float*)NclGetArgValue(
          0,
          3,
          &ndims_x,
          dsizes_x,
		  &missing_x,
		  &has_missing_x,
          NULL,
          2);

  nave = (int*)NclGetArgValue(
          1,
          3,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);

  kopt = (int*)NclGetArgValue(
          2,
          3,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);
/*
 * Test for missing values.
 */
  if( has_missing_x ) {
	xmsg = missing_x.floatval;
  }
  else {
	xmsg = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
  }
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
  size_x = 1;
  for( i = 0; i < ndims_x-1; i++ ) {
    size_x *= dsizes_x[i];
  }
/*
 * Allocate space for work array.
 */
  lwork = npts+2*(*nave/2);
  work = (float *)NclMalloc(lwork*sizeof(float));
  x2   = (float *)NclMalloc( size_x*npts*sizeof(float));
  if( x2 == NULL || work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"runave: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  
/*
 * Call the Fortran version of this routine.
 *
 */
  for( i = 0; i < size_x*npts; i++ ) {
	x2[i] = x[i];
  }
  j = 0;
  for( i = 0; i < size_x; i++ ) {
	NGCALLF(runave, RUNAVE)(&x2[j],&npts,nave,kopt,&xmsg,work,&lwork,&ier);
	j += npts;
  }
  free(work);
	
  if( has_missing_x ) {
	return(NclReturnValue((void*)x2,ndims_x,dsizes_x,&missing_x,NCL_float,0));
  }
  else {
	return(NclReturnValue((void*)x2,ndims_x,dsizes_x,NULL,NCL_float,0));
  }
}


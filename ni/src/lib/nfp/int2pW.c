#include <stdio.h>

/*
 * The following are the required NCAR Graphics include files.
 * They should be located in ${NCARG_ROOT}/include.
 */
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/ncl/defs.h>
#include <ncarg/ncl/NclDataDefs.h>
#include <ncarg/ncl/NclBuiltInSupport.h>
#include <ncarg/gks.h>


NhlErrorTypes int2p_W( void )
{
/*
 * Input array variables
 */
  float *pin;
  int ndims_pin, dsizes_pin[NCL_MAX_DIMENSIONS];
  NclScalar missing_pin;
  int has_missing_pin;
  float *xin;
  int ndims_xin, dsizes_xin[NCL_MAX_DIMENSIONS];
  NclScalar missing_xin;
  int has_missing_xin;
  int *linlog;
  float *pout;
  int ndims_pout, dsizes_pout[NCL_MAX_DIMENSIONS];
/*
 * work arrays
 */
  float *p, *x;
/*
 * output variable 
 */
  float *xout;
  int size_xout;
/*
 * Declare various variables for random purposes.
 */
  int i, j, k, npin, npout, ier = 0;
  float xmsg;
  NclScalar missing;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */

/*
 * Retrieve argument #1
 */
  pin = (float*)NclGetArgValue(
          0,
          4,
          &ndims_pin,
          dsizes_pin,
          &missing_pin,
          &has_missing_pin,
          NULL,
          2);
  if( has_missing_pin ) xmsg = missing_pin.floatval;
  else                  xmsg = 1.e36;
/*
 * Retrieve argument #2
 */
  xin = (float*)NclGetArgValue(
          1,
          4,
          &ndims_xin,
          dsizes_xin,
          &missing_xin,
          &has_missing_xin,
          NULL,
          2);
  if( !has_missing_pin && has_missing_xin ) xmsg = missing_xin.floatval;

/*
 * Check number of dimensions and/or dimension sizes for arguments #1
 * and #2 .
 */
  if (ndims_pin != ndims_xin) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p: The two input arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_pin; i++ ) {
	if (dsizes_pin[i] != dsizes_xin[i]) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p: The two input arrays must have the same dimensions");
	  return(NhlFATAL);
	}
  }
  npin = dsizes_pin[ndims_pin-1];
/*
 * Retrieve argument #3
 */
  pout = (float*)NclGetArgValue(
          2,
          4,
          &ndims_pout,
          dsizes_pout,
          NULL,
          NULL,
          NULL,
          2);
/*
 * check dimensionality.
 */
  if (ndims_pout != ndims_pin) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p: 'pout' must have the same number of dimensions as 'pin'");
    return(NhlFATAL);
  }
  npout = dsizes_pout[ndims_pout-1];

  if (npout < 1 || npin < 1) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p: The right-most dimension of 'pin' and 'pout' must be at least one.");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_pout-1; i++ ) {
	if (dsizes_pout[i] != dsizes_pin[i]) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p: The 'pout' array must have the same leftmost dimensions as the 'pin' array");
	  return(NhlFATAL);
	}
  }

/*
 * Retrieve argument #4
 */
  linlog = (int*)NclGetArgValue(
          3,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Calculate size of output value.
 */
  size_xout = 1;
  for( i = 0; i < ndims_pout-1; i++ ) {
	size_xout *= dsizes_pout[i];
  }
/*
 * Allocate space for work arrays.
 */
  p = (float*)NclMalloc(npin*sizeof(float));
  x = (float*)NclMalloc(npin*sizeof(float));
  if (p == NULL || x == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p: Unable to allocate space for work arrays\n" );
    return(NhlFATAL);
  }

/*
 * Allocate space for output array.
 */
  xout = (float*)NclMalloc(size_xout*npout*sizeof(float));

/*
 * Call the Fortran version of this routine.
 *
 */
  j = k = 0;
  for( i = 0; i < size_xout; i++ ) {
	NGCALLF(int2p, INT2P)(&pin[j],&xin[j],&p[0],&x[0],&npin,
						  &pout[k],&xout[k],&npout,linlog,&xmsg,&ier);
	if (ier >= 1000) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p: One of the input arrays only contains missing data");
	  return(NhlFATAL);
	}
	j += npin;
	k += npout;
  }
  free(p);
  free(x);

/*
 * Return.
 */
  missing.floatval = xmsg;
  return(NclReturnValue((void*)xout,ndims_pout,dsizes_pout,&missing,NCL_float,0));
}

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


NhlErrorTypes chiinv_W( void )
{
/*
 * Input array variables
 */
  float *p, *df;
  int ndims_p, dsizes_p[NCL_MAX_DIMENSIONS];
  int ndims_df, dsizes_df[NCL_MAX_DIMENSIONS];
/*
 * output variable 
 */
  float *chi;
  int size_chi;
/*
 * Declare various variables for random purposes.
 */
  int i;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */

/*
 * Retrieve argument #1
 */
  p = (float*)NclGetArgValue(
          0,
          2,
          &ndims_p,
          dsizes_p,
          NULL,
          NULL,
          NULL,
          2);
/*
 * Retrieve argument #2
 */
  df = (float*)NclGetArgValue(
          1,
          2,
          &ndims_df,
          dsizes_df,
          NULL,
          NULL,
          NULL,
          2);
/*
 * Check number of dimensions and/or dimension sizes for arguments #1
 * and #2 .
 */
  if (ndims_p != ndims_df) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"chiinv: The two input arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_p; i++ ) {
	if (dsizes_p[i] != dsizes_df[i]) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"chiinv: The two input arrays must have the same dimensions");
	  return(NhlFATAL);
	}
  }
/*
 * Calculate size of output value.
 */
  size_chi = 1;
  for( i = 0; i < ndims_p-1; i++ ) {
	size_chi *= dsizes_p[i];
  }
/*
 * Allocate space for output array.
 */
  chi = (float*)NclMalloc(size_chi*sizeof(float));

/*
 * Call the Fortran version of this routine.
 *
 */
  for( i = 0; i < size_chi; i++ ) {
	NGCALLF(chisub, CHISUB)(&p[i],&df[i],&chi[i]);
  }

/*
 * Return.
 */
  return(NclReturnValue((void*)chi,ndims_p,dsizes_p,NULL,NCL_float,0));
}

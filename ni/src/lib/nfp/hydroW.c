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


NhlErrorTypes hydro_W( void )
{
/*
 * Declare various variables for random purposes.
 */
  int i, j, nlvl, ier=0;
/*
 * Input array variables
 */
  float *p;
  int ndims_p, dsizes_p[NCL_MAX_DIMENSIONS], has_missing_p;
  float *tkv;
  int ndims_tkv, dsizes_tkv[NCL_MAX_DIMENSIONS], has_missing_t;
  float *zsfc;
  int ndims_zsfc, dsizes_zsfc[NCL_MAX_DIMENSIONS], has_missing_z;
/*
 * Output array variables
 */
  float *zh;
  int size_zh;

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
          3,
          &ndims_p,
          dsizes_p,
          NULL,
          &has_missing_p,
          NULL,
          2);

/*
 * Retrieve argument #2
 */
  tkv = (float*)NclGetArgValue(
          1,
          3,
          &ndims_tkv,
          dsizes_tkv,
          NULL,
          &has_missing_t,
          NULL,
          2);

/*
 * Check number of dimensions and/or dimension sizes for arguments #1 and 2.
 */
  if(ndims_tkv != ndims_p) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"hydro: The input arrays 'p' and 'tkv' must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_p; i++ ) {
	if (dsizes_tkv[i] != dsizes_p[i]) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"hydro: The input arrays 'p' and 'tkv' must have the same dimensions");
	  return(NhlFATAL);
	}
  }

/*
 * Retrieve argument #3
 */
  zsfc = (float*)NclGetArgValue(
          2,
          3,
          &ndims_zsfc,
          dsizes_zsfc,
          NULL,
          &has_missing_z,
          NULL,
          2);

/*
 * No missing values are allowed.
 */
  if(has_missing_t || has_missing_p || has_missing_z) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"hydro: The input arrays 'p', 'tkv', and 'zsfc' cannot contain any missing values");
	  return(NhlFATAL);
  }

/*
 * Check number of dimensions and/or dimension sizes for argument #3.
 */
  if ((ndims_p == 1 && ndims_zsfc != 1) || (ndims_p > 1 && ndims_zsfc != ndims_p-1)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"hydro: The array 'zsfc' must be a scalar or one less dimension than the other arrays");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_p-1; i++ ) {
	if (dsizes_zsfc[i] != dsizes_p[i]) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"hydro: The leftmost dimensions of the input array 'zsfc' must be the same as the arrays 'p' and 'tkv'");
	  return(NhlFATAL);
	}
  }

/*
 * Compute the total size of the output array.
 */
  nlvl = dsizes_p[ndims_p-1];
  if (nlvl < 1) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"hydro: 'nlvl' (the last dimension of 'p' and 'tkv') must be at least one");
	return(NhlFATAL);
  }

  size_zh = 1;
  for( i = 0; i < ndims_p-1; i++ ) {
    size_zh *= dsizes_p[i];
  }

/*
 * Allocate space for output value.
 */
  zh = (float *)NclMalloc(size_zh*nlvl*sizeof(float));
  if( zh == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"hydro: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Call the Fortran version of this routine.
 */
  j = 0;
  for( i = 0; i < size_zh; i++ ) {
	NGCALLF(hydro, HYDRO)(&p[j],&tkv[j],&zsfc[i],&nlvl,&zh[j],&ier);
	j += nlvl;
  }

/*
 * Return.
 */
  return(NclReturnValue((void*)zh,ndims_p,dsizes_p,NULL,NCL_float,0));
}

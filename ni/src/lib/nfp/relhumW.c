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

NhlErrorTypes relhum_W( void )
{
  extern float NGCALLF(relhum,RELHUM)(float*,float*,float*);
  int i, j, total;
/*
 * Input variables
 */
  float *t, *w, *p;
  int ndims_t, dsizes_t[NCL_MAX_DIMENSIONS];
  int ndims_w, dsizes_w[NCL_MAX_DIMENSIONS];
  int ndims_p, dsizes_p[NCL_MAX_DIMENSIONS];
/*
 * Output variables
 */
  float *rh;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 *
 * Retrieve argument #1
 */
  t = (float*)NclGetArgValue(
          0,
          3,
          &ndims_t, 
          dsizes_t,
          NULL,
          NULL,
          NULL,
          2);
/*
 * Retrieve argument #2
 */
  w = (float*)NclGetArgValue(
          1,
          3,
          &ndims_w, 
          dsizes_w,
          NULL,
          NULL,
          NULL,
          2);
/*
 * Retrieve argument #3
 */
  p = (float*)NclGetArgValue(
          2,
          3,
          &ndims_p, 
          dsizes_p,
          NULL,
          NULL,
          NULL,
          2);
/*
 * Check dimensions and calculate total size of arrays.
 */
  if( ndims_t != ndims_w || ndims_t != ndims_p ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"relhum: The input arrays must be the same size");
    return(NhlFATAL);
  }

  total = 1;
  for( i = 0; i < ndims_t; i++ ) {
	if( dsizes_t[i] != dsizes_p[i] || dsizes_t[i] != dsizes_w[i] ) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"relhum: The input arrays must have the same dimension sizes");
	  return(NhlFATAL);
	}
	total *= dsizes_t[i];
  }
/*
 * Allocate space for output array.
 */
  rh = (float*)NclMalloc(total*sizeof(float));
  if( rh == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"relhum: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Call function.
 */
  for( i = 0; i < total; i++ ) rh[i] = NGCALLF(relhum,RELHUM)(&t[i],&w[i],&p[i]);
/*
 * Return.
 */
  return(NclReturnValue((void*)rh,ndims_t,dsizes_t,NULL,NCL_float,0));
}


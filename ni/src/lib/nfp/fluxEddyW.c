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

float NGCALLF(flxedy, FLXEDY)(float *,float *,int *,float *,int *);

NhlErrorTypes fluxEddy_W( void )
{
/*
 * Input array variables
 */
  float *x, *y;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  int ndims_y, dsizes_y[NCL_MAX_DIMENSIONS], has_missing_y;
  NclScalar missing_x, missing_y;
/*
 * Output array variables
 */
  float *fluxeddy;
  int ndims_fluxeddy, size_fluxeddy, dsizes_fluxeddy[NCL_MAX_DIMENSIONS];
/*
 * Declare various variables for random purposes.
 */
  int i, j, ntime, ier;
  float xmsg;

/*
 * Retrieve arguments.
 */
  x = (float*)NclGetArgValue(
          0,
          2,
          &ndims_x,
          dsizes_x,
		  &missing_x,
		  &has_missing_x,
          NULL,
          2);

  y = (float*)NclGetArgValue(
          1,
          2,
          &ndims_y,
          dsizes_y,
		  &missing_y,
		  &has_missing_y,
          NULL,
          2);
/*
 * Test for missing values.
 */
  if( has_missing_x ) {
	xmsg = missing_x.floatval;
  }
  else if( has_missing_y ) {
	missing_x = missing_y;
	xmsg = missing_y.floatval;
  }
  else {
	xmsg = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
  }
/*
 * x and y must have the same dimensions.
 */
  if( ndims_x < 1 || ndims_x != ndims_y ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"fluxEddy: The input arrays x and y must have the same number of dimensions");
    return(NhlFATAL);
  }
  ntime = dsizes_x[ndims_x-1];

  for( i = 0; i < ndims_x; i++ ) {
	if(dsizes_x[i] != dsizes_y[i]) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"fluxEddy: The input arrays x and y must have the same dimension sizes");
	  return(NhlFATAL);
	}
  }
  
/*
 * Compute the total size of the output array (minus the last dimension).
 */
  size_fluxeddy = 1;
  dsizes_fluxeddy[0] = 1;
  for( i = 0; i < ndims_x-1; i++ ) {
    size_fluxeddy *= dsizes_x[i];
	dsizes_fluxeddy[i] = dsizes_x[i];
  }

/*
 * Allocate space for output value.
 */
  fluxeddy = (float *)NclMalloc(size_fluxeddy*sizeof(float));
  if( fluxeddy == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"fluxEddy: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Call the Fortran version of this routine.
 *
 */
  j = 0;
  for( i = 0; i < size_fluxeddy; i++ ) {
	fluxeddy[i] = NGCALLF(flxedy, FLXEDY)(&x[j],&y[j],&ntime,&xmsg,&ier);
	j += ntime;
  }

  ndims_fluxeddy  = ndims_x - 1 < 1 ? 1 : ndims_x - 1;
	
  if( has_missing_x || has_missing_y ) {
	return(NclReturnValue((void*)fluxeddy,ndims_fluxeddy,dsizes_fluxeddy,&missing_x,NCL_float,0));
  }
  else {
	return(NclReturnValue((void*)fluxeddy,ndims_fluxeddy,dsizes_fluxeddy,NULL,NCL_float,0));
  }
}


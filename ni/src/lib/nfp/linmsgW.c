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

NhlErrorTypes linmsg_W( void )
{
/*
 * Input variables
 */
  float *x;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  NclScalar missing_x;
  int *mflag; 
/*
 * Output variables
 */
  float *xnew;
/*
 * Other variables
 */
  int i, j, k, total, npts;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
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

  mflag = (int*)NclGetArgValue(
          1,
          2,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);

/*
 * A missing value must be set for this routine to work.
 */
  if(!has_missing_x) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linmsg: The input array must have the _FillValue attribute set");
    return(NhlFATAL);
  }
/*
 * Calculate total size of input array.
 */
  npts = dsizes_x[ndims_x-1];
  total = 1;
  for( i = 0; i <= ndims_x-2; i++ ) total *= dsizes_x[i];
/*
 * Allocate space for output array.
 */
  xnew = (float*)NclMalloc(total*npts*sizeof(float));
  if( xnew == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linmsg: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Call Fortran function.
 */
  j = 0;
  for( i = 0; i < total; i++ ) {
	for( k = 0; k < npts; k++ ) xnew[j+k] = x[j+k];
    NGCALLF(linmsg,LINMSG)(&xnew[j],&npts,&missing_x.floatval,mflag);
    j += npts;
  }
  return(NclReturnValue((void*)xnew,ndims_x,dsizes_x,&missing_x,NCL_float,0));
}

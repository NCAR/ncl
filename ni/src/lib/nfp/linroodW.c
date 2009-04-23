#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(linrood,LINROOD)(double*,double*,int*);
extern void NGCALLF(linroodwt,LINROODWT)(double*,int*);

NhlErrorTypes linrood_latwgt_W( void )
{
/*
 * Input array variables
 */
  int *npts;
/*
 * Output array variables
 */
  int dsizes_output[2];
  double *lat, *wgt, *output;
  int i;
/*
 * Retrieve arguments.
 */
  npts = (int*)NclGetArgValue(
          0,
          1,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

  if( *npts < 1) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linrood_latwgt: npts must be at least 1");
    return(NhlFATAL);
  }
/*
 * Allocate space for output array
 */
  lat    = (double*)calloc(*npts,sizeof(double));
  wgt    = (double*)calloc(*npts,sizeof(double));
  output = (double*)calloc(*npts*2,sizeof(double));
  if( lat == NULL || wgt == NULL || output == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linrood_latwgt: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Call the Fortran version of this routine.
 */
  NGCALLF(linrood,LINROOD)(lat,wgt,npts);
  for( i = 0; i < *npts; i++ ) {
    output[2*i]   = lat[i];
    output[2*i+1] = wgt[i];
  }
  NclFree(lat);
  NclFree(wgt);

  dsizes_output[0] = *npts;
  dsizes_output[1] = 2;
  return(NclReturnValue((void*)output,2,dsizes_output,NULL,NCL_double,0));
}


NhlErrorTypes linrood_wgt_W( void )
{
/*
 * Input array variables
 */
  int *nlat;
/*
 * Output array variables
 */
  double *wgt;
  int dsizes_wgt[1];

/*
 * Retrieve arguments.
 */
  nlat = (int*)NclGetArgValue(
          0,
          1,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

/*
 * Allocate space for output array
 */
  wgt = (double*)calloc(*nlat,sizeof(double));
  if( wgt == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linrood_wgt: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Call the Fortran version of this routine.
 */
  NGCALLF(linroodwt,LINROODWT)(wgt,nlat);

  dsizes_wgt[0] = *nlat;
  return(NclReturnValue(wgt,1,dsizes_wgt,NULL,NCL_double,0));
}

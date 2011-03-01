#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(linrood,LINROOD)(double*,double*,int*);
extern void NGCALLF(linroodwt,LINROODWT)(double*,int*);

NhlErrorTypes linrood_latwgt_W( void )
{
/*
 * Input array variables
 */
  void *N;
  ng_size_t *npts;
  int inpts;
  NclBasicDataTypes type_N;
/*
 * Output array variables
 */
  ng_size_t i, dsizes_output[2];
  double *lat, *wgt, *output;
/*
 * Retrieve arguments.
 */
  N = (void*)NclGetArgValue(
          0,
          1,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_N,
          DONT_CARE);

/*
 * Check the input dimension size.
 */
  npts = get_dimensions(N,1,type_N,"linrood_latwgt");
  if(npts == NULL) 
    return(NhlFATAL);

  if( *npts < 1) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linrood_latwgt: npts must be at least 1");
    return(NhlFATAL);
  }
  if(*npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linrood_latwgt: npts is greater than INT_MAX");
    return(NhlFATAL);
  }
  inpts = (int) *npts;
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
  NGCALLF(linrood,LINROOD)(lat,wgt,&inpts);
  for( i = 0; i < *npts; i++ ) {
    output[2*i]   = lat[i];
    output[2*i+1] = wgt[i];
  }
  NclFree(lat);
  NclFree(wgt);
  dsizes_output[0] = *npts;
  dsizes_output[1] = 2;
  NclFree(npts);
  return(NclReturnValue((void*)output,2,dsizes_output,NULL,NCL_double,0));
}


NhlErrorTypes linrood_wgt_W( void )
{
/*
 * Input array variables
 */
  void *N;
  ng_size_t *nlat;
  int inlat;
  NclBasicDataTypes type_N;
/*
 * Output array variables
 */
  double *wgt;
  ng_size_t dsizes_wgt[1];

/*
 * Retrieve arguments.
 */
  N = (void*)NclGetArgValue(
          0,
          1,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_N,
          DONT_CARE);

/*
 * Check the input dimension size.
 */
  nlat = get_dimensions(N,1,type_N,"linrood_wgt");
  if(nlat == NULL) 
    return(NhlFATAL);

  if(*nlat > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linrood_wgt: nlat is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlat = (int) *nlat;

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
  NGCALLF(linroodwt,LINROODWT)(wgt,&inlat);

  dsizes_wgt[0] = *nlat;
  return(NclReturnValue(wgt,1,dsizes_wgt,NULL,NCL_double,0));
}

#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(gauslobat,GAUSLOBAT)(double*,double*,int*);
extern void NGCALLF(findglw,FINDGLW)(double*,double*,int*);

NhlErrorTypes gaus_lobat_W( void )
{
/*
 * Input array variables
 */
  int *npts;
/*
 * Output array variables
 */
  int dsizes_output[2];
  double *xgl, *wgt, *output;
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
          2);

  if( *npts < 1) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gaus_lobat: npts must be at least 1");
    return(NhlFATAL);
  }
/*
 * Allocate space for output array
 */
  xgl    = (double*)calloc(*npts,sizeof(double));
  wgt    = (double*)calloc(*npts,sizeof(double));
  output = (double*)calloc(*npts*2,sizeof(double));
  if( xgl == NULL || wgt == NULL || output == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gaus_lobat: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Call the Fortran version of this routine.
 */
  NGCALLF(gauslobat,GAUSLOBAT)(xgl,wgt,npts);
  for( i = 0; i < *npts; i++ ) {
    output[2*i]   = xgl[i];
    output[2*i+1] = wgt[i];
  }
  NclFree(xgl);
  NclFree(wgt);

  dsizes_output[0] = *npts;
  dsizes_output[1] = 2;
  return(NclReturnValue((void*)output,2,dsizes_output,NULL,NCL_double,0));
}


NhlErrorTypes gaus_lobat_wgt_W( void )
{
/*
 * Input array variables
 */
  void *lat;
  double *tmp_lat;
  int dsizes_lat[1];
  NclBasicDataTypes type_lat;
/*
 * Output array variables
 */
  double *wgt;
  int dsizes_wgt[1];
/*
 * Declare various variables for random purposes.
 */
  int npts;
/*
 * Retrieve arguments.
 */
  lat = (void*)NclGetArgValue(
          0,
          1,
          NULL,
          dsizes_lat,
          NULL,
          NULL,
          &type_lat,
          2);

  npts = dsizes_lat[0];
/*
 * Coerce data to double no matter what, since input array may get
 * changed by Fortran routine.
 */
  tmp_lat = (double*)calloc(npts,sizeof(double));
  if( tmp_lat == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gaus_lobat_wgt: Unable to allocate memory for making a copy of the input array");
    return(NhlFATAL);
  }
  coerce_subset_input_double(lat,tmp_lat,0,type_lat,npts,0,NULL,NULL);
/*
 * Allocate space for output array
 */
  wgt = (double*)calloc(npts,sizeof(double));
  if( wgt == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gaus_lobat_wgt: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Call the Fortran version of this routine.
 */
  NGCALLF(findglw,FINDGLW)(tmp_lat,wgt,&npts);

/*
 * Free memory.
 */
  NclFree(tmp_lat);

  dsizes_wgt[0] = npts;
  return(NclReturnValue(wgt,1,dsizes_wgt,NULL,NCL_double,0));
}

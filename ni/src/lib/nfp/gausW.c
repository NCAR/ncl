#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(gauslobat,GAUSLOBAT)(double*,double*,int*);
extern void NGCALLF(findglw,FINDGLW)(double*,double*,int*);

NhlErrorTypes gaus_lobat_W( void )
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
  double *xgl, *wgt, *output;
  int ret;
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
  npts = get_dimensions(N,1,type_N,"gaus_lobat");
  if(npts == NULL) 
    return(NhlFATAL);


  if( *npts < 1) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gaus_lobat: npts must be at least 1");
    return(NhlFATAL);
  }

  if( *npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gaus_lobat: npts is greater than INT_MAX");
    return(NhlFATAL);
  }
  inpts = (int) *npts;
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
  NGCALLF(gauslobat,GAUSLOBAT)(xgl,wgt,&inpts);
  for( i = 0; i < *npts; i++ ) {
    output[2*i]   = xgl[i];
    output[2*i+1] = wgt[i];
  }
  NclFree(xgl);
  NclFree(wgt);

  dsizes_output[0] = *npts;
  dsizes_output[1] = 2;
  ret = NclReturnValue((void*)output,2,dsizes_output,NULL,NCL_double,0);
  NclFree(npts);
  return(ret);
}


NhlErrorTypes gaus_lobat_wgt_W( void )
{
/*
 * Input array variables
 */
  void *lat;
  double *tmp_lat;
  ng_size_t dsizes_lat[1];
  NclBasicDataTypes type_lat;
/*
 * Output array variables
 */
  double *wgt;
  ng_size_t dsizes_wgt[1];
/*
 * Declare various variables for random purposes.
 */
  ng_size_t npts;
  int inpts;
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
          DONT_CARE);

  if( dsizes_lat[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gaus_lobat_wgt: the length of lat is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_lat[0];
  inpts = (int) npts;
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
  NGCALLF(findglw,FINDGLW)(tmp_lat,wgt,&inpts);

/*
 * Free memory.
 */
  NclFree(tmp_lat);

  dsizes_wgt[0] = npts;
  return(NclReturnValue(wgt,1,dsizes_wgt,NULL,NCL_double,0));
}

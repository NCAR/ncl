#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "wrapper.h"

extern void NGCALLF(numrun2,NUMRUN2)(int *, int*, int*, int*, int*);

NhlErrorTypes dim_numrun_n_W( void)
{
/*
 * Input
 */
  int *x, *opt, *dims;
  int ndims_x;
  ng_size_t dims_dsizes[1], dsizes_x[NCL_MAX_DIMENSIONS];
/*
 * Output
 */
  int *xrun;
/*
 * various
 */
  ng_size_t i, j, k, nrnx, index_x, index_nrx;
  ng_size_t total_nl, total_nr, total_elements, npts;
  int inpts, ret, one=1;

/* 
 * Retrieve input from NCL script.
 */
  x = (int*)NclGetArgValue(
           0,
           3,
           &ndims_x,
           dsizes_x,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

  opt = (int*)NclGetArgValue(
           1,
           3,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

  dims = (int*)NclGetArgValue(
           2,
           3,
           NULL,
           dims_dsizes,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
/*
 * Some error checking. Make sure input dimensions are valid.
 */
  for(i = 0; i < dims_dsizes[0]; i++ ) {
    if(dims[i] < 0 || dims[i] >= ndims_x) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_numrun_n: Invalid dimension sizes to do count across, can't continue");
      return(NhlFATAL);
    }
    if(i > 0 && dims[i] != (dims[i-1]+1)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_numrun_n: Input dimension sizes must be monotonically increasing, can't continue");
      return(NhlFATAL);
    }
  }

/*
 * Compute the total number of elements in input in 
 * the dimensions to the right and left of the dimensions
 * to do the operation across.
 *
 * The dimension(s) to do the count across are "dims".
 */
  npts = total_nl = total_nr = total_elements = 1;
  for(i = 0; i < dims[0];   i++) {
    total_nl *= dsizes_x[i];
  }
  for(i = 0; i < dims_dsizes[0] ; i++) {
    npts = npts*dsizes_x[dims[i]];
  }
  for(i = dims[dims_dsizes[0]-1]+1; i < ndims_x; i++) {
    total_nr *= dsizes_x[i];
  }
  
  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_numrun_n: npts = %ld is greater than INT_MAX", npts);
    return(NhlFATAL);
  }
  inpts = (int) npts;

  total_elements = npts * total_nr * total_nl;

/*
 * Allocate space for output.
 */
  xrun = (int*)calloc(total_elements, sizeof(int));

  if(xrun == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_numrun_n: Unable to allocate memory for output");
    return(NhlFATAL);
  }
/*
 * Call the f77 double version of 'medmrng' with the full argument list.
 */
  nrnx = total_nr * npts;
  for(i = 0; i < total_nl; i++) {
    index_nrx = i*nrnx;
    for(j = 0; j < total_nr; j++) {
      index_x   = index_nrx + j;
      NGCALLF(numrun2,NUMRUN2)(&inpts,&one,&x[index_x],opt, &xrun[index_x]);
    }
  }

  ret = NclReturnValue(xrun, ndims_x, dsizes_x, NULL, NCL_int, 0);
  return(ret);
}

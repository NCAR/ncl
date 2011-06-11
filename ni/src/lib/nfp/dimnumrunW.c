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
  int *x, *opt, *dim;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
/*
 * Output
 */
  int *xrun;
/*
 * various
 */
  ng_size_t i, j, k, ii, nrnx, index_x, index_nrx;
  ng_size_t total_nl, total_nr, total_elements, nx;
  int inx, ret, one=1;
  int *tmp_x, *tmp_xrun;
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

  dim = (int*)NclGetArgValue(
           2,
           3,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
/*
 * Some error checking. Make sure input dimensions are valid.
 */
  if(*dim < 0 || *dim >= ndims_x) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_numrun_n: Invalid dimension size to do count across, can't continue");
    return(NhlFATAL);
  }

/*
 * Compute the total number of elements in input in 
 * the dimensions to the right and left of the dimensions
 * to do the operation across.
 *
 * The dimension to do the count across is "dim".
 */
  nx = total_nl = total_nr = total_elements = 1;
  for(i = 0; i < *dim;   i++) {
    total_nl *= dsizes_x[i];
  }
  nx = nx*dsizes_x[*dim];
  for(i = *dim+1; i < ndims_x; i++) {
    total_nr *= dsizes_x[i];
  }
  
  if(nx > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_numrun_n: nx = %ld is greater than INT_MAX", nx);
    return(NhlFATAL);
  }
  inx = (int) nx;

  total_elements = nx * total_nr * total_nl;

/*
 * Allocate space for output and temporary input.
 */
  tmp_x    = (int*)calloc(nx, sizeof(int));
  xrun     = (int*)calloc(total_elements, sizeof(int));
  tmp_xrun = (int*)calloc(nx, sizeof(int));

  if(tmp_x == NULL || xrun == NULL || tmp_xrun == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_numrun_n: Unable to allocate memory for input/output");
    return(NhlFATAL);
  }
/*
 * Call the f77 subroutine.
 */
  nrnx = total_nr * nx;
  for(i = 0; i < total_nl; i++) {
    index_nrx = i*nrnx;
    for(j = 0; j < total_nr; j++) {
      index_x   = index_nrx + j;
      for(k = 0; k < nx; k++ ) {
	ii = total_nr*k;
	tmp_x[k] = x[index_x+ii];
      }
      NGCALLF(numrun2,NUMRUN2)(&inx,&one,tmp_x,opt, tmp_xrun);
      for(k = 0; k < nx; k++ ) {
	ii = total_nr*k;
	xrun[index_x+ii] = tmp_xrun[k];
      }
    }
  }

  ret = NclReturnValue(xrun, ndims_x, dsizes_x, NULL, NCL_int, 0);
  return(ret);
}

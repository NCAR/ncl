#include <stdio.h>
#include <strings.h>
#include "wrapper.h"

extern void NGCALLF(kenstst,KENSTST)(double *, int *, int *, double *, 
                                     double *, double *, int *, 
                                     double *, logical *, double *);

NhlErrorTypes trend_manken_W( void )
{

/*
 * Input variables
 */
/*
 * Argument # 0
 */
  void *x;
  double *tmp_x;
  int       ndims_x;
  ng_size_t nx, dsizes_x[NCL_MAX_DIMENSIONS];
  int inx;
  NclBasicDataTypes type_x;

/*
 * Argument # 1
 */
  int *dims;
  ng_size_t ndims;
/*
 * Argument # 2
 */
  logical *opt;
/*
 * Return variable
 */
  void *tm;
  int       ndims_tm;
  ng_size_t *dsizes_tm;
  NclBasicDataTypes type_tm;

/*
 * Various
 */
  int nslp, s;
  logical *tieflag;
  double z, prob, trend, eps, *slope;
  ng_size_t i, j, nrnx, total_nl, total_nr, total_elements, size_output;
  ng_size_t index_nrx, index_nr, index_x, index_tm;
  int ret;

/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
/*
 * Get argument # 0
 */
  x = (void*)NclGetArgValue(
           0,
           3,
           &ndims_x,
           dsizes_x,
           NULL,
           NULL,
           &type_x,
           DONT_CARE);

/*
 * Get argument # 1
 */
  opt = (logical*)NclGetArgValue(
           1,
           3,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

/*
 * Get argument # 2
 */
  dims = (int *)NclGetArgValue(2,3,NULL,&ndims,NULL,NULL,NULL,0);
/*
 * Some error checking. Make sure input dimensions are valid.
 */
  for(i = 0; i < ndims; i++ ) {
    if(dims[i] < 0 || dims[i] >= ndims_x) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"trend_manken: Invalid dimension sizes to do calculation on, can't continue");
      return(NhlFATAL);
    }
    if(i > 0 && dims[i] != (dims[i-1]+1)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"trend_manken: Input dimension sizes must be monotonically increasing, can't continue");
      return(NhlFATAL);
    }
  }

/* 
 * Allocate space for output dimension sizes and set them.
 */
  ndims_tm = ndims_x - ndims + 1;
  dsizes_tm = (ng_size_t*)calloc(ndims_tm,sizeof(ng_size_t));  
  if( dsizes_tm == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"trend_manken: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }

  if(ndims_tm == 1) j = 0;
  else              j = 1;
  dsizes_tm[0] = 2;

  nx = total_nl = total_nr = 1;
  for(i = 0; i < dims[0]; i++) {
    total_nl *= dsizes_x[i];
    dsizes_tm[j+i] = dsizes_x[i];
  }
  for(i = 0; i < ndims ; i++) {
    nx = nx*dsizes_x[dims[i]];
  }
  for(i = dims[ndims-1]+1; i < ndims_x; i++) {
    total_nr *= dsizes_x[i];
    dsizes_tm[j+i-ndims] = dsizes_x[i];
  }

  if(nx > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"trend_manken: nx = %ld is greater than INT_MAX", nx);
    return(NhlFATAL);
  }
  inx  = (int) nx;
  nslp = inx*(inx-1)/2;
/*
 * Allocate space for tmp_x.
 */
  tmp_x = (double *)calloc(nx,sizeof(double));
  if(tmp_x == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"trend_manken: Unable to allocate memory for coercing input array to double");
    return(NhlFATAL);
  }

/* 
 * Allocate space for output array.
 */
  total_elements = total_nr * total_nl;
  size_output    = 2 * total_elements;
  if(type_x != NCL_double) {
    type_tm = NCL_float;
    tm = (void *)calloc(size_output, sizeof(float));
  }
  else {
    type_tm = NCL_double;
    tm = (void *)calloc(size_output, sizeof(double));
  }
  if(tm == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"trend_manken: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/* 
 * Allocate space for various other arrays.
 */
  slope   = (double *)calloc(nslp, sizeof(double));
  tieflag = (logical *)calloc(nslp, sizeof(logical));
  if(slope == NULL || tieflag == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"trend_manken: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }
  eps = 1.0e-5;

/*
 * Loop across rightmost/leftmost dimensions and call 
 * the Fortran routine for each subsection of the 
 * input arrays.
 */
  nrnx = total_nr * nx;
  for(i = 0; i < total_nl; i++) {
    index_nrx = i*nrnx;
    index_nr  = i*total_nr;
    for(j = 0; j < total_nr; j++) {
      index_x  = index_nrx + j;
      index_tm = index_nr + j;
/*
 * Coerce subsection of x (tmp_x) to double.
 */
      coerce_subset_input_double_step(x,tmp_x,index_x,total_nr,type_x,
                                      nx,0,NULL,NULL);

/*
 * Call the Fortran routine.
 */
      NGCALLF(kenstst,KENSTST)(tmp_x, &inx, &s, &z, &prob, &trend,
                               &nslp, slope, tieflag, &eps);

/*
 * Coerce output array to appropriate type
 */
      coerce_output_float_or_double(tm,&prob,type_tm,1,index_tm);
      coerce_output_float_or_double(tm,&trend,type_tm,1,index_tm+total_elements);
    }
  }

/*
 * Free unneeded memory.
 */
  NclFree(tmp_x);
  NclFree(slope);
  NclFree(tieflag);

/*
 * Return value back to NCL script.
 */
  ret = NclReturnValue(tm,ndims_tm,dsizes_tm,NULL,type_tm,0);

  NclFree(dsizes_tm);
  return(ret);
}

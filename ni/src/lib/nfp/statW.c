
#include <stdio.h>
#include <stdlib.h>
/*
* The following are the required NCAR Graphics include files.
* They should be located in ${NCARG_ROOT}/include
*/

#include "wrapper.h"
#include <math.h>

#define max(x,y)  ((x) > (y) ? (x) : (y))

extern void NGCALLF(stat2,STAT2)(float*, int*, float*, float*, float*, 
                                 float*, int*, int*); 

extern void NGCALLF(stat4,STAT4)(float*, int*, float*, float*, float*, 
                                 float*, float*, float*, int*, int*); 

extern void NGCALLF(dstat4,DSTAT4)(double*, int*, double*, double*, double*, 
                                   double*, double*, double*, int*, int*); 

extern void NGCALLF(stat2t,STAT2T)(float*, int*, float*, float*, float*, 
                                   float*, int*, float*, float*, int*); 

extern void NGCALLF(medmrng,MEDMRNG)(float*, float*, int*, float*, float*,
                                     float*, float*, int*, int*); 

extern void NGCALLF(xstnd,XSTND)(float*, int*, float*, int*, int*);

extern void NGCALLF(drmvmean,DRMVMEAN)(double*, int*, double*, int*);

extern void NGCALLF(drmvmed,DRMVMED)(double*, double*, int*, double*, int*);

extern void NGCALLF(dmedmrng,DMEDMRNG)(double*, double*, int*, double*, 
                                       double*, double*, double*, int*, 
                                       int*); 

extern void NGCALLF(drmsd,DRMSD)(double *, double *, int *, double *, 
                                 double *, double *, int *, int *);

extern void NGCALLF(dxstnd,DXSTND)(double*, int*, double*, int*, int*);

extern void NGCALLF(desauto,DESAUTO)(double*, int*, double*, double*, 
                                     double*, int*, double*, double*, int*);

extern void NGCALLF(descros,DESCROS)(double*, double*, int*, double*, 
                                     double*, double*, double*, double*,
                                     double*, int*, double*, double*, int*);

NhlErrorTypes stat2_W( void )
{
/*
 * Input array variables
 */
  float *x;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  NclScalar missing_x;
/*
 * Output array variables
 */
  float *xmean, *xvar;
  int *nptused;
  int ndims_xmean, dsizes_xmean[NCL_MAX_DIMENSIONS];
  int ndims_xvar, dsizes_xvar[NCL_MAX_DIMENSIONS];
  int ndims_nptused, dsizes_nptused[NCL_MAX_DIMENSIONS];
  int ndims_out, *dsizes_out;
/*
 * various
 */
  int i, l1, l2, total_elem_x, ier = 0, npts;
  float xmsg, xsd;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  x = (float*)NclGetArgValue(
           0,
           4,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           NULL,
           2);
/*
 * Test for a missing value.
 */
  if( has_missing_x ) {
    xmsg = missing_x.floatval;
  }
  else {
    xmsg = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
  }
/*
 * Compute the total number of elements in our x array.
 */
  total_elem_x = 1;
  for(i = 0; i < ndims_x-1; i++) total_elem_x *= dsizes_x[i];
/*
 * Calculate what the size is supposed to be of our output arrays.
 */
  ndims_out = max(ndims_x-1,1);
  dsizes_out = (int*)NclMalloc(ndims_out*sizeof(int));
  dsizes_out[0] = 1;
  for(i = 0; i < ndims_x-1; i++ ) dsizes_out[i] = dsizes_x[i];
/* 
 * Get output variables.
 */
  xmean = (float*)NclGetArgValue(
           1,
           4,
           &ndims_xmean, 
           dsizes_xmean,
           NULL,
           NULL,
           NULL,
           1);
  xvar = (float*)NclGetArgValue(
           2,
           4,
           &ndims_xvar, 
           dsizes_xvar,
           NULL,
           NULL,
           NULL,
           1);
  nptused = (int*)NclGetArgValue(
           3,
           4,
           &ndims_nptused, 
           dsizes_nptused,
           NULL,
           NULL,
           NULL,
           1);

  if( ndims_xmean != ndims_out || ndims_nptused != ndims_out || ndims_xvar != ndims_out ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"stat2: The number of dimensions of xmean, xvar, and nptused must be one less than the number of dimensions of x (or they must all be scalar if x is just a 1-d array)");
    return(NhlFATAL);
  }
/*
 * dimension sizes of xmean, xvar, and nptused must be the same.
 */
  for(i = 0; i < ndims_out; i++ ) {
      if( dsizes_xmean[i] != dsizes_out[i] || dsizes_nptused[i] != dsizes_out[i] || dsizes_xvar[i] != dsizes_out[i] ) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,"stat2: The dimensions of xmean, xvar, and nptused must be the same as the left-most dimensions of x");
          return(NhlFATAL);
      }
  }
/*
 * Call the f77 version of 'stat2' with the full argument list.
 */
  l1 = l2 = 0;
  npts = dsizes_x[ndims_x-1];
  for(i = 1; i <= total_elem_x; i++) {
    NGCALLF(stat2,STAT2)(&x[l1],&npts,&xmsg,
                         &xmean[l2],&xvar[l2],&xsd,&nptused[l2],&ier);
    l1 += npts;
    l2++;
    if (ier == 2) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"stat2: The first input array contains all missing values");
      return(NhlFATAL);
    }
  }
  return(NhlNOERROR);
}

NhlErrorTypes stat_trim_W( void )
{
/*
 * Input array variables
 */
  float *x, *ptrim;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  NclScalar missing_x;
/*
 * Output array variables
 */
  float *xmeant, *xsdt;
  int *nptused;
  int ndims_xmeant, dsizes_xmeant[NCL_MAX_DIMENSIONS];
  int ndims_xsdt, dsizes_xsdt[NCL_MAX_DIMENSIONS];
  int ndims_nptused, dsizes_nptused[NCL_MAX_DIMENSIONS];
  int ndims_out, *dsizes_out;
/*
 * various
 */
  int i, l1, l2, total_elem_x, ier = 0, npts;
  float xmsg, xvart, *work;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  x = (float*)NclGetArgValue(
           0,
           5,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           NULL,
           2);
  ptrim = (float*)NclGetArgValue(
           1,
           5,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           2);
  if( *ptrim < 0. || *ptrim >= 1) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"stat_trim: ptrim must be >= 0.0 and < 1.0");
      return(NhlFATAL);
  }
/*
 * Test for a missing value.
 */
  if( has_missing_x ) {
    xmsg = missing_x.floatval;
  }
  else {
    xmsg = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
  }
/*
 * Compute the total number of elements in our x array.
 */
  total_elem_x = 1;
  for(i = 0; i < ndims_x-1; i++) total_elem_x *= dsizes_x[i];
/*
 * Calculate what the size is supposed to be of our output arrays.
 */
  ndims_out = max(ndims_x-1,1);
  dsizes_out = (int*)NclMalloc(ndims_out*sizeof(int));
  dsizes_out[0] = 1;
  for(i = 0; i < ndims_x-1; i++ ) dsizes_out[i] = dsizes_x[i];
/* 
 * Get output variables.
 */
  xmeant = (float*)NclGetArgValue(
           2,
           5,
           &ndims_xmeant, 
           dsizes_xmeant,
           NULL,
           NULL,
           NULL,
           1);
  xsdt = (float*)NclGetArgValue(
           3,
           5,
           &ndims_xsdt, 
           dsizes_xsdt,
           NULL,
           NULL,
           NULL,
           1);
  nptused = (int*)NclGetArgValue(
           4,
           5,
           &ndims_nptused, 
           dsizes_nptused,
           NULL,
           NULL,
           NULL,
           1);

  if( ndims_xmeant != ndims_out || ndims_nptused != ndims_out || ndims_xsdt != ndims_out ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"stat_trim: The number of dimensions of xmeant, xsdt, and nptused must be one less than the number of dimensions of x (or they must all be scalar if x is just a 1-d array)");
    return(NhlFATAL);
  }
/*
 * dimension sizes of xmeant, xsdt, and nptused must be the same.
 */
  for(i = 0; i < ndims_out; i++ ) {
      if( dsizes_xmeant[i] != dsizes_out[i] || dsizes_nptused[i] != dsizes_out[i] || dsizes_xsdt[i] != dsizes_out[i] ) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,"stat_trim: The dimensions of xmeant, xsdt, and nptused must be the same as the left-most dimensions of x");
          return(NhlFATAL);
      }
  }
/*
 * Allocate space for work array.
 */
  npts = dsizes_x[ndims_x-1];
  work = (float*)calloc(npts,sizeof(float));
  if (work == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"stat_trim: Unable to allocate space for work array" );
    return(NhlFATAL);
  }

/*
 * Call the f77 version of 'stat_trim' with the full argument list.
 */
  l1 = l2 = 0;
  for(i = 1; i <= total_elem_x; i++) {
    NGCALLF(stat2t,STAT2T)(&x[l1],&npts,&xmsg,
                         &xmeant[l2],&xvart,&xsdt[l2],&nptused[l2],work,ptrim,&ier);
    l1 += npts;
    l2++;
    if (ier == 2) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"stat_trim: The first input array contains all missing values");
      return(NhlFATAL);
    }
    if (ier == 4) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"stat_trim: not enough trimmed values");
      return(NhlFATAL);
    }
  }
  NclFree(work);
  return(NhlNOERROR);
}


NhlErrorTypes stat4_W( void )
{
/*
 * Input array variables
 */
  float *x;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  NclScalar missing_x;
/*
 * Output array variables
 */
  float *xmean, *xvar, *xskew, *xkurt;
  int *nptused;
  int ndims_xmean, dsizes_xmean[NCL_MAX_DIMENSIONS];
  int ndims_xskew, dsizes_xskew[NCL_MAX_DIMENSIONS];
  int ndims_xkurt, dsizes_xkurt[NCL_MAX_DIMENSIONS];
  int ndims_xvar, dsizes_xvar[NCL_MAX_DIMENSIONS];
  int ndims_nptused, dsizes_nptused[NCL_MAX_DIMENSIONS];
  int ndims_out, *dsizes_out;
/*
 * various
 */
  int i, l1, l2, total_elem_x, ier = 0, npts;
  float xmsg, xsd;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  x = (float*)NclGetArgValue(
           0,
           6,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           NULL,
           2);
/*
 * Test for a missing value.
 */
  if( has_missing_x ) {
    xmsg = missing_x.floatval;
  }
  else {
    xmsg = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
  }
/*
 * Compute the total number of elements in our x array.
 */
  total_elem_x = 1;
  for(i = 0; i < ndims_x-1; i++) total_elem_x *= dsizes_x[i];
/*
 * Calculate what the size is supposed to be of our output arrays.
 */
  ndims_out = max(ndims_x-1,1);
  dsizes_out = (int*)NclMalloc(ndims_out*sizeof(int));
  dsizes_out[0] = 1;
  for(i = 0; i < ndims_x-1; i++ ) dsizes_out[i] = dsizes_x[i];
/* 
 * Get output variables.
 */
  xmean = (float*)NclGetArgValue(
           1,
           6,
           &ndims_xmean, 
           dsizes_xmean,
           NULL,
           NULL,
           NULL,
           1);
  xvar = (float*)NclGetArgValue(
           2,
           6,
           &ndims_xvar, 
           dsizes_xvar,
           NULL,
           NULL,
           NULL,
           1);
  xskew = (float*)NclGetArgValue(
           3,
           6,
           &ndims_xskew, 
           dsizes_xskew,
           NULL,
           NULL,
           NULL,
           1);
  xkurt = (float*)NclGetArgValue(
           4,
           6,
           &ndims_xkurt, 
           dsizes_xkurt,
           NULL,
           NULL,
           NULL,
           1);
  nptused = (int*)NclGetArgValue(
           5,
           6,
           &ndims_nptused, 
           dsizes_nptused,
           NULL,
           NULL,
           NULL,
           1);

  if( ndims_xmean != ndims_out || ndims_nptused != ndims_out || ndims_xskew != ndims_out || ndims_xkurt != ndims_out || ndims_xvar != ndims_out ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"stat4: The number of dimensions of xmean, xvar, xskew, xkurt, and nptused must be one less than the number of dimensions of x (or they must all be scalar if x is just a 1-d array)");
    return(NhlFATAL);
  }
/*
 * dimension sizes of xmean, xskew, and nptused must be the same.
 */
  for(i = 0; i < ndims_out; i++ ) {
      if( dsizes_xmean[i] != dsizes_out[i] || dsizes_xkurt[i] != dsizes_out[i] || 
         dsizes_xvar[i] != dsizes_out[i] || dsizes_nptused[i] != dsizes_out[i] ||
         dsizes_xskew[i] != dsizes_out[i] ) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,"stat4: The dimensions of xmean, xskew, xkurt, xvar, and nptused must be the same as the left-most dimensions of x");
          return(NhlFATAL);
      }
  }
/*
 * Call the f77 version of 'stat4' with the full argument list.
 */
  l1 = l2 = 0;
  npts = dsizes_x[ndims_x-1];
  for(i = 1; i <= total_elem_x; i++) {
    NGCALLF(stat4,STAT4)(&x[l1],&npts,&xmsg,
                         &xmean[l2],&xvar[l2],&xsd,&xskew[l2],&xkurt[l2],&nptused[l2],&ier);
    l1 += npts;
    l2++;
    if (ier == 2) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"stat4: The first input array contains all missing values");
      return(NhlFATAL);
    }
  }
  return(NhlNOERROR);
}

NhlErrorTypes stat_medrng_W( void )
{
/*
 * Input array variables
 */
  float *x;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  NclScalar missing_x;
/*
 * Output array variables
 */
  float *xmedian, *xmrange, *xrange;
  int *nptused;
  int ndims_xmedian, dsizes_xmedian[NCL_MAX_DIMENSIONS];
  int ndims_xrange, dsizes_xrange[NCL_MAX_DIMENSIONS];
  int ndims_xmrange, dsizes_xmrange[NCL_MAX_DIMENSIONS];
  int ndims_nptused, dsizes_nptused[NCL_MAX_DIMENSIONS];
  int ndims_out, *dsizes_out;
/*
 * various
 */
  int i, l1, l2, total_elem_x, ier = 0, npts;
  float xmsg, *work;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  x = (float*)NclGetArgValue(
           0,
           5,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           NULL,
           2);
/*
 * Test for a missing value.
 */
  if( has_missing_x ) {
    xmsg = missing_x.floatval;
  }
  else {
    xmsg = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
  }
/*
 * Compute the total number of elements in our x array.
 */
  total_elem_x = 1;
  for(i = 0; i < ndims_x-1; i++) total_elem_x *= dsizes_x[i];
/*
 * Calculate what the size is supposed to be of our output arrays.
 */
  ndims_out = max(ndims_x-1,1);
  dsizes_out = (int*)NclMalloc(ndims_out*sizeof(int));
  dsizes_out[0] = 1;
  for(i = 0; i < ndims_x-1; i++ ) dsizes_out[i] = dsizes_x[i];
/* 
 * Get output variables.
 */
  xmedian = (float*)NclGetArgValue(
           1,
           5,
           &ndims_xmedian, 
           dsizes_xmedian,
           NULL,
           NULL,
           NULL,
           1);
  xmrange = (float*)NclGetArgValue(
           2,
           5,
           &ndims_xmrange, 
           dsizes_xmrange,
           NULL,
           NULL,
           NULL,
           1);
  xrange = (float*)NclGetArgValue(
           3,
           5,
           &ndims_xrange, 
           dsizes_xrange,
           NULL,
           NULL,
           NULL,
           1);
  nptused = (int*)NclGetArgValue(
           4,
           5,
           &ndims_nptused, 
           dsizes_nptused,
           NULL,
           NULL,
           NULL,
           1);

  if( ndims_xmedian != ndims_out || ndims_nptused != ndims_out || ndims_xrange != ndims_out || ndims_xmrange != ndims_out ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"stat_medrng: The number of dimensions of xmedian, xmrange, xrange, and nptused must be one less than the number of dimensions of x (or they must all be scalar if x is just a 1-d array)");
    return(NhlFATAL);
  }
/*
 * dimension sizes of xmedian, xrange, and nptused must be the same.
 */
  for(i = 0; i < ndims_out; i++ ) {
      if( dsizes_xmedian[i] != dsizes_out[i] || dsizes_xmrange[i] != dsizes_out[i] ||
          dsizes_nptused[i] != dsizes_out[i] || dsizes_xrange[i] != dsizes_out[i] ) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,"stat_medrng: The dimensions of xmedian, xrange, xmrange, and nptused must be the same as the left-most dimensions of x");
          return(NhlFATAL);
      }
  }
/*
 * Allocate space for work array.
 */
  npts = dsizes_x[ndims_x-1];
  work = (float*)calloc(npts,sizeof(float));
  if (work == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"stat_medrng: Unable to allocate space for work array" );
    return(NhlFATAL);
  }
/*
 * Call the f77 version of 'stat_medrng' with the full argument list.
 */
  l1 = l2 = 0;
  npts = dsizes_x[ndims_x-1];
  for(i = 1; i <= total_elem_x; i++) {
    NGCALLF(medmrng,MEDMRNG)(&x[l1],work,&npts,&xmsg,
                 &xmedian[l2],&xmrange[l2],&xrange[l2],&nptused[l2],&ier);
    l1 += npts;
    l2++;
    if (ier == 2) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"stat_medrng: The first input array contains all missing values");
      return(NhlFATAL);
    }
  }
  NclFree(work);
  return(NhlNOERROR);
}


NhlErrorTypes dim_median_W( void )
{
/*
 * Input array variables
 */
  void *x;
  double *tmp_x;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  NclScalar missing_x, missing_dx, missing_rx;
  NclBasicDataTypes type_x;
/*
 * Output array variables
 */
  void *xmedian;
  double xrange, xmrange, *tmp_xmedian;
  int dsizes_median[NCL_MAX_DIMENSIONS];
  int nptused, ndims_median;
/*
 * various
 */
  int i, l1, total_elements, ier = 0, npts;
  double *work;
/*
 * Retrieve parameter.
 */
  x = (void*)NclGetArgValue(
           0,
           1,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           2);
/*
 * Compute the total number of elements in output and input.
 */
  ndims_median = max(ndims_x-1,1);
  dsizes_median[0] = 1;

  total_elements = 1;
  for(i = 0; i < ndims_x-1; i++) {
    total_elements *= dsizes_x[i];
    dsizes_median[i] = dsizes_x[i];
  }    

  npts = dsizes_x[ndims_x-1];
/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Allocate space for in/output arrays.
 */
  if(type_x != NCL_double) {
    xmedian     = (void*)calloc(total_elements,sizeof(float));
    tmp_x       = (double*)calloc(npts,sizeof(double));
    tmp_xmedian = (double*)calloc(1,sizeof(double));
    if(xmedian == NULL || tmp_xmedian == NULL || tmp_x == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_median: Unable to allocate memory for in/output arrays");
      return(NhlFATAL);
    }
  }
  else {
    xmedian = (void*)calloc(total_elements,sizeof(double));
    if (xmedian == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_median: Unable to allocate memory for output array" );
      return(NhlFATAL);
    }
  }
/*
 * Allocate space for work array.
 */
  work = (double*)calloc(npts,sizeof(double));
  if (work == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_median: Unable to allocate space for work array" );
    return(NhlFATAL);
  }
/*
 * Call the f77 double version of 'medmrng' with the full argument list.
 */
  l1 = 0;
  for(i = 0; i < total_elements; i++) {
    if(type_x != NCL_double) {
/*
 * Coerce subsection of x (tmp_x) to double.
 */
      coerce_subset_input_double(x,tmp_x,l1,type_x,npts,0,NULL,NULL);
    }
    else {
      tmp_x       = &((double*)x)[l1];
      tmp_xmedian = &((double*)xmedian)[i];
    }

    NGCALLF(dmedmrng,DMEDMRNG)(tmp_x,work,&npts,&missing_dx.doubleval,
                               tmp_xmedian,&xmrange,&xrange,&nptused,&ier);

    if(type_x != NCL_double) ((float*)xmedian)[i] = (float)(*tmp_xmedian);

    l1 += npts;
    if (ier == 2) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"dim_median: The input array contains all missing values");
    }
  }
/*
 * Free unneeded memory.
 */
  NclFree(work);
  if(type_x != NCL_double) {
    NclFree(tmp_x);
    NclFree(tmp_xmedian);
  }
/*
 * Return float if input isn't double, otherwise return double.
 */
  if(type_x != NCL_double) {
    return(NclReturnValue(xmedian,ndims_median,dsizes_median,&missing_rx,
                          NCL_float,0));
  }
  else {
/*
 * return double values
 */
    return(NclReturnValue(xmedian,ndims_median,dsizes_median,&missing_dx,
                          NCL_double,0));
  }
}


NhlErrorTypes dim_rmvmean_W( void )
{
/*
 * Input array variables
 */
  void *x;
  double *tmp_x;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  NclScalar missing_x, missing_dx, missing_rx;
  NclBasicDataTypes type_x;
/*
 * Output
 */
  void *rmvmean;
/*
 * various
 */
  int i, j, l1, total_size_x, total_size_x1, ier = 0, npts;
/*
 * Retrieve parameter.
 */
  x = (void*)NclGetArgValue(
           0,
           1,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           2);
/*
 * Compute the total number of elements in output and input.
 */
  total_size_x1 = 1;
  for(i = 0; i < ndims_x-1; i++) total_size_x1 *= dsizes_x[i];

  npts = dsizes_x[ndims_x-1];
  total_size_x = total_size_x1 * npts;
/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Coerce x to double no matter what, since we need a copy of the input
 * array.
 */
  tmp_x = (double*)calloc(npts,sizeof(double));
  if( tmp_x == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmvmean: Unable to allocate memory for coercing x array to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate space for output array
 */
  if(type_x != NCL_double) {
    rmvmean = (void*)calloc(total_size_x,sizeof(float));
  }
  else {
    rmvmean = (void*)calloc(total_size_x,sizeof(double));
  }
  if( rmvmean == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmvmean: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Call the f77 double version of 'rmvmean' with the full argument list.
 */
  l1 = 0;
  for(i = 0; i < total_size_x1; i++) {
/*
 * Coerce subsection of x (tmp_x) to double.
 */
    coerce_subset_input_double(x,tmp_x,l1,type_x,npts,0,NULL,NULL);

    NGCALLF(drmvmean,DRMVMEAN)(tmp_x,&npts,&missing_dx.doubleval,&ier);
    for(j = 0; j < npts; j++) {
      if(type_x != NCL_double) {
        ((float*)rmvmean)[l1+j] = (float)(tmp_x[j]);
      }
      else {
        ((double*)rmvmean)[l1+j] = tmp_x[j];
      }
    }

    l1 += npts;
    if (ier == 2) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"dim_rmvmean: The input array contains all missing values");
    }
  }
/*
 * Free temp array.
 */
  NclFree(tmp_x);
/*
 * Return float if input isn't double, otherwise return double.
 */
  if(type_x != NCL_double) {
/*
 * Return float values. 
 */
    return(NclReturnValue(rmvmean,ndims_x,dsizes_x,&missing_rx,
                          NCL_float,0));
  }
  else {
/*
 * Return double values. 
 */
    return(NclReturnValue(rmvmean,ndims_x,dsizes_x,&missing_dx,
                          NCL_double,0));
  }
}


NhlErrorTypes dim_rmvmed_W( void )
{
/*
 * Input array variables
 */
  void *x;
  double *tmp_x;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  NclScalar missing_x, missing_dx, missing_rx;
  NclBasicDataTypes type_x;
/*
 * Output array variables
 */
  void *rmvmed;
  double *work;
/*
 * various
 */
  int i, j, l1, total_size_x1, total_size_x, ier = 0, npts;
/*
 * Retrieve parameter.
 */
  x = (void*)NclGetArgValue(
           0,
           1,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           2);
/*
 * Compute the total number of elements minus the last dimension.
 */
  total_size_x1 = 1;
  for(i = 0; i < ndims_x-1; i++) total_size_x1 *= dsizes_x[i];

  npts = dsizes_x[ndims_x-1];
  total_size_x = total_size_x1 * npts;
/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Coerce x to double no matter what, since we need a copy of the input
 * array.
 */
  tmp_x = (double*)calloc(npts,sizeof(double));
  if( tmp_x == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmvmed: Unable to allocate memory for coercing x array to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate space for output array
 */
  if(type_x != NCL_double) {
    rmvmed = (void*)calloc(total_size_x,sizeof(float));
  }
  else {
    rmvmed = (void*)calloc(total_size_x,sizeof(double));
  }
  if( rmvmed == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmvmedian: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Allocate space for work array.
 */
  work = (double*)calloc(npts,sizeof(double));
  if (work == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmvmed: Unable to allocate space for work array" );
    return(NhlFATAL);
  }
/*
 * Call the f77 double version of 'rmvmed' with the full argument list.
 */
  l1 = 0;
  for(i = 0; i < total_size_x1; i++) {
/*
 * Coerce subsection of x (tmp_x) to double.
 */
    coerce_subset_input_double(x,tmp_x,l1,type_x,npts,0,NULL,NULL);

    NGCALLF(drmvmed,DRMVMED)(tmp_x,work,&npts,&missing_dx.doubleval,&ier);
    for(j = 0; j < npts; j++) {
      if(type_x != NCL_double) {
        ((float*)rmvmed)[l1+j] = (float)(tmp_x[j]);
      }
      else {
        ((double*)rmvmed)[l1+j] = tmp_x[j];
      }
    }

    l1 += npts;
    if (ier == 2) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"dim_rmvmed: The input array contains all missing values");
    }
  }
/*
 * Free work array.
 */
  NclFree(work);
  NclFree(tmp_x);
/*
 * Return float if input isn't double, otherwise return double.
 */
  if(type_x != NCL_double) {
/*
 * Return float values. 
 */
    return(NclReturnValue(rmvmed,ndims_x,dsizes_x,&missing_rx,
                          NCL_float,0));
  }
  else {
/*
 * Return double values. 
 */
    return(NclReturnValue(rmvmed,ndims_x,dsizes_x,&missing_dx,
                          NCL_double,0));
  }
}

NhlErrorTypes dim_standardize_W( void )
{
/*
 * Input array variables
 */
  void *x;
  double *tmp_x;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  NclScalar missing_x, missing_dx, missing_rx;
  NclBasicDataTypes type_x;
  int *opt;
/*
 * Output
 */
  void *standardize;
/*
 * various
 */
  int i, j, l1, total_size_x1, total_size_x, ier = 0, npts;
/*
 * Retrieve parameter.
 */
  x = (void*)NclGetArgValue(
           0,
           2,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           2);
/*
 * Get second argument.
 */ 
  opt = (int*)NclGetArgValue(
           1,
           2,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           2);
/*
 * Compute the total number of elements minus the last dimension.
 */
  total_size_x1 = 1;
  for(i = 0; i < ndims_x-1; i++) total_size_x1 *= dsizes_x[i];

  npts = dsizes_x[ndims_x-1];
  total_size_x = total_size_x1 * npts;
/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Coerce x to double no matter what, since we need a copy of the input
 * array.
 */
  tmp_x = (double*)calloc(npts,sizeof(double));
  if( tmp_x == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_standardize: Unable to allocate memory for coercing x array to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate space for output array
 */
  if(type_x != NCL_double) {
    standardize = (void*)calloc(total_size_x,sizeof(float));
  }
  else {
    standardize = (void*)calloc(total_size_x,sizeof(double));
  }
  if( standardize == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_standardize: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Call the f77 double version of 'xstnd' with the full argument list.
 */
  l1 = 0;
  for(i = 0; i < total_size_x1; i++) {
/*
 * Coerce subsection of x (tmp_x) to double.
 */
    coerce_subset_input_double(x,tmp_x,l1,type_x,npts,0,NULL,NULL);

    NGCALLF(dxstnd,DXSTND)(tmp_x,&npts,&missing_dx.doubleval,opt,&ier);
    for(j = 0; j < npts; j++) {
      if(type_x != NCL_double) {
        ((float*)standardize)[l1+j] = (float)(tmp_x[j]);
      }
      else {
        ((double*)standardize)[l1+j] = tmp_x[j];
      }
    }

    l1 += npts;
    if (ier == 2) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"dim_standardize: The input array contains all missing values");
    }
  }
/*
 * Free temp array.
 */
  NclFree(tmp_x);
/*
 * Return float if input isn't double, otherwise return double.
 */
  if(type_x != NCL_double) {
/*
 * Return float values. 
 */
    return(NclReturnValue(standardize,ndims_x,dsizes_x,&missing_rx,
                          NCL_float,0));
  }
  else {
/*
 * Return double values. 
 */
    return(NclReturnValue(standardize,ndims_x,dsizes_x,&missing_dx,
                          NCL_double,0));
  }
}


NhlErrorTypes dim_rmsd_W( void )
{
/*
 * Input array variables
 */
  void *x, *y;
  double *tmp_x, *tmp_y;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  int ndims_y, dsizes_y[NCL_MAX_DIMENSIONS], has_missing_y;
  NclScalar missing_x, missing_dx, missing_rx;
  NclScalar missing_y, missing_dy, missing_ry;
  NclBasicDataTypes type_x, type_y;
/*
 * Output array variables
 */
  void *rmsd;
  double *tmp_rmsd;
  int dsizes_rmsd[NCL_MAX_DIMENSIONS];
  int nptused, ndims_rmsd;
  NclScalar missing_rmsd;
  NclBasicDataTypes type_rmsd;
/*
 * various
 */
  int i, total_elements, ier = 0, npts, index_xy;
/*
 * Retrieve parameters.
 */
  x = (void*)NclGetArgValue(
           0,
           2,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           2);
  y = (void*)NclGetArgValue(
           1,
           2,
           &ndims_y, 
           dsizes_y,
           &missing_y,
           &has_missing_y,
           &type_y,
           2);
/*
 * x and y must be the same size.
 */
  if(ndims_x != ndims_y) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmsd: x and y must have the same dimension sizes");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_x; i++ ) {
    if(dsizes_x[i] != dsizes_y[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmsd: x and y must have the same dimension sizes");
    return(NhlFATAL);
    }
  }

/*
 * Compute the total number of elements in output and input.
 */
  ndims_rmsd = max(ndims_x-1,1);
  dsizes_rmsd[0] = 1;

  total_elements = 1;
  for(i = 0; i < ndims_x-1; i++) {
    total_elements *= dsizes_x[i];
    dsizes_rmsd[i] = dsizes_x[i];
  }    

/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
  coerce_missing(type_y,has_missing_y,&missing_y,&missing_dy,&missing_ry);

/*
 * Allocate space for output array.
 */
  if(type_x != NCL_double && type_y != NCL_double) {
    type_rmsd    = NCL_float;
    missing_rmsd = missing_rx;
    rmsd         = (void*)calloc(total_elements,sizeof(float));
    tmp_rmsd     = (double*)calloc(1,sizeof(double));
    if(rmsd == NULL || tmp_rmsd == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmsd: Unable to allocate memory for output arrays");
      return(NhlFATAL);
    }
  }
  else {
    type_rmsd    = NCL_double;
    missing_rmsd = missing_dx;
    rmsd         = (void*)calloc(total_elements,sizeof(double));
    if (rmsd == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmsd: Unable to allocate memory for output array" );
      return(NhlFATAL);
    }
  }
/*
 * Allocate space for coercing input arrays to double, if necessary.
 */
  npts = dsizes_x[ndims_x-1];
  if(type_x != NCL_double) {
    tmp_x = (double*)calloc(npts,sizeof(double));
    if(tmp_x == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmsd: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  if(type_y != NCL_double) {
    tmp_y = (double*)calloc(npts,sizeof(double));
    if(tmp_y == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmsd: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }

/*
 * Call the f77 double version of 'rmsd' with the full argument list.
 */
  index_xy = 0;
  for(i = 0; i < total_elements; i++) {
    if(type_x != NCL_double) {
/*
 * Coerce subsection of x (tmp_x) to double.
 */
      coerce_subset_input_double(x,tmp_x,index_xy,type_x,npts,
                                 has_missing_x,&missing_x,&missing_dx);
    }
    else {
      tmp_x = &((double*)x)[index_xy];
    }

    if(type_y != NCL_double) {
/*
 * Coerce subsection of y (tmp_y) to double.
 */
      coerce_subset_input_double(y,tmp_y,index_xy,type_y,npts,
                                 has_missing_y,&missing_y,&missing_dy);
    }
    else {
      tmp_y = &((double*)y)[index_xy];
    }

    if(type_rmsd == NCL_double) {
      tmp_rmsd = &((double*)rmsd)[i];
    }

    NGCALLF(drmsd,DRMSD)(tmp_x,tmp_y,&npts,&missing_dx.doubleval,
                         &missing_dy.doubleval,tmp_rmsd,&nptused,&ier);

    if(type_rmsd != NCL_double) ((float*)rmsd)[i] = (float)(*tmp_rmsd);

    index_xy += npts;
    if (ier == 2) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"dim_rmsd: The input array contains all missing values");
    }
  }
/*
 * Free unneeded memory.
 */
  if(type_x    != NCL_double) NclFree(tmp_x);
  if(type_y    != NCL_double) NclFree(tmp_y);
  if(type_rmsd != NCL_double) NclFree(tmp_rmsd);

/*
 * Return.
 */
  if(has_missing_x || has_missing_y) {
    return(NclReturnValue(rmsd,ndims_rmsd,dsizes_rmsd,&missing_rmsd,
                          type_rmsd,0));
  }
  else {
    return(NclReturnValue(rmsd,ndims_rmsd,dsizes_rmsd,NULL,type_rmsd,0));
  }
}


NhlErrorTypes esacr_W( void )
{
/*
 * Input array variables
 */
  void *x;
  double *tmp_x;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  NclScalar missing_x, missing_dx, missing_rx;
  NclBasicDataTypes type_x;
  int *mxlag, mxlag1;
/*
 * Output array variables
 */
  void *acr;
  double *tmp_acr, *tmp_acv;
  int *dsizes_acr;
/*
 * various
 */
  int i, j, index_x, index_acr, total_size_x1, total_size_x, total_size_acr;
  int ier = 0, ier_count2 = 0, ier_count5 = 0, npts;
  double xmean, xvar;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  x = (void*)NclGetArgValue(
           0,
           2,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           2);
/*
 * Get second argument.
 */
  mxlag = (int*)NclGetArgValue(
           1,
           2,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           2);
/*
 * Calculate size of input/output values.
 */
  npts = dsizes_x[ndims_x-1];
  if(npts < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"esacr: npts must be >= 2");
    return(NhlFATAL);
  }
/*
 * Check mxlag
 */
  if( *mxlag < 0 || *mxlag > npts ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"esacr: mxlag must be between 0 and npts");
    return(NhlFATAL);
  }
  mxlag1 = *mxlag + 1;
/*
 * Compute the total number of elements in our x array.
 */
  total_size_x1 = 1;
  for(i = 0; i < ndims_x-1; i++) total_size_x1 *= dsizes_x[i];
  total_size_x = total_size_x1 * npts;
/* 
 * Get size of output variables.
 */
  total_size_acr = total_size_x1*mxlag1;
  dsizes_acr = (int*)calloc(ndims_x,sizeof(int));
  if (dsizes_acr == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"esacr: Unable to allocate space for output arrays" );
    return(NhlFATAL);
  }
  dsizes_acr[0] = 1;
  for( i = 0; i < ndims_x-1; i++ ) dsizes_acr[i] = dsizes_x[i];
  dsizes_acr[ndims_x-1] = mxlag1;
/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Coerce x to double if necessary.
 */
  if(type_x != NCL_double) {
    acr     = (void*)calloc(total_size_acr,sizeof(float));
    tmp_x   = (double*)calloc(npts,sizeof(double));
    tmp_acr = (double*)calloc(mxlag1,sizeof(double));
    tmp_acv = (double*)calloc(mxlag1,sizeof(double));
    if(acr == NULL || tmp_acr == NULL || tmp_acv == NULL || tmp_x == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esacr: Unable to allocate memory for in/output arrays");
      return(NhlFATAL);
    }
  }
  else {
    acr     = (void*)calloc(total_size_acr,sizeof(double));
    tmp_acv = (double*)calloc(mxlag1,sizeof(double));
    if(acr == NULL || tmp_acv == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esacr: Unable to allocate memory for output arrays");
      return(NhlFATAL);
    }
  }

/*
 * Call the f77 version of 'desauto' with the full argument list.
 */
  index_x = index_acr = 0;
    
  for(i = 0; i < total_size_x1; i++) {
    if(type_x != NCL_double) {
/*
 * Coerce subsection of x (tmp_x) to double.
 */
      coerce_subset_input_double(x,tmp_x,index_x,type_x,npts,0,NULL,NULL);
    }
    else {
      tmp_x   = &((double*)x)[index_x];
      tmp_acr = &((double*)acr)[index_acr];
    }

    xvar = xmean = missing_dx.doubleval;
    NGCALLF(desauto,DESAUTO)(tmp_x,&npts,&missing_dx.doubleval,&xmean,
                             &xvar,mxlag,tmp_acv,tmp_acr,&ier);

    if (ier == -2) ier_count2++;
    if (ier == -5) ier_count5++;

    if(type_x != NCL_double) {
      for(j = 0; j < mxlag1; j++) {
        ((float*)acr)[index_acr+j] = (float)(tmp_acr[j]);
      }
    }

    index_x   += npts;
    index_acr += mxlag1;
  }
/*
 * Check errors.
 */
  if (ier_count2) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"esacr: %i input array(s) contained all missing values",ier_count2);
  }
  if (ier_count5) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"esacr: the sample variance was zero for %i input array(s)",ier_count5);
  }
/*
 * free memory.
 */
  NclFree(tmp_acv);
  if(type_x != NCL_double) {
    NclFree(tmp_x);
    NclFree(tmp_acr);
  }
/*
 * Return values. 
 */
  if(type_x != NCL_double) {
/*
 * Return float values with missing value set.
 */
    return(NclReturnValue(acr,ndims_x,dsizes_acr,&missing_rx,NCL_float,0));
  }
  else {
/*
 * Return double values with missing value set.
 */
    return(NclReturnValue(acr,ndims_x,dsizes_acr,&missing_dx,NCL_double,0));
  }
}

NhlErrorTypes esacv_W( void )
{
/*
 * Input array variables
 */
  void *x;
  double *tmp_x;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  NclScalar missing_x, missing_dx, missing_rx;
  NclBasicDataTypes type_x;
  int *mxlag, mxlag1;
/*
 * Output array variables
 */
  void *acv;
  double *tmp_acr, *tmp_acv;
  int *dsizes_acv;
/*
 * various
 */
  int i, j, index_x, index_acv, total_size_x1, total_size_x, total_size_acv;
  int ier = 0, ier_count2 = 0, ier_count5 = 0, npts;
  double xmean, xvar;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  x = (void*)NclGetArgValue(
           0,
           2,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           2);
/*
 * Get second argument.
 */
  mxlag = (int*)NclGetArgValue(
           1,
           2,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           2);
/*
 * Calculate size of input/output values.
 */
  npts = dsizes_x[ndims_x-1];
  if(npts < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"esacv: npts must be >= 2");
    return(NhlFATAL);
  }
/*
 * Check mxlag
 */
  if( *mxlag < 0 || *mxlag > npts ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"esacv: mxlag must be between 0 and npts");
    return(NhlFATAL);
  }
  mxlag1 = *mxlag + 1;
/*
 * Compute the total number of elements in our x array.
 */
  total_size_x1 = 1;
  for(i = 0; i < ndims_x-1; i++) total_size_x1 *= dsizes_x[i];
  total_size_x = total_size_x1 * npts;
/* 
 * Get size of output variables.
 */
  total_size_acv = total_size_x1*mxlag1;
  dsizes_acv = (int*)calloc(ndims_x,sizeof(int));
  if (dsizes_acv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"esacv: Unable to allocate space for output arrays" );
    return(NhlFATAL);
  }
  dsizes_acv[0] = 1;
  for( i = 0; i < ndims_x-1; i++ ) dsizes_acv[i] = dsizes_x[i];
  dsizes_acv[ndims_x-1] = mxlag1;
/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Coerce x to double if necessary.
 */
  if(type_x != NCL_double) {
    acv     = (void*)calloc(total_size_acv,sizeof(float));
    tmp_x   = (double*)calloc(npts,sizeof(double));
    tmp_acr = (double*)calloc(mxlag1,sizeof(double));
    tmp_acv = (double*)calloc(mxlag1,sizeof(double));
    if(acv == NULL || tmp_acr == NULL || tmp_acv == NULL || tmp_x == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esacv: Unable to allocate memory for in/output arrays");
      return(NhlFATAL);
    }
  }
  else {
    acv     = (void*)calloc(total_size_acv,sizeof(double));
    tmp_acr = (double*)calloc(mxlag1,sizeof(double));
    if(acv == NULL || tmp_acr == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esacv: Unable to allocate memory for output arrays");
      return(NhlFATAL);
    }
  }

/*
 * Call the f77 version of 'desauto' with the full argument list.
 */
  index_x = index_acv = 0;

  for(i = 0; i < total_size_x1; i++) {
    if(type_x != NCL_double) {
/*
 * Coerce subsection of x (tmp_x) to double.
 */
      coerce_subset_input_double(x,tmp_x,index_x,type_x,npts,0,NULL,NULL);
    }
    else {
      tmp_x   = &((double*)x)[index_x];
      tmp_acv = &((double*)acv)[index_acv];
    }

    xvar = xmean = missing_dx.doubleval;
    NGCALLF(desauto,DESAUTO)(tmp_x,&npts,&missing_dx.doubleval,&xmean,
                             &xvar,mxlag,tmp_acv,tmp_acr,&ier);

    if (ier == -2) ier_count2++;
    if (ier == -5) ier_count5++;

    if(type_x != NCL_double) {
      for(j = 0; j < mxlag1; j++) {
        ((float*)acv)[index_acv+j] = (float)(tmp_acv[j]);
      }
    }

    index_x   += npts;
    index_acv += mxlag1;
  }
/*
 * Check errors.
 */
  if (ier_count2) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"esacv: %i input array(s) contained all missing values",ier_count2);
  }
  if (ier_count5) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"esacv: the sample variance was zero for %i input array(s)",ier_count5);
  }
/*
 * free memory.
 */
  NclFree(tmp_acr);
  if(type_x != NCL_double) {
    NclFree(tmp_x);
    NclFree(tmp_acv);
  }

/*
 * Return values. 
 */
  if(type_x != NCL_double) {
/*
 * Return float values with missing value set.
 */
    return(NclReturnValue(acv,ndims_x,dsizes_acv,&missing_rx,NCL_float,0));
  }
  else {
/*
 * Return double values with missing value set.
 */
    return(NclReturnValue(acv,ndims_x,dsizes_acv,&missing_dx,NCL_double,0));
  }
}


NhlErrorTypes esccr_W( void )
{
/*
 * Input array variables
 */
  void *x, *y;
  double *tmp_x, *tmp_y;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  int ndims_y, dsizes_y[NCL_MAX_DIMENSIONS], has_missing_y;
  NclScalar missing_x, missing_y;
  NclScalar missing_rx, missing_dx, missing_dy;
  NclBasicDataTypes type_x, type_y;
  int *mxlag, mxlag1;
/*
 * Output array variables
 */
  void *ccr;
  double *tmp_ccr, *tmp_ccv;
  int ndims_ccr, *dsizes_ccr;
  NclBasicDataTypes type_ccr;
/*
 * various
 */
  int i, j, k, index_x, index_y, index_ccr;
  int total_size_x1, total_size_x, total_size_y1, total_size_y;
  int total_size_ccr;
  int ier = 0, ier_count, npts, dimsizes_same;
  double xmean, xsd, ymean, ysd;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  x = (void*)NclGetArgValue(
           0,
           3,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           2);
  y = (void*)NclGetArgValue(
           1,
           3,
           &ndims_y, 
           dsizes_y,
           &missing_y,
           &has_missing_y,
           &type_y,
           2);
  mxlag = (int*)NclGetArgValue(
           2,
           3,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           2);
/*
 * The last dimension of x and y must be the same.
 */
  if( dsizes_x[ndims_x-1] != dsizes_y[ndims_y-1] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esccr: the rightmost dimension of x and y must be the same");
      return(NhlFATAL);
  }
/*
 * If all the dimensions of x and y are the same, then we don't treat
 * the dimensions differently:  i.e. if x is 64 x 128 x 21 and y is
 * 64 x 128 x 21, then what gets returned will be 64 x 128 x (mxlag+1),
 * and NOT 64 x 128 x 64 x 128 x (mxlag+1).
 */
  if(ndims_x == ndims_y) {
    dimsizes_same = 1;
    for(i = 0; i < ndims_x; i++) {
      if(dsizes_x[i] != dsizes_y[i]) {
        dimsizes_same = 0;
        break;
      }
    }
  }
  else {
    dimsizes_same = 0;
  }
/*      
 * Calculate size of input/output values.
 */
  npts = dsizes_x[ndims_x-1];
  if(npts < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"esccr: npts must be >= 2");
    return(NhlFATAL);
  }
/*
 * Check mxlag
 */
  if( *mxlag < 0 || *mxlag > npts ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"esccr: mxlag must be between 0 and npts");
    return(NhlFATAL);
  }
  mxlag1 = *mxlag + 1;
/*
 * Compute the total number of elements in our x and y arrays.
 */
  total_size_x1 = 1;
  for(i = 0; i < ndims_x-1; i++) total_size_x1 *= dsizes_x[i];
  total_size_x = total_size_x1 * npts;

  total_size_y1 = 1;
  for(i = 0; i < ndims_y-1; i++) total_size_y1 *= dsizes_y[i];
  total_size_y = total_size_y1 * npts;
/* 
 * Get size of output variables.
 */
  if(dimsizes_same) {
    ndims_ccr = ndims_x;
    total_size_ccr = total_size_x1 * mxlag1;
  }
  else {
    ndims_ccr = ndims_x + ndims_y - 1;
    total_size_ccr = total_size_x1 * total_size_y1 * mxlag1;
  }
  dsizes_ccr = (int*)calloc(ndims_ccr,sizeof(int));
  if (dsizes_ccr == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"esccr: Unable to allocate space for output arrays" );
    return(NhlFATAL);
  }

/*
 * Calculate dimensions for ccr. If dimension sizes are *not* the
 * same, and input is:
 *
 *    x(npts)     and y(npts)   ==> ccr(mxlag+1)
 *    x(m,n,npts) and y(k,npts) ==> ccr(m,n,k,mxlag+1)
 *    x(npts)     and y(k,npts) ==> ccr(k,mxlag+1)
 *
 *  etc.
 * 
 * If dimension sizes *are* the same, then you'll get the following:
 *
 *    x(m,n,npts) and y(m,n,npts) ==> ccr(m,n,mxlag+1)
 *    x(k,m,n,npts) and y(k,m,n,npts) ==> ccr(k,m,n,mxlag+1)
 *
 */
  if(dimsizes_same) {
    for( i = 0; i < ndims_x-1; i++ ) dsizes_ccr[i] = dsizes_x[i];
  }
  else {
    for( i = 0; i < ndims_x-1; i++ ) dsizes_ccr[i] = dsizes_x[i];
    for( i = 0; i < ndims_y-1; i++ ) dsizes_ccr[ndims_x-1+i] = dsizes_y[i];
  }

  dsizes_ccr[ndims_ccr-1] = mxlag1;
/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
  coerce_missing(type_y,has_missing_y,&missing_y,&missing_dy,NULL);
/*
 * Coerce x to double if necessary.
 */
  if(type_x != NCL_double) {
    tmp_x   = (double*)calloc(npts,sizeof(double));
    if(tmp_x == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esccr: Unable to allocate memory for inputput array");
      return(NhlFATAL);
    }
  }
  if(type_y != NCL_double) {
    tmp_y   = (double*)calloc(npts,sizeof(double));
    if(tmp_y == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esccr: Unable to allocate memory for inputput array");
      return(NhlFATAL);
    }
  }
  if(type_x != NCL_double && type_y != NCL_double) {
    type_ccr = NCL_float;
    ccr     = (void*)calloc(total_size_ccr,sizeof(float));
    tmp_ccr = (double*)calloc(mxlag1,sizeof(double));
    tmp_ccv = (double*)calloc(mxlag1,sizeof(double));
    if(ccr == NULL || tmp_ccr == NULL || tmp_ccv == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esccr: Unable to allocate memory for output arrays");
      return(NhlFATAL);
    }
  }
  else {
    type_ccr = NCL_double;
    ccr     = (void*)calloc(total_size_ccr,sizeof(double));
    tmp_ccv = (double*)calloc(mxlag1,sizeof(double));
    if(ccr == NULL || tmp_ccv == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esccr: Unable to allocate memory for output arrays");
      return(NhlFATAL);
    }
  }
/*
 * Call the f77 version of 'descros' with the full argument list.
 */
  if(dimsizes_same) {
    index_x = index_ccr = 0;
    ier_count = 0;
    for(i = 1; i <= total_size_x1; i++) {
      if(type_x != NCL_double) {
/*
 * Coerce subsection of x (tmp_x) to double.
 */
        coerce_subset_input_double(x,tmp_x,index_x,type_x,npts,0,NULL,NULL);
      }
      else {
        tmp_x   = &((double*)x)[index_x];
      }
      if(type_y != NCL_double) {
/*
 * Coerce subsection of y (tmp_y) to double.
 */
        coerce_subset_input_double(y,tmp_y,index_x,type_y,npts,0,NULL,NULL);
      }
      else {
        tmp_y   = &((double*)y)[index_x];
      }

      if(type_ccr == NCL_double) {
        tmp_ccr = &((double*)ccr)[index_ccr];
      }
      xmean = xsd = missing_dx.doubleval;
      ymean = ysd = missing_dy.doubleval;
      NGCALLF(descros,DESCROS)(tmp_x,tmp_y,&npts,&missing_dx.doubleval,
                               &missing_dy.doubleval,&xmean,&ymean,&xsd,
                               &ysd,mxlag,tmp_ccv,tmp_ccr,&ier);
      if(type_ccr != NCL_double) {
        for(j = 0; j < mxlag1; j++) {
          ((float*)ccr)[index_ccr+j] = (float)(tmp_ccr[j]);
        }
      }

      index_ccr += mxlag1;
      index_x   += npts;
      if(ier < 0) ier_count++;
    }
    if(ier_count > 0) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"esccr: Non-fatal conditions encountered: all missing or constant values");
    }
  }
  else {
    index_x = index_ccr = 0;
    ier_count = 0;
    for(i = 1; i <= total_size_x1; i++) {
      if(type_x != NCL_double) {
/*
 * Coerce subsection of x (tmp_x) to double.
 */
        coerce_subset_input_double(x,tmp_x,index_x,type_x,npts,0,NULL,NULL);
      }
      else {
        tmp_x   = &((double*)x)[index_x];
      }
      index_y = 0;
      for(j = 1; j <= total_size_y1; j++) {
        if(type_y != NCL_double) {
/*
 * Coerce subsection of y (tmp_y) to double.
 */
          coerce_subset_input_double(y,tmp_y,index_y,type_y,npts,0,NULL,NULL);
        }
        else {
          tmp_y   = &((double*)y)[index_y];
        }

        if(type_ccr == NCL_double) {
          tmp_ccr = &((double*)ccr)[index_ccr];
        }
        xmean = xsd = missing_dx.doubleval;
        ymean = ysd = missing_dy.doubleval;
        NGCALLF(descros,DESCROS)(tmp_x,tmp_y,&npts,&missing_dx.doubleval,
                                 &missing_dy.doubleval,&xmean,&ymean,&xsd,
                                 &ysd,mxlag,tmp_ccv,tmp_ccr,&ier);
        if(type_ccr != NCL_double) {
          for(k = 0; k < mxlag1; k++) {
            ((float*)ccr)[index_ccr+k] = (float)(tmp_ccr[k]);
          }
        }
        index_y   += npts;
        index_ccr += mxlag1;
        if(ier < 0) ier_count++;
      }
      index_x += npts;
      if(ier_count > 0) {
        NhlPError(NhlWARNING,NhlEUNKNOWN,"esccr: Non-fatal conditions encountered: all missing or constant values");
      }
    }
  }
/*
 * free memory.
 */
  NclFree(tmp_ccv);
  if(type_x   != NCL_double) NclFree(tmp_x);
  if(type_y   != NCL_double) NclFree(tmp_y);
  if(type_ccr != NCL_double) NclFree(tmp_ccr);

/*
 * Return values. 
 */
  if(type_ccr != NCL_double) {
/*
 * Return float values with missing value set.
 */
    return(NclReturnValue(ccr,ndims_ccr,dsizes_ccr,&missing_rx,
                          NCL_float,0));
  }
  else {
/*
 * Return double values with missing value set.
 */
    return(NclReturnValue(ccr,ndims_ccr,dsizes_ccr,&missing_dx,
                          NCL_double,0));
  }
}


NhlErrorTypes esccv_W( void )
{
/*
 * Input array variables
 */
  void *x, *y;
  double *tmp_x, *tmp_y;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  int ndims_y, dsizes_y[NCL_MAX_DIMENSIONS], has_missing_y;
  NclScalar missing_x, missing_y;
  NclScalar missing_rx, missing_dx, missing_dy;
  NclBasicDataTypes type_x, type_y;
  int *mxlag, mxlag1;
/*
 * Output array variables
 */
  void *ccv;
  double *tmp_ccr, *tmp_ccv;
  int ndims_ccv, *dsizes_ccv;
  NclBasicDataTypes type_ccv;
/*
 * various
 */
  int i, j, k, index_x, index_y, index_ccv;
  int total_size_x1, total_size_x, total_size_y1, total_size_y;
  int total_size_ccv;
  int ier = 0, ier_count, npts, dimsizes_same;
  double xmean, xsd, ymean, ysd;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  x = (void*)NclGetArgValue(
           0,
           3,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           2);
  y = (void*)NclGetArgValue(
           1,
           3,
           &ndims_y, 
           dsizes_y,
           &missing_y,
           &has_missing_y,
           &type_y,
           2);
  mxlag = (int*)NclGetArgValue(
           2,
           3,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           2);
/*
 * The last dimension of x and y must be the same.
 */
  if( dsizes_x[ndims_x-1] != dsizes_y[ndims_y-1] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esccv: the rightmost dimension of x and y must be the same");
      return(NhlFATAL);
  }
/*
 * If all the dimensions of x and y are the same, then we don't treat
 * the dimensions differently:  i.e. if x is 64 x 128 x 21 and y is
 * 64 x 128 x 21, then what gets returned will be 64 x 128 x (mxlag+1),
 * and NOT 64 x 128 x 64 x 128 x (mxlag+1).
 */
  if(ndims_x == ndims_y) {
    dimsizes_same = 1;
    for(i = 0; i < ndims_x; i++) {
      if(dsizes_x[i] != dsizes_y[i]) {
        dimsizes_same = 0;
        break;
      }
    }
  }
  else {
    dimsizes_same = 0;
  }
/*      
 * Check rightmost dimension.
 */
  npts = dsizes_x[ndims_x-1];
  if(npts < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"esccv: npts must be >= 2");
    return(NhlFATAL);
  }
/*
 * Check mxlag
 */
  if( *mxlag < 0 || *mxlag > npts ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"esccv: mxlag must be between 0 and npts");
    return(NhlFATAL);
  }
  mxlag1 = *mxlag + 1;
/*
 * Compute the total number of elements in our x and y arrays.
 */
  total_size_x1 = 1;
  for(i = 0; i < ndims_x-1; i++) total_size_x1 *= dsizes_x[i];
  total_size_x = total_size_x1 * npts;

  total_size_y1 = 1;
  for(i = 0; i < ndims_y-1; i++) total_size_y1 *= dsizes_y[i];
  total_size_y = total_size_y1 * npts;
/* 
 * Get size of output variables.
 */
  if(dimsizes_same) {
    ndims_ccv = ndims_x;
    total_size_ccv = total_size_x1 * mxlag1;
  }
  else {
    ndims_ccv = ndims_x + ndims_y - 1;
    total_size_ccv = total_size_x1 * total_size_y1 * mxlag1;
  }
  dsizes_ccv = (int*)calloc(ndims_ccv,sizeof(int));
  if (dsizes_ccv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"esccv: Unable to allocate space for output arrays" );
    return(NhlFATAL);
  }
/*
 * Calculate dimensions for ccv. If dimension sizes are *not* the
 * same, and input is:
 *
 *    x(npts)     and y(npts)   ==> ccv(mxlag+1)
 *    x(m,n,npts) and y(k,npts) ==> ccv(m,n,k,mxlag+1)
 *    x(npts)     and y(k,npts) ==> ccv(k,mxlag+1)
 *
 *  etc.
 * 
 * If dimension sizes *are* the same, then you'll get the following:
 *
 *    x(m,n,npts) and y(m,n,npts) ==> ccv(m,n,mxlag+1)
 *    x(k,m,n,npts) and y(k,m,n,npts) ==> ccv(k,m,n,mxlag+1)
 *
 */
  if(dimsizes_same) {
    for( i = 0; i < ndims_x-1; i++ ) dsizes_ccv[i] = dsizes_x[i];
  }
  else {
    for( i = 0; i < ndims_x-1; i++ ) dsizes_ccv[i] = dsizes_x[i];
    for( i = 0; i < ndims_y-1; i++ ) dsizes_ccv[ndims_x-1+i] = dsizes_y[i];
  }

  dsizes_ccv[ndims_ccv-1] = mxlag1;
/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
  coerce_missing(type_y,has_missing_y,&missing_y,&missing_dy,NULL);
/*
 * Coerce x to double if necessary.
 */
  if(type_x != NCL_double) {
    tmp_x   = (double*)calloc(npts,sizeof(double));
    if(tmp_x == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esccv: Unable to allocate memory for inputput array");
      return(NhlFATAL);
    }
  }
  if(type_y != NCL_double) {
    tmp_y   = (double*)calloc(npts,sizeof(double));
    if(tmp_y == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esccv: Unable to allocate memory for inputput array");
      return(NhlFATAL);
    }
  }
  if(type_x != NCL_double && type_y != NCL_double) {
    type_ccv = NCL_float;
    ccv     = (void*)calloc(total_size_ccv,sizeof(float));
    tmp_ccr = (double*)calloc(mxlag1,sizeof(double));
    tmp_ccv = (double*)calloc(mxlag1,sizeof(double));
    if(ccv == NULL || tmp_ccr == NULL || tmp_ccv == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esccv: Unable to allocate memory for output arrays");
      return(NhlFATAL);
    }
  }
  else {
    type_ccv = NCL_double;
    ccv     = (void*)calloc(total_size_ccv,sizeof(double));
    tmp_ccr = (double*)calloc(mxlag1,sizeof(double));
    if(ccv == NULL || tmp_ccr == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esccv: Unable to allocate memory for output arrays");
      return(NhlFATAL);
    }
  }
/*
 * Call the f77 version of 'descros' with the full argument list.
 */
  if(dimsizes_same) {
    index_x = index_ccv = 0;
    ier_count = 0;
    for(i = 1; i <= total_size_x1; i++) {
      if(type_x != NCL_double) {
/*
 * Coerce subsection of x (tmp_x) to double.
 */
        coerce_subset_input_double(x,tmp_x,index_x,type_x,npts,0,NULL,NULL);
      }
      else {
        tmp_x   = &((double*)x)[index_x];
      }
      if(type_y != NCL_double) {
/*
 * Coerce subsection of y (tmp_y) to double.
 */
        coerce_subset_input_double(y,tmp_y,index_x,type_y,npts,0,NULL,NULL);
      }
      else {
        tmp_y   = &((double*)y)[index_x];
      }

      if(type_ccv == NCL_double) {
        tmp_ccv = &((double*)ccv)[index_ccv];
      }

      xmean = xsd = missing_dx.doubleval;
      ymean = ysd = missing_dy.doubleval;
      NGCALLF(descros,DESCROS)(tmp_x,tmp_y,&npts,&missing_dx.doubleval,
                               &missing_dy.doubleval,&xmean,&ymean,&xsd,
                               &ysd,mxlag,tmp_ccv,tmp_ccr,&ier);
      if(type_ccv != NCL_double) {
        for(j = 0; j < mxlag1; j++) {
          ((float*)ccv)[index_ccv+j] = (float)(tmp_ccv[j]);
        }
      }

      index_ccv += mxlag1;
      index_x   += npts;
      if(ier < 0) ier_count++;
    }
    if(ier_count > 0) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"esccv: Non-fatal conditions encountered: all missing or constant values");
    }
  }
  else {
    index_x = index_ccv = 0;
    ier_count = 0;
    for(i = 1; i <= total_size_x1; i++) {
      if(type_x != NCL_double) {
/*
 * Coerce subsection of x (tmp_x) to double.
 */
        coerce_subset_input_double(x,tmp_x,index_x,type_x,npts,0,NULL,NULL);
      }
      else {
        tmp_x   = &((double*)x)[index_x];
      }
      index_y = 0;
      for(j = 1; j <= total_size_y1; j++) {
        if(type_y != NCL_double) {
/*
 * Coerce subsection of y (tmp_y) to double.
 */
          coerce_subset_input_double(y,tmp_y,index_y,type_y,npts,0,NULL,NULL);
        }
        else {
          tmp_y   = &((double*)y)[index_y];
        }

        if(type_ccv == NCL_double) {
          tmp_ccv = &((double*)ccv)[index_ccv];
        }

        xmean = xsd = missing_dx.doubleval;
        ymean = ysd = missing_dy.doubleval;
        NGCALLF(descros,DESCROS)(tmp_x,tmp_y,&npts,&missing_dx.doubleval,
                                 &missing_dy.doubleval,&xmean,&ymean,&xsd,
                                 &ysd,mxlag,tmp_ccv,tmp_ccr,&ier);
        if(type_ccv != NCL_double) {
          for(k = 0; k < mxlag1; k++) {
            ((float*)ccv)[index_ccv+k] = (float)(tmp_ccv[k]);
          }
        }
        index_y   += npts;
        index_ccv += mxlag1;
        if(ier < 0) ier_count++;
      }
      index_x += npts;
      if(ier_count > 0) {
        NhlPError(NhlWARNING,NhlEUNKNOWN,"esccv: Non-fatal conditions encountered: all missing or constant values");
      }
    }
  }
/*
 * free memory.
 */
  NclFree(tmp_ccr);
  if(type_x   != NCL_double) NclFree(tmp_x);
  if(type_y   != NCL_double) NclFree(tmp_y);
  if(type_ccv != NCL_double) NclFree(tmp_ccv);

/*
 * Return values. 
 */
  if(type_ccv != NCL_double) {
/*
 * Return float values with missing value set.
 */
    return(NclReturnValue(ccv,ndims_ccv,dsizes_ccv,&missing_rx,
                          NCL_float,0));
  }
  else {
/*
 * Return double values with missing value set.
 */
    return(NclReturnValue(ccv,ndims_ccv,dsizes_ccv,&missing_dx,
                          NCL_double,0));
  }
}

NhlErrorTypes dim_stat4_W( void )
{
/*
 * Input array variables
 */
  void *x;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  NclScalar missing_x, missing_dx, missing_rx;
  NclBasicDataTypes type_x;
/*
 * Output array variables
 */
  void *stat;
  double dxmean, dxvar, dxskew, dxkurt;
  int nptused, *dsizes_out;
/*
 * various
 */
  int i, total_leftmost, ier = 0, index_x, npts;
  double xsd, *tmp_x;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  x = (void*)NclGetArgValue(
           0,
           1,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           2);
/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Compute the total number of lefmost and rightmost elements in our x array.
 */
  npts = dsizes_x[ndims_x-1];
  total_leftmost = 1;
  for(i = 0; i < ndims_x-1; i++) total_leftmost *= dsizes_x[i];
/*
 * Calculate size of output arrays.
 */
  dsizes_out = (int*)calloc(ndims_x,sizeof(int));
  if(type_x == NCL_double) {
    stat = (void*)calloc(4*total_leftmost,sizeof(double));
    if(dsizes_out == NULL || stat == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_stat4: Unable to allocate memory for output arrays");
      return(NhlFATAL);
    }
  }
  else {
    tmp_x = (double*)calloc(npts,sizeof(double));
    stat  = (void*)calloc(4*total_leftmost,sizeof(float));
    if(dsizes_out == NULL || stat == NULL || tmp_x == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_stat4: Unable to allocate memory for output arrays");
      return(NhlFATAL);
    }
  }
/*
 * The output array will be 4 x (leftmost-1 dimensions of x)
 */
  dsizes_out[0] = 4;
  for(i = 1; i <= ndims_x-1; i++ ) dsizes_out[i] = dsizes_x[i-1];
/*
 * Call the f77 version of 'dim_stat4' with the full argument list.
 */
  index_x = 0;
  for(i = 0; i < total_leftmost; i++) {
    if(type_x != NCL_double) {
/*
 * Coerce subsection of x (tmp_x) to double.
 */
      coerce_subset_input_double(x,tmp_x,index_x,type_x,npts,0,NULL,NULL);
    }
    else {
      tmp_x   = &((double*)x)[index_x];
    }
    NGCALLF(dstat4,DSTAT4)(tmp_x,&npts,&missing_dx.doubleval,&dxmean,&dxvar,
                           &xsd,&dxskew,&dxkurt,&nptused,&ier);
    if (ier == 2) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_stat4: The input array contains all missing values");
      return(NhlFATAL);
    }
    if(type_x != NCL_double) {
      ((float*)stat)[i]                  = (float)dxmean;
      ((float*)stat)[i+total_leftmost]   = (float)dxvar;
      ((float*)stat)[i+2*total_leftmost] = (float)dxskew;
      ((float*)stat)[i+3*total_leftmost] = (float)dxkurt;
    }
    else {
      ((double*)stat)[i]                  = dxmean;
      ((double*)stat)[i+total_leftmost]   = dxvar;
      ((double*)stat)[i+2*total_leftmost] = dxskew;
      ((double*)stat)[i+3*total_leftmost] = dxkurt;
    }
    index_x    += npts;
  }
/*
 * free memory.
 */
  if(type_x != NCL_double) NclFree(tmp_x);

/*
 * Return values. 
 */
  if(type_x != NCL_double) {
/*
 * Return float values with missing value set.
 */
    return(NclReturnValue(stat,ndims_x,dsizes_out,&missing_rx,NCL_float,0));
  }
  else {
/*
 * Return double values with missing value set.
 */
    return(NclReturnValue(stat,ndims_x,dsizes_out,&missing_dx,NCL_double,0));
  }
}


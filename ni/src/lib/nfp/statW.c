#include <stdio.h>
#include <stdlib.h>
/*
* The following are the required NCAR Graphics include files.
* They should be located in ${NCARG_ROOT}/include
*/
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/ncl/defs.h>
#include "Symbol.h"
#include "NclMdInc.h"
#include "Machine.h"
#include <ncarg/ncl/NclVar.h>
#include "DataSupport.h"
#include "VarSupport.h"
#include "NclCoordVar.h"
#include <ncarg/ncl/NclCallBacksI.h>
#include <ncarg/ncl/NclDataDefs.h>
#include <ncarg/ncl/NclBuiltInSupport.h>
#include <ncarg/gks.h>
#include <math.h>

#define max(x,y)  ((x) > (y) ? (x) : (y))

extern void NGCALLF(stat2,STAT2)(float*, int*, float*, float*, float*, 
                                 float*, int*, int*); 

extern void NGCALLF(stat4,STAT4)(float*, int*, float*, float*, float*, 
                                 float*, float*, float*, int*, int*); 

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
    NhlPError(NhlFATAL,NhlEUNKNOWN,"stat_trim: Unable to allocate space for work array\n" );
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
  free(work);
  return(NhlNOERROR);
}


NhlErrorTypes stat4_W( void )
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
    NhlPError(NhlFATAL,NhlEUNKNOWN,"stat_medrng: Unable to allocate space for work array\n" );
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
  free(work);
  return(NhlNOERROR);
}


NhlErrorTypes dim_median_W( void )
{
/*
 * Input array variables
 */
  NclStackEntry data;
  NclMultiDValData tmp_md = NULL, tmp1_md;
  NclScalar missing;
/*
 * Output array variables
 */
  double *xmedian, *xrange, *xmrange, xmsg;
  float *xrmedian;
  int dsizes_median[NCL_MAX_DIMENSIONS];
  int *nptused;
  int ndims_median;
/*
 * various
 */
  int i, l1, l2, total_elements, ier = 0, npts, ndims;
  double *work;
/*
 * Retrieve parameter.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  data = _NclGetArg(0,1,DONT_CARE);
  switch(data.kind) {
  case NclStk_VAR:
    tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
    break;
  case NclStk_VAL:
    tmp_md = (NclMultiDValData)data.u.data_obj;
    break;
  }

/*
 * Compute the total number of elements minus the last dimension.
 */
  ndims = tmp_md->multidval.n_dims;
  ndims_median = max(ndims-1,1);
  dsizes_median[0] = 1;
  total_elements = 1;
  for(i = 0; i < ndims-1; i++) {
    total_elements *= tmp_md->multidval.dim_sizes[i];
    dsizes_median[i] = tmp_md->multidval.dim_sizes[i];
  }    
/*
 * Coerce input to double if necessary.
 */
  if(tmp_md->multidval.data_type != NCL_double) {
    tmp1_md = _NclCoerceData(tmp_md,Ncl_Typedouble,NULL);
    if(tmp1_md == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_median: Unable to convert input to double");
      return(NhlFATAL);
    }   
/*
 * Get the missing value if there is one.  Otherwise, get the default 
 * missing value.
 */
    if(tmp_md->multidval.missing_value.has_missing) {
      if(tmp_md->multidval.data_type == NCL_float) {
    missing.floatval = tmp_md->multidval.missing_value.value.floatval;
      }
      else {
    missing.floatval = (float)tmp_md->multidval.missing_value.value.intval;
      }
    }
    else {
      missing.floatval = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
    }
    xmsg = (double)missing.floatval;
  }
  else {
/*
 * Input is already double, so no coercion needs to take place.
 */
    tmp1_md = tmp_md;
/*
 * Get the missing value if there is one.  Otherwise, get the default 
 * missing value.
 */
    if(tmp_md->multidval.missing_value.has_missing) {
      missing.doubleval = tmp_md->multidval.missing_value.value.doubleval;
    }
    else {
      missing.doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;

    }
    xmsg = (double)missing.doubleval;
  }

/*
 * Allocate space for output arrays.
 */
  xmedian = (double*)NclMalloc(total_elements*sizeof(double));
  xrange  = (double*)NclMalloc(total_elements*sizeof(double));
  xmrange = (double*)NclMalloc(total_elements*sizeof(double));
  nptused = (int*)NclMalloc(total_elements*sizeof(int));
  if (xmedian == NULL || xrange == NULL || xmrange == NULL || 
      nptused == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_median: Unable to allocate space for output array\n" );
    return(NhlFATAL);
  }
/*
 * Allocate space for work array.
 */
  npts = tmp_md->multidval.dim_sizes[ndims-1];
  work = (double*)calloc(npts,sizeof(double));
  if (work == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_median: Unable to allocate space for work array\n" );
    return(NhlFATAL);
  }
/*
 * Call the f77 double version of 'medmrng' with the full argument list.
 */
  l1 = l2 = 0;
  for(i = 1; i <= total_elements; i++) {
    NGCALLF(dmedmrng,DMEDMRNG)(&((double*)tmp1_md->multidval.val)[l1],work,
                   &npts,&xmsg,&xmedian[l2],&xmrange[l2],
                   &xrange[l2],&nptused[l2],&ier);
    l1 += npts;
    l2++;
    if (ier == 2) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"dim_median: The input array contains all missing values");
    }
  }
/*
 * Free unneeded memory.
 */
  free(xrange);
  free(xmrange);
  free(nptused);
  free(work);

/*
 * Return float if input isn't double, otherwise return double.
 */
  if(tmp_md->multidval.data_type != NCL_double) {
    xrmedian = (float*)NclMalloc(total_elements*sizeof(float));
    if (xrmedian == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_median: Unable to allocate space for output array\n" );
      return(NhlFATAL);
    }
/*
 * Copy double values to float values.
 */
    for( i = 0; i < total_elements; i++ ) xrmedian[i] = (float)xmedian[i];

    if(tmp_md->multidval.missing_value.has_missing) {
      return(NclReturnValue((void*)xrmedian,ndims_median,dsizes_median,
                &missing,NCL_float,0));
  }
    else {
      return(NclReturnValue((void*)xrmedian,ndims_median,dsizes_median,
                NULL,NCL_float,0));
    }
  }
  else {
    if(tmp_md->multidval.missing_value.has_missing) {
      return(NclReturnValue((void*)xmedian,ndims_median,dsizes_median,
                &missing,NCL_double,0));
  }
    else {
      return(NclReturnValue((void*)xmedian,ndims_median,dsizes_median,NULL,
                NCL_double,0));
    }
  }
}


NhlErrorTypes dim_rmvmean_W( void )
{
/*
 * Input array variables
 */
  NclStackEntry data;
  NclMultiDValData tmp_md = NULL;
  double *x;
  NclScalar missing;
/*
 * Output array variables
 */
  float *xrmvmean;
  double xmsg;
/*
 * various
 */
  int i, l1, total_elements, ier = 0, npts, ndims;
/*
 * Retrieve parameter.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  data = _NclGetArg(0,1,DONT_CARE);
  switch(data.kind) {
  case NclStk_VAR:
    tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
    break;
  case NclStk_VAL:
    tmp_md = (NclMultiDValData)data.u.data_obj;
    break;
  }

/*
 * Compute the total number of elements minus the last dimension.
 */
  ndims = tmp_md->multidval.n_dims;
  total_elements = 1;
  for(i = 0; i < ndims-1; i++) {
    total_elements *= tmp_md->multidval.dim_sizes[i];
  }    
  npts = tmp_md->multidval.dim_sizes[ndims-1];
/*
 * Copy input to double. Since we need to make an extra copy of the input
 * array to keep the input array from being changed, we do the coercion
 * no matter what.
 */
  x = (double*)NclMalloc(total_elements*npts*sizeof(double));
  if(x == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmvmean: Unable to convert input to double");
    return(NhlFATAL);
  } 
  if( tmp_md->multidval.data_type == NCL_double ) {
    for(i = 0; i < total_elements*npts; i++) {
      x[i] = ((double *)tmp_md->multidval.val)[i];
    }
  }
  else if( tmp_md->multidval.data_type == NCL_float ) {
    for(i = 0; i < total_elements*npts; i++) {
      x[i] = (double)((float *)tmp_md->multidval.val)[i];
    }
  }
  else {
    for(i = 0; i < total_elements*npts; i++) {
      x[i] = (double)((int *)tmp_md->multidval.val)[i];
    }
  }
/*
 * Get the missing value if there is one.  Otherwise, get the default 
 * missing value.
 */
  if(tmp_md->multidval.missing_value.has_missing) {
    if(tmp_md->multidval.data_type == NCL_double) {
      missing.doubleval = tmp_md->multidval.missing_value.value.doubleval;
      xmsg = missing.doubleval;
    }
    else if(tmp_md->multidval.data_type == NCL_float) {
      missing.floatval = tmp_md->multidval.missing_value.value.floatval;
      xmsg = (double)missing.floatval;
    }
    else {
      missing.floatval = (float)tmp_md->multidval.missing_value.value.intval;
      xmsg = (double)missing.floatval;
    }
  }
  else {
/*
 * Get the missing value if there is one.  Otherwise, get the default 
 * missing value.
 */
    if(tmp_md->multidval.data_type == NCL_double) {
      missing.doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
      xmsg = missing.doubleval;
    }
    else {
      missing.floatval = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
      xmsg = (double)missing.floatval;
    }
  }

/*
 * Call the f77 double version of 'rmvmean' with the full argument list.
 */
  l1 = 0;
  for(i = 1; i <= total_elements; i++) {
    NGCALLF(drmvmean,DRMVMEAN)(&x[l1],&npts,&xmsg,&ier);
    l1 += npts;
    if (ier == 2) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"dim_rmvmean: The input array contains all missing values");
    }
  }
/*
 * Return float if input isn't double, otherwise return double.
 */
  if(tmp_md->multidval.data_type != NCL_double) {
    xrmvmean = (float*)NclMalloc(total_elements*npts*sizeof(float));
    if (xrmvmean == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmvmean: Unable to allocate space for output array\n" );
      return(NhlFATAL);
    }
/*
 * Copy double values to float values.
 */
    for( i = 0; i < total_elements*npts; i++ ) {
      xrmvmean[i] = (float)x[i];
    }

    free(x);
    if(tmp_md->multidval.missing_value.has_missing) {
      return(NclReturnValue((void*)xrmvmean,ndims,
                tmp_md->multidval.dim_sizes,&missing,
                NCL_float,0));
    }
    else {
      return(NclReturnValue((void*)xrmvmean,ndims,
                tmp_md->multidval.dim_sizes,NULL,NCL_float,0));
    }
  }
  else {
    if(tmp_md->multidval.missing_value.has_missing) {
      return(NclReturnValue((void*)x,ndims,tmp_md->multidval.dim_sizes,
                &missing,NCL_double,0));
    }
    else {
      return(NclReturnValue((void*)x,ndims,tmp_md->multidval.dim_sizes,
                NULL,NCL_double,0));
    }
  }
}


NhlErrorTypes dim_rmvmed_W( void )
{
/*
 * Input array variables
 */
  NclStackEntry data;
  NclMultiDValData tmp_md = NULL;
  double *x;
  NclScalar missing;
/*
 * Output array variables
 */
  float *xrmvmedian;
  double *work, xmsg;
/*
 * various
 */
  int i, l1, total_elements, ier = 0, npts, ndims;
/*
 * Retrieve parameter.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  data = _NclGetArg(0,1,DONT_CARE);
  switch(data.kind) {
  case NclStk_VAR:
    tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
    break;
  case NclStk_VAL:
    tmp_md = (NclMultiDValData)data.u.data_obj;
    break;
  }

/*
 * Compute the total number of elements minus the last dimension.
 */
  ndims = tmp_md->multidval.n_dims;
  total_elements = 1;
  for(i = 0; i < ndims-1; i++) {
    total_elements *= tmp_md->multidval.dim_sizes[i];
  }    
  npts = tmp_md->multidval.dim_sizes[ndims-1];
/*
 * Copy input to double. Since we need to make an extra copy of the input
 * array to keep the input array from being changed, we do the coercion
 * no matter what.
 */
  x = (double*)NclMalloc(total_elements*npts*sizeof(double));
  if(x == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmvmed: Unable to convert input to double");
    return(NhlFATAL);
  }
  if( tmp_md->multidval.data_type == NCL_double ) {
    for(i = 0; i < total_elements*npts; i++) {
      x[i] = ((double *)tmp_md->multidval.val)[i];
    }
  }
  else if( tmp_md->multidval.data_type == NCL_float ) {
    for(i = 0; i < total_elements*npts; i++) {
      x[i] = (double)((float *)tmp_md->multidval.val)[i];
    }
  }
  else {
    for(i = 0; i < total_elements*npts; i++) {
      x[i] = (double)((int *)tmp_md->multidval.val)[i];
    }
  }
/*
 * Get the missing value if there is one.  Otherwise, get the default 
 * missing value.
 */
  if(tmp_md->multidval.missing_value.has_missing) {
    if(tmp_md->multidval.data_type == NCL_double) {
      missing.doubleval = tmp_md->multidval.missing_value.value.doubleval;
      xmsg = missing.doubleval;
    }
    else if(tmp_md->multidval.data_type == NCL_float) {
      missing.floatval = tmp_md->multidval.missing_value.value.floatval;
      xmsg = (double)missing.floatval;
    }
    else {
      missing.floatval = (float)tmp_md->multidval.missing_value.value.intval;
      xmsg = (double)missing.floatval;
    }
  }
  else {
/*
 * Get the missing value if there is one.  Otherwise, get the default 
 * missing value.
 */
    if(tmp_md->multidval.data_type == NCL_double) {
      missing.doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
      xmsg = missing.doubleval;
    }
    else {
      missing.floatval = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
      xmsg = (double)missing.floatval;
    }
  }
/*
 * Allocate space for work array.
 */
  work = (double*)calloc(npts,sizeof(double));
  if (work == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmvmed: Unable to allocate space for work array\n" );
    return(NhlFATAL);
  }

/*
 * Call the f77 double version of 'rmvmed' with the full argument list.
 */
  l1 = 0;
  for(i = 1; i <= total_elements; i++) {
    NGCALLF(drmvmed,DRMVMED)(&x[l1],work,&npts,&xmsg,&ier);
    l1 += npts;
    if (ier == 2) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"dim_rmvmed: The input array contains all missing values");
    }
  }
  free(work);
/*
 * Return float if input isn't double, otherwise return double.
 */
  if(tmp_md->multidval.data_type != NCL_double) {
    xrmvmedian = (float*)NclMalloc(total_elements*npts*sizeof(float));
    if (xrmvmedian == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmvmed: Unable to allocate space for output array\n" );
      return(NhlFATAL);
    }
/*
 * Copy double values to float values.
 */
    for( i = 0; i < total_elements*npts; i++ ) {
      xrmvmedian[i] = (float)x[i];
    }

    free(x);

    if(tmp_md->multidval.missing_value.has_missing) {
      return(NclReturnValue((void*)xrmvmedian,ndims,
                tmp_md->multidval.dim_sizes,&missing,
                NCL_float,0));
    }
    else {
      return(NclReturnValue((void*)xrmvmedian,ndims,
                tmp_md->multidval.dim_sizes,NULL,NCL_float,0));
    }
  }
  else {
    if(tmp_md->multidval.missing_value.has_missing) {
      return(NclReturnValue((void*)x,ndims,tmp_md->multidval.dim_sizes,
                &missing,NCL_double,0));
    }
    else {
      return(NclReturnValue((void*)x,ndims,tmp_md->multidval.dim_sizes,NULL,
                NCL_double,0));
    }
  }
}

NhlErrorTypes dim_standardize_W( void )
{
/*
 * Input array variables
 */
  NclStackEntry data;
  NclMultiDValData tmp_md = NULL;
  NclScalar missing;
  double *x;
  int *opt;
/*
 * Output array variables
 */
  float *xstandardize;
  double xmsg;
/*
 * various
 */
  int i, l1, total_elements, ier = 0, npts, ndims;
/*
 * Retrieve parameter.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  data = _NclGetArg(0,2,DONT_CARE);
  switch(data.kind) {
  case NclStk_VAR:
    tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
    break;
  case NclStk_VAL:
    tmp_md = (NclMultiDValData)data.u.data_obj;
    break;
  }
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
  ndims = tmp_md->multidval.n_dims;
  total_elements = 1;
  for(i = 0; i < ndims-1; i++) {
    total_elements *= tmp_md->multidval.dim_sizes[i];
  }    
  npts = tmp_md->multidval.dim_sizes[ndims-1];
/*
 * Copy input to double. Since we need to make an extra copy of the input
 * array to keep the input array from being changed, we do the coercion
 * no matter what.
 */
  x = (double*)NclMalloc(total_elements*npts*sizeof(double));
  if(x == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_standardize: Unable to convert input to double");
    return(NhlFATAL);
  }
  if( tmp_md->multidval.data_type == NCL_double ) {
    for(i = 0; i < total_elements*npts; i++) {
      x[i] = ((double *)tmp_md->multidval.val)[i];
    }
  }
  else if( tmp_md->multidval.data_type == NCL_float ) {
    for(i = 0; i < total_elements*npts; i++) {
      x[i] = (double)((float *)tmp_md->multidval.val)[i];
    }
  }
  else {
    for(i = 0; i < total_elements*npts; i++) {
      x[i] = (double)((int *)tmp_md->multidval.val)[i];
    }
  }
/*
 * Get the missing value if there is one.  Otherwise, get the default 
 * missing value.
 */
  if(tmp_md->multidval.missing_value.has_missing) {
    if(tmp_md->multidval.data_type == NCL_double) {
      missing.doubleval = tmp_md->multidval.missing_value.value.doubleval;
      xmsg = missing.doubleval;
    }
    else if(tmp_md->multidval.data_type == NCL_float) {
      missing.floatval = tmp_md->multidval.missing_value.value.floatval;
      xmsg = (double)missing.floatval;
    }
    else {
      missing.floatval = (float)tmp_md->multidval.missing_value.value.intval;
      xmsg = (double)missing.floatval;
    }
  }
  else {
/*
 * Get the missing value if there is one.  Otherwise, get the default 
 * missing value.
 */
    if(tmp_md->multidval.data_type == NCL_double) {
      missing.doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
      xmsg = missing.doubleval;
    }
    else {
      missing.floatval = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
      xmsg = (double)missing.floatval;
    }
  }

/*
 * Call the f77 double version of 'xstnd' with the full argument list.
 */
  l1 = 0;
  for(i = 1; i <= total_elements; i++) {
    NGCALLF(dxstnd,DXSTND)(&x[l1],&npts,&xmsg,opt,&ier);
    l1 += npts;
    if (ier == 2) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"dim_standardize: The input array contains all missing values");
    }
  }
/*
 * Return float if input isn't double, otherwise return double.
 */
  if(tmp_md->multidval.data_type != NCL_double) {
    xstandardize = (float*)NclMalloc(total_elements*npts*sizeof(float));
    if (xstandardize == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_standardize: Unable to allocate space for output array\n" );
      return(NhlFATAL);
    }
/*
 * Copy double values to float values.
 */
    for( i = 0; i < total_elements*npts; i++ ) {
      xstandardize[i] = (float)x[i];
    }
    free(x);
    if(tmp_md->multidval.missing_value.has_missing) {
      return(NclReturnValue((void*)xstandardize,ndims,
                tmp_md->multidval.dim_sizes,&missing,
                NCL_float,0));
    }
    else {
      return(NclReturnValue((void*)xstandardize,ndims,
                tmp_md->multidval.dim_sizes,NULL,NCL_float,0));
    }
  }
  else {
    if(tmp_md->multidval.missing_value.has_missing) {
      return(NclReturnValue((void*)x,ndims,tmp_md->multidval.dim_sizes,
                &missing,NCL_double,0));
    }
    else {
      return(NclReturnValue((void*)x,ndims,tmp_md->multidval.dim_sizes,NULL,
                NCL_double,0));
    }
  }
}


NhlErrorTypes esacr_W( void )
{
/*
 * Input array variables
 */
  void *x;
  double *dx;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  NclScalar missing_x, missing_dx, missing_rx;
  NclBasicDataTypes type_x;
  int *mxlag;
/*
 * Output array variables
 */
  float *racr;
  double *acr;
  int *dsizes_acr;
/*
 * various
 */
  int i, l1, l2, total_size_x1, total_size_x, total_size_acr, ier = 0, npts;
  double xmean, xvar, *acv;
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
/*
 * Compute the total number of elements in our x array.
 */
  total_size_x1 = 1;
  for(i = 0; i < ndims_x-1; i++) total_size_x1 *= dsizes_x[i];
  total_size_x = total_size_x1 * npts;
/*
 * Check for missing values.
 */
  if(has_missing_x) {
/*
 * Coerce missing value to double.
 */
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               &missing_dx,
               &missing_x,
               1,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));

    if(type_x != NCL_double) {
      _Nclcoerce((NclTypeClass)nclTypefloatClass,
                 &missing_rx,
                 &missing_x,
                 1,
                 NULL,
                 NULL,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
    }
  }
  else {
/*
 * Get the default missing value.
 */ 
    if(type_x != NCL_double) {
      missing_rx.floatval = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
      missing_dx.doubleval = (double)missing_rx.floatval;
    }
    else {
      missing_dx.doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
    }
  }
/*
 * Coerce data to double if necessary.
 */
  if(type_x != NCL_double) {
    dx = (double*)NclMalloc(sizeof(double)*total_size_x);
    if( dx == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esacr: Unable to allocate memory for coercing x array to double precision");
      return(NhlFATAL);
    }
    if(has_missing_x) {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dx,
                 x,
                 total_size_x,
                 &missing_dx,
                 &missing_x,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
    }
    else {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dx,
                 x,
                 total_size_x,
                 NULL,
                 NULL,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
    }
  }
  else {
/*
 * Input is already double.
 */
    dx = (double*)x;
  }

/* 
 * Get size of output variables.
 */
  total_size_acr = total_size_x1*(*mxlag+1);
  acr = (double*)NclMalloc(total_size_acr*sizeof(double));
  acv = (double*)NclMalloc(total_size_acr*sizeof(double));
  dsizes_acr = (int*)NclMalloc(ndims_x*sizeof(int));
  if (acv == NULL || acr == NULL || dsizes_acr == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"esacr: Unable to allocate space for output arrays\n" );
    return(NhlFATAL);
  }
  dsizes_acr[0] = 1;
  for( i = 0; i < ndims_x-1; i++ ) dsizes_acr[i] = dsizes_x[i];
  dsizes_acr[ndims_x-1] = *mxlag+1;
/*
 * Call the f77 version of 'desauto' with the full argument list.
 */
  l1 = l2 = 0;
    
  for(i = 1; i <= total_size_x1; i++) {
    xvar = xmean = missing_dx.doubleval;
    NGCALLF(desauto,DESAUTO)(&dx[l1],&npts,&missing_dx.doubleval,&xmean,
                             &xvar,mxlag,&acv[l2],&acr[l2],&ier);
    l1 += npts;
    l2 += (*mxlag+1);
    if (ier == -2) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esacr: The input array contains all missing values");
      return(NhlFATAL);
    }
    else if (ier == -5) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esacr: The sample variance is zero");
      return(NhlFATAL);
    }
  }
/*
 * free memory.
 */
  if((void*)dx != x) {
    NclFree(dx);
  }
/*
 * Return values. 
 */
  if(type_x != NCL_double) {
/*
 * Copy double values to float values.
 */
    racr = (float*)NclMalloc(sizeof(float)*total_size_acr);
    if( racr == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esacr: Unable to allocate memory for return array");
      return(NhlFATAL);
    }
    for( i = 0; i < total_size_acr; i++ ) {
      racr[i] = (float)acr[i];
    }
/*
 * Free double precision values.
 */
    free(acr);
/*
 * Return float values with missing value set.
 */
    return(NclReturnValue((void*)racr,ndims_x,dsizes_acr,&missing_rx,
                          NCL_float,0));
  }
  else {
/*
 * Return double values with missing value set.
 */
    return(NclReturnValue((void*)acr,ndims_x,dsizes_acr,&missing_dx,
                          NCL_double,0));
  }
}

NhlErrorTypes esacv_W( void )
{
/*
 * Input array variables
 */
  void *x;
  double *dx;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  NclScalar missing_x, missing_dx, missing_rx;
  NclBasicDataTypes type_x;
  int *mxlag;
/*
 * Output array variables
 */
  float *racv;
  double *acv;
  int *dsizes_acv;
/*
 * various
 */
  int i, l1, l2, total_size_x1, total_size_x, total_size_acv, ier = 0, npts;
  double xmean, xvar, *acr;
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
/*
 * Compute the total number of elements in our x array.
 */
  total_size_x1 = 1;
  for(i = 0; i < ndims_x-1; i++) total_size_x1 *= dsizes_x[i];
  total_size_x = total_size_x1 * npts;
/*
 * Check for missing values.
 */
  if(has_missing_x) {
/*
 * Coerce missing value to double.
 */
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               &missing_dx,
               &missing_x,
               1,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));

    if(type_x != NCL_double) {
      _Nclcoerce((NclTypeClass)nclTypefloatClass,
                 &missing_rx,
                 &missing_x,
                 1,
                 NULL,
                 NULL,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
    }
  }
  else {
/*
 * Get the default missing value.
 */ 
    if(type_x != NCL_double) {
      missing_rx.floatval = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
      missing_dx.doubleval = (double)missing_rx.floatval;
    }
    else {
      missing_dx.doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
    }
  }
/*
 * Coerce data to double if necessary.
 */
  if(type_x != NCL_double) {
    dx = (double*)NclMalloc(sizeof(double)*total_size_x);
    if( dx == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esacv: Unable to allocate memory for coercing x array to double precision");
      return(NhlFATAL);
    }
    if(has_missing_x) {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dx,
                 x,
                 total_size_x,
                 &missing_dx,
                 &missing_x,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
    }
    else {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dx,
                 x,
                 total_size_x,
                 NULL,
                 NULL,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
    }
  }
  else {
/*
 * Input is already double.
 */
    dx = (double*)x;
  }

/* 
 * Get size of output variables.
 */
  total_size_acv = total_size_x1*(*mxlag+1);
  acv = (double*)NclMalloc(total_size_acv*sizeof(double));
  acr = (double*)NclMalloc(total_size_acv*sizeof(double));
  dsizes_acv = (int*)NclMalloc(ndims_x*sizeof(int));
  if (acv == NULL || acr == NULL || dsizes_acv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"esacv: Unable to allocate space for output arrays\n" );
    return(NhlFATAL);
  }
  dsizes_acv[0] = 1;
  for( i = 0; i < ndims_x-1; i++ ) dsizes_acv[i] = dsizes_x[i];
  dsizes_acv[ndims_x-1] = *mxlag+1;
/*
 * Call the f77 version of 'desauto' with the full argument list.
 */
  l1 = l2 = 0;

  for(i = 1; i <= total_size_x1; i++) {
    xvar = xmean = missing_dx.doubleval;
    NGCALLF(desauto,DESAUTO)(&dx[l1],&npts,&missing_dx.doubleval,&xmean,
                             &xvar,mxlag,&acv[l2],&acr[l2],&ier);
    l1 += npts;
    l2 += (*mxlag+1);
    if (ier == -2) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esacv: The input array contains all missing values");
      return(NhlFATAL);
    }
    else if (ier == -5) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esacv: The sample variance is zero");
      return(NhlFATAL);
    }
  }
/*
 * free memory.
 */
  if((void*)dx != x) {
    NclFree(dx);
  }
/*
 * Return values. 
 */
  if(type_x != NCL_double) {
/*
 * Copy double values to float values.
 */
    racv = (float*)NclMalloc(sizeof(float)*total_size_acv);
    if( racv == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esacv: Unable to allocate memory for return array");
      return(NhlFATAL);
    }
    for( i = 0; i < total_size_acv; i++ ) {
      racv[i] = (float)acv[i];
    }
/*
 * Free double precision values.
 */
    free(acv);
/*
 * Return float values with missing value set.
 */
    return(NclReturnValue((void*)racv,ndims_x,dsizes_acv,&missing_rx,
                          NCL_float,0));
  }
  else {
/*
 * Return double values with missing value set.
 */
    return(NclReturnValue((void*)acv,ndims_x,dsizes_acv,&missing_dx,
                          NCL_double,0));
  }
}

NhlErrorTypes esccr_W( void )
{
/*
 * Input array variables
 */
  void *x, *y;
  double *dx, *dy;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  int ndims_y, dsizes_y[NCL_MAX_DIMENSIONS], has_missing_y;
  NclScalar missing_x, missing_y;
  NclScalar missing_rx, missing_dx, missing_dy;
  NclBasicDataTypes type_x, type_y;
  int *mxlag;
/*
 * Output array variables
 */
  double *ccr;
  float *rccr;
  int ndims_ccr, *dsizes_ccr;
/*
 * various
 */
  int i, j, lx, ly, lc;
  int total_size_x1, total_size_x, total_size_y1, total_size_y;
  int total_size_ccr;
  int ier = 0, ier_count, npts;
  double xmean, xsd, ymean, ysd, *ccv;
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
 * Check rightmost dimension.
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
 * Coerce missing values to double.
 *
 * Use the default missing value if one isn't set.
 */
  if(has_missing_x) {
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               &missing_dx,
               &missing_x,
               1,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));

    if(type_x != NCL_double) {
      _Nclcoerce((NclTypeClass)nclTypefloatClass,
                 &missing_rx,
                 &missing_x,
                 1,
                 NULL,
                 NULL,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
    }
  }
  else {
/*
 * Get the default missing value.
 */ 
    if(type_x != NCL_double) {
      missing_dx.doubleval = (double)((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
      missing_rx.floatval = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
    }
    else {
      missing_dx.doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
    }
  }
  if(has_missing_y) {
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               &missing_dy,
               &missing_y,
               1,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_y)));
  }
  else {
/*
 * Get the default missing value.
 */ 
    if(type_y != NCL_double) {
      missing_dy.doubleval = (double)((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
    }
    else {
      missing_dy.doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
    }
  }
/*
 * Coerce data to double if necessary.
 */
  if(type_x != NCL_double) {
    dx = (double*)NclMalloc(sizeof(double)*total_size_x);
    if( dx == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esccr: Unable to allocate memory for coercing x array to double precision");
      return(NhlFATAL);
    }
    if(has_missing_x) {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dx,
                 x,
                 total_size_x,
                 &missing_dx,
                 &missing_x,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
    }
    else {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dx,
                 x,
                 total_size_x,
                 NULL,
                 NULL,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
    }
  }
  else {
/*
 * Input is already double.
 */
    dx = (double*)x;
  }

  if(type_y != NCL_double) {
    dy = (double*)NclMalloc(sizeof(double)*total_size_y);
    if( dy == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esccr: Unable to allocate memory for coercing y array to double precision");
      return(NhlFATAL);
    }
    if(has_missing_y) {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dy,
                 y,
                 total_size_y,
                 &missing_dy,
                 &missing_y,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_y)));
    }
    else {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dy,
                 y,
                 total_size_y,
                 NULL,
                 NULL,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_y)));
    }
  }
  else {
/*
 * Input is already double.
 */
    dy = (double*)y;
  }
/* 
 * Get size of output variables.
 */
  ndims_ccr = ndims_x + ndims_y - 1;
  total_size_ccr = total_size_x1 * total_size_y1 * (*mxlag+1);
  ccr = (double*)NclMalloc(total_size_ccr*sizeof(double));
  ccv = (double*)NclMalloc(total_size_ccr*sizeof(double));
  dsizes_ccr = (int*)NclMalloc(ndims_ccr*sizeof(int));
  if (ccv == NULL || ccr == NULL || dsizes_ccr == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"esccr: Unable to allocate space for output arrays\n" );
    return(NhlFATAL);
  }
/*
 * Calculate dimensions for ccr. If input is:
 *
 *    x(npts)     and y(npts)   ==> ccr(mxlag+1)
 *    x(m,n,npts) and y(k,npts) ==> ccr(m,n,k,mxlag+1)
 *    x(npts)     and y(k,npts) ==> ccr(k,mxlag+1)
 *
 *  etc.
 */
  for( i = 0; i < ndims_x-1; i++ ) dsizes_ccr[i] = dsizes_x[i];
  for( i = 0; i < ndims_y-1; i++ ) dsizes_ccr[ndims_x-1+i] = dsizes_y[i];

  dsizes_ccr[ndims_ccr-1] = *mxlag+1;
/*
 * Call the f77 version of 'descros' with the full argument list.
 */
  lx = lc = 0;
  ier_count = 0;
  for(i = 1; i <= total_size_x1; i++) {
    ly = 0;
    for(j = 1; j <= total_size_y1; j++) {
      xmean = xsd = missing_dx.doubleval;
      ymean = ysd = missing_dy.doubleval;
      NGCALLF(descros,DESCROS)(&dx[lx],&dy[ly],&npts,&missing_dx.doubleval,
                               &missing_dy.doubleval,&xmean,&ymean,&xsd,
                               &ysd,mxlag,&ccv[lc],&ccr[lc],&ier);
      ly += npts;
      lc += (*mxlag+1);
      if(ier < 0) ier_count++;
    }
    lx += npts;
    if(ier_count > 0) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"esccr: Non-fatal conditions encountered: all missing or constant values");
    }
  }
/*
 * free memory.
 */
  if((void*)dx != x) {
    NclFree(dx);
  }
  if((void*)dy != y) {
    NclFree(dy);
  }
/*
 * Return values. 
 */
  if(type_x != NCL_double && type_y != NCL_double) {
/*
 * Copy double values to float values.
 */
    rccr = (float*)NclMalloc(sizeof(float)*total_size_ccr);
    if( rccr == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esccr: Unable to allocate memory for return array");
      return(NhlFATAL);
    }
    for( i = 0; i < total_size_ccr; i++ ) {
      rccr[i] = (float)ccr[i];
    }
    free(ccr);
/*
 * Return float values with missing value set.
 */
    return(NclReturnValue((void*)rccr,ndims_ccr,dsizes_ccr,&missing_rx,
                          NCL_float,0));
  }
  else {
/*
 * Return double values with missing value set.
 */
    return(NclReturnValue((void*)ccr,ndims_ccr,dsizes_ccr,&missing_dx,
                          NCL_double,0));
  }
}


NhlErrorTypes esccv_W( void )
{
/*
 * Input array variables
 */
  void *x, *y;
  double *dx, *dy;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  int ndims_y, dsizes_y[NCL_MAX_DIMENSIONS], has_missing_y;
  NclScalar missing_x, missing_y;
  NclScalar missing_rx, missing_dx, missing_dy;
  NclBasicDataTypes type_x, type_y;
  int *mxlag;
/*
 * Output array variables
 */
  double *ccv;
  float *rccv;
  int ndims_ccv, *dsizes_ccv;
/*
 * various
 */
  int i, j, lx, ly, lc;
  int total_size_x1, total_size_x, total_size_y1, total_size_y;
  int total_size_ccv;
  int ier = 0, ier_count, npts;
  double xmean, xsd, ymean, ysd, *ccr;
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
 * Coerce missing values to double.
 *
 * Use the default missing value if one isn't set.
 */
  if(has_missing_x) {
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               &missing_dx,
               &missing_x,
               1,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
    if(type_x != NCL_double) {
      _Nclcoerce((NclTypeClass)nclTypefloatClass,
                 &missing_rx,
                 &missing_x,
                 1,
                 NULL,
                 NULL,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
    }
  }
  else {
/*
 * Get the default missing value.
 */ 
    if(type_x != NCL_double) {
      missing_dx.doubleval = (double)((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
      missing_rx.floatval = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
    }
    else {
      missing_dx.doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
    }
  }

  if(has_missing_y) {
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               &missing_dy,
               &missing_y,
               1,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_y)));
  }
  else {
/*
 * Get the default missing value.
 */ 
    if(type_y != NCL_double) {
      missing_dy.doubleval = (double)((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
    }
    else {
      missing_dy.doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
    }
  }

/*
 * Coerce data to double if necessary.
 */
  if(type_x != NCL_double) {
    dx = (double*)NclMalloc(sizeof(double)*total_size_x);
    if( dx == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esccv: Unable to allocate memory for coercing x array to double precision");
      return(NhlFATAL);
    }
    if(has_missing_x) {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dx,
                 x,
                 total_size_x,
                 &missing_dx,
                 &missing_x,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
    }
    else {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dx,
                 x,
                 total_size_x,
                 NULL,
                 NULL,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
    }
  }
  else {
/*
 * Input is already double.
 */
    dx = (double*)x;
  }

  if(type_y != NCL_double) {
    dy = (double*)NclMalloc(sizeof(double)*total_size_y);
    if( dy == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esccv: Unable to allocate memory for coercing y array to double precision");
      return(NhlFATAL);
    }
    if(has_missing_y) {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dy,
                 y,
                 total_size_y,
                 &missing_dy,
                 &missing_y,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_y)));
    }
    else {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dy,
                 y,
                 total_size_y,
                 NULL,
                 NULL,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_y)));
    }
  }
  else {
/*
 * Input is already double.
 */
    dy = (double*)y;
  }
/* 
 * Get size of output variables.
 */
  ndims_ccv = ndims_x + ndims_y - 1;
  total_size_ccv = total_size_x1 * total_size_y1 * (*mxlag+1);
  ccr = (double*)NclMalloc(total_size_ccv*sizeof(double));
  ccv = (double*)NclMalloc(total_size_ccv*sizeof(double));
  dsizes_ccv = (int*)NclMalloc(ndims_ccv*sizeof(int));
  if (ccr == NULL || ccv == NULL || dsizes_ccv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"esccv: Unable to allocate space for output arrays\n" );
    return(NhlFATAL);
  }
/*
 * Calculate dimensions for ccv. If input is:
 *
 *    x(npts)     and y(npts)   ==> ccv(mxlag+1)
 *    x(m,n,npts) and y(k,npts) ==> ccv(m,n,k,mxlag+1)
 *    x(npts)     and y(k,npts) ==> ccv(k,mxlag+1)
 *
 *  etc.
 */
  for( i = 0; i < ndims_x-1; i++ ) dsizes_ccv[i] = dsizes_x[i];
  for( i = 0; i < ndims_y-1; i++ ) dsizes_ccv[ndims_x-1+i] = dsizes_y[i];

  dsizes_ccv[ndims_ccv-1] = *mxlag+1;
/*
 * Call the f77 version of 'descros' with the full argument list.
 */
  lx = lc = 0;
  ier_count = 0;
  for(i = 1; i <= total_size_x1; i++) {
    ly = 0;
    for(j = 1; j <= total_size_y1; j++) {
      xmean = xsd = missing_dx.doubleval;
      ymean = ysd = missing_dy.doubleval;
      NGCALLF(descros,DESCROS)(&dx[lx],&dy[ly],&npts,&missing_dx.doubleval,
                               &missing_dy.doubleval,&xmean,&ymean,&xsd,
                               &ysd,mxlag,&ccv[lc],&ccr[lc],&ier);
      ly += npts;
      lc += (*mxlag+1);
      if(ier < 0) ier_count++;
    }
    lx += npts;
    if(ier_count > 0) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"esccv: Non-fatal conditions encountered: all missing or constant values");
    }
  }
/*
 * free memory.
 */
  if((void*)dx != x) {
    NclFree(dx);
  }
  if((void*)dy != y) {
    NclFree(dy);
  }
/*
 * Return values. 
 */
  if(type_x != NCL_double && type_y != NCL_double) {
/*
 * Copy double values to float values.
 */
    rccv = (float*)NclMalloc(sizeof(float)*total_size_ccv);
    if( rccv == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esccv: Unable to allocate memory for return array");
      return(NhlFATAL);
    }
    for( i = 0; i < total_size_ccv; i++ ) {
      rccv[i] = (float)ccv[i];
    }
    free(ccv);
/*
 * Return float values with missing value set.
 */
    return(NclReturnValue((void*)rccv,ndims_ccv,dsizes_ccv,&missing_rx,
                          NCL_float,0));
  }
  else {
/*
 * Return double values with missing value set.
 */
    return(NclReturnValue((void*)ccv,ndims_ccv,dsizes_ccv,&missing_dx,
                          NCL_double,0));
  }
}


#include <stdio.h>
/*
* The following are the required NCAR Graphics include files.
* They should be located in ${NCARG_ROOT}/include
*/
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/ncl/defs.h>
#include "Symbol.h"
#include "NclMdInc.h"
#include <ncarg/ncl/NclDataDefs.h>
#include <ncarg/ncl/NclBuiltInSupport.h>
#include <ncarg/gks.h>
#include <math.h>

#define max(x,y)  ((x) > (y) ? (x) : (y))

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
  work = (float*)NclMalloc(npts*sizeof(int));
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
  work = (float*)NclMalloc(npts*sizeof(int));
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


NhlErrorTypes esacr_W( void )
{
/*
 * Input array variables
 */
  float *x;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  NclScalar missing_x;
  int *mxlag;
/*
 * Output array variables
 */
  float *acr;
  int *dsizes_acr;
/*
 * various
 */
  int i, l1, l2, total_elem_x, ier = 0, npts;
  float xmsg, xmean, xvar, *acv;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
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
  mxlag = (int*)NclGetArgValue(
           1,
           2,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           2);
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
 * Get size of output variables.
 */
  acr = (float*)NclMalloc(total_elem_x*(*mxlag+1)*sizeof(float));
  acv = (float*)NclMalloc(total_elem_x*(*mxlag+1)*sizeof(float));
  dsizes_acr = (int*)NclMalloc(ndims_x*sizeof(float));
  if (acv == NULL || acr == NULL || dsizes_acr == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"esacr: Unable to allocate space for output arrays\n" );
    return(NhlFATAL);
  }
  dsizes_acr[0] = 1;
  for( i = 0; i < ndims_x-1; i++ ) dsizes_acr[i] = dsizes_x[i];
  dsizes_acr[ndims_x-1] = *mxlag+1;
/*
 * Call the f77 version of 'esauto' with the full argument list.
 */
  l1 = l2 = 0;
	
  for(i = 1; i <= total_elem_x; i++) {
	xvar = xmean = xmsg;
	NGCALLF(esauto,ESAUTO)(&x[l1],&npts,&xmsg,&xmean,&xvar,
						   mxlag,&acv[l2],&acr[l2],&ier);
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
  if( has_missing_x ) {
	return(NclReturnValue((void*)acr,ndims_x,dsizes_acr,&missing_x,NCL_float,0));
  }
  else {
	return(NclReturnValue((void*)acr,ndims_x,dsizes_acr,NULL,NCL_float,0));
  }
}

NhlErrorTypes esacv_W( void )
{
/*
 * Input array variables
 */
  float *x;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  NclScalar missing_x;
  int *mxlag;
/*
 * Output array variables
 */
  float *acv;
  int *dsizes_acv;
/*
 * various
 */
  int i, l1, l2, total_elem_x, ier = 0, npts;
  float xmsg, xmean, xvar, *acr;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
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
  mxlag = (int*)NclGetArgValue(
           1,
           2,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           2);
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
 * Get size of output variables.
 */
  acv = (float*)NclMalloc(total_elem_x*(*mxlag+1)*sizeof(float));
  acr = (float*)NclMalloc(total_elem_x*(*mxlag+1)*sizeof(float));
  dsizes_acv = (int*)NclMalloc(ndims_x*sizeof(float));
  if (acv == NULL || acr == NULL || dsizes_acv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"esacv: Unable to allocate space for output arrays\n" );
    return(NhlFATAL);
  }
  dsizes_acv[0] = 1;
  for( i = 0; i < ndims_x-1; i++ ) dsizes_acv[i] = dsizes_x[i];
  dsizes_acv[ndims_x-1] = *mxlag+1;
/*
 * Call the f77 version of 'esauto' with the full argument list.
 */
  l1 = l2 = 0;
  for(i = 1; i <= total_elem_x; i++) {
	xvar = xmean = xmsg;
	NGCALLF(esauto,ESAUTO)(&x[l1],&npts,&xmsg,&xmean,&xvar,
						   mxlag,&acv[l2],&acr[l2],&ier);
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
  if( has_missing_x ) {
	return(NclReturnValue((void*)acv,ndims_x,dsizes_acv,&missing_x,NCL_float,0));
  }
  else {
	return(NclReturnValue((void*)acv,ndims_x,dsizes_acv,NULL,NCL_float,0));
  }
}


NhlErrorTypes esccr_W( void )
{
/*
 * Input array variables
 */
  float *x, *y;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  int ndims_y, dsizes_y[NCL_MAX_DIMENSIONS], has_missing_y;
  NclScalar missing_x, missing_y;
  int *mxlag;
/*
 * Output array variables
 */
  float *ccr;
  int *dsizes_ccr;
/*
 * various
 */
  int i, l1, l2, total_elem_x, ier = 0, ier_count, npts;
  float xmsg, ymsg, xmean, xsd, ymean, ysd, *ccv;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  x = (float*)NclGetArgValue(
           0,
           3,
           &ndims_x, 
           dsizes_x,
		   &missing_x,
		   &has_missing_x,
           NULL,
           2);
  y = (float*)NclGetArgValue(
           1,
           3,
           &ndims_y, 
           dsizes_y,
		   &missing_y,
		   &has_missing_y,
           NULL,
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
 * X and Y dimensions must be the same.
 */
  if( ndims_x != ndims_y ) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"esccr: x and y must have the same number of dimensions");
	  return(NhlFATAL);
  }
  for( i = 0; i < ndims_x; i++ ) {
	  if( dsizes_x[i] != dsizes_y[i] ) {
		  NhlPError(NhlFATAL,NhlEUNKNOWN,"esccr: x and y dimensions must be the same");
		  return(NhlFATAL);
	  }
  }
	  
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
 * Get the missing values.
 */
  if( has_missing_x ) {
	xmsg = missing_x.floatval;
  }
  else {
	xmsg = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
  }

  if( has_missing_y ) {
	ymsg = missing_y.floatval;
  }
  else {
	ymsg = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
  }

/*
 * Compute the total number of elements in our arrays.
 */
  total_elem_x = 1;
  for(i = 0; i < ndims_x-1; i++) total_elem_x *= dsizes_x[i];
/* 
 * Get size of output variables.
 */
  ccr = (float*)NclMalloc(total_elem_x*(*mxlag+1)*sizeof(float));
  ccv = (float*)NclMalloc(total_elem_x*(*mxlag+1)*sizeof(float));
  dsizes_ccr = (int*)NclMalloc(ndims_x*sizeof(float));
  if (ccv == NULL || ccr == NULL || dsizes_ccr == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"esccr: Unable to allocate space for output arrays\n" );
    return(NhlFATAL);
  }
  dsizes_ccr[0] = 1;
  for( i = 0; i < ndims_x-1; i++ ) dsizes_ccr[i] = dsizes_x[i];
  dsizes_ccr[ndims_x-1] = *mxlag+1;
/*
 * Call the f77 version of 'esauto' with the full argument list.
 */
  l1 = l2 = 0;
  ier_count = 0;
  for(i = 1; i <= total_elem_x; i++) {
	xmean = ymean = xsd = ysd = xmsg;
	NGCALLF(escros,ESCROS)(&x[l1],&y[l1],&npts,&xmsg,&ymsg,
						   &xmean,&ymean,&xsd,&ysd,mxlag,&ccv[l2],&ccr[l2],&ier);
	l1 += npts;
	l2 += (*mxlag+1);
	if(ier < 0) ier_count++;
  }
  if(ier_count > 0) {
	  NhlPError(NhlWARNING,NhlEUNKNOWN,"esccr: Non-fatal conditions encountered: all missing or constant values");
  }
  if( has_missing_x ) {
	return(NclReturnValue((void*)ccr,ndims_x,dsizes_ccr,&missing_x,NCL_float,0));
  }
  else if( has_missing_y) {
	return(NclReturnValue((void*)ccr,ndims_x,dsizes_ccr,&missing_y,NCL_float,0));
  }
  else {
	return(NclReturnValue((void*)ccr,ndims_x,dsizes_ccr,NULL,NCL_float,0));
  }
}


NhlErrorTypes esccv_W( void )
{
/*
 * Input array variables
 */
  float *x, *y;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  int ndims_y, dsizes_y[NCL_MAX_DIMENSIONS], has_missing_y;
  NclScalar missing_x, missing_y;
  int *mxlag;
/*
 * Output array variables
 */
  float *ccv;
  int *dsizes_ccv;
/*
 * various
 */
  int i, l1, l2, total_elem_x, ier = 0, ier_count, npts;
  float xmsg, ymsg, xmean, xsd, ymean, ysd, *ccr;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  x = (float*)NclGetArgValue(
           0,
           3,
           &ndims_x, 
           dsizes_x,
		   &missing_x,
		   &has_missing_x,
           NULL,
           2);
  y = (float*)NclGetArgValue(
           1,
           3,
           &ndims_y, 
           dsizes_y,
		   &missing_y,
		   &has_missing_y,
           NULL,
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
 * X and Y dimensions must be the same.
 */
  if( ndims_x != ndims_y ) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"esccv: x and y must have the same number of dimensions");
	  return(NhlFATAL);
  }
  for( i = 0; i < ndims_x; i++ ) {
	  if( dsizes_x[i] != dsizes_y[i] ) {
		  NhlPError(NhlFATAL,NhlEUNKNOWN,"esccv: x and y dimensions must be the same");
		  return(NhlFATAL);
	  }
  }
	  
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
 * Get the missing values.
 */
  if( has_missing_x ) {
	xmsg = missing_x.floatval;
  }
  else {
	xmsg = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
  }

  if( has_missing_y ) {
	ymsg = missing_y.floatval;
  }
  else {
	ymsg = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
  }
/*
 * Compute the total number of elements in our arrays.
 */
  total_elem_x = 1;
  for(i = 0; i < ndims_x-1; i++) total_elem_x *= dsizes_x[i];
/* 
 * Get size of output variables.
 */
  ccv = (float*)NclMalloc(total_elem_x*(*mxlag+1)*sizeof(float));
  ccr = (float*)NclMalloc(total_elem_x*(*mxlag+1)*sizeof(float));
  dsizes_ccv = (int*)NclMalloc(ndims_x*sizeof(float));
  if (ccr == NULL || ccv == NULL || dsizes_ccv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"esccv: Unable to allocate space for output arrays\n" );
    return(NhlFATAL);
  }
  dsizes_ccv[0] = 1;
  for( i = 0; i < ndims_x-1; i++ ) dsizes_ccv[i] = dsizes_x[i];
  dsizes_ccv[ndims_x-1] = *mxlag+1;
/*
 * Call the f77 version of 'esauto' with the full argument list.
 */
  l1 = l2 = 0;
  ier_count = 0;
  for(i = 1; i <= total_elem_x; i++) {
	xmean = ymean = xsd = ysd = xmsg;
	NGCALLF(escros,ESCROS)(&x[l1],&y[l1],&npts,&xmsg,&ymsg,
						   &xmean,&ymean,&xsd,&ysd,mxlag,&ccv[l2],&ccr[l2],&ier);
	l1 += npts;
	l2 += (*mxlag+1);
	if(ier < 0) ier_count++;
  }
  if(ier_count > 0) {
	  NhlPError(NhlWARNING,NhlEUNKNOWN,"esccv: Non-fatal conditions encountered: all missing or constant values");
  }
  if( has_missing_x ) {
	return(NclReturnValue((void*)ccv,ndims_x,dsizes_ccv,&missing_x,NCL_float,0));
  }
  else if( has_missing_y) {
	return(NclReturnValue((void*)ccv,ndims_x,dsizes_ccv,&missing_y,NCL_float,0));
  }
  else {
	return(NclReturnValue((void*)ccv,ndims_x,dsizes_ccv,NULL,NCL_float,0));
  }
}


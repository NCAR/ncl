#include <stdio.h>

/*
 * The following are the required NCAR Graphics include files.
 * They should be located in ${NCARG_ROOT}/include.
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

extern void NGCALLF(smth9,SMTH9)(float *,float *,int *,int *,float *,float *,
                                 float *,int *,int *);

NhlErrorTypes smth9_W( void )
{
/*
 * Input variables
 */
  float *x, *xout, *p, *q, xmsg, *work;
  logical *lwrap;
  int has_missing_x, ni, nj, lwork, i, j, nt, ier;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS];
  NclScalar missing_x;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
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
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_x < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"smth9: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }
  else {
	nj = dsizes_x[0];
	ni = dsizes_x[1];
/*
 * Compute the total number of elements in our array.
 */
	nt = 1;
	for(i = 0; i < ndims_x-2; i++) {
	  nt *= dsizes_x[i];
	}
  }

  p = (float*)NclGetArgValue(
          1,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);

  q = (float*)NclGetArgValue(
          2,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);

  lwrap = (logical*)NclGetArgValue(
          3,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);
/*
 * Check that src_array has a missing value set.
 */
  if(!has_missing_x) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"smth9: No missing values are being set.\nDefault missing values will be used.\nBe careful of results.");
    xmsg = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
  }
  else {
    xmsg = missing_x.floatval;
  }

/*
 * Allocate space for work array.
 */
  lwork = ni*nj;
  work  = (float*)calloc(lwork*sizeof(float),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"smth9: Unable to allocate memory for work array");
    return(NhlFATAL);
  }

/*
 * Allocate space for output array.
 */
  xout = (float*)calloc(nt*lwork*sizeof(float),1);
  if( xout == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"smth9: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
  memcpy(&xout[0],&x[0],nt*lwork*sizeof(float));

/*
 * Call Fortran routine.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
	NGCALLF(smth9,SMTH9)(&xout[j],work,&ni,&nj,p,q,&xmsg,lwrap,&ier);
    j += lwork;
  }

  free(work);
/*
 * Return
 */
  if( has_missing_x ) {
    return(NclReturnValue((void*)xout,ndims_x,dsizes_x,&missing_x,NCL_float,0));
  }
  else {
    return(NclReturnValue((void*)xout,ndims_x,dsizes_x,NULL,NCL_float,0));
  }
}



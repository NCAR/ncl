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

extern void NGCALLF(dsmth9,DSMTH9)(double *,double *,int *,int *,double *,
                                   double *,double *,int *,int *);

NhlErrorTypes smth9_W( void )
{
/*
 * Input variables
 */
  void *x, *p, *q;
  double *dx, *dp, *dq;
  float *rx;
  logical *lwrap;
  int has_missing_x;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS];
  NclScalar missing_x, missing_dx, missing_rx;
  NclBasicDataTypes type_x, type_p, type_q;
/*
 * Output variables
 */
  double *work;
/*
 * Various
 */
  int total_size_x, ni, nj, lwork, i, j, nt, ier, any_double = 0;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  x = (void*)NclGetArgValue(
          0,
          4,
          &ndims_x,
          dsizes_x,
          &missing_x,
          &has_missing_x,
          &type_x,
          2);
  p = (void*)NclGetArgValue(
          1,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_p,
          2);

  q = (void*)NclGetArgValue(
          2,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_q,
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
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_x < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"smth9: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }

  nj = dsizes_x[ndims_x-2];
  ni = dsizes_x[ndims_x-1];
/*
 * Compute the total number of elements in our array.
 */
  nt = 1;
  for(i = 0; i < ndims_x-2; i++) nt *= dsizes_x[i];

  total_size_x = nt * ni * nj;

/*
 * Check that input array has a missing value set.
 */
  if(!has_missing_x) {
/*
 * Print a warning.
 */
    NhlPError(NhlWARNING,NhlEUNKNOWN,"smth9: No missing values are being set.\nDefault missing values will be used.\nBe careful of results.");
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
  else {
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
/*
 * Coerce data to double no matter what, because we need to make a copy of
 * the input array to keep it from getting modified.
 */
  if(type_x == NCL_double) any_double = 1;

  dx = (double*)NclMalloc(sizeof(double)*total_size_x);
  if( dx == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"smth9: Unable to allocate memory for coercing x array to double precision");
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

/*
 * Coerce p to double.
 */
  if(type_p != NCL_double) {
    dp = (double*)NclMalloc(sizeof(double));
    if( dp == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"smth9: Unable to allocate memory for coercing p to double precision");
      return(NhlFATAL);
    }
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               dp,
               p,
               1,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_p)));
  }
  else {
    any_double = 1;
/*
 * p is already double.
 */
    dp = (double*)p;
  }

/*
 * Coerce q to double.
 */
  if(type_q != NCL_double) {
    dq = (double*)NclMalloc(sizeof(double));
    if( dq == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"smth9: Unable to allocate memory for coercing q to double precision");
      return(NhlFATAL);
    }
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               dq,
               q,
               1,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_q)));
  }
  else {
    any_double = 1;
/*
 * q is already double.
 */
    dq = (double*)q;
  }

/*
 * Allocate space for work array.
 */
  lwork = ni*nj;
  work  = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"smth9: Unable to allocate memory for work array");
    return(NhlFATAL);
  }

/*
 * Call Fortran routine.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dsmth9,DSMTH9)(&dx[j],work,&ni,&nj,dp,dq,&missing_dx.doubleval,
                           lwrap,&ier);
    j += lwork;
  }

/*
 * free memory.
 */
  NclFree(work);
  if((void*)dp != p) NclFree(dp);
  if((void*)dq != q) NclFree(dq);

/*
 * Return values. 
 */
  if(!any_double) {
/*
 * Copy double values to float values.
 */
    rx = (float*)NclMalloc(sizeof(float)*total_size_x);
    if( rx == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"smth9: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    for( i = 0; i < total_size_x; i++ ) rx[i] = (float)dx[i];
    NclFree(dx);
/*
 * Return float values with missing value set.
 */
    return(NclReturnValue((void*)rx,ndims_x,dsizes_x,&missing_rx,
                          NCL_float,0));
  }
  else {
    return(NclReturnValue((void*)dx,ndims_x,dsizes_x,&missing_dx,
                          NCL_double,0));
  }
}

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
#include <ncarg/ncl/NclDataDefs.h>
#include <ncarg/ncl/NclBuiltInSupport.h>
#include <ncarg/gks.h>

NhlErrorTypes relhum_W( void )
{
  extern double NGCALLF(drelhum,DRELHUM)(double*,double*,double*);
  int i, j, total;
/*
 * Input variables
 */
  void *t, *w, *p;
  double *dt, *dw, *dp;
  int ndims_t, dsizes_t[NCL_MAX_DIMENSIONS];
  int ndims_w, dsizes_w[NCL_MAX_DIMENSIONS];
  int ndims_p, dsizes_p[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_t, type_w, type_p;
/*
 * Output variables
 */
  double *rh;
  float *rrh;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 *
 * Retrieve argument #1
 */
  t = (void*)NclGetArgValue(
          0,
          3,
          &ndims_t, 
          dsizes_t,
          NULL,
          NULL,
          &type_t,
          2);
/*
 * Retrieve argument #2
 */
  w = (void*)NclGetArgValue(
          1,
          3,
          &ndims_w, 
          dsizes_w,
          NULL,
          NULL,
          &type_w,
          2);
/*
 * Retrieve argument #3
 */
  p = (void*)NclGetArgValue(
          2,
          3,
          &ndims_p, 
          dsizes_p,
          NULL,
          NULL,
          &type_p,
          2);
/*
 * Check dimensions and calculate total size of arrays.
 */
  if( ndims_t != ndims_w || ndims_t != ndims_p ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"relhum: The input arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
/*
 * Calculate total size of arrays.
 */
  total = 1;
  for( i = 0; i < ndims_t; i++ ) {
    if( dsizes_t[i] != dsizes_p[i] || dsizes_t[i] != dsizes_w[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"relhum: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
    total *= dsizes_t[i];
  }
/*
 * Coerce data to double if necessary.
 */
  if(type_t != NCL_double) {
    dt = (double*)NclMalloc(sizeof(double)*total);
    if( dt == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"relhum: Unable to allocate memory for coercing t array to double precision");
      return(NhlFATAL);
    }
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               dt,
               t,
               total,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_t)));
  }
  else {
/*
 * t is already double.
 */
    dt = (double*)t;
  }
/*
 * Coerce w.
 */
  if(type_w != NCL_double) {
    dw = (double*)NclMalloc(sizeof(double)*total);
    if( dw == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"relhum: Unable to allocate memory for coercing w array to double precision");
      return(NhlFATAL);
    }
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               dw,
               w,
               total,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_w)));
  }
  else {
/*
 * w is already double.
 */
    dw = (double*)w;
  }
/*
 * Coerce p.
 */
  if(type_p != NCL_double) {
    dp = (double*)NclMalloc(sizeof(double)*total);
    if( dp == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"relhum: Unable to allocate memory for coercing p array to double precision");
      return(NhlFATAL);
    }
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               dp,
               p,
               total,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_p)));
  }
  else {
/*
 * Input is already double.
 */
    dp = (double*)p;
  }
/*
 * Allocate space for output array.
 */
  rh = (double*)NclMalloc(total*sizeof(double));
  if( rh == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"relhum: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Call function.
 */
  for( i = 0; i < total; i++ ) {
    rh[i] = NGCALLF(drelhum,DRELHUM)(&dt[i],&dw[i],&dp[i]);
  }
/*
 * Free memory.
 */
  if((void*)dt != t) {
    NclFree(dt);
  }
  if((void*)dw != w) {
    NclFree(dw);
  }
  if((void*)dp != p) {
    NclFree(dp);
  }
/*
 * Return.
 */
  if(type_t != NCL_double && type_w != NCL_double && type_p != NCL_double) {
/*
 * None of the input is double, so return float values.
 *
 * First copy double values to float values.
 */
    rrh = (float*)NclMalloc(sizeof(float)*total);
    if( rrh == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"relhum: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    for( i = 0; i < total; i++ ) rrh[i] = (float)rh[i];
    NclFree(rh);
/*
 * Return float values.
 */
    return(NclReturnValue((void*)rrh,ndims_t,dsizes_t,NULL,NCL_float,0));
  }
  else {
/*
 * Return double values.
 */
    return(NclReturnValue((void*)rh,ndims_t,dsizes_t,NULL,NCL_double,0));
  }
}


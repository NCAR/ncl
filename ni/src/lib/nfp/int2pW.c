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
#include <ncarg/ncl/NclDataDefs.h>
#include <ncarg/ncl/NclBuiltInSupport.h>
#include <ncarg/gks.h>

extern void NGCALLF(dint2p,DINT2P)(double *,double *,double *,double *,
                                   int *,double *,double *,int *,int *,
                                   double *,int*);

NhlErrorTypes int2p_W( void )
{
/*
 * Input array variables
 */
  void *pin, *xin, *pout;
  double *dpin, *dxin, *dpout;
  int ndims_pin, dsizes_pin[NCL_MAX_DIMENSIONS];
  NclScalar missing_pin, missing_xin, missing_dx, missing_rx;
  int has_missing_pin, has_missing_xin;
  int ndims_xin, dsizes_xin[NCL_MAX_DIMENSIONS];
  int ndims_pout, dsizes_pout[NCL_MAX_DIMENSIONS];
  int *linlog;
  NclBasicDataTypes type_pin, type_xin, type_pout;
/*
 * work arrays
 */
  double *p, *x;
/*
 * output variable 
 */
  double *xout;
  float *rxout;
  int size_pin, size_pout, size_xout;
/*
 * Declare various variables for random purposes.
 */
  int i, j, k, npin, npout, ier = 0;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */

/*
 * Retrieve argument #1
 */
  pin = (void*)NclGetArgValue(
          0,
          4,
          &ndims_pin,
          dsizes_pin,
          &missing_pin,
          &has_missing_pin,
          &type_pin,
          2);
/*
 * Retrieve argument #2
 */
  xin = (void*)NclGetArgValue(
          1,
          4,
          &ndims_xin,
          dsizes_xin,
          &missing_xin,
          &has_missing_xin,
          &type_xin,
          2);
/*
 * Retrieve argument #3
 */
  pout = (void*)NclGetArgValue(
          2,
          4,
          &ndims_pout,
          dsizes_pout,
          NULL,
          NULL,
          &type_pout,
          2);

/*
 * Retrieve argument #4
 */
  linlog = (int*)NclGetArgValue(
          3,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Check number of dimensions and/or dimension sizes for arguments pin,
 * xin, and pout.
 */
  if (ndims_pin != ndims_xin) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p: The two input arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  if (ndims_pout != ndims_pin) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p: 'pout' must have the same number of dimensions as 'pin'");
    return(NhlFATAL);
  }
  size_pin = size_pout = 1;
  for( i = 0; i < ndims_pin; i++ ) {
    if (dsizes_pin[i] != dsizes_xin[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p: The two input arrays must have the same dimensions");
      return(NhlFATAL);
    }
  }

  for( i = 0; i < ndims_pout-1; i++ ) {
    if (dsizes_pout[i] != dsizes_pin[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p: The 'pout' array must have the same leftmost dimensions as the 'pin' array");
      return(NhlFATAL);
    }
    size_pin *= dsizes_pin[i];
    size_pout *= dsizes_pout[i];
  }
  npin  = dsizes_pin[ndims_pin-1];
  npout = dsizes_pout[ndims_pout-1];
  if (npout < 1 || npin < 1) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p: The right-most dimension of 'pin' and 'pout' must be at least one.");
    return(NhlFATAL);
  }
  size_pin  *= npin;
  size_xout = size_pout;
  size_pout *= npout;

/*
 * Check for missing values.
 */
  if(has_missing_xin) {
/*
 * Coerce missing value to double.
 */
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               &missing_dx,
               &missing_xin,
               1,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_xin)));

    if(type_xin != NCL_double) {
      _Nclcoerce((NclTypeClass)nclTypefloatClass,
                 &missing_rx,
                 &missing_xin,
                 1,
                 NULL,
                 NULL,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_xin)));
    }
  }
  else if(has_missing_pin) {
/*
 * Coerce missing value to double.
 */
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               &missing_dx,
               &missing_pin,
               1,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_pin)));

    if(type_pin != NCL_double) {
      _Nclcoerce((NclTypeClass)nclTypefloatClass,
                 &missing_rx,
                 &missing_pin,
                 1,
                 NULL,
                 NULL,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_pin)));
    }
  }
  else {
/*
 * Assign a default missing value.
 */ 
    if(type_pin != NCL_double && type_xin != NCL_double && 
       type_pout != NCL_double) {
      missing_rx.floatval  = 1.e36;
    }
    missing_dx.doubleval = 1.e36;
  }
/*
 * Coerce data to double if necessary.
 */
  if(type_xin != NCL_double) {
    dxin = (double*)NclMalloc(sizeof(double)*size_pin);
    if( dxin == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p: Unable to allocate memory for coercing xin array to double precision");
      return(NhlFATAL);
    }
    if(has_missing_xin) {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dxin,
                 xin,
                 size_pin,
                 &missing_dx,
                 &missing_xin,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_xin)));
    }
    else {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dxin,
                 xin,
                 size_pin,
                 NULL,
                 NULL,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_xin)));
    }
  }
  else {
/*
 * Input is already double.
 */
    dxin = (double*)xin;
  }

/*
 * Coerce pin to double if necessary.
 */
  if(type_pin != NCL_double) {
    dpin = (double*)NclMalloc(sizeof(double)*size_pin);
    if( dpin == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p: Unable to allocate memory for coercing pin array to double precision");
      return(NhlFATAL);
    }
    if(has_missing_pin) {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dpin,
                 pin,
                 size_pin,
                 &missing_dx,
                 &missing_pin,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_pin)));
    }
    else {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dpin,
                 pin,
                 size_pin,
                 NULL,
                 NULL,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_pin)));
    }
  }
  else {
/*
 * Input is already double.
 */
    dpin = (double*)pin;
  }


/*
 * Coerce pout to double if necessary.
 */
  if(type_pout != NCL_double) {
    dpout = (double*)NclMalloc(sizeof(double)*size_pout);
    if( dpout == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p: Unable to allocate memory for coercing pout array to double precision");
      return(NhlFATAL);
    }
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               dpout,
               pout,
               size_pout,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_pout)));
  }
  else {
/*
 * Input is already double.
 */
    dpout = (double*)pout;
  }

/*
 * Allocate space for work arrays.
 */
  p = (double*)NclMalloc(npin*sizeof(double));
  x = (double*)NclMalloc(npin*sizeof(double));
  if (p == NULL || x == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p: Unable to allocate space for work arrays\n" );
    return(NhlFATAL);
  }

/*
 * Allocate space for output array.
 */
  xout = (double*)NclMalloc(size_pout*sizeof(double));
  if( xout == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Call the Fortran version of this routine.
 *
 */
  j = k = 0;
  for( i = 0; i < size_xout; i++ ) {
    NGCALLF(dint2p,DINT2P)(&dpin[j],&dxin[j],&p[0],&x[0],&npin,
                           &dpout[k],&xout[k],&npout,linlog,
                           &missing_dx.doubleval,&ier);
    if (ier >= 1000) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p: One of the input arrays contains all missing data");
      return(NhlFATAL);
    }
    j += npin;
    k += npout;
  }
/*
 * Free memory.
 */
  NclFree(p);
  NclFree(x);

  if((void*)dpin != pin) {
    NclFree(dpin);
  }
  if((void*)dxin != xin) {
    NclFree(dxin);
  }
  if((void*)dpout != pout) {
    NclFree(dpout);
  }

/*
 * Return values.
 */
  if(type_xin != NCL_double && type_pin != NCL_double  &&
     type_pout != NCL_double) {
/*
 * None of the input is double, so return float values.
 *
 * First copy double values to float values.
 */
    rxout = (float*)NclMalloc(sizeof(float)*size_pout);
    if( rxout == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    for( i = 0; i < size_pout; i++ ) rxout[i] = (float)xout[i];
/*
 * Free double precision values.
 */
    NclFree(xout);
/*
 * Return float values with missing value set.
 */
    return(NclReturnValue((void*)rxout,ndims_pout,dsizes_pout,&missing_rx,
                          NCL_float,0));
  }
  else {
/*
 * Return double values with missing value set.
 */
    return(NclReturnValue((void*)xout,ndims_pout,dsizes_pout,&missing_dx,
                          NCL_double,0));
  }
}

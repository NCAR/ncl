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
#include <math.h>
#include <ncarg/gks.h>

extern double NGCALLF(dflxedy,DFLXEDY)(double *,double *,int *,double *,
                                       int *);

NhlErrorTypes fluxEddy_W( void )
{
/*
 * Input array variables
 */
  void *x, *y;
  double *dx, *dy;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  int ndims_y, dsizes_y[NCL_MAX_DIMENSIONS], has_missing_y;
  NclScalar missing_x, missing_y;
  NclScalar missing_dx, missing_dy, missing_rx;
  NclBasicDataTypes type_x, type_y;
/*
 * Output array variables
 */
  double *fluxeddy;
  float *rfluxeddy;
  int ndims_fluxeddy, size_fluxeddy, dsizes_fluxeddy[NCL_MAX_DIMENSIONS];
  int size_xy;
/*
 * Declare various variables for random purposes.
 */
  int i, j, ntime, ier;

/*
 * Retrieve arguments.
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
 * x and y must have the same dimensions.
 */
  if( ndims_x < 1 || ndims_x != ndims_y ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"fluxEddy: The input arrays x and y must have the same number of dimensions");
    return(NhlFATAL);
  }
  ntime = dsizes_x[ndims_x-1];

  for( i = 0; i < ndims_x; i++ ) {
    if(dsizes_x[i] != dsizes_y[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"fluxEddy: The input arrays x and y must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Compute the total size of the output array (minus the last dimension).
 */
  size_fluxeddy = size_xy = 1;

  dsizes_fluxeddy[0] = 1;
  for( i = 0; i < ndims_x-1; i++ ) {
    size_fluxeddy *= dsizes_x[i];
    dsizes_fluxeddy[i] = dsizes_x[i];
  }
  size_xy = size_fluxeddy * dsizes_x[ndims_x-1];
  ndims_fluxeddy  = ndims_x - 1 < 1 ? 1 : ndims_x - 1;
        
/*
 * Coerce missing values to double.
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

/*
 * Coerce x to double if necessary.
 */
  if(type_x != NCL_double) {
    dx = (double*)NclMalloc(sizeof(double)*size_xy);
    if( dx == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"fluxEddy: Unable to allocate memory for coercing x array to double precision");
      return(NhlFATAL);
    }
    if(has_missing_x) {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dx,
                 x,
                 size_xy,
                 &missing_dx,
                 &missing_x,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
    }
    else {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dx,
                 x,
                 size_xy,
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
 * Coerce y to double if necessary.
 */
  if(type_y != NCL_double) {
    dy = (double*)NclMalloc(sizeof(double)*size_xy);
    if( dy == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"fluxEddy: Unable to allocate memory for coercing y array to double precision");
      return(NhlFATAL);
    }
    if(has_missing_y) {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dy,
                 y,
                 size_xy,
                 &missing_dy,
                 &missing_y,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_y)));
    }
    else {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dy,
                 y,
                 size_xy,
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
 * Allocate space for output value.
 */
  fluxeddy = (double *)NclMalloc(size_fluxeddy*sizeof(double));
  if( fluxeddy == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"fluxEddy: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Call the Fortran version of this routine.
 *
 */
  j = 0;
  for( i = 0; i < size_fluxeddy; i++ ) {
    fluxeddy[i] = NGCALLF(dflxedy,DFLXEDY)(&dx[j],&dy[j],&ntime,
                                           &missing_dx.doubleval,&ier);
    j += ntime;
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
    rfluxeddy = (float*)NclMalloc(sizeof(float)*size_fluxeddy);
    if( rfluxeddy == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"fluxEddy: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    for( i = 0; i < size_fluxeddy; i++ ) rfluxeddy[i] = (float)fluxeddy[i];

/*
 * Free double precision array since we don't need it anymore.
 */
    NclFree(fluxeddy);
/*
 * Return float values.  A missing value is returned regardless if a
 * missing value was originally set (the default is used if none set).
 */
    return(NclReturnValue((void*)rfluxeddy,ndims_fluxeddy,dsizes_fluxeddy,
                          &missing_rx,NCL_float,0));
  }
  else {
/*
 * Return double values.  A missing value is returned regardless if a
 * missing value was originally set (the default is used if none set).
 */
    return(NclReturnValue((void*)fluxeddy,ndims_fluxeddy,dsizes_fluxeddy,
                          &missing_dx,NCL_double,0));
  }
}


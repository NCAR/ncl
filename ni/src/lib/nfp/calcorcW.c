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

extern void NGCALLF(dcalcorc,DCALCORC)(double*,double*,double*,double*,int*,
                                       double*,double*,double*,int*);

NhlErrorTypes escorc_W( void )
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
/*
 * Output array variables
 */
  double *corc;
  float *rcorc;
  int ndims_corc, dsizes_corc[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  int i, j, l1, l2, l3, ier = 0, nxy;
  int total_size_x1, total_size_x, total_size_y1, total_size_y;
  int total_size_corc;
  double xave, xstd;
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
 * The last dimension of x and y both must be the same.
 */
  if( dsizes_x[ndims_x-1] != dsizes_y[ndims_y-1] ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"escorc: The last dimension of x must be equal to the last dimension of y");
    return(NhlFATAL);
  }
      
/*
 * Compute the total number of elements in our arrays.
 */
  nxy = dsizes_x[ndims_x-1];

  total_size_x1 = 1;
  for(i = 0; i < ndims_x-1; i++) total_size_x1 *= dsizes_x[i];
  total_size_x = total_size_x1 * nxy;

  total_size_y1 = 1;
  for(i = 0; i < ndims_y-1; i++) total_size_y1 *= dsizes_y[i];
  total_size_y = total_size_y1 * nxy;
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
      NhlPError(NhlFATAL,NhlEUNKNOWN,"escorc: Unable to allocate memory for coercing x array to double precision");
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
      NhlPError(NhlFATAL,NhlEUNKNOWN,"escorc: Unable to allocate memory for coercing y array to double precision");
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
  total_size_corc = total_size_x1 * total_size_y1;

  corc = (double*)NclMalloc(total_size_corc*sizeof(double));
  if (corc == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"escorc: Unable to allocate space for output array");
    return(NhlFATAL);
  }

  ndims_corc = ndims_x + ndims_y - 2;
  if(!ndims_corc) ndims_corc = 1;

  dsizes_corc[0] = 1;

  for( i = 0; i < ndims_x-1; i++ ) dsizes_corc[i] = dsizes_x[i];
  for( i = 0; i < ndims_y-1; i++ ) dsizes_corc[ndims_x-1+i] = dsizes_y[i];

/*
 * Call the f77 version of 'descros' with the full argument list.
 */
  l2 = l3 = 0;

  for(i = 1; i <= total_size_x1; i++) {
    l1 = 0;
    for(j = 1; j <= total_size_y1; j++) {
      xave = xstd = missing_dx.doubleval;
      NGCALLF(dcalcorc,DCALCORC)(&dx[l2],&xave,&xstd,&missing_dx.doubleval,
                                 &nxy,&dy[l1],&missing_dy.doubleval,
                                 &corc[l3],&ier);
      if(ier || xstd == 0.) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"escorc: error encountered in series or xstd = 0.0");
        return(NhlFATAL);
      }
      l3++;
      l1 += nxy;
    }
    l2 += nxy;
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
    rcorc = (float*)NclMalloc(sizeof(float)*total_size_corc);
    if( rcorc == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"escorc: Unable to allocate memory for return array");
      return(NhlFATAL);
    }
    for( i = 0; i < total_size_corc; i++ ) {
      rcorc[i] = (float)corc[i];
    }
/*
 * Free double precision array since we don't need it anymore.
 */
    free(corc);

/*
 * Return float values.  A missing value is returned regardless if a
 * missing value was originally set (the default is used if none set).
 */
    return(NclReturnValue((void*)rcorc,ndims_corc,dsizes_corc,&missing_rx,
                          NCL_float,0));
  }
  else {
/*
 * Return double values.
 */
    return(NclReturnValue((void*)corc,ndims_corc,dsizes_corc,&missing_dx,
                          NCL_double,0));
  }
}



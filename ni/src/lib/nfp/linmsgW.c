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

extern void NGCALLF(dlinmsg,DLINMSG)(double *,int *,double *,int *);

NhlErrorTypes linmsg_W( void )
{
/*
 * Input variables
 */
  void *x;
  double *dx;
  float *rx;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  NclScalar missing_x, missing_dx, missing_rx;
  int *mflag; 
  NclBasicDataTypes type_x;
/*
 * Other variables
 */
  int i, j, total_size_x, total_size_x1, npts;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
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

  mflag = (int*)NclGetArgValue(
          1,
          2,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Compute the total number of elements in our x array.
 */
  npts = dsizes_x[ndims_x-1];
  total_size_x1 = 1;
  for( i = 0; i < ndims_x-1; i++ ) total_size_x1 *= dsizes_x[i];

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
 * Coerce data to double, even if it is already double. We need to
 * make a copy of the input array to keep it from getting modified.
 */
  dx = (double*)NclMalloc(sizeof(double)*total_size_x);
  if( dx == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linmsg: Unable to allocate memory for coercing x array to double precision");
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
 * Call Fortran function.
 */
  j = 0;
  for( i = 0; i < total_size_x1; i++ ) {
    NGCALLF(dlinmsg,DLINMSG)(&dx[j],&npts,&missing_dx.doubleval,mflag);
    j += npts;
  }

  if(type_x != NCL_double) {
/*
 * Copy double values to float values.
 */
    rx = (float*)NclMalloc(sizeof(float)*total_size_x);
    if( rx == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"linmsg: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    for( i = 0; i < total_size_x; i++ ) rx[i] = (float)dx[i];
/*
 * Free double precision values.
 */
    NclFree(dx);
/*
 * Return float values with missing value set.
 */
    return(NclReturnValue((void*)rx,ndims_x,dsizes_x,&missing_rx,
                          NCL_float,0));
  }
  else {
/*
 * Return double values with missing value set.
 */
    return(NclReturnValue((void*)dx,ndims_x,dsizes_x,&missing_dx,
                          NCL_double,0));
  }
}

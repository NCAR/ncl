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

extern void NGCALLF(dwgtrunave,DWGTRUNAVE)(double*,int*,double*,int*,int*,
                                           double*,double*,int*,int*);

extern void NGCALLF(drunave,DRUNAVE)(double*,int*,int*,int*,double*,double*,
                                     int*,int*);

NhlErrorTypes wgt_runave_W( void )
{
/*
 * Input array variables
 */
  void *x, *wgt;
  float *fx;
  double *dx, *dwgt;
  int *kopt;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  int ndims_wgt, dsizes_wgt[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_x, type_wgt;
  NclScalar missing_x, missing_dx, missing_rx;
/*
 * Work array.
 */
  int lwork;
  double *work;
/*
 * Declare various variables for random purposes.
 */
  int i, j, npts, nwgt, ier, total_size_x1, total_size_x;

/*
 * Retrieve arguments.
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

  wgt = (void*)NclGetArgValue(
          1,
          3,
          &ndims_wgt,
          dsizes_wgt,
          NULL,
          NULL,
          &type_wgt,
          2);

  kopt = (int*)NclGetArgValue(
          2,
          3,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);
/*
 * Check input dimension sizes.
 */
  npts = dsizes_x[ndims_x-1];
  nwgt = dsizes_wgt[0];
  if( nwgt > npts ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_runave: The length of wgt must be less than or equal to the last dimension of x");
    return(NhlFATAL);
  }
/*
 * wgt must be one-dimensional
 */
  if( ndims_wgt != 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_runave: The input array wgt must be one-dimensional"); 
    return(NhlFATAL);
  }
/*
 * Compute the total size of the output array (minus the last dimension).
 */
  total_size_x1 = 1;
  for( i = 0; i < ndims_x-1; i++ ) {
    total_size_x1 *= dsizes_x[i];
  }
  total_size_x = total_size_x1 * npts;

/*
 * Coerce data to double if necessary. Since we have to make a copy of
 * the first input array anyway (the input array will be overwritten by
 * output array), we coerce it no matter what.
 */
  dx   = (double*)NclMalloc(sizeof(double)*total_size_x);
  if( dx == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_runave: Unable to allocate memory for coercing input array to double precision");
    return(NhlFATAL);
  }
/*
 * Coerce the missing value first.
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
/*
 * Coerce the data with missing values.
 */
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               dx,
               x,
               total_size_x,
               &missing_dx,
               &missing_x,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
  }
  else {
/*
 * Coerce the data without missing values.
 */
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               dx,
               x,
               total_size_x,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
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
 * Coerce second input array if necessary.
 */
  if(type_wgt != NCL_double) {
    dwgt = (double*)NclMalloc(sizeof(double)*nwgt);
    if( dwgt == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_runave: Unable to allocate memory for coercing weight array to double precision");
      return(NhlFATAL);
    }
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               dwgt,
               wgt,
               nwgt,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_wgt)));
  }
  else {
    dwgt = (double*)wgt;
  }

/*
 * Allocate space for work array.
 */
  lwork = npts+2*(nwgt/2);
  work = (double *)NclMalloc(lwork*sizeof(double));
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_runave: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  
/*
 * Call the Fortran version of this routine.
 *
 */
  j = 0;
  for( i = 0; i < total_size_x1; i++ ) {
    NGCALLF(dwgtrunave,DWGTRUNAVE)(&dx[j],&npts,dwgt,&nwgt,kopt,
                                   &missing_dx.doubleval,work,&lwork,&ier);
    j += npts;
  }
  free(work);
  if((void*)dwgt != wgt) {
    NclFree(dwgt);
  }
        
  if(type_x != NCL_double) {
/*
 * Copy double values to float values.
 */
    fx = (float*)NclMalloc(sizeof(float)*total_size_x);
    if( fx == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_runave: Unable to allocate memory for return array");
      return(NhlFATAL);
    }
    for( i = 0; i < total_size_x; i++ ) {
      fx[i] = (float)dx[i];
    }

    NclFree(dx);
/*
 * Return float values with missing value set.
 */
    return(NclReturnValue((void*)fx,ndims_x,dsizes_x,&missing_rx,NCL_float,0));
  }
  else {
/*
 * Return double values with missing value set.
 */
    return(NclReturnValue((void*)dx,ndims_x,dsizes_x,&missing_dx,NCL_double,0));
  }
}


NhlErrorTypes runave_W( void )
{
/*
 * Input array variables
 */
  void *x;
  double *dx;
  float *fx;
  int *nave, *kopt;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  NclBasicDataTypes type_x;
  NclScalar missing_x, missing_dx, missing_rx;
/*
 * Work array.
 */
  int lwork;
  double *work;
/*
 * Declare various variables for random purposes.
 */
  int i, j, k, npts, ier, total_size_x, total_size_x1;

/*
 * Retrieve arguments.
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

  nave = (int*)NclGetArgValue(
          1,
          3,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);

  kopt = (int*)NclGetArgValue(
          2,
          3,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);
/*
 * x and wgt must have the same leftmost dimensions.
 */
  npts = dsizes_x[ndims_x-1];

  if( *nave > npts ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"runave: nave must be less than or equal to the last dimension of x");
    return(NhlFATAL);
  }
/*
 * Compute the total size of the output array (minus the last dimension).
 */
  total_size_x1 = 1;
  for( i = 0; i < ndims_x-1; i++ ) {
    total_size_x1 *= dsizes_x[i];
  }
  total_size_x = total_size_x1 * npts;
/*
 * Coerce data to double if necessary. Since we have to make a copy of
 * the first input array anyway (the input array will be overwritten by
 * output array), we coerce it no matter what.
 *
 * First allocate space for double precision input array.
 */
  dx   = (double*)NclMalloc(sizeof(double)*total_size_x);
  if( dx == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"runave: Unable to allocate memory for coercing input array to double precision");
    return(NhlFATAL);
  }
/*
 * Coerce the missing value first.
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
/*
 * Coerce data values with missing values.
 */
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
             dx,
             x,
             total_size_x,
             &missing_dx,
             &missing_x,
             _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
  }
  else {
/*
 * Coerce the data without missing values.
 */
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               dx,
               x,
               total_size_x,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
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
 * Allocate space for work array.
 */
  lwork = npts+2*(*nave/2);
  work = (double *)NclMalloc(lwork*sizeof(double));
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"runave: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  
/*
 * Call the Fortran version of this routine.
 *
 */
  j = 0;
  for( i = 0; i < total_size_x1; i++ ) {
    NGCALLF(drunave,DRUNAVE)(&dx[j],&npts,nave,kopt,&missing_dx.doubleval,
                             work,&lwork,&ier);
    j += npts;
  }
  free(work);
        
  if(type_x != NCL_double) {
/*
 * Copy double values to float values.
 */
    fx = (float*)NclMalloc(sizeof(float)*total_size_x);
    if( fx == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"runave: Unable to allocate memory for return array");
      return(NhlFATAL);
    }
    for( i = 0; i < total_size_x; i++ ) {
      fx[i] = (float)dx[i];
    }
    
    NclFree(dx);
/*
 * Return float values with missing value set.
 */
    return(NclReturnValue((void*)fx,ndims_x,dsizes_x,&missing_rx,NCL_float,0));
  }
  else {
/*
 * Return double values with missing value set.
 */
    return(NclReturnValue((void*)dx,ndims_x,dsizes_x,&missing_dx,NCL_double,0));
  }
}


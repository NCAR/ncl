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
#include "Machine.h"
#include "NclAtt.h"
#include <ncarg/ncl/NclVar.h>
#include "DataSupport.h"
#include "AttSupport.h"
#include "VarSupport.h"
#include "NclCoordVar.h"
#include <ncarg/ncl/NclCallBacksI.h>
#include <ncarg/ncl/NclDataDefs.h>
#include <ncarg/ncl/NclBuiltInSupport.h>
#include <ncarg/gks.h>
#include <math.h>

#define max(x,y)  ((x) > (y) ? (x) : (y))

extern void NGCALLF(dregcoef,DREGCOEF)(double *,double *,int *,double *,
                                       double *,double *,int *,double *,
                                       double *,int *);

NhlErrorTypes regcoef_W( void )
{
/*
 * Input array variables
 */
  void *x, *y;
  double *dx, *dy;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS];
  int ndims_y, dsizes_y[NCL_MAX_DIMENSIONS];
  NclScalar missing_x, missing_y, missing_dx, missing_dy, missing_rx;
  NclBasicDataTypes type_x, type_y;
  int ndims_extra, has_missing_x, has_missing_y, npts;
/*
 * Output array variables
 */
  void *tval;
  double *dtval, *rcoef, *xave, *yave;
  float *rtval, *rrcoef;
  int ndims_tval, dsizes_tval[NCL_MAX_DIMENSIONS];
  int ndims_nptxy, dsizes_nptxy[NCL_MAX_DIMENSIONS];
  int ndims_rcoef, *dsizes_rcoef;
  NclBasicDataTypes type_tval;
  int *nptxy;
/*
 * various
 */
  int i, j, k, ly, lx, ln, ier = 0;
  int total_size_x1, total_size_y1, total_size_x, total_size_y;
  int total_size_rcoef;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
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
  y = (void*)NclGetArgValue(
           1,
           4,
           &ndims_y, 
           dsizes_y,
           &missing_y,
           &has_missing_y,
           &type_y,
           2);
/*
 * Get output vars.
 */
  tval = (void*)NclGetArgValue(
           2,
           4,
           &ndims_tval, 
           dsizes_tval,
           NULL,
           NULL,
           &type_tval,
           1);
  nptxy = (int*)NclGetArgValue(
           3,
           4,
           &ndims_nptxy, 
           dsizes_nptxy,
           NULL,
           NULL,
           NULL,
           1);
/*
 * The x and y coming in can be any dimension, but there are certain rules
 * about having the same dimensions.
 */
  if( ndims_x > ndims_y ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regcoef: The input array y must have as many or more dimensions than x");
    return(NhlFATAL);
  }
/*
 * Check the dimensions of x and y and see if they are the same.
 */
  ndims_extra = ndims_y - ndims_x;
  for(i = 0; i < ndims_x; i++ ) {
    if( dsizes_x[i] != dsizes_y[ndims_extra + i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"regcoef: The rightmost dimensions of y must the same dimensions as x");
      return(NhlFATAL);
    }
  }
/*
 * Get and check number of input points.
 */
  npts = dsizes_x[ndims_x-1];
  if( npts < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regcoef: The rightmost dimension of x must be at least 2");
    return(NhlFATAL);
  }  
/*
 * Compute the total number of elements in our x and y arrays.
 * total_size_x1 is the total number of elements in the X array minus
 * the last dimension (npts). total_size_y1 is the total number of elements
 * in the Y array that go beyond the size of the X array.  For example, if
 * X is dimensioned 2 x 3 x 4, and Y is dimensioned 5 x 2 x 3 x 4, then
 * total_size_x1 is 6 (2x3), total_size_y1 is 5, and npts is 4.
 */
  ndims_rcoef = max(ndims_y-1,1);
  dsizes_rcoef = (int *)NclMalloc(ndims_rcoef*sizeof(int));
  total_size_x1 = 1;
  total_size_y1 = 1;
  dsizes_rcoef[0] = 1;

  for(i = 0; i < ndims_x-1; i++)   total_size_x1 *= dsizes_x[i];
  for(i = 0; i < ndims_extra; i++) total_size_y1 *= dsizes_y[i];
  for(i = 0; i < ndims_y-1; i++)   dsizes_rcoef[i] = dsizes_y[i];

  total_size_x     = total_size_x1 * npts;
  total_size_y     = total_size_x1 * total_size_y1 * npts;
  total_size_rcoef = total_size_x1 * total_size_y1;
/*
 * dimension sizes of tval, nptxy must be the same as rcoef.
 */
  if( ndims_tval != ndims_rcoef || ndims_nptxy != ndims_rcoef ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regcoef: The number of dimensions of tval and nptxy must be one less than the number of dimensions of y (or they must both be scalar if y is just a 1-d array)");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_rcoef; i++ ) {
    if( dsizes_tval[i]  != dsizes_rcoef[i] || 
        dsizes_nptxy[i] != dsizes_rcoef[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"regcoef: The dimensions of tval and nptxy must be the same as the leftmost dimensions of y");
      return(NhlFATAL);
    }
  }
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
 * tval must be a float or double. It doesn't matter what the input type
 * is.
 */
  if(type_tval != NCL_float && type_tval != NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regcoef: tval must be of type float or double");
    return(NhlFATAL);
  }

/*
 * The x and y missing values must be the same.
 */
  if(missing_dx.doubleval != missing_dy.doubleval) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regcoef: The x and y missing values must be the same");
    return(NhlFATAL);
  }

/*
 * Coerce x to double if necessary.
 */
  if(type_x != NCL_double) {
    dx = (double*)NclMalloc(sizeof(double)*total_size_x);
    if( dx == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"regcoef: Unable to allocate memory for coercing x array to double precision");
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
 * Coerce y to double if necessary.
 */
  if(type_y != NCL_double) {
    dy = (double*)NclMalloc(sizeof(double)*total_size_y);
    if( dy == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"regcoef: Unable to allocate memory for coercing y array to double precision");
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
 * Allocate space for double precision tval. There's no need to do a
 * coercion because tval is an output-only variable (i.e, there are no
 * values coming in).  tval can only be float or double, so only allocate
 * space for a d.p. array if tval is float.
 */
  if(type_tval == NCL_float) {
    dtval = (double*)NclMalloc(sizeof(double)*total_size_rcoef);
    if( dtval == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"regcoef: Unable to allocate memory for coercing tval array to double precision");
      return(NhlFATAL);
    }
  }
  else {
/*
 * Input is already double.
 */
    dtval = (double*)tval;
  }
/* 
 * Allocate size for output array (or scalar).
 */
  rcoef = (double *)NclMalloc(total_size_rcoef*sizeof(double));
  xave  = (double *)NclMalloc(sizeof(double));
  yave  = (double *)NclMalloc(sizeof(double));
  if( rcoef == NULL || xave == NULL || yave == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regcoef: Unable to allocate memory for output variables");
    return(NhlFATAL);
  }
/*
 * Call the f77 version of 'regcoef' with the full argument list.
 */
  ly = ln = 0;
  for(i = 1; i <= total_size_y1; i++) {
    lx = 0;
    for(j = 1; j <= total_size_x1; j++) {
      NGCALLF(dregcoef,DREGCOEF)(&dx[lx],&dy[ly],&npts,&missing_dx.doubleval,
                                 &rcoef[ln],&dtval[ln],&nptxy[ln],
                                 xave,yave,&ier);
      ly += npts;
      lx += npts;
      ln ++;
      if (ier == 5) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"regcoef: The x and/or y array contains all missing values");
        return(NhlFATAL);
      }
      if (ier == 6) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"regcoef: The x and/or y array contains less than 3 non-missing values");
        return(NhlFATAL);
      }
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
 * If returning float values, we need to copy the coerced float values
 * back to the original location of tval.  Do this by creating a pointer of
 * type float that points to the original location, and then loop through
 * the values and do the coercion.
 */
  if(type_tval == NCL_float) {
    rtval = (float*)tval;     /* Float pointer to original tval array */
    for( i = 0; i < total_size_rcoef; i++ ) rtval[i]  = (float)dtval[i];
    NclFree(dtval);   /* Free up the double array */
  }

/*
 * Return values. 
 */
  if(type_x != NCL_double && type_y != NCL_double) {
/*
 * None of the input is double, so return floats.
 *
 * First copy double values to float values.
 */
    rrcoef = (float*)NclMalloc(sizeof(float)*total_size_rcoef);
    if( rrcoef == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"regcoef: Unable to allocate memory for coercing return values to floating point");
      return(NhlFATAL);
    }
    for( i = 0; i < total_size_rcoef; i++ ) rrcoef[i] = (float)rcoef[i];
/*
 * Free up double precision values since we don't need them anymore.
 */
    NclFree(rcoef);
/*
 * Return float values with missing value set.
 */
    return(NclReturnValue((void*)rrcoef,ndims_rcoef,dsizes_rcoef,&missing_rx,
                          NCL_float,0));
  }
  else {
/*
 * Return double values with missing value set.
 */
    return(NclReturnValue((void*)rcoef,ndims_rcoef,dsizes_rcoef,&missing_dx,
                          NCL_double,0));
  }
}


NhlErrorTypes regline_W( void )
{
/*
 * Input array variables
 */
  void *x, *y;
  double *dx, *dy;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS];
  int ndims_y, dsizes_y[NCL_MAX_DIMENSIONS];
  NclScalar missing_x, missing_y, missing_dx, missing_dy, missing_rx;
  NclBasicDataTypes type_x, type_y;
  int ndims_extra, has_missing_x, has_missing_y, npts;
/*
 * Output array variables
 */
  double *rcoef, *tval, *xave, *yave;
  float *rrcoef, *rtval,*rxave,*ryave;
  int *nptxy, ier;
/*
 * Attribute variables
 */
  int att_id, dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
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
 * The x and y arrays coming in must be one-dimensional and have
 * the same length.
 */
  if( ndims_x != 1 || ndims_y != 1 || dsizes_x[0] != dsizes_y[0] ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regline: The input arrays must be 1-dimensional and be of the same length");
    return(NhlFATAL);
  }
/*
 * Get and check number of input points.
 */
  npts = dsizes_x[0];
  if( npts < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regline: The length of x and y must be at least 2");
    return(NhlFATAL);
  }  
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
 * The x and y missing values must be the same.
 */
  if(missing_dx.doubleval != missing_dy.doubleval) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regline: The x and y missing values must be the same");
    return(NhlFATAL);
  }

/*
 * Coerce data to double if necessary.
 */
  if(type_x != NCL_double) {
    dx = (double*)NclMalloc(sizeof(double)*dsizes_x[0]);
    if( dx == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"regline: Unable to allocate memory for coercing x array to double precision");
      return(NhlFATAL);
    }
    if(has_missing_x) {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dx,
                 x,
                 dsizes_x[0],
                 &missing_dx,
                 &missing_x,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
    }
    else {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dx,
                 x,
                 dsizes_x[0],
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
 * Coerce y to double.
 */
  if(type_y != NCL_double) {
    dy = (double*)NclMalloc(sizeof(double)*dsizes_y[0]);
    if( dy == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"regline: Unable to allocate memory for coercing y array to double precision");
      return(NhlFATAL);
    }
    if(has_missing_y) {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dy,
                 y,
                 dsizes_y[0],
                 &missing_dy,
                 &missing_y,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_y)));
    }
    else {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dy,
                 y,
                 dsizes_y[0],
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
 * Allocate space for output variables.
 */
  rcoef = (double *)NclMalloc(sizeof(double));
  tval  = (double *)NclMalloc(sizeof(double));
  xave  = (double *)NclMalloc(sizeof(double));
  yave  = (double *)NclMalloc(sizeof(double));
  nptxy =   (int *)NclMalloc(sizeof(int));
  if( rcoef == NULL || tval == NULL || xave == NULL || yave == NULL ||
      nptxy == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regline: Unable to allocate memory for output values");
    return(NhlFATAL);
  }

/*
 * Call the f77 version of 'regline' with the full argument list.
 */
  NGCALLF(dregcoef,DREGCOEF)(&dx[0],&dy[0],&npts,&missing_dx.doubleval,
                             rcoef,tval,nptxy,xave,yave,&ier);
  if (ier == 5) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regline: The x and/or y array contains all missing values");
    return(NhlFATAL);
  }
  if (ier == 6) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regline: The x and/or y array contains less than 3 non-missing values");
    return(NhlFATAL);
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
 * Set up variable to return.
 */
  dsizes[0] = 1;

  if(type_x != NCL_double && type_y != NCL_double) {
/*
 * None of the input is double, so return floats.
 *
 * Allocate space for coercing output to float.
 */
    rrcoef = (float *)NclMalloc(sizeof(float));
    rtval  = (float *)NclMalloc(sizeof(float));
    rxave  = (float *)NclMalloc(sizeof(float));
    ryave  = (float *)NclMalloc(sizeof(float));
    if( rrcoef == NULL || rtval == NULL || rxave == NULL || ryave == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"regline: Unable to allocate memory for coercing output values back to floating point");
      return(NhlFATAL);
    }
/*
 * Coerce double to float.
 */
    *rrcoef = (float)*rcoef;
    *rtval  = (float)*tval;
    *rxave  = (float)*xave;
    *ryave  = (float)*yave;
/*
 * Free up variables holding double precision values.
 */
    NclFree(rcoef);
    NclFree(tval);
    NclFree(xave);
    NclFree(yave);
/*
 * Set up return structure.
 */
    return_md = _NclCreateVal(
                      NULL,
                      NULL,
                      Ncl_MultiDValData,
                      0,
                      (void*)rrcoef,
                      &missing_rx,
                      1,
                      dsizes,
                      TEMPORARY,
                      NULL,
                      (NclObjClass)nclTypefloatClass
                      );
/*
 * Set up attributes to return.
 */
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   rtval,
                   NULL,
                   1,
                   dsizes,
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypefloatClass
                   );
    _NclAddAtt(
               att_id,
               "tval",
               att_md,
               NULL
               );

    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   nptxy,
                   NULL,
                   1,
                   dsizes,
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypeintClass
                   );
    _NclAddAtt(
               att_id,
               "nptxy",
               att_md,
               NULL
               );
    
    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   rxave,
                   NULL,
                   1,
                   dsizes,
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypefloatClass
                   );
    _NclAddAtt(
               att_id,
               "xave",
               att_md,
               NULL
               );

    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   ryave,
                   NULL,
                   1,
                   dsizes,
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypefloatClass
                   );
    _NclAddAtt(
               att_id,
               "yave",
               att_md,
               NULL
               );
  }
  else {
/* 
 * Either x and/or y are double, so return doubles.
 *
 * Set up return structure.
 */
    return_md = _NclCreateVal(
                      NULL,
                      NULL,
                      Ncl_MultiDValData,
                      0,
                      (void*)rcoef,
                      &missing_dx,
                      1,
                      dsizes,
                      TEMPORARY,
                      NULL,
                      (NclObjClass)nclTypedoubleClass
                      );
/*
 * Set up attributes to return.
 */
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   tval,
                   NULL,
                   1,
                   dsizes,
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypedoubleClass
                   );
    _NclAddAtt(
               att_id,
               "tval",
               att_md,
               NULL
               );

    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   nptxy,
                   NULL,
                   1,
                   dsizes,
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypeintClass
                   );
    _NclAddAtt(
               att_id,
               "nptxy",
               att_md,
               NULL
               );
    
    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   xave,
                   NULL,
                   1,
                   dsizes,
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypedoubleClass
                   );
    _NclAddAtt(
               att_id,
               "xave",
               att_md,
               NULL
               );

    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   yave,
                   NULL,
                   1,
                   dsizes,
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypedoubleClass
                   );
    _NclAddAtt(
               att_id,
               "yave",
               att_md,
               NULL
               );
  }
  tmp_var = _NclVarCreate(
                          NULL,
                          NULL,
                          Ncl_Var,
                          0,
                          NULL,
                          return_md,
                          NULL,
                          att_id,
                          NULL,
                          RETURNVAR,
                          NULL,
                          TEMPORARY
                          );
/*
 * Return output grid and attributes to NCL.
 */
  return_data.kind = NclStk_VAR;
  return_data.u.data_var = tmp_var;
  _NclPlaceReturn(return_data);
  return(NhlNOERROR);
}


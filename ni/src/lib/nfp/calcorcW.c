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

#define max(x,y)  ((x) > (y) ? (x) : (y))

extern void NGCALLF(dcalcorc,DCALCORC)(double*,double*,double*,double*,int*,
                                       double*,double*,double*,int*);

extern void NGCALLF(dstat2,DSTAT2)(double*,int*,double*,double*,double*,
				   double*,int*,int*);

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
  int i, j, lx, ly, lc, ier, ier_count, nxy, dimsizes_same;
  int total_size_x1, total_size_x, total_size_y1, total_size_y;
  int total_size_corc;
  double xave, xstd, xvar;
  int nptusx;
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
 * If all the dimensions of x and y are the same, then we don't treat
 * the dimensions differently:  i.e. if x is 64 x 128 x 21 and y is
 * 64 x 128 x 21, then what gets returned will be 64 x 128 x (mxlag+1),
 * and NOT 64 x 128 x 64 x 128 x (mxlag+1).
 */
  if(ndims_x == ndims_y) {
    dimsizes_same = 1;
    for(i = 0; i < ndims_x; i++) {
      if(dsizes_x[i] != dsizes_y[i]) {
	dimsizes_same = 0;
	break;
      }
    }
  }
  else {
    dimsizes_same = 0;
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
 *
 * First coerce the x missing value, if there is one.  If there isn't,
 * a default value is used.
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
 * No missing value is set for x, so use the default missing value.
 */ 
    if(type_x != NCL_double) {
      missing_dx.doubleval = (double)((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
      missing_rx.floatval = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
    }
    else {
      missing_dx.doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
    }
  }
/*
 * Coerce the y missing value, if there is one.
 */
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
 * No missing value is set for y, so use the default missing value.
 */ 
    if(type_y != NCL_double) {
      missing_dy.doubleval = (double)((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
    }
    else {
      missing_dy.doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
    }
  }

/*
 * Coerce x to double if necessary.
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
 * x is already double.
 */
    dx = (double*)x;
  }

/*
 * Coerce y to double if necessary.
 */
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
 * y is already double.
 */
    dy = (double*)y;
  }
/* 
 * Get size of output variable, which is equal to the product of all but
 * the last dimension of x and y (unless the dimension sizes of x and y
 * are the same, in which case the output will be the product of the all
 * but the last dimension of x).
 */
  if(dimsizes_same) {
    ndims_corc = max(1,ndims_x-1);
    total_size_corc = total_size_x1;
    if(ndims_x == 1) {
      dsizes_corc[0] = 1;
    }
    else {
      for( i = 0; i < ndims_x-1; i++ ) dsizes_corc[i] = dsizes_x[i];
    }
  }
  else {
    total_size_corc = total_size_x1 * total_size_y1;
    ndims_corc = max(1,ndims_x + ndims_y - 2);
    dsizes_corc[0] = 1;

    for( i = 0; i < ndims_x-1; i++ ) dsizes_corc[i] = dsizes_x[i];
    for( i = 0; i < ndims_y-1; i++ ) dsizes_corc[ndims_x-1+i] = dsizes_y[i];
  }
  corc = (double*)NclMalloc(total_size_corc*sizeof(double));
  if (corc == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"escorc: Unable to allocate space for output array");
    return(NhlFATAL);
  }

/*
 * Call the f77 version of 'descros' with the full argument list.
 */
  ier = ier_count = lx = lc = 0;

  if(dimsizes_same) {
    for(i = 1; i <= total_size_x1; i++) {
      xvar = xave = xstd = missing_dx.doubleval;
      NGCALLF(dstat2,DSTAT2)(&dx[lx],&nxy,&missing_dx.doubleval,&xave,&xvar,
			     &xstd,&nptusx,&ier);
      NGCALLF(dcalcorc,DCALCORC)(&dx[lx],&xave,&xstd,&missing_dx.doubleval,
				 &nxy,&dy[lx],&missing_dy.doubleval,
				 &corc[lc],&ier);
	
      if(ier < 0) ier_count++;
      lc++;
      lx += nxy;
    }
  }
  else {
    for(i = 1; i <= total_size_x1; i++) {
      ly = 0;
      xvar = xave = xstd = missing_dx.doubleval;
      NGCALLF(dstat2,DSTAT2)(&dx[lx],&nxy,&missing_dx.doubleval,&xave,&xvar,
			     &xstd,&nptusx,&ier);
      for(j = 1; j <= total_size_y1; j++) {
	NGCALLF(dcalcorc,DCALCORC)(&dx[lx],&xave,&xstd,&missing_dx.doubleval,
				   &nxy,&dy[ly],&missing_dy.doubleval,
				   &corc[lc],&ier);
	
	if(ier < 0) ier_count++;
	
	lc++;
	ly += nxy;
      }
      lx += nxy;
    }
  }
  if(ier_count > 0) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"escorc: Non-fatal conditions encountered in series or xstd = 0.0");
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
 * Return values. 
 */
  if(type_x != NCL_double && type_y != NCL_double) {
/*
 * Neither input array is double, so return float values.
 * 
 * First copy double values to float values.
 */
    rcorc = (float*)NclMalloc(sizeof(float)*total_size_corc);
    if( rcorc == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"escorc: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    for( i = 0; i < total_size_corc; i++ ) rcorc[i] = (float)corc[i];
/*
 * Free double precision array since we don't need it anymore.
 */
    NclFree(corc);

/*
 * Return float values.  A missing value is returned regardless if an
 * x missing value was originally set (the default is used if none set).
 */
    return(NclReturnValue((void*)rcorc,ndims_corc,dsizes_corc,&missing_rx,
                          NCL_float,0));
  }
  else {
/*
 * At least one input array was double, so return double values.
 */
    return(NclReturnValue((void*)corc,ndims_corc,dsizes_corc,&missing_dx,
                          NCL_double,0));
  }
}



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
#include <ncarg/ncl/NclDataDefs.h>
#include <ncarg/ncl/NclBuiltInSupport.h>
#include <ncarg/gks.h>
#include <math.h>

#define max(x,y)  ((x) > (y) ? (x) : (y))

extern void NGCALLF(dvibeta,DVIBETA)(double *,double *,int *,double *,
                                     int *,double *,double *,double *,
                                     double *,double *,double *,int *);

NhlErrorTypes vibeta_W( void )
{
/*
 * Input array variables
 */
  void *p, *x, *psfc, *pbot, *ptop;
  double *dp, *dx, *dpsfc, *dpbot, *dptop;
  int ndims_p, dsizes_p[NCL_MAX_DIMENSIONS];
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  int ndims_psfc, dsizes_psfc[NCL_MAX_DIMENSIONS], has_missing_psfc;
  int dsizes_pbot[NCL_MAX_DIMENSIONS], dsizes_ptop[NCL_MAX_DIMENSIONS];
  NclScalar missing_x, missing_dx, missing_rx;
  NclScalar missing_psfc, missing_dpsfc;
  NclBasicDataTypes type_p, type_x, type_psfc, type_pbot, type_ptop;
  int *linlog;
/*
 * Output array variables
 */
  double *vint;
  float *rvint;
/*
 * various
 */
  int i, j, l, ier = 0, nlev, total_size_x, total_size_psfc;
  double plvcrt, xsfc, any_double = 0;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  p = (void*)NclGetArgValue(
           0,
           6,
           &ndims_p, 
           dsizes_p,
           NULL,
           NULL,
           &type_p,
           2);
  x = (void*)NclGetArgValue(
           1,
           6,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           2);
  linlog = (int*)NclGetArgValue(
           2,
           6,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           2);
  psfc = (void*)NclGetArgValue(
           3,
           6,
           &ndims_psfc, 
           dsizes_psfc,
           &missing_psfc,
           &has_missing_psfc,
           &type_psfc,
           2);
  pbot = (void*)NclGetArgValue(
           4,
           6,
           NULL,
           dsizes_pbot,
           NULL,
           NULL,
           &type_pbot,
           2);
  ptop = (void*)NclGetArgValue(
           5,
           6,
           NULL,
           dsizes_ptop,
           NULL,
           NULL,
           &type_ptop,
           2);
/*
 * Some error checking.
 */
  if( (ndims_x > 1 && ndims_psfc != ndims_x-1) || 
      (ndims_x == 1 && dsizes_psfc[0] != 1)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vibeta: 'psfc' must have one less dimension than 'x' or else be a constant" );
    return(NhlFATAL);
  }

  total_size_psfc = 1;
  for( i = 0; i < ndims_x-1; i++ ) {
    if( dsizes_psfc[i] != dsizes_x[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vibeta: The leftmost dimensions of x and psfc must be the same size" );
      return(NhlFATAL);
    }
    total_size_psfc *= dsizes_psfc[i];
  }
  total_size_x = total_size_psfc * dsizes_x[ndims_x-1];

  if( dsizes_p[0] != dsizes_x[ndims_x-1] ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vibeta: The rightmost dimension of x and p must be the same size" );
    return(NhlFATAL);
  }
  nlev = dsizes_p[0];

  if( nlev < 3 || nlev > 150) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vibeta: nlev must be at least 3 and less than 151" );
    return(NhlFATAL);
  }

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
 * Check for psfc missing value.
 */
  if(has_missing_psfc) {
/*
 * Coerce missing value to double.
 */
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               &missing_dpsfc,
               &missing_psfc,
               1,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_psfc)));
  }
/*
 * Coerce x to double if necessary.
 */
  if(type_x != NCL_double) {
    dx = (double*)NclMalloc(sizeof(double)*total_size_x);
    if( dx == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vibeta: Unable to allocate memory for coercing x array to double precision");
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
    any_double = 1;
/*
 * x is already double.
 */
    dx = (double*)x;
  }
/*
 * Coerce p to double if necessary.
 */
  if(type_p != NCL_double) {
    dp = (double*)NclMalloc(sizeof(double)*dsizes_p[ndims_p-1]);
    if( dp == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vibeta: Unable to allocate memory for coercing p array to double precision");
      return(NhlFATAL);
    }
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               dp,
               p,
               dsizes_p[ndims_p-1],
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
 * Coerce psfc to double if necessary.
 */
  if(type_psfc != NCL_double) {
    dpsfc = (double*)NclMalloc(sizeof(double)*total_size_psfc);
    if( dpsfc == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vibeta: Unable to allocate memory for coercing psfc array to double precision");
      return(NhlFATAL);
    }
    if(has_missing_psfc) {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dpsfc,
                 psfc,
                 total_size_psfc,
                 &missing_dpsfc,
                 &missing_psfc,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_psfc)));
    }
    else {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dpsfc,
                 psfc,
                 total_size_psfc,
                 NULL,
                 NULL,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_psfc)));
    }
  }
  else {
    any_double = 1;
/*
 * psfc is already double.
 */
    dpsfc = (double*)psfc;
  }
/*
 * Test for presence of missing values in psfc.
 */
  if(has_missing_psfc) {
    for( i = 0; i < total_size_psfc; i++ ) {
      if(dpsfc[i] == missing_dpsfc.doubleval) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"vibeta: psfc must not contain any missing values" );
        return(NhlFATAL);
      }
    }
  }
/*
 * Coerce pbot to double if necessary.
 */
  if(type_pbot != NCL_double) {
    dpbot = (double*)NclMalloc(sizeof(double)*dsizes_pbot[0]);
    if( dpbot == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vibeta: Unable to allocate memory for coercing pbot array to double precision");
      return(NhlFATAL);
    }
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               dpbot,
               pbot,
               dsizes_pbot[0],
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_pbot)));
  }
  else {
    any_double = 1;
/*
 * pbot is already double.
 */
    dpbot = (double*)pbot;
  }
/*
 * Coerce ptop to double if necessary.
 */
  if(type_ptop != NCL_double) {
    dptop = (double*)NclMalloc(sizeof(double)*dsizes_ptop[0]);
    if( dptop == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vibeta: Unable to allocate memory for coercing ptop array to double precision");
      return(NhlFATAL);
    }
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               dptop,
               ptop,
               dsizes_ptop[0],
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_ptop)));
  }
  else {
    any_double = 1;
/*
 * ptop is already double.
 */
    dptop = (double*)ptop;
  }
/*
 * Coerce p to double if necessary.
 */
  if(type_p != NCL_double) {
    dp = (double*)NclMalloc(sizeof(double)*dsizes_p[0]);
    if( dp == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vibeta: Unable to allocate memory for coercing p array to double precision");
      return(NhlFATAL);
    }
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               dp,
               p,
               dsizes_p[0],
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
  }
  else {
    any_double = 1;
/*
 * p is already double.
 */
    dp = (double*)p;
  }
/*
 * Some more error checking.
 */
  if(*dptop >= *dpbot) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vibeta: ptop must be less than pbot" );
    return(NhlFATAL);
  }

  if(dp[nlev-1] < *dptop) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vibeta: The last element of 'p' must be greater than or equal to ptop" );
    return(NhlFATAL);
  }

/*
 * Set some other input parameters. 
 */
  plvcrt = dp[nlev-1];
  xsfc = dx[0];
/*
 * Allocate space for output value.
 */
  vint = (double*)NclMalloc(total_size_psfc*sizeof(double));

/*
 * Call the f77 version of 'vibeta' with the full argument list.
 */
  l = 0;
  for( i = 0; i < total_size_psfc; i++ ) {
    NGCALLF(dvibeta,DVIBETA)(dp,&dx[l],&nlev,&missing_dx.doubleval,
                             linlog,&dpsfc[i],&xsfc,dpbot,dptop,
                             &plvcrt,&vint[i],&ier);
    if(ier == -999) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vibeta: there must be at least three levels with data above the surface" );
      return(NhlFATAL);
    }
    l += nlev;
  }

/*
 * free memory.
 */
  if((void*)dx != x) NclFree(dx);
  if((void*)dp != p) NclFree(dp);
  if((void*)dpsfc != psfc) NclFree(dpsfc);
  if((void*)dpbot != pbot) NclFree(dpbot);
  if((void*)dptop != ptop) NclFree(dptop);
/*
 * Check error.
 */
  if(ier) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vibeta: ier = %d", ier);
    return(NhlFATAL);
  }
/*
 * Return.
 */
  if(!any_double) {
/*
 * None of the input is double, so return float values.
 *
 * First copy double values to float values.
 */
    rvint = (float*)NclMalloc(sizeof(float)*total_size_psfc);
    if( rvint == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vibeta: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    for( i = 0; i < total_size_psfc; i++ ) rvint[i] = (float)vint[i];
/*
 * Free double precision values.
 */
    NclFree(vint);
/*
 * Return float values with missing value set.
 */
    return(NclReturnValue((void*)rvint,ndims_psfc,dsizes_psfc,&missing_rx,NCL_float,0));
  }
  else {
/*
 * Return double values with missing value set.
 */
    return(NclReturnValue((void*)vint,ndims_psfc,dsizes_psfc,&missing_dx,NCL_double,0));
  }
}

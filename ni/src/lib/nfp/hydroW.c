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

extern void NGCALLF(dhydro,DHYDRO)(double *,double *,double *,int *,
				   double *,int *);

NhlErrorTypes hydro_W( void )
{
/*
 * Declare various variables for random purposes.
 */
  int i, j, nlvl, ier=0;
/*
 * Input array variables
 */
  void *p, *tkv, *zsfc;
  double *dp, *dtkv, *dzsfc;
  int ndims_p, dsizes_p[NCL_MAX_DIMENSIONS], has_missing_p;
  int ndims_tkv, dsizes_tkv[NCL_MAX_DIMENSIONS], has_missing_t;
  int ndims_zsfc, dsizes_zsfc[NCL_MAX_DIMENSIONS], has_missing_z;
  NclBasicDataTypes type_p, type_tkv, type_zsfc;
/*
 * Output array variables
 */
  double *zh;
  float *rzh;
  int size_zsfc, size_p;

/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */

/*
 * Retrieve argument #1
 */
  p = (void*)NclGetArgValue(
          0,
          3,
          &ndims_p,
          dsizes_p,
          NULL,
          &has_missing_p,
          &type_p,
          2);

/*
 * Retrieve argument #2
 */
  tkv = (void*)NclGetArgValue(
          1,
          3,
          &ndims_tkv,
          dsizes_tkv,
          NULL,
          &has_missing_t,
          &type_tkv,
          2);

/*
 * Retrieve argument #3
 */
  zsfc = (void*)NclGetArgValue(
          2,
          3,
          &ndims_zsfc,
          dsizes_zsfc,
          NULL,
          &has_missing_z,
          &type_zsfc,
          2);

/*
 * Check number of dimensions and/or dimension sizes for p and tkv.
 */
  if(ndims_tkv != ndims_p) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"hydro: The input arrays 'p' and 'tkv' must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_p; i++ ) {
    if (dsizes_tkv[i] != dsizes_p[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"hydro: The input arrays 'p' and 'tkv' must have the same dimensions");
      return(NhlFATAL);
    }
  }

/*
 * No missing values are allowed.
 */
  if(has_missing_t || has_missing_p || has_missing_z) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"hydro: The input arrays 'p', 'tkv', and 'zsfc' cannot contain any missing values");
    return(NhlFATAL);
  }

/*
 * Check number of dimensions and/or dimension sizes for zsfc.
 */
  if ((ndims_p == 1 && ndims_zsfc != 1) || 
      (ndims_p > 1 && ndims_zsfc != ndims_p-1)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"hydro: The array 'zsfc' must be a scalar or one less dimension than the other arrays");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_p-1; i++ ) {
    if (dsizes_zsfc[i] != dsizes_p[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"hydro: The dimensions of the input array 'zsfc' must be the same as the arrays 'p' and 'tkv', minus the last dimension");
      return(NhlFATAL);
    }
  }

/*
 * Compute the total size of the output array.
 */
  nlvl = dsizes_p[ndims_p-1];
  if (nlvl < 1) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"hydro: 'nlvl' (the last dimension of 'p' and 'tkv') must be at least one");
    return(NhlFATAL);
  }

  size_zsfc = 1;
  for( i = 0; i < ndims_zsfc; i++ ) {
    size_zsfc *= dsizes_zsfc[i];
  }
  size_p = nlvl * size_zsfc;
/*
 * Coerce data to double if necessary.
 */
  if(type_p != NCL_double) {
    dp = (double*)NclMalloc(sizeof(double)*size_p);
    if( dp == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"hydro: Unable to allocate memory for coercing p array to double precision");
      return(NhlFATAL);
    }
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
	       dp,
	       p,
	       size_p,
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
 * Coerce tkv to double precision.
 */
  if(type_tkv != NCL_double) {
    dtkv = (double*)NclMalloc(sizeof(double)*size_p);
    if( dtkv == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"hydro: Unable to allocate memory for coercing tkv array to double precision");
      return(NhlFATAL);
    }
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
	       dtkv,
	       tkv,
	       size_p,
	       NULL,
	       NULL,
	       _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_tkv)));
  }
  else {
/*
 * Input is already double.
 */
    dtkv = (double*)tkv;
  }

/*
 * Coerce tkv to double precision.
 */
  if(type_zsfc != NCL_double) {
    dzsfc = (double*)NclMalloc(sizeof(double)*size_zsfc);
    if( dzsfc == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"hydro: Unable to allocate memory for coercing zsfc array to double precision");
      return(NhlFATAL);
    }
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
	       dzsfc,
	       zsfc,
	       size_zsfc,
	       NULL,
	       NULL,
	       _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_zsfc)));
  }
  else {
/*
 * Input is already double.
 */
    dzsfc = (double*)zsfc;
  }

/*
 * Allocate space for output value.
 */
  zh = (double *)NclMalloc(size_p*sizeof(double));
  if( zh == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"hydro: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Call the Fortran version of this routine.
 */
  j = 0;
  for( i = 0; i < size_zsfc; i++ ) {
    NGCALLF(dhydro,DHYDRO)(&dp[j],&dtkv[j],&dzsfc[i],&nlvl,&zh[j],&ier);
    j += nlvl;
  }
/*
 * free memory.
 */
  if((void*)dp != p) {
    NclFree(dp);
  }
  if((void*)dtkv != tkv) {
    NclFree(dtkv);
  }
  if((void*)dzsfc != zsfc) {
    NclFree(dzsfc);
  }

/*
 * Return values
 */
  if(type_p != NCL_double && type_tkv != NCL_double && 
     type_zsfc != NCL_double) {
/*
 * Copy double values to float values.
 */
    rzh = (float*)NclMalloc(sizeof(float)*size_p);
    if( rzh == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"hydro: Unable to allocate memory for return array");
      return(NhlFATAL);
    }
    for( i = 0; i < size_p; i++ ) {
      rzh[i] = (float)zh[i];
    }
/*
 * Free double precision values.
 */
    free(zh);
/*
 * Return float values.
 */
    return(NclReturnValue((void*)rzh,ndims_p,dsizes_p,NULL,NCL_float,0));
  }
  else {
/*
 * Return float values.
 */
    return(NclReturnValue((void*)zh,ndims_p,dsizes_p,NULL,NCL_double,0));
  }
}

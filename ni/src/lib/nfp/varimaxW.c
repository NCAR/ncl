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
#include <ncarg/ncl/NclVar.h>
#include "DataSupport.h"
#include "AttSupport.h"
#include "VarSupport.h"
#include "NclCoordVar.h"
#include <ncarg/ncl/NclCallBacksI.h>
#include <ncarg/ncl/NclDataDefs.h>
#include <ncarg/ncl/NclBuiltInSupport.h>
#include <math.h>
#include <ncarg/gks.h>

extern void NGCALLF(vors,VORS)(int *, int *, double *, double *, double *, 
                               double *, int *);

NhlErrorTypes eof_varimax_W( void )
{
/*
 * Input array variables
 */
  void *evec;
  double *devec;
  int ndims_evec, dsizes_evec[NCL_MAX_DIMENSIONS], has_missing_evec;
  NclScalar missing_evec, missing_devec;
  NclBasicDataTypes type_evec;
  int nvar, nfac, ldevec, total_size_evec;
/*
 * Work array variables.
 */
  double *a, *b, *w;
/*
 * Output array variable
 */
  float  *revec_out;
  int i, found_missing;
/*
 * Retrieve parameters
 */
  evec = (void*)NclGetArgValue(
           0,
           1,
           &ndims_evec, 
           dsizes_evec,
           &missing_evec,
           &has_missing_evec,
           &type_evec,
           2);
/*
 * Check dimensions.
 */
  if( ndims_evec < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eof_varimax: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }

/*
 * Calculate size of output array.
 */
  nfac = dsizes_evec[0];

  nvar = 1;
  for( i = 1; i <= ndims_evec-1; i++ ) nvar *= dsizes_evec[i];
  ldevec = nvar;

  if( nvar < 1 || nfac < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eof_varimax: The dimensions of the input array must both be at least 1");
    return(NhlFATAL);
  }
  total_size_evec = nvar * nfac;

/*
 * Coerce evec missing value to double.
 */
  if(has_missing_evec) {
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               &missing_devec,
               &missing_evec,
               1,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_evec)));
  }

/*
 * Coerce evec to double no matter what, since we need to make a copy of
 * the input array anyway.
 */
  devec = (double*)NclMalloc(sizeof(double)*total_size_evec);
  if( devec == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eof_varimax: Unable to allocate memory for coercing evec array to double precision");
    return(NhlFATAL);
  }
  if(has_missing_evec) {
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               devec,
               evec,
               total_size_evec,
               &missing_devec,
               &missing_evec,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_evec)));
  }
  else {
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               devec,
               evec,
               total_size_evec,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_evec)));
  }

/*
 * Check for a missing value.
 */
  found_missing = 0;
  if(has_missing_evec) {
    i = 0;
    while( i < total_size_evec && !found_missing ) {
      if(devec[i] == missing_devec.doubleval) found_missing = 1;
      i++;
    }
  }
  if(found_missing) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eof_varimax: The input array contains missing values.");
    return(NhlFATAL);
  }

/*
 * Allocate memory for work arrays.
 */
  a = (double *)NclMalloc(nvar*sizeof(double));
  b = (double *)NclMalloc(nvar*sizeof(double));
  w = (double *)NclMalloc(nvar*sizeof(double));
  if( a == NULL || b == NULL || w == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eof_varimax: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
/*
 * Call the Fortran 77 version of 'vors' with the full argument list.
 */
  NGCALLF(vors,VORS)(&nvar, &nfac, devec, a, b, w, &ldevec);

/*
 * Free unneeded memory.
 */
  NclFree(w);
  NclFree(a);
  NclFree(b);

/*
 * Convert input array so that tmp_md is not used and also to preserve input
 * array.
 */
  if(type_evec == NCL_float) {
    revec_out = (float *)NclMalloc(total_size_evec*sizeof(float));
    if( revec_out == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eof_varimax: Unable to allocate memory for floating point output array");
      return(NhlFATAL);
    }
    for( i = 0; i < total_size_evec; i++ ) revec_out[i] = (float)devec[i];
/*
 * Free double precision array.
 */
    NclFree(devec);
/*
 * Return float values
 */
    return(NclReturnValue((void*)revec_out,ndims_evec,dsizes_evec,NULL,
                          NCL_float,0));
  }
  else {
/*
 * Return double values
 */
    return(NclReturnValue((void*)devec,ndims_evec,dsizes_evec,NULL,
                          NCL_double,0));
  }
}



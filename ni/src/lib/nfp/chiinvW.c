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
#include "wrapper.h"
extern void NGCALLF(chisub,CHISUB)(double*,double*,double*);

NhlErrorTypes chiinv_W( void )
{
/*
 * Input array variables
 */
  void *p, *df;
  double *dp, *ddf;
  int ndims_p, dsizes_p[NCL_MAX_DIMENSIONS];
  int ndims_df, dsizes_df[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_p, type_df;
/*
 * output variable 
 */
  double *chi;
  float *rchi;
  int size_chi;
/*
 * Declare various variables for random purposes.
 */
  int i;
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
          2,
          &ndims_p,
          dsizes_p,
          NULL,
          NULL,
          &type_p,
          2);
/*
 * Retrieve argument #2
 */
  df = (void*)NclGetArgValue(
          1,
          2,
          &ndims_df,
          dsizes_df,
          NULL,
          NULL,
          &type_df,
          2);
/*
 * Check number of dimensions and/or dimension sizes for arguments #1
 * and #2 .
 */
  if (ndims_p != ndims_df) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"chiinv: The two input arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_p; i++ ) {
    if (dsizes_p[i] != dsizes_df[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"chiinv: The two input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Calculate size of output value.
 */
  size_chi = 1;
  for( i = 0; i < ndims_p; i++ ) size_chi *= dsizes_p[i];

/*
 * Coerce p and df to double if necessary.
 */
  dp  = coerce_input_double( p, type_p,size_chi,0,NULL,NULL,NULL);
  ddf = coerce_input_double(df,type_df,size_chi,0,NULL,NULL,NULL);
  if(ddf == NULL || dp == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"chiinv: unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }

/*
 * Allocate space for output array.
 */
  chi = (double*)NclMalloc(size_chi*sizeof(double));
  if( chi == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"chiinv: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Call the Fortran version of this routine.
 *
 */
  for( i = 0; i < size_chi; i++ ) {
    NGCALLF(chisub,CHISUB)(&dp[i],&ddf[i],&chi[i]);
  }
/*
 * free memory.
 */
  if((void*)dp  != p) NclFree(dp);
  if((void*)ddf != df) NclFree(ddf);

/*
 * Copy double values to float values.
 */
  if(type_p != NCL_double && type_df != NCL_double) {
/*
 * Neither input array was double, so copy return floats.
 *
 * First copy double values to float array.
 */
    rchi = coerce_output_float(chi, NULL, size_chi, 0);
    if( rchi == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"chiinv: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
/*
 * Return float values.
 */
    return(NclReturnValue((void*)rchi,ndims_p,dsizes_p,NULL,NCL_float,0));
  }
  else {
/*
 * One or both input arrays were double, so return double precision values.
 */
    return(NclReturnValue((void*)chi,ndims_p,dsizes_p,NULL,NCL_double,0));
  }
}

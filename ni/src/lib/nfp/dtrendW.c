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
#include <math.h>
#include <ncarg/gks.h>

NhlErrorTypes dtrend_W( void )
{
/*
 * Input array variables
 */
  float *x;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x, found_missing;
  NclScalar missing_x;
  logical *return_slope;
/*
 * Output array variables
 */
  float *x2, xmean, xvari, xvaro;
/*
 * Attribute variables
 */
  int att_id, dsizes_slope[NCL_MAX_DIMENSIONS];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
  float *slope;
/*
 * Declare various variables for random purposes.
 */
  int i, j, l, npts, size_x, ier, iopt = 1;
  float c[3];
/*
 * Retrieve arguments.
 */
  x = (float*)NclGetArgValue(
          0,
          2,
          &ndims_x,
          dsizes_x,
		  &missing_x,
		  &has_missing_x,
          NULL,
          2);

  npts = dsizes_x[ndims_x-1];
  if( npts < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dtrend: The last dimension of x must be greater than 2");
    return(NhlFATAL);
  }
  
  return_slope = (logical*)NclGetArgValue(
          1,
          2,
          NULL,
          NULL,
		  NULL,
		  NULL,
          NULL,
          2);
/*
 * Compute the total size of the output array (minus the last dimension).
 */
  size_x = 1;
  l = 0;
  for( i = 0; i < ndims_x-1; i++ ) {
	size_x *= dsizes_x[i];
  }
/*
 * Compute size of slope.
 */
  if(*return_slope) {
	slope = (float *)NclMalloc(size_x*sizeof(float));
	if( slope == NULL ) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"dtrend: Cannot allocate space for 'slope'");
	  return(NhlFATAL);
	}
  }
  
/*
 * Call the Fortran version of this routine.
 *
 */
  x2 = (float *)NclMalloc(size_x*npts*sizeof(float));
  if( x2 == NULL ) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"dtrend: Cannot allocate space for output array");
	return(NhlFATAL);
  }

  l = 0;
  for( i = 0; i < size_x; i++ ) {
/*
 * Check for missing values.
 */
	found_missing = 0;
	if(has_missing_x) {
	  j = 0;
	  while( j < npts && !found_missing ) {
		if(x[l+j] == missing_x.floatval) {
		  found_missing = 1;
		}
		j++;
	  }
	}
	if(found_missing) {
	  for(j = 0; j < npts; j++) {
		x2[l+j] = missing_x.floatval;
	  }
	  NhlPError(NhlWARNING,NhlEUNKNOWN,"dtrend: An input array contains missing values. No dtrending performed on this array.");
	}
	else {

	  for( j = 0; j < npts; j++ ) {
		x2[l+j] = x[l+j];
	  }
	  NGCALLF(dtrndx, DTRNDX)(&x2[l],&npts,&iopt,&xmean,&xvari,&xvaro,c,&ier);
	  if(*return_slope) slope[i] = c[1];
	}
	l += npts;
  }
  if(*return_slope) {
/*
 * Set up variable to return.
 */
	if(has_missing_x) {
	  return_md = _NclCreateVal(
								NULL,
								NULL,
								Ncl_MultiDValData,
								0,
								(void*)x2,
								&missing_x,
								ndims_x,
								dsizes_x,
								TEMPORARY,
								NULL,
								(NclObjClass)nclTypefloatClass
								);
	}
	else {
	  return_md = _NclCreateVal(
								NULL,
								NULL,
								Ncl_MultiDValData,
								0,
								(void*)x2,
								NULL,
								ndims_x,
								dsizes_x,
								TEMPORARY,
								NULL,
								(NclObjClass)nclTypefloatClass
								);
	}
/*
 * Set up attributes to return.
 */
	att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

	dsizes_slope[0] = size_x;
	att_md = _NclCreateVal(
						   NULL,
						   NULL,
						   Ncl_MultiDValData,
						   0,
						   slope,
						   NULL,
						   1,
						   dsizes_slope,
						   TEMPORARY,
						   NULL,
						   (NclObjClass)nclTypefloatClass
						   );
	_NclAddAtt(
			   att_id,
			   "slope",
			   att_md,
			   NULL
			   );

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
  else {
	if(has_missing_x) {
	  return(NclReturnValue((void*)x2,ndims_x,dsizes_x,&missing_x,NCL_float,0));
	}
	else {
	  return(NclReturnValue((void*)x2,ndims_x,dsizes_x,NULL,NCL_float,0));
	}
  }
}


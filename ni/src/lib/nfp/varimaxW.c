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
  NclStackEntry data;
  NclMultiDValData tmp_md = NULL;
  double *dinput;
  int ndims, nvar, nfac, ldevec;
/*
 * Work array variables.
 */
  double *a, *b, *w;
/*
 * Output array variable
 */
  float  *revec_out;
  int dsizes_evec_out[NCL_MAX_DIMENSIONS];
  int i, found_missing;
/*
 * Retrieve parameters
 */
  data = _NclGetArg(0,1,DONT_CARE);
  switch(data.kind) {
  case NclStk_VAR:
	tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
	break;
  case NclStk_VAL:
	tmp_md = (NclMultiDValData)data.u.data_obj;
	break;
  }
  ndims = tmp_md->multidval.n_dims;
  if( ndims < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eof_varimax: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }

  nfac = tmp_md->multidval.dim_sizes[0];

  nvar = 1;
  for( i = 1; i <= ndims-1; i++ ) {
    nvar *= tmp_md->multidval.dim_sizes[i];
  }
  ldevec = nvar;

  if( nvar < 1 || nfac < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eof_varimax: The dimensions of the input array must both be at least 1");
    return(NhlFATAL);
  }

/*
 * Check our input array to be sure it is a double or float.
 */
  if( tmp_md->multidval.data_type != NCL_float && tmp_md->multidval.data_type != NCL_double ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eof_varimax: The input array must be a float or a double array");
    return(NhlFATAL);
  }

/*
 * Check for a missing value.
 */
  found_missing = 0;
  if(tmp_md->multidval.missing_value.has_missing) {
    if( tmp_md->multidval.data_type == NCL_double ) {
      i = 0;
      while( i < nvar*nfac && !found_missing ) {
		if(((double *)tmp_md->multidval.val)[i] == 
		   tmp_md->multidval.missing_value.value.doubleval) {
		  found_missing = 1;
		}
		i++;
      }
    }
    else {
      i = 0;
      while( i < nvar*nfac && !found_missing ) {
		if(((float *)tmp_md->multidval.val)[i] == 
		   tmp_md->multidval.missing_value.value.floatval) {
		  found_missing = 1;
		}
		i++;
      }
    }
  }
  if(found_missing) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eof_varimax: The input array contains missing values.");
    return(NhlFATAL);
  }
/*
 * Allocate memory for return variable.
 */
  for( i = 0; i <= ndims-1; i++ ) {
    dsizes_evec_out[i] = tmp_md->multidval.dim_sizes[i];
  }

  dinput = (double *)NclMalloc(nfac*nvar*sizeof(double));
  if( dinput == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eof_varimax: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Save input array to another variable.
 */
  if( tmp_md->multidval.data_type == NCL_float ) {
    for( i = 0; i < nvar*nfac; i++ ) {
      dinput[i] = (double)((float *)tmp_md->multidval.val)[i];
    }
  }
  else {
    for( i = 0; i < nvar*nfac; i++ ) {
      dinput[i] = (double)((double *)tmp_md->multidval.val)[i];
    }
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
  NGCALLF(vors,VORS)(&nvar, &nfac, dinput, a, b, w, &ldevec);

/*
 * Free unneeded memory.
 */
  free((double*)w);
  free((double*)a);
  free((double*)b);

/*
 * Convert input array so that tmp_md is not used and also to preserve input
 * array.
 */
  if( tmp_md->multidval.data_type == NCL_float ) {
	revec_out = (float *)NclMalloc(nfac*nvar*sizeof(float));
	if( revec_out == NULL ) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"eof_varimax: Unable to allocate memory for floating point output array");
	  return(NhlFATAL);
	}
    for( i = 0; i < nvar*nfac; i++ ) {
      revec_out[i] = (float)dinput[i];
    }
    return(NclReturnValue((void*)revec_out,ndims,dsizes_evec_out,NULL,NCL_float,0));
  }
  else {
    return(NclReturnValue((void*)dinput,ndims,dsizes_evec_out,NULL,NCL_double,0));
  }
}



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

extern void NGCALLF(wgtrunave,WGTRUNAVE)(double*,int*,double*,int*,int*,
                                         double*,double*,int*,int*);

extern void NGCALLF(runave,RUNAVE)(double*,int*,int*,int*,double*,double*,int*,
                                   int*);

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
  NclScalar missing_x;
/*
 * Work array.
 */
  int lwork;
  double *work;
/*
 * Declare various variables for random purposes.
 */
  int i, j, npts, nwgt, ier, size_x, total_size_x;
  double xmsg;

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
  size_x = 1;
  for( i = 0; i < ndims_x-1; i++ ) {
    size_x *= dsizes_x[i];
  }
  total_size_x = size_x * npts;

/*
 * Coerce data to double if necessary. Since we have to make a copy of
 * the first input array anyway (the input array will be overwritten by
 * output array), we coerce it no matter what.
 */
  dx   = (double*)NclMalloc(sizeof(double)*total_size_x);
  if( dx == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_runave: Unable to allocate memory for coercing input array");
    return(NhlFATAL);
  }
  _Nclcoerce((NclTypeClass)nclTypedoubleClass,
			 dx,
			 x,
			 total_size_x,
			 &missing_x,
			 NULL,
			 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
  
  if(type_wgt != NCL_double) {
	dwgt = (double*)NclMalloc(sizeof(double)*nwgt);
	if( dwgt == NULL ) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_runave: Unable to allocate memory for coercing weight array");
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
 * Test for missing values.
 */
  if( has_missing_x ) {
	if(type_x == NCL_double) {
	  xmsg = (double)missing_x.doubleval;
	}
	else {
	  xmsg = (double)missing_x.floatval;
	}
  }
  else {
	xmsg = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
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
  for( i = 0; i < size_x; i++ ) {
	NGCALLF(wgtrunave, WGTRUNAVE)(&dx[j],&npts,dwgt,&nwgt,kopt,&xmsg,
									work,&lwork,&ier);
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
	if( has_missing_x ) {
	  return(NclReturnValue((void*)fx,ndims_x,dsizes_x,&missing_x,NCL_float,0));
	}
	else {
	  return(NclReturnValue((void*)fx,ndims_x,dsizes_x,NULL,NCL_float,0));
	}
  }
  else {
	if( has_missing_x ) {
	  return(NclReturnValue((void*)dx,ndims_x,dsizes_x,&missing_x,NCL_double,0));
	}
	else {
	  return(NclReturnValue((void*)dx,ndims_x,dsizes_x,NULL,NCL_double,0));
	}
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
  NclScalar missing_x;
/*
 * Work array.
 */
  int lwork;
  double *work;
/*
 * Declare various variables for random purposes.
 */
  int i, j, k, npts, ier, total_size_x, size_x;
  double xmsg;

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
  size_x = 1;
  for( i = 0; i < ndims_x-1; i++ ) {
    size_x *= dsizes_x[i];
  }
  total_size_x = size_x * npts;
/*
 * Coerce data to double if necessary. Since we have to make a copy of
 * the first input array anyway (the input array will be overwritten by
 * output array), we coerce it no matter what.
 */
  dx   = (double*)NclMalloc(sizeof(double)*total_size_x);
  if( dx == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"runave: Unable to allocate memory for coercing input array");
    return(NhlFATAL);
  }
  _Nclcoerce((NclTypeClass)nclTypedoubleClass,
			 dx,
			 x,
			 total_size_x,
			 &missing_x,
			 NULL,
			 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
  
/*
 * Test for missing values.
 */
  if( has_missing_x ) {
	if(type_x == NCL_double) {
	  xmsg = missing_x.doubleval;
	}
	else {
	  xmsg = (double)missing_x.floatval;
	}
  }
  else {
	xmsg = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
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
  for( i = 0; i < size_x; i++ ) {
	NGCALLF(runave, RUNAVE)(&dx[j],&npts,nave,kopt,&xmsg,work,&lwork,&ier);
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

	if( has_missing_x ) {
	  return(NclReturnValue((void*)fx,ndims_x,dsizes_x,&missing_x,NCL_float,0));
	}
	else {
	  return(NclReturnValue((void*)fx,ndims_x,dsizes_x,NULL,NCL_float,0));
	}
  }
  else {
	if( has_missing_x ) {
	  return(NclReturnValue((void*)dx,ndims_x,dsizes_x,&missing_x,NCL_double,0));
	}
	else {
	  return(NclReturnValue((void*)dx,ndims_x,dsizes_x,NULL,NCL_double,0));
	}
  }
}


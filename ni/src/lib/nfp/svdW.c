#include <stdio.h>
/*
* The following are the required NCAR Graphics include files.
* They should be located in ${NCARG_ROOT}/include
*/
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/ncl/defs.h>
#include <ncarg/ncl/NclDataDefs.h>
#include <ncarg/ncl/NclBuiltInSupport.h>
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
#include <math.h>
#include <ncarg/gks.h>

#define min(x,y)  ((x) < (y) ? (x) : (y))
#define max(x,y)  ((x) > (y) ? (x) : (y))

extern void NGCALLF(dsvdlap,DSVDLAP)(double *,double *,int *,int *,int *,
                                     int *,int *,int *,double  *,int *,
                                     double *,int *,double *, double *,
                                     double *, double *, double *,int *);

extern void NGCALLF(dsvdsv,DSVDSV)(double *,double *, int *,int *,int *,
                                   int *,int *,int *,double *, int *,
                                   double *, double *,double *, double *,
                                   double *,double *, double *,int *,
                                   double *,int *,int *);

NhlErrorTypes svdcov_W( void )
{
/*
 * Input array variables
 */
  NclStackEntry args[6];
  NclMultiDValData tmp_md[6], tmp1_md[6];
  int ndims_x, ndims_y, ntimes, ncolx, ncoly;
  int *nsvd;
  double xymsg;
  int ier = 0, iflag=0, iprint=0;
/*
 * Work array variables
 */
  double *w;
  int lwk;
/*
 * Output array variables
 */
  double *svdpcv;
  float *rsvdpcv;
  double *fnorm, *condn;
  float *rfnorm, *rcondn;
  int *lapack_err, ndims_svdpcv, dsizes_svdpcv[1];
/*
 * Attribute variables
 */
  int att_id;
  int dsizes[NCL_MAX_DIMENSIONS];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
/*
 * various
 */
  int i, j, ipt7;

/*
 * Retrieve first two input parameters.
 *
 * tmp_md[0] -> x
 * tmp_md[1] -> y
 */
  for( i = 0; i <= 1; i++ ) { 
	args[i] = _NclGetArg(i,7,DONT_CARE);
	switch(args[i].kind) {
	case NclStk_VAR:
	  tmp_md[i] = _NclVarValueRead(args[i].u.data_var,NULL,NULL);
	  break;
	case NclStk_VAL:
	  tmp_md[i] = args[i].u.data_obj;
	  break;
	}
  }
/*
 * The grids coming in must be 2-dimensional.
 */
  ndims_x = tmp_md[0]->multidval.n_dims;
  ndims_y = tmp_md[1]->multidval.n_dims;
  if( ndims_x != 2 || ndims_y != 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov: The input arrays must be 2-dimensional");
    return(NhlFATAL);
  }
  if( tmp_md[0]->multidval.dim_sizes[1] != tmp_md[1]->multidval.dim_sizes[1] ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov: The last dimension of both input arrays must be the same size");
    return(NhlFATAL);
  }
  ncolx  = tmp_md[0]->multidval.dim_sizes[0];
  ncoly  = tmp_md[1]->multidval.dim_sizes[0];
  ntimes = tmp_md[0]->multidval.dim_sizes[1];
/*
 * Get the missing values.
 */
  if(!tmp_md[0]->multidval.missing_value.has_missing) {
	if( tmp_md[0]->multidval.data_type == NCL_double ) {
	  xymsg = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
	}
	else {
	  xymsg = (double)((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
	}
  }
  else {
	if( tmp_md[0]->multidval.data_type == NCL_double ) {
	  xymsg = tmp_md[0]->multidval.missing_value.value.doubleval;
	}
	else {
	  xymsg = (double)tmp_md[0]->multidval.missing_value.value.floatval;
	}
  }
/*
 * Get number of SVD patterns to be calculated.
 */
  nsvd = (int *)NclGetArgValue(
            2,
            7, 
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            2);
  if (*nsvd > min(ncolx,ncoly)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov: nsvx exceeds maximum possible patterns");
    return(NhlFATAL);
  }
/*
 * Get homogenous/heterogenous arrays (output variables)
 *
 * tmp_md[2] -> homlft
 * tmp_md[3] -> hetlft
 * tmp_md[4] -> homrgt
 * tmp_md[5] -> hetrgt
 */
  for( i = 2; i <= 5; i++ ) { 
	args[i] = _NclGetArg(i+1,7,1);
	switch(args[i].kind) {
	case NclStk_VAR:
	  tmp_md[i] = _NclVarValueRead(args[i].u.data_var,NULL,NULL);
	  break;
	case NclStk_VAL:
	  tmp_md[i] = (NclMultiDValData)args[i].u.data_obj;
	  break;
	}
  }
/*
 * Check dimension sizes.  The hom/het arrays must be 2 dimensions, and the
 * first dimension must be nsvd.The second dimension must be the same as
 * the first dimension of x (or y if it's the rgt arrays).
 */
  if( tmp_md[2]->multidval.n_dims != 2 || tmp_md[3]->multidval.n_dims != 2 ||
	  tmp_md[4]->multidval.n_dims != 2 || tmp_md[5]->multidval.n_dims != 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov: The homogeneous/heterogenous arrays must be 2-dimensionsal");
    return(NhlFATAL);
  }
  if( tmp_md[2]->multidval.dim_sizes[0] != *nsvd || 
	  tmp_md[3]->multidval.dim_sizes[0] != *nsvd ||
	  tmp_md[2]->multidval.dim_sizes[1] != tmp_md[0]->multidval.dim_sizes[0] ||
	  tmp_md[3]->multidval.dim_sizes[1] != tmp_md[0]->multidval.dim_sizes[0]) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov: The second dimension of the homlft/hetlft arrays must be the same as the first dimension of x, and the first dimension must be nsvx");
	return(NhlFATAL);
  }
  if( tmp_md[4]->multidval.dim_sizes[0] != *nsvd || 
	  tmp_md[5]->multidval.dim_sizes[0] != *nsvd ||
	  tmp_md[4]->multidval.dim_sizes[1] != tmp_md[1]->multidval.dim_sizes[0] ||
	  tmp_md[5]->multidval.dim_sizes[1] != tmp_md[1]->multidval.dim_sizes[0]) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov: The second dimension of the homrgt/hetrgt arrays must be the same as the first dimension of y, and the first dimension must be nsvx");
	return(NhlFATAL);
  }
/*
 * Make sure all arrays are the same type and are double or float.
 */
  for( i = 0; i <= 5; i++ ) {
	if((tmp_md[i]->multidval.data_type != NCL_float && 
		tmp_md[i]->multidval.data_type != NCL_double) ||
	    tmp_md[0]->multidval.data_type != tmp_md[i]->multidval.data_type ) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov: The input arrays must all be the same type and be either float or double");
	  return(NhlFATAL);
	}
  }
/*
 * Promote input arrays to double.
 */
  if(tmp_md[0]->multidval.data_type == NCL_float ) {
	for( i = 0; i <= 5; i++ ) {
	  tmp1_md[i] = _NclCoerceData(tmp_md[i],Ncl_Typedouble,NULL);
	  if(tmp1_md[i] == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov: Unable to convert parameter to double");
		return(NhlFATAL);
	  }	
	}
  }
  else {
	for( i = 0; i <= 5; i++ ) tmp1_md[i] = tmp_md[i];
  }
/*
 * Allocate space for output array.
 */
  dsizes_svdpcv[0] = *nsvd;
  svdpcv = (double *)NclMalloc(*nsvd*sizeof(double));
  if( svdpcv == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Allocate memory for work array.
 */
  lwk = ncolx*ncoly + ncolx*ncolx + ncoly*ncoly +
        ntimes*ncolx + ntimes*ncoly + min(ncolx,ncoly) +
        max(3*min(ncolx,ncoly) + max(ncolx,ncoly), 5*min(ncolx,ncoly)-4);
  w = (double *)NclMalloc(lwk*sizeof(double));
  if( w == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * Call the Fortran version of 'svdlap' with the full argument list.
 */
  NGCALLF(dsvdlap,DSVDLAP)(((double*)tmp1_md[0]->multidval.val),
			   ((double*)tmp1_md[1]->multidval.val),
			   &ntimes,&ntimes,&ncolx,&ncoly,nsvd,
			   &iflag,&xymsg,&iprint,w,&lwk,svdpcv,
			   ((double*)tmp1_md[2]->multidval.val),
			   ((double*)tmp1_md[4]->multidval.val),
			   ((double*)tmp1_md[3]->multidval.val),
			   ((double*)tmp1_md[5]->multidval.val),
			   &ier);
  if (ier) {
	NhlPError(NhlWARNING,NhlEUNKNOWN,"svdcov: ier = %d\n", ier );
  }
/*
 * Allocate memory for attributes.
 */
  fnorm      = (double *)NclMalloc(sizeof(double));
  condn      = (double *)NclMalloc(sizeof(double));
  lapack_err = (int *)NclMalloc(sizeof(int));
  if( fnorm == NULL || condn == NULL || lapack_err == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov: Unable to allocate memory for attribute arrays");
    return(NhlFATAL);
  }
/*
 * Set some attribute values.
 */
  ipt7 = ncolx*ncoly + *nsvd*(ncolx+ncoly) + ntimes*(ncolx+ncoly) + *nsvd;
  *fnorm      = w[ipt7];
  *condn      = w[ipt7+1];
  *lapack_err = (int)w[ipt7+3];
/*
 * Free memory.
 */
  free((double*)w);
/*
 * Return output grid to NCL.
 */
  if( tmp_md[0]->multidval.data_type == NCL_float ) {
	if(tmp1_md[0]->obj.id != tmp_md[0]->obj.id) 
		_NclDestroyObj((NclObj)tmp1_md[0]);
	if(tmp1_md[1]->obj.id != tmp_md[1]->obj.id) 
		_NclDestroyObj((NclObj)tmp1_md[1]);
	for( i = 2; i <= 5; i++ ) {
	  for( j = 0; j < tmp_md[i]->multidval.totalelements; j++ ) {
		((float*)tmp_md[i]->multidval.val)[j] = (float)((double *)tmp1_md[i]->multidval.val)[j];
	  }
	  if(tmp1_md[i]->obj.id != tmp_md[i]->obj.id) 
	    _NclDestroyObj((NclObj)tmp1_md[i]);
	}
	rsvdpcv = (float *)NclMalloc(*nsvd*sizeof(float));
	for( i = 0; i < *nsvd; i++ ) rsvdpcv[i] = (float)svdpcv[i];
/*
 * Free up memory.
 */
	free((double*)svdpcv);

/*
 * Set up variable to return.
 */
	return_md = _NclCreateVal(
				  NULL,
				  NULL,
				  Ncl_MultiDValData,
				  0,
				  (void*)rsvdpcv,
				  NULL,
				  1,
				  dsizes_svdpcv,
				  TEMPORARY,
				  NULL,
				  (NclObjClass)nclTypefloatClass
				 );
/*
 * Set up attributes to return.
 */
  att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

/*
 * Convert doubles to floats.
 */
  rfnorm      = (float *)NclMalloc(sizeof(float));
  rcondn      = (float *)NclMalloc(sizeof(float));
  if( rfnorm == NULL || rcondn == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov: Unable to allocate memory for attributes");
    return(NhlFATAL);
  }
  *rfnorm      = (float)*fnorm;
  *rcondn      = (float)*condn;

  dsizes[0] = 1;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)rfnorm,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypefloatClass
                         );
  _NclAddAtt(
             att_id,
             "fnorm",
             att_md,
             NULL
             );

  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)rcondn,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypefloatClass
                         );
  _NclAddAtt(
             att_id,
             "condn",
             att_md,
             NULL
             );

  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)lapack_err,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypeintClass
                         );
  _NclAddAtt(
             att_id,
             "lapack_err",
             att_md,
             NULL
             );

  }
  else {
/*
 * Set up variable to return.
 */
	return_md = _NclCreateVal(
				  NULL,
				  NULL,
				  Ncl_MultiDValData,
				  0,
				  (void*)svdpcv,
				  NULL,
				  1,
				  dsizes_svdpcv,
				  TEMPORARY,
				  NULL,
				  (NclObjClass)nclTypedoubleClass
				  );
/*
 * Set up attributes to return.
 */
  att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

  dsizes[0] = 1;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)fnorm,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypedoubleClass
                         );
  _NclAddAtt(
             att_id,
             "fnorm",
             att_md,
             NULL
             );

  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)condn,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypedoubleClass
                         );
  _NclAddAtt(
             att_id,
             "condn",
             att_md,
             NULL
             );

  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)lapack_err,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypeintClass
                         );
  _NclAddAtt(
             att_id,
             "lapack_err",
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


NhlErrorTypes svdstd_W( void )
{
/*
 * Input array variables
 */
  NclStackEntry args[6];
  NclMultiDValData tmp_md[6], tmp1_md[6];
  int ndims_x, ndims_y, ntimes, ncolx, ncoly;
  int *nsvd;
  double xymsg;
  int ier = 0, iflag=1, iprint=0;
/*
 * Work array variables
 */
  double *w;
  int lwk;
/*
 * Output array variables
 */
  double *svdpcv;
  float *rsvdpcv;
  double *fnorm, *condn;
  float *rfnorm, *rcondn;
  int *lapack_err, ndims_svdpcv, dsizes_svdpcv[1];
/*
 * Attribute variables
 */
  int att_id;
  int dsizes[NCL_MAX_DIMENSIONS];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
/*
 * various
 */
  int i, j, ipt7;

/*
 * Retrieve first two input parameters.
 *
 * tmp_md[0] -> x
 * tmp_md[1] -> y
 */
  for( i = 0; i <= 1; i++ ) { 
	args[i] = _NclGetArg(i,7,DONT_CARE);
	switch(args[i].kind) {
	case NclStk_VAR:
	  tmp_md[i] = _NclVarValueRead(args[i].u.data_var,NULL,NULL);
	  break;
	case NclStk_VAL:
	  tmp_md[i] = args[i].u.data_obj;
	  break;
	}
  }
/*
 * The grids coming in must be 2-dimensional.
 */
  ndims_x = tmp_md[0]->multidval.n_dims;
  ndims_y = tmp_md[1]->multidval.n_dims;
  if( ndims_x != 2 || ndims_y != 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd: The input arrays must be 2-dimensional");
    return(NhlFATAL);
  }
  if( tmp_md[0]->multidval.dim_sizes[1] != tmp_md[1]->multidval.dim_sizes[1] ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd: The last dimension of both input arrays must be the same size");
    return(NhlFATAL);
  }
  ncolx  = tmp_md[0]->multidval.dim_sizes[0];
  ncoly  = tmp_md[1]->multidval.dim_sizes[0];
  ntimes = tmp_md[0]->multidval.dim_sizes[1];
/*
 * Get the missing values.
 */
  if(!tmp_md[0]->multidval.missing_value.has_missing) {
	if( tmp_md[0]->multidval.data_type == NCL_double ) {
	  xymsg = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
	}
	else {
	  xymsg = (double)((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
	}
  }
  else {
	if( tmp_md[0]->multidval.data_type == NCL_double ) {
	  xymsg = tmp_md[0]->multidval.missing_value.value.doubleval;
	}
	else {
	  xymsg = (double)tmp_md[0]->multidval.missing_value.value.floatval;
	}
  }
/*
 * Get number of SVD patterns to be calculated.
 */
  nsvd = (int *)NclGetArgValue(
            2,
            7, 
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            2);
  if (*nsvd > min(ncolx,ncoly)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd: nsvx exceeds maximum possible patterns");
    return(NhlFATAL);
  }
/*
 * Get homogenous/heterogenous arrays (output variables)
 *
 * tmp_md[2] -> homlft
 * tmp_md[3] -> hetlft
 * tmp_md[4] -> homrgt
 * tmp_md[5] -> hetrgt
 */
  for( i = 2; i <= 5; i++ ) { 
	args[i] = _NclGetArg(i+1,7,1);
	switch(args[i].kind) {
	case NclStk_VAR:
	  tmp_md[i] = _NclVarValueRead(args[i].u.data_var,NULL,NULL);
	  break;
	case NclStk_VAL:
	  tmp_md[i] = (NclMultiDValData)args[i].u.data_obj;
	  break;
	}
  }
/*
 * Check dimension sizes.  The hom/het arrays must be 2 dimensions, and the
 * first dimension must be nsvd.The second dimension must be the same as
 * the first dimension of x (or y if it's the rgt arrays).
 */
  if( tmp_md[2]->multidval.n_dims != 2 || tmp_md[3]->multidval.n_dims != 2 ||
	  tmp_md[4]->multidval.n_dims != 2 || tmp_md[5]->multidval.n_dims != 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd: The homogeneous/heterogenous arrays must be 2-dimensionsal");
    return(NhlFATAL);
  }
  if( tmp_md[2]->multidval.dim_sizes[0] != *nsvd || 
	  tmp_md[3]->multidval.dim_sizes[0] != *nsvd ||
	  tmp_md[2]->multidval.dim_sizes[1] != tmp_md[0]->multidval.dim_sizes[0] ||
	  tmp_md[3]->multidval.dim_sizes[1] != tmp_md[0]->multidval.dim_sizes[0]) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd: The second dimension of the homlft/hetlft arrays must be the same as the first dimension of x, and the first dimension must be nsvx");
	return(NhlFATAL);
  }
  if( tmp_md[4]->multidval.dim_sizes[0] != *nsvd || 
	  tmp_md[5]->multidval.dim_sizes[0] != *nsvd ||
	  tmp_md[4]->multidval.dim_sizes[1] != tmp_md[1]->multidval.dim_sizes[0] ||
	  tmp_md[5]->multidval.dim_sizes[1] != tmp_md[1]->multidval.dim_sizes[0]) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd: The second dimension of the homrgt/hetrgt arrays must be the same as the first dimension of y, and the first dimension must be nsvx");
	return(NhlFATAL);
  }

/*
 * Make sure all arrays are the same type and are double or float.
 */
  for( i = 0; i <= 5; i++ ) {
	if((tmp_md[i]->multidval.data_type != NCL_float && 
		tmp_md[i]->multidval.data_type != NCL_double) ||
	    tmp_md[0]->multidval.data_type != tmp_md[i]->multidval.data_type ) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd: The input arrays must all be the same type and be either float or double");
	  return(NhlFATAL);
	}
  }
/*
 * Promote input arrays to double.
 */
  if(tmp_md[0]->multidval.data_type == NCL_float ) {
	for( i = 0; i <= 5; i++ ) {
	  tmp1_md[i] = _NclCoerceData(tmp_md[i],Ncl_Typedouble,NULL);
	  if(tmp1_md[i] == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd: Unable to convert parameter to double");
		return(NhlFATAL);
	  }	
	}
  }
  else {
	for( i = 0; i <= 5; i++ ) tmp1_md[i] = tmp_md[i];
  }
/*
 * Allocate space for output array.
 */
  dsizes_svdpcv[0] = *nsvd;
  svdpcv = (double *)NclMalloc(*nsvd*sizeof(double));
  if( svdpcv == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Allocate memory for work array.
 */
  lwk = ncolx*ncoly + ncolx*ncolx + ncoly*ncoly +
        ntimes*ncolx + ntimes*ncoly + min(ncolx,ncoly) +
        max(3*min(ncolx,ncoly) + max(ncolx,ncoly), 5*min(ncolx,ncoly)-4);
  w = (double *)NclMalloc(lwk*sizeof(double));
  if( w == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * Call the Fortran version of 'svdlap' with the full argument list.
 */
  NGCALLF(dsvdlap,DSVDLAP)(((double*)tmp1_md[0]->multidval.val),
			   ((double*)tmp1_md[1]->multidval.val),
			   &ntimes,&ntimes,&ncolx,&ncoly,nsvd,
			   &iflag,&xymsg,&iprint,w,&lwk,svdpcv,
			   ((double*)tmp1_md[2]->multidval.val),
			   ((double*)tmp1_md[4]->multidval.val),
			   ((double*)tmp1_md[3]->multidval.val),
			   ((double*)tmp1_md[5]->multidval.val),
			   &ier);
  if (ier) {
	NhlPError(NhlWARNING,NhlEUNKNOWN,"svdstd: ier = %d\n", ier );
  }
/*
 * Allocate memory for attributes.
 */
  fnorm      = (double *)NclMalloc(sizeof(double));
  condn      = (double *)NclMalloc(sizeof(double));
  lapack_err = (int *)NclMalloc(sizeof(int));
  if( fnorm == NULL || condn == NULL || lapack_err == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd: Unable to allocate memory for attribute arrays");
    return(NhlFATAL);
  }
/*
 * Set some attribute values.
 */
  ipt7 = ncolx*ncoly + *nsvd*(ncolx+ncoly) + ntimes*(ncolx+ncoly) + *nsvd;
  *fnorm      = w[ipt7];
  *condn      = w[ipt7+1];
  *lapack_err = (int)w[ipt7+3];
/*
 * Free memory.
 */
  free((double*)w);
/*
 * Return output grid to NCL.
 */
  if( tmp_md[0]->multidval.data_type == NCL_float ) {
	if(tmp1_md[0]->obj.id != tmp_md[0]->obj.id) 
		_NclDestroyObj((NclObj)tmp1_md[0]);
	if(tmp1_md[1]->obj.id != tmp_md[1]->obj.id) 
		_NclDestroyObj((NclObj)tmp1_md[1]);
	for( i = 2; i <= 5; i++ ) {
	  for( j = 0; j < tmp_md[i]->multidval.totalelements; j++ ) {
		((float*)tmp_md[i]->multidval.val)[j] = (float)((double *)tmp1_md[i]->multidval.val)[j];
	  }
	   if(tmp1_md[i]->obj.id != tmp_md[i]->obj.id) 
		  _NclDestroyObj((NclObj)tmp1_md[i]);
	}
	rsvdpcv = (float *)NclMalloc(*nsvd*sizeof(float));
	for( i = 0; i < *nsvd; i++ ) rsvdpcv[i] = (float)svdpcv[i];
/*
 * Free up memory.
 */
	free((double*)svdpcv);

/*
 * Set up variable to return.
 */
	return_md = _NclCreateVal(
				  NULL,
				  NULL,
				  Ncl_MultiDValData,
				  0,
				  (void*)rsvdpcv,
				  NULL,
				  1,
				  dsizes_svdpcv,
				  TEMPORARY,
				  NULL,
				  (NclObjClass)nclTypefloatClass
				  );
/*
 * Set up attributes to return.
 */
  att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

/*
 * Convert doubles to floats.
 */
  rfnorm      = (float *)NclMalloc(sizeof(float));
  rcondn      = (float *)NclMalloc(sizeof(float));
  if( rfnorm == NULL || rcondn == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd: Unable to allocate memory for attributes");
    return(NhlFATAL);
  }
  *rfnorm      = (float)*fnorm;
  *rcondn      = (float)*condn;

  dsizes[0] = 1;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)rfnorm,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypefloatClass
                         );
  _NclAddAtt(
             att_id,
             "fnorm",
             att_md,
             NULL
             );

  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)rcondn,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypefloatClass
                         );
  _NclAddAtt(
             att_id,
             "condn",
             att_md,
             NULL
             );

  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)lapack_err,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypeintClass
                         );
  _NclAddAtt(
             att_id,
             "lapack_err",
             att_md,
             NULL
             );

  }
  else {
/*
 * Set up variable to return.
 */
	return_md = _NclCreateVal(
				  NULL,
				  NULL,
				  Ncl_MultiDValData,
				  0,
				  (void*)svdpcv,
				  NULL,
				  1,
				  dsizes_svdpcv,
				  TEMPORARY,
				  NULL,
				  (NclObjClass)nclTypedoubleClass
				  );
/*
 * Set up attributes to return.
 */
  att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

  dsizes[0] = 1;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)fnorm,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypedoubleClass
                         );
  _NclAddAtt(
             att_id,
             "fnorm",
             att_md,
             NULL
             );

  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)condn,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypedoubleClass
                         );
  _NclAddAtt(
             att_id,
             "condn",
             att_md,
             NULL
             );

  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)lapack_err,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypeintClass
                         );
  _NclAddAtt(
             att_id,
             "lapack_err",
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

NhlErrorTypes svdcov_sv_W( void )
{
/*
 * Input array variables
 */
  NclStackEntry args[4];
  NclMultiDValData tmp_md[4], tmp1_md[4];
  int ndims_x, ndims_y, ntimes, ncolx, ncoly;
  int *nsvd;
  double xymsg;
  int ier = 0, iflag=0, iprint=0;
/*
 * Work array variables
 */
  double *w, *crv, *u, *vt, *sv;
  int lwork, nsvmx;
/*
 * Output array variables
 */
  double *svdpcv;
  float *rsvdpcv, *rsv;
  int ndims_svdpcv, dsizes_svdpcv[1];
/*
 * Attribute variables
 */
  int att_id;
  int dsizes[NCL_MAX_DIMENSIONS];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
/*
 * Various
 */
  int i, j, k, l;

/*
 * Retrieve first two input parameters.
 *
 * tmp_md[0] -> x
 * tmp_md[1] -> y
 */
  for( i = 0; i <= 1; i++ ) { 
	args[i] = _NclGetArg(i,5,DONT_CARE);
	switch(args[i].kind) {
	case NclStk_VAR:
	  tmp_md[i] = _NclVarValueRead(args[i].u.data_var,NULL,NULL);
	  break;
	case NclStk_VAL:
	  tmp_md[i] = args[i].u.data_obj;
	  break;
	}
  }
/*
 * The grids coming in must be 2-dimensional.
 */
  ndims_x = tmp_md[0]->multidval.n_dims;
  ndims_y = tmp_md[1]->multidval.n_dims;
  if( ndims_x != 2 || ndims_y != 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov_sv: The input arrays must be 2-dimensional");
    return(NhlFATAL);
  }
  if( tmp_md[0]->multidval.dim_sizes[1] != tmp_md[1]->multidval.dim_sizes[1] ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov:_sv The last dimension of both input arrays must be the same size");
    return(NhlFATAL);
  }
  ncolx  = tmp_md[0]->multidval.dim_sizes[0];
  ncoly  = tmp_md[1]->multidval.dim_sizes[0];
  ntimes = tmp_md[0]->multidval.dim_sizes[1];
/*
 * Get the missing values.
 */
  if(!tmp_md[0]->multidval.missing_value.has_missing) {
	if( tmp_md[0]->multidval.data_type == NCL_double ) {
	  xymsg = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
	}
	else {
	  xymsg = (double)((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
	}
  }
  else {
	if( tmp_md[0]->multidval.data_type == NCL_double ) {
	  xymsg = tmp_md[0]->multidval.missing_value.value.doubleval;
	}
	else {
	  xymsg = (double)tmp_md[0]->multidval.missing_value.value.floatval;
	}
  }
/*
 * Get number of SVD patterns to be calculated.
 */
  nsvd = (int *)NclGetArgValue(
            2,
            5, 
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            2);
  if (*nsvd > min(ncolx,ncoly)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov_sv: nsvx exceeds maximum possible patterns");
    return(NhlFATAL);
  }
/*
 * Get left/right arrays (output variables)
 *
 * tmp_md[2] -> left
 * tmp_md[3] -> right
 */
  for( i = 2; i <= 3; i++ ) { 
	args[i] = _NclGetArg(i+1,5,1);
	switch(args[i].kind) {
	case NclStk_VAR:
	  tmp_md[i] = _NclVarValueRead(args[i].u.data_var,NULL,NULL);
	  break;
	case NclStk_VAL:
	  tmp_md[i] = (NclMultiDValData)args[i].u.data_obj;
	  break;
	}
  }
/*
 * Check dimension sizes.  The hom/het arrays must be 2 dimensions, and the
 * first dimension must be nsvd.The second dimension must be the same as
 * the first dimension of x (or y if it's the rgt arrays).
 */
  if( tmp_md[2]->multidval.n_dims != 2 || tmp_md[3]->multidval.n_dims != 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov_sv: The left/right arrays must be 2-dimensionsal");
    return(NhlFATAL);
  }
  if( tmp_md[2]->multidval.dim_sizes[0] != *nsvd || 
      tmp_md[2]->multidval.dim_sizes[1] != tmp_md[0]->multidval.dim_sizes[0]) { 
	NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov_sv: The second dimension of the left array must be the same as the first dimension of x, and the first dimension must be nsvx");
	return(NhlFATAL);
  }
  if( tmp_md[3]->multidval.dim_sizes[0] != *nsvd || 
      tmp_md[3]->multidval.dim_sizes[1] != tmp_md[1]->multidval.dim_sizes[0]) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov_sv: The second dimension of the right array must be the same as the first dimension of y, and the first dimension must be nsvx");
	return(NhlFATAL);
  }
/*
 * Make sure all arrays are the same type and are double or float.
 */
  for( i = 0; i <= 3; i++ ) {
	if((tmp_md[i]->multidval.data_type != NCL_float && 
		tmp_md[i]->multidval.data_type != NCL_double) ||
	    tmp_md[0]->multidval.data_type != tmp_md[i]->multidval.data_type ) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov_sv: The input arrays must all be the same type and be either float or double");
	  return(NhlFATAL);
	}
  }
/*
 * Promote input arrays to double.
 */
  if(tmp_md[0]->multidval.data_type == NCL_float ) {
	for( i = 0; i <= 3; i++ ) {
	  tmp1_md[i] = _NclCoerceData(tmp_md[i],Ncl_Typedouble,NULL);
	  if(tmp1_md[i] == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov_sv: Unable to convert parameter to double");
		return(NhlFATAL);
	  }	
	}
  }
  else {
	for( i = 0; i <= 3; i++ ) tmp1_md[i] = tmp_md[i];
  }
/*
 * Allocate space for output array.
 */
  dsizes_svdpcv[0] = *nsvd;
  svdpcv = (double *)NclMalloc(*nsvd*sizeof(double));
  if( svdpcv == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov_sv: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Allocate memory for work and output arrays.
 */
  nsvmx = min(ncolx,ncoly);
  lwork = max(3*nsvmx+max(ncolx,ncoly),5*min(ncolx,ncoly)-4);
  w   = (double *)NclMalloc(lwork*sizeof(double));
  u   = (double *)NclMalloc(nsvmx*ncolx*sizeof(double));
  vt  = (double *)NclMalloc(nsvmx*ncoly*sizeof(double));
  sv  = (double *)NclMalloc(nsvmx*sizeof(double));
  crv = (double *)NclMalloc(ncolx*ncoly*sizeof(double));
  if( w == NULL || crv == NULL || u == NULL || vt == NULL || sv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov_sv: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
/*
 * Call the Fortran version of 'svdlap' with the full argument list.
 */
  NGCALLF(dsvdsv,DSVDSV)(((double*)tmp1_md[0]->multidval.val),
			 ((double*)tmp1_md[1]->multidval.val),
			 &ntimes,&ntimes,&ncolx,&ncoly,nsvd,&iflag,
			 &xymsg,&iprint,
			 ((double*)tmp1_md[2]->multidval.val),
			 ((double*)tmp1_md[3]->multidval.val),
			 svdpcv,crv,u,vt,sv,&nsvmx,w,&lwork,&ier);
/*
 * Free memory.
 */
  free((double*)w);
  free((double*)crv);
  free((double*)u);
  free((double*)vt);

/*
 * Set up variable to return.
 */
  if( tmp_md[0]->multidval.data_type == NCL_float ) {
	if(tmp1_md[0]->obj.id != tmp_md[0]->obj.id) 
		_NclDestroyObj((NclObj)tmp1_md[0]);
	if(tmp1_md[1]->obj.id != tmp_md[1]->obj.id) 
		_NclDestroyObj((NclObj)tmp1_md[1]);
	for( i = 2; i <= 3; i++ ) {
	  for( j = 0; j < tmp_md[i]->multidval.totalelements; j++ ) {
		((float*)tmp_md[i]->multidval.val)[j] = (float)((double *)tmp1_md[i]->multidval.val)[j];
	  }
	  if(tmp_md[i]->obj.id != tmp1_md[i]->obj.id) 
	  	_NclDestroyObj((NclObj)tmp1_md[i]);
	}
	rsvdpcv = (float *)NclMalloc(*nsvd*sizeof(float));
	for( i = 0; i < *nsvd; i++ ) rsvdpcv[i] = (float)svdpcv[i];
	free((double*)svdpcv);

	return_md = _NclCreateVal(
				  NULL,
				  NULL,
				  Ncl_MultiDValData,
				  0,
				  (void*)rsvdpcv,
				  NULL,
				  1,
				  dsizes_svdpcv,
				  TEMPORARY,
				  NULL,
				  (NclObjClass)nclTypefloatClass
				 );
/*
 * Set up attributes to return.
 */
	att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);
/*
 * Convert doubles to floats.
 */
	rsv = (float *)NclMalloc(nsvmx*sizeof(float));
	if( rsv == NULL ) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov_sv: Unable to allocate memory for attributes");
	  return(NhlFATAL);
	}
	
	for(i = 0; i < nsvmx; i++) rsv[i] = (float)sv[i];
	free((double*)sv);

	dsizes[0] = nsvmx;
	att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)rsv,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypefloatClass
                         );
	_NclAddAtt(
			   att_id,
			   "sv",
			   att_md,
			   NULL
			   );

  }

  else {
	return_md = _NclCreateVal(
				  NULL,
				  NULL,
				  Ncl_MultiDValData,
				  0,
				  (void*)svdpcv,
				  NULL,
				  1,
				  dsizes_svdpcv,
				  TEMPORARY,
				  NULL,
				  (NclObjClass)nclTypedoubleClass
				 );
/*
 * Set up attributes to return.
 */
	att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

	dsizes[0] = nsvmx;
	att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)sv,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypedoubleClass
                         );
	_NclAddAtt(
			   att_id,
			   "sv",
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


NhlErrorTypes svdstd_sv_W( void )
{
/*
 * Input array variables
 */
  NclStackEntry args[4];
  NclMultiDValData tmp_md[4], tmp1_md[4];
  int ndims_x, ndims_y, ntimes, ncolx, ncoly;
  int *nsvd;
  double xymsg;
  int ier = 0, iflag=1, iprint=0;
/*
 * Work array variables
 */
  double *w, *crv, *u, *vt, *sv;
  int lwork, nsvmx;
/*
 * Output array variables
 */
  double *svdpcv;
  float *rsvdpcv, *rsv;
  int ndims_svdpcv, dsizes_svdpcv[1];
/*
 * Attribute variables
 */
  int att_id;
  int dsizes[NCL_MAX_DIMENSIONS];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
/*
 * Various
 */
  int i, j, k, l;

/*
 * Retrieve first two input parameters.
 *
 * tmp_md[0] -> x
 * tmp_md[1] -> y
 */
  for( i = 0; i <= 1; i++ ) { 
	args[i] = _NclGetArg(i,5,DONT_CARE);
	switch(args[i].kind) {
	case NclStk_VAR:
	  tmp_md[i] = _NclVarValueRead(args[i].u.data_var,NULL,NULL);
	  break;
	case NclStk_VAL:
	  tmp_md[i] = args[i].u.data_obj;
	  break;
	}
  }
/*
 * The grids coming in must be 2-dimensional.
 */
  ndims_x = tmp_md[0]->multidval.n_dims;
  ndims_y = tmp_md[1]->multidval.n_dims;
  if( ndims_x != 2 || ndims_y != 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd_sv: The input arrays must be 2-dimensional");
    return(NhlFATAL);
  }
  if( tmp_md[0]->multidval.dim_sizes[1] != tmp_md[1]->multidval.dim_sizes[1] ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd:_sv The last dimension of both input arrays must be the same size");
    return(NhlFATAL);
  }
  ncolx  = tmp_md[0]->multidval.dim_sizes[0];
  ncoly  = tmp_md[1]->multidval.dim_sizes[0];
  ntimes = tmp_md[0]->multidval.dim_sizes[1];
/*
 * Get the missing values.
 */
  if(!tmp_md[0]->multidval.missing_value.has_missing) {
	if( tmp_md[0]->multidval.data_type == NCL_double ) {
	  xymsg = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
	}
	else {
	  xymsg = (double)((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
	}
  }
  else {
	if( tmp_md[0]->multidval.data_type == NCL_double ) {
	  xymsg = tmp_md[0]->multidval.missing_value.value.doubleval;
	}
	else {
	  xymsg = (double)tmp_md[0]->multidval.missing_value.value.floatval;
	}
  }
/*
 * Get number of SVD patterns to be calculated.
 */
  nsvd = (int *)NclGetArgValue(
            2,
            5, 
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            2);
  if (*nsvd > min(ncolx,ncoly)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd_sv: nsvx exceeds maximum possible patterns");
    return(NhlFATAL);
  }
/*
 * Get left/right arrays (output variables)
 *
 * tmp_md[2] -> left
 * tmp_md[3] -> right
 */
  for( i = 2; i <= 3; i++ ) { 
	args[i] = _NclGetArg(i+1,5,1);
	switch(args[i].kind) {
	case NclStk_VAR:
	  tmp_md[i] = _NclVarValueRead(args[i].u.data_var,NULL,NULL);
	  break;
	case NclStk_VAL:
	  tmp_md[i] = (NclMultiDValData)args[i].u.data_obj;
	  break;
	}
  }
/*
 * Check dimension sizes.  The hom/het arrays must be 2 dimensions, and the
 * first dimension must be nsvd.The second dimension must be the same as
 * the first dimension of x (or y if it's the rgt arrays).
 */
  if( tmp_md[2]->multidval.n_dims != 2 || tmp_md[3]->multidval.n_dims != 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd_sv: The left/right arrays must be 2-dimensionsal");
    return(NhlFATAL);
  }
  if( tmp_md[2]->multidval.dim_sizes[0] != *nsvd || 
      tmp_md[2]->multidval.dim_sizes[1] != tmp_md[0]->multidval.dim_sizes[0]) { 
	NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd_sv: The second dimension of the left array must be the same as the first dimension of x, and the first dimension must be nsvx");
	return(NhlFATAL);
  }
  if( tmp_md[3]->multidval.dim_sizes[0] != *nsvd || 
      tmp_md[3]->multidval.dim_sizes[1] != tmp_md[1]->multidval.dim_sizes[0]) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd_sv: The second dimension of the right array must be the same as the first dimension of y, and the first dimension must be nsvx");
	return(NhlFATAL);
  }
/*
 * Make sure all arrays are the same type and are double or float.
 */
  for( i = 0; i <= 3; i++ ) {
	if((tmp_md[i]->multidval.data_type != NCL_float && 
		tmp_md[i]->multidval.data_type != NCL_double) ||
	    tmp_md[0]->multidval.data_type != tmp_md[i]->multidval.data_type ) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd_sv: The input arrays must all be the same type and be either float or double");
	  return(NhlFATAL);
	}
  }
/*
 * Promote input arrays to double.
 */
  if(tmp_md[0]->multidval.data_type == NCL_float ) {
	for( i = 0; i <= 3; i++ ) {
	  tmp1_md[i] = _NclCoerceData(tmp_md[i],Ncl_Typedouble,NULL);
	  if(tmp1_md[i] == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd_sv: Unable to convert parameter to double");
		return(NhlFATAL);
	  }	
	}
  }
  else {
	for( i = 0; i <= 3; i++ ) tmp1_md[i] = tmp_md[i];
  }
/*
 * Allocate space for output array.
 */
  dsizes_svdpcv[0] = *nsvd;
  svdpcv = (double *)NclMalloc(*nsvd*sizeof(double));
  if( svdpcv == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd_sv: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Allocate memory for work and output arrays.
 */
  nsvmx = min(ncolx,ncoly);
  lwork = max(3*nsvmx+max(ncolx,ncoly),5*min(ncolx,ncoly)-4);
  w   = (double *)NclMalloc(lwork*sizeof(double));
  u   = (double *)NclMalloc(nsvmx*ncolx*sizeof(double));
  vt  = (double *)NclMalloc(nsvmx*ncoly*sizeof(double));
  sv  = (double *)NclMalloc(nsvmx*sizeof(double));
  crv = (double *)NclMalloc(ncolx*ncoly*sizeof(double));
  if( w == NULL || crv == NULL || u == NULL || vt == NULL || sv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd_sv: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
/*
 * Call the Fortran version of 'svdlap' with the full argument list.
 */
  NGCALLF(dsvdsv,DSVDSV)(((double*)tmp1_md[0]->multidval.val),
			 ((double*)tmp1_md[1]->multidval.val),
			 &ntimes,&ntimes,&ncolx,&ncoly,nsvd,&iflag,
			 &xymsg,&iprint,
			 ((double*)tmp1_md[2]->multidval.val),
			 ((double*)tmp1_md[3]->multidval.val),
			 svdpcv,crv,u,vt,sv,&nsvmx,w,&lwork,&ier);
/*
 * Free memory.
 */
  free((double*)w);
  free((double*)crv);
  free((double*)u);
  free((double*)vt);
/*
 * Set up variable to return.
 */
  if( tmp_md[0]->multidval.data_type == NCL_float ) {
	if(tmp1_md[0]->obj.id != tmp_md[0]->obj.id) 
		_NclDestroyObj((NclObj)tmp1_md[0]);
	if(tmp1_md[1]->obj.id != tmp_md[1]->obj.id) 
		_NclDestroyObj((NclObj)tmp1_md[1]);
	for( i = 2; i <= 3; i++ ) {
	  for( j = 0; j < tmp_md[i]->multidval.totalelements; j++ ) {
		((float*)tmp_md[i]->multidval.val)[j] = (float)((double *)tmp1_md[i]->multidval.val)[j];
	  }
	  if(tmp_md[i]->obj.id != tmp1_md[i]->obj.id) 
	  	_NclDestroyObj((NclObj)tmp1_md[i]);
	}
	rsvdpcv = (float *)NclMalloc(*nsvd*sizeof(float));
	for( i = 0; i < *nsvd; i++ ) rsvdpcv[i] = (float)svdpcv[i];
	free((double*)svdpcv);

	return_md = _NclCreateVal(
				  NULL,
				  NULL,
				  Ncl_MultiDValData,
				  0,
				  (void*)rsvdpcv,
				  NULL,
				  1,
				  dsizes_svdpcv,
				  TEMPORARY,
				  NULL,
				  (NclObjClass)nclTypefloatClass
				 );
/*
 * Set up attributes to return.
 */
	att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);
/*
 * Convert doubles to floats.
 */
	rsv = (float *)NclMalloc(nsvmx*sizeof(float));
	if( rsv == NULL ) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd_sv: Unable to allocate memory for attributes");
	  return(NhlFATAL);
	}
	
	for(i = 0; i < nsvmx; i++) rsv[i] = (float)sv[i];
	free((double*)sv);

	dsizes[0] = nsvmx;
	att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)rsv,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypefloatClass
                         );
	_NclAddAtt(
			   att_id,
			   "sv",
			   att_md,
			   NULL
			   );

  }

  else {
	return_md = _NclCreateVal(
				  NULL,
				  NULL,
				  Ncl_MultiDValData,
				  0,
				  (void*)svdpcv,
				  NULL,
				  1,
				  dsizes_svdpcv,
				  TEMPORARY,
				  NULL,
				  (NclObjClass)nclTypedoubleClass
				 );
/*
 * Set up attributes to return.
 */
	att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

	dsizes[0] = nsvmx;
	att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)sv,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypedoubleClass
                         );
	_NclAddAtt(
			   att_id,
			   "sv",
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



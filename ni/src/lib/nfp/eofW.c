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
#include <math.h>
#include <ncarg/gks.h>

NhlErrorTypes eofcov_W( void )
{
/*
 * Input array variables
 */
  NclStackEntry data;
  NclMultiDValData tmp_md = NULL;
  int ndims, nrow, ncol, nobs, msta;
  int *neval, iopt = 0, jopt = 0, i, ier = 0;
  double xmsg;
  NclScalar missing;
/*
 * Work array variables.
 */
  double *cssm, *work, *weval;
  int   *iwork, *ifail;
  int lcssm, lwork, liwork, lifail;
  double *dinput;
/*
 * Attribute variables
 */
  int att_id;
  int dsizes[NCL_MAX_DIMENSIONS];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
  double *trace, *eval;
  float *pcvar;
  float *rtrace, *reval;
/*
 * Output array variables
 */
  double *evec;
  float *revec;
  int dsizes_evec[NCL_MAX_DIMENSIONS];
/*
 * Retrieve parameters
 */
  data = _NclGetArg(0,2,DONT_CARE);
  switch(data.kind) {
  case NclStk_VAR:
	tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
	break;
  case NclStk_VAL:
	tmp_md = (NclMultiDValData)data.u.data_obj;
	break;
  }
/*
 * The grid coming in must be at least 2-dimensional.
 */
  ndims = tmp_md->multidval.n_dims;
  if( ndims < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }
  msta = 1;
  for( i = 0; i <= ndims-2; i++ ) {
	msta *= tmp_md->multidval.dim_sizes[i];
  }
  ncol = msta;
  nobs = nrow = tmp_md->multidval.dim_sizes[ndims-1];
  if( msta < 1 || nobs < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov: The dimensions of the input array must both be at least 1");
    return(NhlFATAL);
  }
/*
 * Get missing value, if there is one.
 */
  if(!tmp_md->multidval.missing_value.has_missing) {
	if( tmp_md->multidval.data_type == NCL_double ) {
	  xmsg = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
	}
	else {
	  xmsg = (double)((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
	}
  }
  else {
	if( tmp_md->multidval.data_type == NCL_double ) {
	  xmsg = tmp_md->multidval.missing_value.value.doubleval;
	  missing.doubleval = xmsg;
	}
	else {
	  xmsg = (double)tmp_md->multidval.missing_value.value.floatval;
	  missing.floatval = tmp_md->multidval.missing_value.value.floatval;
	}
  }

/*
 * Check our input array to be sure it is a double or float.
 */
  if( tmp_md->multidval.data_type != NCL_float && tmp_md->multidval.data_type != NCL_double ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov: The input array must be a float or a double array");
    return(NhlFATAL);
  }
/*
 * Get number of eigenvalues and eigen vectors to be computed.
 */
  neval = (int *)NclGetArgValue(
            1,
            2, 
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            2);
/*
 * Allocate memory for return variable.
 */
  dsizes_evec[0] = *neval;
  for( i = 0; i <= ndims-2; i++ ) {
	dsizes_evec[i+1] = tmp_md->multidval.dim_sizes[i];
  }
  evec = (double *)NclMalloc(ncol*(*neval)*sizeof(double));
  if( evec == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Allocate memory for attributes.
 */
  trace = (double *)NclMalloc(sizeof(double));
  eval =  (double *)NclMalloc(*neval*sizeof(double));
  pcvar = (float *)NclMalloc(*neval*sizeof(float));
  if( trace == NULL || pcvar == NULL || eval == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov: Unable to allocate memory for attribute arrays");
    return(NhlFATAL);
  }
/*
 * Create a few more work arrays.  This is necessary to avoid having
 * these arrays created dynamically in the Fortran file (which makes
 * it Fortran 90, and unportable to some systems. 
 */
  lcssm  = msta*(msta+1)/2;
  lwork  = 8*msta;
  liwork = 5*msta;
  lifail = msta;
  cssm   = (double *)NclMalloc(lcssm*sizeof(double));
  work   = (double *)NclMalloc(lwork*sizeof(double));
  weval  = (double *)NclMalloc(lifail*sizeof(double));
  iwork  =   (int *)NclMalloc(liwork*sizeof(int));
  ifail  =   (int *)NclMalloc(lifail*sizeof(int));
  dinput = (double *)NclMalloc(nrow*ncol*sizeof(double));
  if( cssm == NULL || work == NULL || weval == NULL || iwork == NULL || ifail == NULL || dinput == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
/*
 * Convert input array so that tmp_md is not used.  This is so you don't
 * get funny business from tmp_md and the missing value. Problems arise
 * with the missing value if your data is float, but you try to convert
 * tmp_md to double with a type cast.
 */
  if( tmp_md->multidval.data_type == NCL_float ) {
	for( i = 0; i < nrow*ncol; i++ ) {
	  dinput[i] = (double)((float *)tmp_md->multidval.val)[i];
	}
  }
  else {
	for( i = 0; i < nrow*ncol; i++ ) {
	  dinput[i] = (double)((double *)tmp_md->multidval.val)[i];
	}
  }

/*
 * Call the Fortran 77 version of 'drveof' with the full argument list.
 */
  NGCALLF(ddrveof,DDRVEOF)(dinput,&nrow,&ncol,&nobs,&msta,&xmsg,neval,eval,
                           evec,pcvar,trace,&iopt,&jopt,cssm,&lcssm,
						   work,&lwork,weval,iwork,&liwork,ifail,&lifail,&ier);
/*
 * Check various possible error messages.
 */
  if (ier != 0) {
	if (ier == -1) {
	  NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov: cssm contains one or more missing values\n" );
	}
	else if (ier == -88) {
	  NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov: trace is equal to zero\n" );
	}
	else if (ier < 0) {
	  NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov: The %d-th argument had an illegal value\n", abs(ier) );
	}
	else {
	  NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov: %d eigenvectors failed to converge\n",ier);
	}
  }
/*
 * Free unneeded memory.
 */
  free((double*)work);
  free((double*)cssm);
  free((double*)weval);
  free((int*)iwork);
  free((int*)ifail);
  free((double*)dinput);
/*
 * Set up variable and attributes to return. Output data needs to be
 * same type as input data.  Since the default is for the data to be
 * double, if it is actually float, then we need to coerce it to float.
 */
  if( tmp_md->multidval.data_type == NCL_double ) {
	if(tmp_md->multidval.missing_value.has_missing) {
	  return_md = _NclCreateVal(
								NULL,
								NULL,
								Ncl_MultiDValData,
								0,
								(void*)evec,
								&missing,
								tmp_md->multidval.n_dims,
								dsizes_evec,
								TEMPORARY,
								NULL,
								(NclObjClass)nclTypedoubleClass
								);
	}
	else {
	  return_md = _NclCreateVal(
								NULL,
								NULL,
								Ncl_MultiDValData,
								0,
								(void*)evec,
								NULL,
								tmp_md->multidval.n_dims,
								dsizes_evec,
								TEMPORARY,
								NULL,
								(NclObjClass)nclTypedoubleClass
								);
	}
	att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

	dsizes[0] = *neval;
	att_md = _NclCreateVal(
						   NULL,
						   NULL,
						   Ncl_MultiDValData,
						   0,
						   (void*)eval,
						   NULL,
						   1,
						   dsizes,
						   TEMPORARY,
						   NULL,
						   (NclObjClass)nclTypedoubleClass
						   );
	_NclAddAtt(
			   att_id,
			   "eval",
			   att_md,
			   NULL
			   );

	att_md = _NclCreateVal(
						   NULL,
						   NULL,
						   Ncl_MultiDValData,
						   0,
						   (void*)pcvar,
						   NULL,
						   1,
						   dsizes,
						   TEMPORARY,
						   NULL,
						   (NclObjClass)nclTypefloatClass
						   );
	_NclAddAtt(
			   att_id,
			   "pcvar",
			   att_md,
			   NULL
			   );

	dsizes[0] = 1;
	att_md = _NclCreateVal(
						   NULL,
						   NULL,
						   Ncl_MultiDValData,
						   0,
						   (void*)trace,
						   NULL,
						   1,
						   dsizes,
						   TEMPORARY,
						   NULL,
						   (NclObjClass)nclTypedoubleClass
						   );
	_NclAddAtt(
			   att_id,
			   "trace",
			   att_md,
			   NULL
			   );
  }
  else {
/*
 * Our input data was real, so convert output data to real.
 */
	revec = (float *)NclMalloc(ncol*(*neval)*sizeof(float));
	for( i = 0; i < ncol*(*neval); i++ ) revec[i] = (float)evec[i];
	if(tmp_md->multidval.missing_value.has_missing) {
	  return_md = _NclCreateVal(
								NULL,
								NULL,
								Ncl_MultiDValData,
								0,
								(void*)revec,
								&missing,
								tmp_md->multidval.n_dims,
								dsizes_evec,
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
								(void*)revec,
								NULL,
								tmp_md->multidval.n_dims,
								dsizes_evec,
								TEMPORARY,
								NULL,
								(NclObjClass)nclTypefloatClass
								);
	}
	att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

	dsizes[0] = *neval;
	reval = (float *)NclMalloc(*neval*sizeof(float));
	for( i = 0; i < *neval; i++ ) reval[i] = (float)eval[i];
	att_md = _NclCreateVal(
						   NULL,
						   NULL,
						   Ncl_MultiDValData,
						   0,
						   (void*)reval,
						   NULL,
						   1,
						   dsizes,
						   TEMPORARY,
						   NULL,
						   (NclObjClass)nclTypefloatClass
						   );
	_NclAddAtt(
			   att_id,
			   "eval",
			   att_md,
			   NULL
			   );

	att_md = _NclCreateVal(
						   NULL,
						   NULL,
						   Ncl_MultiDValData,
						   0,
						   (void*)pcvar,
						   NULL,
						   1,
						   dsizes,
						   TEMPORARY,
						   NULL,
						   (NclObjClass)nclTypefloatClass
						   );
	_NclAddAtt(
			   att_id,
			   "pcvar",
			   att_md,
			   NULL
			   );

	dsizes[0] = 1;
	rtrace = (float *)NclMalloc(sizeof(float));
	*rtrace = (float)(*trace);
	att_md = _NclCreateVal(
						   NULL,
						   NULL,
						   Ncl_MultiDValData,
						   0,
						   (void*)rtrace,
						   NULL,
						   1,
						   dsizes,
						   TEMPORARY,
						   NULL,
						   (NclObjClass)nclTypefloatClass
						   );
	_NclAddAtt(
			   att_id,
			   "trace",
			   att_md,
			   NULL
			   );
/*
 * Free unneeded memory.
 */
	free((double*)eval);
	free((double*)evec);
	free((double*)trace);
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


NhlErrorTypes eofcor_W( void )
{
/*
 * Input array variables
 */
  NclStackEntry data;
  NclMultiDValData tmp_md = NULL;
  int ndims, nrow, ncol, nobs, msta;
  int *neval, iopt = 0, jopt = 1, i, ier = 0;
  double xmsg;
  NclScalar missing;
/*
 * Work array variables.
 */
  double *cssm, *work, *weval;
  int   *iwork, *ifail;
  int lcssm, lwork, liwork, lifail;
  double *dinput;
/*
 * Attribute variables
 */
  int att_id;
  int dsizes[NCL_MAX_DIMENSIONS];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
  double *trace, *eval;
  float *pcvar;
  float *rtrace, *reval;
/*
 * Output array variables
 */
  double *evec;
  float *revec;
  int dsizes_evec[NCL_MAX_DIMENSIONS];
/*
 * Retrieve parameters
 */
  data = _NclGetArg(0,2,DONT_CARE);
  switch(data.kind) {
  case NclStk_VAR:
	tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
	break;
  case NclStk_VAL:
	tmp_md = (NclMultiDValData)data.u.data_obj;
	break;
  }
/*
 * The grid coming in must be at least 2-dimensional.
 */
  ndims = tmp_md->multidval.n_dims;
  if( ndims < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }
  msta = 1;
  for( i = 0; i <= ndims-2; i++ ) {
	msta *= tmp_md->multidval.dim_sizes[i];
  }
  ncol = msta;
  nobs = nrow = tmp_md->multidval.dim_sizes[ndims-1];
  if( msta < 1 || nobs < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor: The dimensions of the input array must both be at least 1");
    return(NhlFATAL);
  }
/*
 * Get missing value, if there is one.
 */
  if(!tmp_md->multidval.missing_value.has_missing) {
	if( tmp_md->multidval.data_type == NCL_double ) {
	  xmsg = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
	}
	else {
	  xmsg = (double)((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
	}
  }
  else {
	if( tmp_md->multidval.data_type == NCL_double ) {
	  xmsg = tmp_md->multidval.missing_value.value.doubleval;
	  missing.doubleval = xmsg;
	}
	else {
	  xmsg = (double)tmp_md->multidval.missing_value.value.floatval;
	  missing.floatval = tmp_md->multidval.missing_value.value.floatval;
	}
  }

/*
 * Check our input array to be sure it is a double or float.
 */
  if( tmp_md->multidval.data_type != NCL_float && tmp_md->multidval.data_type != NCL_double ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor: The input array must be a float or a double array");
    return(NhlFATAL);
  }
/*
 * Get number of eigenvalues and eigen vectors to be computed.
 */
  neval = (int *)NclGetArgValue(
            1,
            2, 
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            2);
/*
 * Allocate memory for return variable.
 */
  dsizes_evec[0] = *neval;
  for( i = 0; i <= ndims-2; i++ ) {
	dsizes_evec[i+1] = tmp_md->multidval.dim_sizes[i];
  }
  evec = (double *)NclMalloc(ncol*(*neval)*sizeof(double));
  if( evec == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Allocate memory for attributes.
 */
  trace = (double *)NclMalloc(sizeof(double));
  eval =  (double *)NclMalloc(*neval*sizeof(double));
  pcvar = (float *)NclMalloc(*neval*sizeof(float));
  if( trace == NULL || pcvar == NULL || eval == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor: Unable to allocate memory for attribute arrays");
    return(NhlFATAL);
  }
/*
 * Create a few more work arrays.  This is necessary to avoid having
 * these arrays created dynamically in the Fortran file (which makes
 * it Fortran 90, and unportable to some systems. 
 */
  lcssm  = msta*(msta+1)/2;
  lwork  = 8*msta;
  liwork = 5*msta;
  lifail = msta;
  cssm   = (double *)NclMalloc(lcssm*sizeof(double));
  work   = (double *)NclMalloc(lwork*sizeof(double));
  weval  = (double *)NclMalloc(lifail*sizeof(double));
  iwork  =   (int *)NclMalloc(liwork*sizeof(int));
  ifail  =   (int *)NclMalloc(lifail*sizeof(int));
  dinput = (double *)NclMalloc(nrow*ncol*sizeof(double));
  if( cssm == NULL || work == NULL || weval == NULL || iwork == NULL || ifail == NULL || dinput == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
/*
 * Convert input array so that tmp_md is not used.  This is so you don't
 * get funny business from tmp_md and the missing value. Problems arise
 * with the missing value if your data is float, but you try to convert
 * tmp_md to double with a type cast.
 */
  if( tmp_md->multidval.data_type == NCL_float ) {
	for( i = 0; i < nrow*ncol; i++ ) {
	  dinput[i] = (double)((float *)tmp_md->multidval.val)[i];
	}
  }
  else {
	for( i = 0; i < nrow*ncol; i++ ) {
	  dinput[i] = (double)((double *)tmp_md->multidval.val)[i];
	}
  }

/*
 * Call the Fortran 77 version of 'drveof' with the full argument list.
 */
  NGCALLF(ddrveof,DDRVEOF)(dinput,&nrow,&ncol,&nobs,&msta,&xmsg,neval,eval,
						   evec,pcvar,trace,&iopt,&jopt,cssm,&lcssm,
						   work,&lwork,weval,iwork,&liwork,ifail,&lifail,&ier);
/*
 * Check various possible error messages.
 */
  if (ier != 0) {
	if (ier == -1) {
	  NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcor: cssm contains one or more missing values\n" );
	}
	else if (ier == -88) {
	  NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcor: trace is equal to zero\n" );
	}
	else if (ier < 0) {
	  NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcor: The %d-th argument had an illegal value\n", abs(ier) );
	}
	else {
	  NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcor: %d eigenvectors failed to converge\n",ier);
	}
  }
/*
 * Free unneeded memory.
 */
  free((double*)work);
  free((double*)cssm);
  free((double*)weval);
  free((int*)iwork);
  free((int*)ifail);
  free((double*)dinput);
/*
 * Set up variable and attributes to return. Output data needs to be
 * same type as input data.  Since the default is for the data to be
 * double, if it is actually float, then we need to coerce it to float.
 */
  if( tmp_md->multidval.data_type == NCL_double ) {
	if(tmp_md->multidval.missing_value.has_missing) {
	  return_md = _NclCreateVal(
								NULL,
								NULL,
								Ncl_MultiDValData,
								0,
								(void*)evec,
								&missing,
								tmp_md->multidval.n_dims,
								dsizes_evec,
								TEMPORARY,
								NULL,
								(NclObjClass)nclTypedoubleClass
								);
	}
	else {
	  return_md = _NclCreateVal(
								NULL,
								NULL,
								Ncl_MultiDValData,
								0,
								(void*)evec,
								NULL,
								tmp_md->multidval.n_dims,
								dsizes_evec,
								TEMPORARY,
								NULL,
								(NclObjClass)nclTypedoubleClass
								);
	}
	att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

	dsizes[0] = *neval;
	att_md = _NclCreateVal(
						   NULL,
						   NULL,
						   Ncl_MultiDValData,
						   0,
						   (void*)eval,
						   NULL,
						   1,
						   dsizes,
						   TEMPORARY,
						   NULL,
						   (NclObjClass)nclTypedoubleClass
						   );
	_NclAddAtt(
			   att_id,
			   "eval",
			   att_md,
			   NULL
			   );

	att_md = _NclCreateVal(
						   NULL,
						   NULL,
						   Ncl_MultiDValData,
						   0,
						   (void*)pcvar,
						   NULL,
						   1,
						   dsizes,
						   TEMPORARY,
						   NULL,
						   (NclObjClass)nclTypefloatClass
						   );
	_NclAddAtt(
			   att_id,
			   "pcvar",
			   att_md,
			   NULL
			   );

	dsizes[0] = 1;
	att_md = _NclCreateVal(
						   NULL,
						   NULL,
						   Ncl_MultiDValData,
						   0,
						   (void*)trace,
						   NULL,
						   1,
						   dsizes,
						   TEMPORARY,
						   NULL,
						   (NclObjClass)nclTypedoubleClass
						   );
	_NclAddAtt(
			   att_id,
			   "trace",
			   att_md,
			   NULL
			   );
  }
  else {
/*
 * Our input data was real, so convert output data to real.
 */
	revec = (float *)NclMalloc(ncol*(*neval)*sizeof(float));
	for( i = 0; i < ncol*(*neval); i++ ) revec[i] = (float)evec[i];
	if(tmp_md->multidval.missing_value.has_missing) {
	  return_md = _NclCreateVal(
								NULL,
								NULL,
								Ncl_MultiDValData,
								0,
								(void*)revec,
								&missing,
								tmp_md->multidval.n_dims,
								dsizes_evec,
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
								(void*)revec,
								NULL,
								tmp_md->multidval.n_dims,
								dsizes_evec,
								TEMPORARY,
								NULL,
								(NclObjClass)nclTypefloatClass
								);
	}
	att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

	dsizes[0] = *neval;
	reval = (float *)NclMalloc(*neval*sizeof(float));
	for( i = 0; i < *neval; i++ ) reval[i] = (float)eval[i];
	att_md = _NclCreateVal(
						   NULL,
						   NULL,
						   Ncl_MultiDValData,
						   0,
						   (void*)reval,
						   NULL,
						   1,
						   dsizes,
						   TEMPORARY,
						   NULL,
						   (NclObjClass)nclTypefloatClass
						   );
	_NclAddAtt(
			   att_id,
			   "eval",
			   att_md,
			   NULL
			   );

	att_md = _NclCreateVal(
						   NULL,
						   NULL,
						   Ncl_MultiDValData,
						   0,
						   (void*)pcvar,
						   NULL,
						   1,
						   dsizes,
						   TEMPORARY,
						   NULL,
						   (NclObjClass)nclTypefloatClass
						   );
	_NclAddAtt(
			   att_id,
			   "pcvar",
			   att_md,
			   NULL
			   );

	dsizes[0] = 1;
	rtrace = (float *)NclMalloc(sizeof(float));
	*rtrace = (float)(*trace);
	att_md = _NclCreateVal(
						   NULL,
						   NULL,
						   Ncl_MultiDValData,
						   0,
						   (void*)rtrace,
						   NULL,
						   1,
						   dsizes,
						   TEMPORARY,
						   NULL,
						   (NclObjClass)nclTypefloatClass
						   );
	_NclAddAtt(
			   att_id,
			   "trace",
			   att_md,
			   NULL
			   );
/*
 * Free unneeded memory.
 */
	free((double*)eval);
	free((double*)evec);
	free((double*)trace);
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


NhlErrorTypes eofcov_ts_W( void )
{
/*
 * Input array variables
 */
  NclStackEntry data1, data2;
  NclMultiDValData tmp_md[2];
  int ndims1, ndims2, nrow, ncol, nobs, msta;
  int neval, ntime, jopt = 0, i, ier = 0;
  double xmsg;
  NclScalar missing;
/*
 * Work array variables.
 */
  double *wrk, *wx;
  int lwrk, lwx;
  double *dinput1, *dinput2;
/*
 * Output array variables
 */
  double *evec_ts;
  float *revec_ts;	
  int dsizes_evec_ts[2];
/*
 * Retrieve parameters
 */
  data1 = _NclGetArg(0,2,DONT_CARE);
  switch(data1.kind) {
  case NclStk_VAR:
	tmp_md[0] = _NclVarValueRead(data1.u.data_var,NULL,NULL);
	break;
  case NclStk_VAL:
	tmp_md[0] = (NclMultiDValData)data1.u.data_obj;
	break;
  }
  data2 = _NclGetArg(1,2,DONT_CARE);
  switch(data2.kind) {
  case NclStk_VAR:
	tmp_md[1] = _NclVarValueRead(data2.u.data_var,NULL,NULL);
	break;
  case NclStk_VAL:
	tmp_md[1] = (NclMultiDValData)data2.u.data_obj;
	break;
  }
/*
 * Check the input grids.  They both must be at least two dimensional and
 * have the same number of dimensions.  All but the last dimension of the
 * first input array must be the same as all the but first dimension of
 * the second input array.
 */
  ndims1 = tmp_md[0]->multidval.n_dims;
  ndims2 = tmp_md[1]->multidval.n_dims;
  if( ndims1 < 2 || ndims1 != ndims2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_ts: The input arrays must be at least 2-dimensional and have the same number of dimensions");
    return(NhlFATAL);
  }
  msta = 1;
  for( i = 0; i <= ndims1-2; i++ ) {
	if( tmp_md[0]->multidval.dim_sizes[i] != tmp_md[1]->multidval.dim_sizes[i+1] ) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_ts: All but the last dimension of the first input array must be the same as all but the first dimension of the second input array");
	  return(NhlFATAL);
	}
	msta *= tmp_md[0]->multidval.dim_sizes[i];
  }
  ncol = msta;
  nobs = nrow = ntime = tmp_md[0]->multidval.dim_sizes[ndims1-1];
  neval = tmp_md[1]->multidval.dim_sizes[0];
  if( msta < 1 || nobs < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_ts: The dimensions of the input array must both be at least 1");
    return(NhlFATAL);
  }
/*
 * Get missing value, if there is one.
 */
  if(!tmp_md[0]->multidval.missing_value.has_missing) {
	if( tmp_md[0]->multidval.data_type == NCL_double ) {
	  xmsg = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
	}
	else {
	  xmsg = (double)((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
	}
  }
  else {
	if( tmp_md[0]->multidval.data_type == NCL_double ) {
	  xmsg = tmp_md[0]->multidval.missing_value.value.doubleval;
	  missing.doubleval = xmsg;
	}
	else {
	  xmsg = (double)tmp_md[0]->multidval.missing_value.value.floatval;
	  missing.floatval = tmp_md[0]->multidval.missing_value.value.floatval;
	}
  }
/*
 * Check our input arrays to be sure they are double or float and promote
 * them to double precision if they are floats.
 */
  if( tmp_md[0]->multidval.data_type != tmp_md[1]->multidval.data_type ||
	  (tmp_md[0]->multidval.data_type != NCL_double &&
	  tmp_md[0]->multidval.data_type != NCL_float) ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_ts: The input arrays must be of the same type (float or double)");
    return(NhlFATAL);
  }
/*
 * Convert input arrays so that tmp_md is not used.  This is so you don't
 * get funny business from tmp_md and the missing value. Problems arise
 * with the missing value if your data is float, but you try to convert
 * tmp_md to double with a type cast.
 */
  dinput1 = (double *)NclMalloc(nrow*msta*sizeof(double));
  dinput2 = (double *)NclMalloc(msta*neval*sizeof(double));

  if( dinput1 == NULL || dinput2 == NULL ) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_ts: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  if( tmp_md[0]->multidval.data_type == NCL_float)  {
	for( i = 0; i < nrow*msta; i++ ) {
	  dinput1[i] = (double)((float *)tmp_md[0]->multidval.val)[i];
	}
	for( i = 0; i < msta*neval; i++ ) {
	  dinput2[i] = (double)((float *)tmp_md[1]->multidval.val)[i];
	}
  }
  else {
	for( i = 0; i < nrow*msta; i++ ) {
	  dinput1[i] = (double)((double *)tmp_md[0]->multidval.val)[i];
	}
	for( i = 0; i < msta*neval; i++ ) {
	  dinput2[i] = (double)((double *)tmp_md[1]->multidval.val)[i];
	}
  }
/*
 * Allocate memory for return variable.
 */
  dsizes_evec_ts[0] = neval;
  dsizes_evec_ts[1] = ntime;
  evec_ts = (double *)NclMalloc(ntime*neval*sizeof(double));
  if( evec_ts == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_ts: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Create a couple of work arrays.  This is necessary to avoid having
 * these arrays created dynamically in the Fortran file (which makes
 * it Fortran 90, and unportable to some systems. 
 */
  lwrk = nobs;
  lwx  = nrow*ncol;
  wrk  = (double *)NclMalloc(lwrk*sizeof(double));
  wx   = (double *)NclMalloc(lwx*sizeof(double));
  if( wrk == NULL || wx == NULL ) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_ts: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
/*
 * Call the Fortran 77 version of 'drveof' with the full argument list.
 */
  NGCALLF(deofts7,DEOFTS7)(dinput1,&nrow,&ncol,&nobs,&msta,&xmsg,&neval,
						   dinput2,&jopt,wx,wrk,evec_ts,&ier);
/*
 * Check various possible error messages.
 */
  if (ier != 0) {
	if (ier == -1) {
	  NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov_ts: cssm contains one or more missing values\n" );
	}
	else if (ier == -88) {
	  NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov_ts: trace is equal to zero\n" );
	}
	else if (ier < 0) {
	  NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov_ts: The %d-th argument had an illegal value\n", abs(ier) );
	}
	else {
	  NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov_ts: %d eigenvectors failed to converge\n",ier);
	}
  }
/*
 * Free unneeded memory.
 */
  free((double*)dinput1);
  free((double*)dinput2);
  free((double*)wx);
  free((double*)wrk);
/*
 * Return output grid to NCL. Return float or double depending on whether
 * input was float or double.
 */
  if( tmp_md[0]->multidval.data_type == NCL_float ) {
	revec_ts = (float *)NclMalloc(ntime*neval*sizeof(float));
	for( i = 0; i < ntime*neval; i++ ) revec_ts[i] = (float)evec_ts[i];
/*
 * Free unneeded memory before returning.
 */
	free((double*)evec_ts);

	if(!tmp_md[0]->multidval.missing_value.has_missing) {
	  return(NclReturnValue((void*)revec_ts,2,dsizes_evec_ts,NULL,NCL_float,0));
	}
	else {
	  return(NclReturnValue((void*)revec_ts,2,dsizes_evec_ts,&missing,NCL_float,0));
	}
  }
  else {
	if(!tmp_md[0]->multidval.missing_value.has_missing) {
	  return(NclReturnValue((void*)evec_ts,2,dsizes_evec_ts,NULL,NCL_double,0));
	}
	else {
	  return(NclReturnValue((void*)evec_ts,2,dsizes_evec_ts,&missing,NCL_double,0));
	}
  }
}


NhlErrorTypes eofcor_ts_W( void )
{
/*
 * Input array variables
 */
  NclStackEntry data1, data2;
  NclMultiDValData tmp_md[2];
  int ndims1, ndims2, nrow, ncol, nobs, msta;
  int neval, ntime, jopt = 1, i, ier = 0;
  double xmsg;
  NclScalar missing;
/*
 * Work array variables.
 */
  double *wrk, *wx;
  int lwrk, lwx;
  double *dinput1, *dinput2;
/*
 * Output array variables
 */
  double *evec_ts;
  float *revec_ts;	
  int dsizes_evec_ts[2];
/*
 * Retrieve parameters
 */
  data1 = _NclGetArg(0,2,DONT_CARE);
  switch(data1.kind) {
  case NclStk_VAR:
	tmp_md[0] = _NclVarValueRead(data1.u.data_var,NULL,NULL);
	break;
  case NclStk_VAL:
	tmp_md[0] = (NclMultiDValData)data1.u.data_obj;
	break;
  }
  data2 = _NclGetArg(1,2,DONT_CARE);
  switch(data2.kind) {
  case NclStk_VAR:
	tmp_md[1] = _NclVarValueRead(data2.u.data_var,NULL,NULL);
	break;
  case NclStk_VAL:
	tmp_md[1] = (NclMultiDValData)data2.u.data_obj;
	break;
  }
/*
 * Check the input grids.  They both must be at least two dimensional and
 * have the same number of dimensions.  All but the last dimension of the
 * first input array must be the same as all the but first dimension of
 * the second input array.
 */
  ndims1 = tmp_md[0]->multidval.n_dims;
  ndims2 = tmp_md[1]->multidval.n_dims;
  if( ndims1 < 2 || ndims1 != ndims2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_ts: The input arrays must be at least 2-dimensional and have the same number of dimensions");
    return(NhlFATAL);
  }
  msta = 1;
  for( i = 0; i <= ndims1-2; i++ ) {
	if( tmp_md[0]->multidval.dim_sizes[i] != tmp_md[1]->multidval.dim_sizes[i+1] ) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_ts: All but the last dimension of the first input array must be the same as all but the first dimension of the second input array");
	  return(NhlFATAL);
	}
	msta *= tmp_md[0]->multidval.dim_sizes[i];
  }
  ncol = msta;
  nobs = nrow = ntime = tmp_md[0]->multidval.dim_sizes[ndims1-1];
  neval = tmp_md[1]->multidval.dim_sizes[0];
  if( msta < 1 || nobs < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_ts: The dimensions of the input array must both be at least 1");
    return(NhlFATAL);
  }
/*
 * Get missing value, if there is one.
 */
  if(!tmp_md[0]->multidval.missing_value.has_missing) {
	if( tmp_md[0]->multidval.data_type == NCL_double ) {
	  xmsg = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
	}
	else {
	  xmsg = (double)((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
	}
  }
  else {
	if( tmp_md[0]->multidval.data_type == NCL_double ) {
	  xmsg = tmp_md[0]->multidval.missing_value.value.doubleval;
	  missing.doubleval = xmsg;
	}
	else {
	  xmsg = (double)tmp_md[0]->multidval.missing_value.value.floatval;
	  missing.floatval = tmp_md[0]->multidval.missing_value.value.floatval;
	}
  }
/*
 * Check our input arrays to be sure they are double or float and promote
 * them to double precision if they are floats.
 */
  if( tmp_md[0]->multidval.data_type != tmp_md[1]->multidval.data_type ||
	  (tmp_md[0]->multidval.data_type != NCL_double &&
	  tmp_md[0]->multidval.data_type != NCL_float) ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_ts: The input arrays must be of the same type (float or double)");
    return(NhlFATAL);
  }
/*
 * Convert input arrays so that tmp_md is not used.  This is so you don't
 * get funny business from tmp_md and the missing value. Problems arise
 * with the missing value if your data is float, but you try to convert
 * tmp_md to double with a type cast.
 */
  dinput1 = (double *)NclMalloc(nrow*msta*sizeof(double));
  dinput2 = (double *)NclMalloc(msta*neval*sizeof(double));

  if( dinput1 == NULL || dinput2 == NULL ) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_ts: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  if( tmp_md[0]->multidval.data_type == NCL_float)  {
	for( i = 0; i < nrow*msta; i++ ) {
	  dinput1[i] = (double)((float *)tmp_md[0]->multidval.val)[i];
	}
	for( i = 0; i < msta*neval; i++ ) {
	  dinput2[i] = (double)((float *)tmp_md[1]->multidval.val)[i];
	}
  }
  else {
	for( i = 0; i < nrow*msta; i++ ) {
	  dinput1[i] = (double)((double *)tmp_md[0]->multidval.val)[i];
	}
	for( i = 0; i < msta*neval; i++ ) {
	  dinput2[i] = (double)((double *)tmp_md[1]->multidval.val)[i];
	}
  }
/*
 * Allocate memory for return variable.
 */
  dsizes_evec_ts[0] = neval;
  dsizes_evec_ts[1] = ntime;
  evec_ts = (double *)NclMalloc(ntime*neval*sizeof(double));
  if( evec_ts == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_ts: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Create a couple of work arrays.  This is necessary to avoid having
 * these arrays created dynamically in the Fortran file (which makes
 * it Fortran 90, and unportable to some systems. 
 */
  lwrk = nobs;
  lwx  = nrow*ncol;
  wrk  = (double *)NclMalloc(lwrk*sizeof(double));
  wx   = (double *)NclMalloc(lwx*sizeof(double));
  if( wrk == NULL || wx == NULL ) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_ts: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
/*
 * Call the Fortran 77 version of 'drveof' with the full argument list.
 */
  NGCALLF(deofts7,DEOFTS7)(dinput1,&nrow,&ncol,&nobs,&msta,&xmsg,&neval,
						   dinput2,&jopt,wx,wrk,evec_ts,&ier);
/*
 * Check various possible error messages.
 */
  if (ier != 0) {
	if (ier == -1) {
	  NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcor_ts: cssm contains one or more missing values\n" );
	}
	else if (ier == -88) {
	  NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcor_ts: trace is equal to zero\n" );
	}
	else if (ier < 0) {
	  NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcor_ts: The %d-th argument had an illegal value\n", abs(ier) );
	}
	else {
	  NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcor_ts: %d eigenvectors failed to converge\n",ier);
	}
  }
/*
 * Free unneeded memory.
 */
  free((double*)dinput1);
  free((double*)dinput2);
  free((double*)wx);
  free((double*)wrk);
/*
 * Return output grid to NCL. Return float or double depending on whether
 * input was float or double.
 */
  if( tmp_md[0]->multidval.data_type == NCL_float ) {
	revec_ts = (float *)NclMalloc(ntime*neval*sizeof(float));
	for( i = 0; i < ntime*neval; i++ ) revec_ts[i] = (float)evec_ts[i];
/*
 * Free unneeded memory before returning.
 */
	free((double*)evec_ts);

	if(!tmp_md[0]->multidval.missing_value.has_missing) {
	  return(NclReturnValue((void*)revec_ts,2,dsizes_evec_ts,NULL,NCL_float,0));
	}
	else {
	  return(NclReturnValue((void*)revec_ts,2,dsizes_evec_ts,&missing,NCL_float,0));
	}
  }
  else {
	if(!tmp_md[0]->multidval.missing_value.has_missing) {
	  return(NclReturnValue((void*)evec_ts,2,dsizes_evec_ts,NULL,NCL_double,0));
	}
	else {
	  return(NclReturnValue((void*)evec_ts,2,dsizes_evec_ts,&missing,NCL_double,0));
	}
  }
}



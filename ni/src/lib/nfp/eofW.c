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


extern void NGCALLF(ddrveof,DDRVEOF)(double *,int *,int *,int *,int *,
                                     double *,int *,double *, double *,
                                     float*,double *,int *,int *,double*,
                                     int *, double *,int *,double *,int *,
                                     int *,int *,int *,int *);

extern void NGCALLF(deofts7,DEOFTS7)(double *,int *,int *,int *,int *,
                                     double *,int *, double *,int *,
                                     double *,double *,double *,int *);

NhlErrorTypes eofcov_W( void )
{
/*
 * Input array variables
 */
  void *x;
  double *dx;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  NclScalar missing_x, missing_rx, missing_dx;
  NclBasicDataTypes type_x;
  int nrow, ncol, nobs, msta, total_size_x;
  int *neval, iopt = 0, jopt = 0, i, ier = 0;
/*
 * Work array variables.
 */
  double *cssm, *work, *weval;
  int   *iwork, *ifail;
  int lcssm, lwork, liwork, lifail;
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
  int total_size_evec, dsizes_evec[NCL_MAX_DIMENSIONS];
/*
 * Retrieve parameters
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
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_x < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }
/*
 * Check dimension sizes.
 */
  msta = 1;
  for( i = 0; i <= ndims_x-2; i++ ) msta *= dsizes_x[i];
  ncol = msta;
  nobs = nrow = dsizes_x[ndims_x-1];

  total_size_x = ncol * nrow;

  if( msta < 1 || nobs < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov: The dimensions of the input array must both be at least 1");
    return(NhlFATAL);
  }
/*
 * Coerce missing values to double.
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
 * Get the default missing value.
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
 * Coerce x to double if necessary.
 */
  if(type_x != NCL_double) {
    dx = (double*)NclMalloc(sizeof(double)*total_size_x);
    if( dx == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov: Unable to allocate memory for coercing x array to double precision");
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
 * Input is already double.
 */
    dx = (double*)x;
  }

/*
 * Allocate memory for return variable.
 */
  dsizes_evec[0] = *neval;
  for( i = 0; i <= ndims_x-2; i++ ) dsizes_evec[i+1] = dsizes_x[i];

  total_size_evec = *neval * ncol;

  evec = (double *)NclMalloc(total_size_evec*sizeof(double));
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
  if( cssm == NULL || work == NULL || weval == NULL || iwork == NULL || 
      ifail == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
/*
 * Call the Fortran 77 version of 'drveof' with the full argument list.
 */
  NGCALLF(ddrveof,DDRVEOF)(dx,&nrow,&ncol,&nobs,&msta,&missing_dx.doubleval,
                           neval,eval,evec,pcvar,trace,&iopt,&jopt,
                           cssm,&lcssm,work,&lwork,weval,iwork,&liwork,
                           ifail,&lifail,&ier);
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
  if((void*)dx != x) {
    NclFree(dx);
  }
  NclFree(work);
  NclFree(cssm);
  NclFree(weval);
  NclFree(iwork);
  NclFree(ifail);
/*
 * Return values. 
 */
  if(type_x != NCL_double) {
/*
 * Copy double values to float values.
 */
    revec = (float*)NclMalloc(total_size_evec*sizeof(float));
    if( revec == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    for( i = 0; i < total_size_evec; i++ ) revec[i] = (float)evec[i];

/*
 * Free up double precision array.
 */
    NclFree(evec);
/*
 * Set up return value.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)revec,
                              &missing_rx,
                              ndims_x,
                              dsizes_evec,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypefloatClass
                              );
/*
 * Set up attributes to return.
 */
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);
/*
 * Coerce eval to float.
 */
    reval = (float *)NclMalloc(*neval*sizeof(float));
    for( i = 0; i < *neval; i++ ) reval[i] = (float)eval[i];
/*
 * Free double precision eval.
 */
    NclFree(eval);

    dsizes[0] = *neval;
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

/*
 * pcvar is returned as float no matter what. 
 */
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

/*
 * Coerce trace to float.
 */
    rtrace = (float *)NclMalloc(sizeof(float));
    *rtrace = (float)(*trace);
    dsizes[0] = 1;
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
  }
  else {
/*
 *  Return doubles.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)evec,
                              &missing_dx,
                              ndims_x,
                              dsizes_evec,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypedoubleClass
                              );

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
/*
 * pcvar is returned as float no matter what.
 */
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
  void *x;
  double *dx;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  NclScalar missing_x, missing_rx, missing_dx;
  NclBasicDataTypes type_x;
  int nrow, ncol, nobs, msta, total_size_x;
  int *neval, iopt = 0, jopt = 1, i, ier = 0;
/*
 * Work array variables.
 */
  double *cssm, *work, *weval;
  int   *iwork, *ifail;
  int lcssm, lwork, liwork, lifail;
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
  int total_size_evec, dsizes_evec[NCL_MAX_DIMENSIONS];
/*
 * Retrieve parameters
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
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_x < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }
/*
 * Check dimension sizes.
 */
  msta = 1;
  for( i = 0; i <= ndims_x-2; i++ ) msta *= dsizes_x[i];
  ncol = msta;
  nobs = nrow = dsizes_x[ndims_x-1];

  total_size_x = ncol * nrow;

  if( msta < 1 || nobs < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor: The dimensions of the input array must both be at least 1");
    return(NhlFATAL);
  }
/*
 * Coerce missing values to double.
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
 * Get the default missing value.
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
 * Coerce x to double if necessary.
 */
  if(type_x != NCL_double) {
    dx = (double*)NclMalloc(sizeof(double)*total_size_x);
    if( dx == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor: Unable to allocate memory for coercing x array to double precision");
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
 * Input is already double.
 */
    dx = (double*)x;
  }

/*
 * Allocate memory for return variable.
 */
  dsizes_evec[0] = *neval;
  for( i = 0; i <= ndims_x-2; i++ ) dsizes_evec[i+1] = dsizes_x[i];

  total_size_evec = *neval * ncol;

  evec = (double *)NclMalloc(total_size_evec*sizeof(double));
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
  if( cssm == NULL || work == NULL || weval == NULL || iwork == NULL || 
      ifail == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
/*
 * Call the Fortran 77 version of 'drveof' with the full argument list.
 */
  NGCALLF(ddrveof,DDRVEOF)(dx,&nrow,&ncol,&nobs,&msta,&missing_dx.doubleval,
                           neval,eval,evec,pcvar,trace,&iopt,&jopt,
                           cssm,&lcssm,work,&lwork,weval,iwork,&liwork,
                           ifail,&lifail,&ier);
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
  if((void*)dx != x) {
    NclFree(dx);
  }
  NclFree(work);
  NclFree(cssm);
  NclFree(weval);
  NclFree(iwork);
  NclFree(ifail);
/*
 * Return values. 
 */
  if(type_x != NCL_double) {
/*
 * Copy double values to float values.
 */
    revec = (float*)NclMalloc(total_size_evec*sizeof(float));
    if( revec == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    for( i = 0; i < total_size_evec; i++ ) revec[i] = (float)evec[i];

/*
 * Free up double precision array.
 */
    NclFree(evec);
/*
 * Set up return value.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)revec,
                              &missing_rx,
                              ndims_x,
                              dsizes_evec,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypefloatClass
                              );
/*
 * Set up attributes to return.
 */
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);
/*
 * Coerce eval to float.
 */
    reval = (float *)NclMalloc(*neval*sizeof(float));
    for( i = 0; i < *neval; i++ ) reval[i] = (float)eval[i];
/*
 * Free double precision eval.
 */
    NclFree(eval);

    dsizes[0] = *neval;
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

/*
 * pcvar is returned as float no matter what. 
 */
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

/*
 * Coerce trace to float.
 */
    rtrace = (float *)NclMalloc(sizeof(float));
    *rtrace = (float)(*trace);
    dsizes[0] = 1;
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
  }
  else {
/*
 *  Return doubles.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)evec,
                              &missing_dx,
                              ndims_x,
                              dsizes_evec,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypedoubleClass
                              );

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
/*
 * pcvar is returned as float no matter what.
 */
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
  void *x, *evec;
  double *dx, *devec;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  int ndims_evec, dsizes_evec[NCL_MAX_DIMENSIONS], has_missing_evec;
  NclScalar missing_x, missing_evec, missing_devec, missing_rx, missing_dx;
  NclBasicDataTypes type_x, type_evec;
  int nrow, ncol, nobs, msta, total_size_x, total_size_evec;
  int neval, ntime, jopt = 0, i, ier = 0;
/*
 * Work array variables.
 */
  double *wrk, *wx;
  int lwrk, lwx;
/*
 * Output array variables
 */
  double *evec_ts;
  float *revec_ts;      
  int dsizes_evec_ts[2];
/*
 * Retrieve parameters
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
  evec = (void*)NclGetArgValue(
           1,
           2,
           &ndims_evec, 
           dsizes_evec,
           &missing_evec,
           &has_missing_evec,
           &type_evec,
           2);
/*
 * Check the input grids.  They both must be at least two dimensional and
 * have the same number of dimensions.  All but the last dimension of the
 * first input array must be the same as all the but first dimension of
 * the second input array.
 */
  if( ndims_x < 2 || ndims_x != ndims_evec ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_ts: The input arrays must be at least 2-dimensional and have the same number of dimensions");
    return(NhlFATAL);
  }
  msta = 1;
  for( i = 0; i <= ndims_x-2; i++ ) {
    if( dsizes_x[i] != dsizes_evec[i+1] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_ts: All but the last dimension of the first input array must be the same as all but the first dimension of the second input array");
      return(NhlFATAL);
    }
    msta *= dsizes_x[i];
  }
  ncol = msta;
  nobs = nrow = ntime = dsizes_x[ndims_x-1];
  neval = dsizes_evec[0];

  total_size_x    = ncol * nrow;
  total_size_evec = msta * neval;

  if( msta < 1 || nobs < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_ts: The dimensions of the input array must both be at least 1");
    return(NhlFATAL);
  }
/*
 * Coerce x missing value to double.
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
 * Get the default missing value.
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
 * Coerce x to double if necessary.
 */
  if(type_x != NCL_double) {
    dx = (double*)NclMalloc(sizeof(double)*total_size_x);
    if( dx == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_ts: Unable to allocate memory for coercing x array to double precision");
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
 * Input is already double.
 */
    dx = (double*)x;
  }

/*
 * Coerce evec to double if necessary.
 */
  if(type_evec != NCL_double) {
    devec = (double*)NclMalloc(sizeof(double)*total_size_evec);
    if( devec == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_ts: Unable to allocate memory for coercing evec array to double precision");
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
  }
  else {
/*
 * Input is already double.
 */
    devec = (double*)evec;
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
  NGCALLF(deofts7,DEOFTS7)(dx,&nrow,&ncol,&nobs,&msta,&missing_dx.doubleval,
                           &neval,devec,&jopt,wx,wrk,evec_ts,&ier);
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
  if((void*)dx != x) {
    NclFree(dx);
  }
  if((void*)devec != evec) {
    NclFree(devec);
  }
  NclFree(wx);
  NclFree(wrk);
/*
 * Return values. 
 */
  if(type_x != NCL_double && type_evec != NCL_double) {
/*
 * Neither input array is double, so return float values.
 *
 * First copy double values to float values.
 */
    revec_ts = (float *)NclMalloc(ntime*neval*sizeof(float));
    if( revec_ts == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_ts: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    for( i = 0; i < ntime*neval; i++ ) revec_ts[i] = (float)evec_ts[i];
/*
 * Free up double precision array.
 */
    NclFree(evec_ts);

/*
 * Return float values. 
 */
    return(NclReturnValue((void*)revec_ts,2,dsizes_evec_ts,&missing_rx,
                          NCL_float,0));
  }
  else {
/*
 * Return double values. 
 */
    return(NclReturnValue((void*)evec_ts,2,dsizes_evec_ts,&missing_dx,
                          NCL_double,0));
  }
}


NhlErrorTypes eofcor_ts_W( void )
{
/*
 * Input array variables
 */
  void *x, *evec;
  double *dx, *devec;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  int ndims_evec, dsizes_evec[NCL_MAX_DIMENSIONS], has_missing_evec;
  NclScalar missing_x, missing_evec, missing_devec, missing_rx, missing_dx;
  NclBasicDataTypes type_x, type_evec;
  int nrow, ncol, nobs, msta, total_size_x, total_size_evec;
  int neval, ntime, jopt = 1, i, ier = 0;
/*
 * Work array variables.
 */
  double *wrk, *wx;
  int lwrk, lwx;
/*
 * Output array variables
 */
  double *evec_ts;
  float *revec_ts;      
  int dsizes_evec_ts[2];
/*
 * Retrieve parameters
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
  evec = (void*)NclGetArgValue(
           1,
           2,
           &ndims_evec, 
           dsizes_evec,
           &missing_evec,
           &has_missing_evec,
           &type_evec,
           2);
/*
 * Check the input grids.  They both must be at least two dimensional and
 * have the same number of dimensions.  All but the last dimension of the
 * first input array must be the same as all the but first dimension of
 * the second input array.
 */
  if( ndims_x < 2 || ndims_x != ndims_evec ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_ts: The input arrays must be at least 2-dimensional and have the same number of dimensions");
    return(NhlFATAL);
  }
  msta = 1;
  for( i = 0; i <= ndims_x-2; i++ ) {
    if( dsizes_x[i] != dsizes_evec[i+1] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_ts: All but the last dimension of the first input array must be the same as all but the first dimension of the second input array");
      return(NhlFATAL);
    }
    msta *= dsizes_x[i];
  }
  ncol = msta;
  nobs = nrow = ntime = dsizes_x[ndims_x-1];
  neval = dsizes_evec[0];

  total_size_x    = ncol * nrow;
  total_size_evec = msta * neval;

  if( msta < 1 || nobs < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_ts: The dimensions of the input array must both be at least 1");
    return(NhlFATAL);
  }
/*
 * Coerce x missing value to double.
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
 * Get the default missing value.
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
 * Coerce x to double if necessary.
 */
  if(type_x != NCL_double) {
    dx = (double*)NclMalloc(sizeof(double)*total_size_x);
    if( dx == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_ts: Unable to allocate memory for coercing x array to double precision");
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
 * Input is already double.
 */
    dx = (double*)x;
  }

/*
 * Coerce evec to double if necessary.
 */
  if(type_evec != NCL_double) {
    devec = (double*)NclMalloc(sizeof(double)*total_size_evec);
    if( devec == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_ts: Unable to allocate memory for coercing evec array to double precision");
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
  }
  else {
/*
 * Input is already double.
 */
    devec = (double*)evec;
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
  NGCALLF(deofts7,DEOFTS7)(dx,&nrow,&ncol,&nobs,&msta,&missing_dx.doubleval,
                           &neval,devec,&jopt,wx,wrk,evec_ts,&ier);
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
  if((void*)dx != x) {
    NclFree(dx);
  }
  if((void*)devec != evec) {
    NclFree(devec);
  }
  NclFree(wx);
  NclFree(wrk);
/*
 * Return values. 
 */
  if(type_x != NCL_double && type_evec != NCL_double) {
/*
 * Neither input array is double, so return float values.
 *
 * First copy double values to float values.
 */
    revec_ts = (float *)NclMalloc(ntime*neval*sizeof(float));
    if( revec_ts == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_ts: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    for( i = 0; i < ntime*neval; i++ ) revec_ts[i] = (float)evec_ts[i];
/*
 * Free up double precision array.
 */
    NclFree(evec_ts);

/*
 * Return float values. 
 */
    return(NclReturnValue((void*)revec_ts,2,dsizes_evec_ts,&missing_rx,
                          NCL_float,0));
  }
  else {
/*
 * Return double values. 
 */
    return(NclReturnValue((void*)evec_ts,2,dsizes_evec_ts,&missing_dx,
                          NCL_double,0));
  }
}



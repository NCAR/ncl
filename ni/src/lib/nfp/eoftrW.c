#include <stdio.h>
#include <math.h>
#include "wrapper.h"

extern void NGCALLF(ddrveoft,DDRVEOFT)(double *, int *, int *, int *, int *,
                                      double *, int *, double *, double *,
                                      float *, double *, int *, int *,
                                      double *, long long int *, double *,
                                      int *, int *, int *, int *, int *,
                                      double *, double *, int *);

extern NGCALLF(dtncleof,DTNCLEOF)(double *, int *, int *, int *, int *,
                                 double *, int *, double *, double *,
                                 float *, double *, int *, int *, double *, 
                                 double *, double *, double *, double *,
                                 double *, long long int *, double *, int *, 
                                 int *, int *, int *, int *, int *);

NhlErrorTypes eofcov_tr_W( void )
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
  double *cssm, *work, *teof, *weval;
  int   *iwork, *ifail;
  int lwork, liwork, lifail;
  long long int lcssm;
/*
 * Attribute variables
 */
  int att_id;
  int dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
  int *eof_function;
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
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_tr: The input array must be at least 2-dimensional");
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
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_tr: The dimensions of the input array must both be at least 1");
    return(NhlFATAL);
  }
/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Coerce x to double if necessary.
 */
  dx = coerce_input_double(x,type_x,total_size_x,has_missing_x,&missing_x,
                           &missing_dx);
  if( dx == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_tr: Unable to allocate memory for coercing x array to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate memory for return variable.
 */
  dsizes_evec[0] = *neval;
  for( i = 0; i <= ndims_x-2; i++ ) dsizes_evec[i+1] = dsizes_x[i];

  total_size_evec = *neval * ncol;

  evec = (double *)calloc(total_size_evec,sizeof(double));
  if( evec == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_tr: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Allocate memory for attributes.
 */
  trace = (double *)calloc(1,sizeof(double));
  eval  = (double *)calloc(*neval,sizeof(double));
  pcvar =  (float *)calloc(*neval,sizeof(float));
  if( trace == NULL || pcvar == NULL || eval == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_tr: Unable to allocate memory for attribute arrays");
    return(NhlFATAL);
  }
/*
 * Create a few more work arrays.  This is necessary to avoid having
 * these arrays created dynamically in the Fortran file (which makes
 * it Fortran 90, and unportable to some systems. 
 */
  lcssm  = nobs*(nobs+1)/2;
  lwork  = 8*nobs;
  liwork = 5*nobs;
  lifail = nobs;
  cssm   = (double *)calloc(lcssm,sizeof(double));
  work   = (double *)calloc(lwork,sizeof(double));
  teof   = (double *)calloc(*neval*nobs,sizeof(double));
  iwork  =    (int *)calloc(liwork,sizeof(int));
  ifail  =    (int *)calloc(lifail,sizeof(int));
  weval  = (double *)calloc(lifail,sizeof(double));

  if( cssm == NULL || work == NULL || teof == NULL || iwork == NULL ||
      ifail == NULL || weval == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_tr: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
/*
 * Call the Fortran 77 version of 'drveof' with the full argument list.
 */
  NGCALLF(ddrveoft,DDRVEOFT)(dx,&nrow,&ncol,&nobs,&msta,&missing_dx.doubleval,
                             neval,eval,evec,pcvar,trace,&iopt,&jopt,cssm,
                             &lcssm,work,&lwork,iwork,&liwork,ifail,
                             &lifail,teof,weval,&ier);
/*
 * Check various possible error messages.
 */
  if (ier != 0) {
    if (ier == -1) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov_tr: cssm contains one or more missing values\n" );
    }
    else if (ier == -88) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov_tr: trace is equal to zero\n" );
    }
    else if (ier < 0) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov_tr: The %d-th argument had an illegal value\n", abs(ier) );
    }
    else {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov_tr: %d eigenvectors failed to converge\n",ier);
    }
  }
/*
 * Free unneeded memory.
 */
  if((void*)dx != x) NclFree(dx);
  NclFree(work);
  NclFree(cssm);
  NclFree(teof);
  NclFree(iwork);
  NclFree(ifail);
  NclFree(weval);
/*
 * Return values. 
 */
  if(type_x != NCL_double) {
/*
 * Copy double values to float values.
 */
    revec = (float*)calloc(total_size_evec,sizeof(float));
    if( revec == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_tr: Unable to allocate memory for output array");
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
    reval = (float *)calloc(*neval,sizeof(float));
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
    rtrace = (float *)calloc(1,sizeof(float));
    *rtrace = (float)(*trace);
/*
 * Free double precision trace.
 */
    NclFree(trace);

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

/*
 * eof_function is returned to indicate which function was used.
 */
  eof_function = (int *)calloc(1,sizeof(int));
  *eof_function = 4;
  dsizes[0] = 1;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)eof_function,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypeintClass
                         );
  _NclAddAtt(
             att_id,
             "eof_function",
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


NhlErrorTypes eofcor_tr_W( void )
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
  double *cssm, *work, *teof, *weval;
  int   *iwork, *ifail;
  int lwork, liwork, lifail;
  long long int lcssm;
/*
 * Attribute variables
 */
  int att_id;
  int dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
  int *eof_function;
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
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_tr: The input array must be at least 2-dimensional");
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
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_tr: The dimensions of the input array must both be at least 1");
    return(NhlFATAL);
  }
/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Coerce x to double if necessary.
 */
  dx = coerce_input_double(x,type_x,total_size_x,has_missing_x,&missing_x,
                           &missing_dx);
  if( dx == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_tr: Unable to allocate memory for coercing x array to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate memory for return variable.
 */
  dsizes_evec[0] = *neval;
  for( i = 0; i <= ndims_x-2; i++ ) dsizes_evec[i+1] = dsizes_x[i];

  total_size_evec = *neval * ncol;

  evec = (double *)calloc(total_size_evec,sizeof(double));
  if( evec == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_tr: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Allocate memory for attributes.
 */
  trace = (double *)calloc(1,sizeof(double));
  eval  = (double *)calloc(*neval,sizeof(double));
  pcvar =  (float *)calloc(*neval,sizeof(float));
  if( trace == NULL || pcvar == NULL || eval == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_tr: Unable to allocate memory for attribute arrays");
    return(NhlFATAL);
  }
/*
 * Create a few more work arrays.  This is necessary to avoid having
 * these arrays created dynamically in the Fortran file (which makes
 * it Fortran 90, and unportable to some systems. 
 */
  lcssm  = nobs*(nobs+1)/2;
  lwork  = 8*nobs;
  liwork = 5*nobs;
  lifail = nobs;
  cssm   = (double *)calloc(lcssm,sizeof(double));
  work   = (double *)calloc(lwork,sizeof(double));
  teof   = (double *)calloc(*neval*nobs,sizeof(double));
  iwork  =    (int *)calloc(liwork,sizeof(int));
  ifail  =    (int *)calloc(lifail,sizeof(int));
  weval  = (double *)calloc(lifail,sizeof(double));

  if( cssm == NULL || work == NULL || teof == NULL || iwork == NULL ||
      ifail == NULL || weval == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_tr: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
/*
 * Call the Fortran 77 version of 'drveof' with the full argument list.
 */
  NGCALLF(ddrveoft,DDRVEOFT)(dx,&nrow,&ncol,&nobs,&msta,&missing_dx.doubleval,
                             neval,eval,evec,pcvar,trace,&iopt,&jopt,cssm,
                             &lcssm,work,&lwork,iwork,&liwork,ifail,
                             &lifail,teof,weval,&ier);
/*
 * Check various possible error messages.
 */
  if (ier != 0) {
    if (ier == -1) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcor_tr: cssm contains one or more missing values\n" );
    }
    else if (ier == -88) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcor_tr: trace is equal to zero\n" );
    }
    else if (ier < 0) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcor_tr: The %d-th argument had an illegal value\n", abs(ier) );
    }
    else {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcor_tr: %d eigenvectors failed to converge\n",ier);
    }
  }
/*
 * Free unneeded memory.
 */
  if((void*)dx != x) NclFree(dx);
  NclFree(work);
  NclFree(cssm);
  NclFree(teof);
  NclFree(iwork);
  NclFree(ifail);
  NclFree(weval);
/*
 * Return values. 
 */
  if(type_x != NCL_double) {
/*
 * Copy double values to float values.
 */
    revec = (float*)calloc(total_size_evec,sizeof(float));
    if( revec == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_tr: Unable to allocate memory for output array");
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
    reval = (float *)calloc(*neval,sizeof(float));
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
    rtrace = (float *)calloc(1,sizeof(float));
    *rtrace = (float)(*trace);
/*
 * Free double precision trace.
 */
    NclFree(trace);

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

/*
 * eof_function is returned to indicate which function was used.
 */
  eof_function = (int *)calloc(1,sizeof(int));
  *eof_function = 5;
  dsizes[0] = 1;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)eof_function,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypeintClass
                         );
  _NclAddAtt(
             att_id,
             "eof_function",
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


NhlErrorTypes eofcov_tr_pcmsg_W( void )
{
/*
 * Input array variables
 */
  void *x, *pcmsg;
  double *dx, *dpcmsg;
  float *rpcmsg;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  NclScalar missing_x, missing_rx, missing_dx;
  NclBasicDataTypes type_x, type_pcmsg;
  int nrow, ncol, nobs, msta, total_size_x;
  int *neval, iopt = 0, jopt = 0, i, ier = 0;
/*
 * Work array variables.
 */
  double *tmp_x, *cssm, *work, *teof, *weval, *evecx;
  int   *iwork, *ifail;
  int lwork, liwork, lifail;
  long long int lcssm, total_mem;
/*
 * Attribute variables
 */
  int att_id;
  int dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
  int *eof_function;
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
           3,
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
            3, 
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            2);

  pcmsg = (void *)NclGetArgValue(
            2,
            3, 
            NULL,
            NULL,
            NULL,
            NULL,
            &type_pcmsg,
            2);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_x < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_tr_pcmsg: The input array must be at least 2-dimensional");
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
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_tr_pcmsg: The dimensions of the input array must both be at least 1");
    return(NhlFATAL);
  }
/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Coerce x to double if necessary.
 */
  dx = coerce_input_double(x,type_x,total_size_x,has_missing_x,&missing_x,
                           &missing_dx);
  if( dx == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_tr_pcmsg: Unable to allocate memory for coercing x array to double precision");
    return(NhlFATAL);
  }

/*  
 * Go ahead and make a separate copy of pcmsg, even if it is already 
 * double, because we'll need to return that double later. We
 * don't want to return it as a pointer to the original double
 * value.
 */
  if(type_pcmsg != NCL_double) {
    dpcmsg = coerce_input_double(pcmsg,type_pcmsg,1,0,NULL,NULL);
  }
  else {
    dpcmsg  = (double*)calloc(1,sizeof(double));
    *dpcmsg = ((double*)pcmsg)[0];
  }
/*
 * Check dpcmsg
 */
  if(*dpcmsg < 0. || *dpcmsg > 100.) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_tr_pcmsg: pcmsg must be between 0 and 100 inclusive");
    return(NhlFATAL);
  }

/*
 * Allocate memory for return variable.
 */
  dsizes_evec[0] = *neval;
  for( i = 0; i <= ndims_x-2; i++ ) dsizes_evec[i+1] = dsizes_x[i];

  total_size_evec = *neval * ncol;

  evec = (double *)calloc(total_size_evec,sizeof(double));
  if(evec == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_tr_pcmsg: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Allocate memory for attributes.
 */
  trace = (double *)calloc(1,sizeof(double));
  eval  = (double *)calloc(*neval,sizeof(double));
  pcvar =  (float *)calloc(*neval,sizeof(float));
  if(trace == NULL || pcvar == NULL || eval == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_tr_pcmsg: Unable to allocate memory for attribute arrays");
    return(NhlFATAL);
  }

/*
 * Create a few more work arrays.  This is necessary to avoid having
 * these arrays created dynamically in the Fortran file (which makes
 * it Fortran 90, and unportable to some systems. 
 */
  lcssm  = nobs*(nobs+1)/2;
  lwork  = 8*nobs;
  liwork = 5*nobs;
  lifail = nobs;
  cssm   = (double *)calloc(lcssm,sizeof(double));
  work   = (double *)calloc(lwork,sizeof(double));
  teof   = (double *)calloc(*neval*nobs,sizeof(double));
  iwork  =    (int *)calloc(liwork,sizeof(int));
  ifail  =    (int *)calloc(lifail,sizeof(int));
  tmp_x  = (double *)calloc(total_size_x,sizeof(double));
  evecx  = (double *)calloc(total_size_evec,sizeof(double));
  weval  = (double *)calloc(lifail,sizeof(double));
  total_mem = 8*(lcssm+lwork+lifail+(*neval*nobs)+total_size_x +
              total_size_evec)+4*(liwork+lifail);

  if(  cssm == NULL ||  work == NULL || weval == NULL || iwork == NULL || 
      ifail == NULL || tmp_x == NULL || evecx == NULL ||  teof == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_tr_pcmsg: Unable to allocate memory for work arrays. A total of %d bytes need to be allocated",total_mem);
    return(NhlFATAL);
  }
/*
 * Call the Fortran 77 version of 'drveof' with the full argument list.
 */
  NGCALLF(dtncleof,DTNCLEOF)(dx,&nrow,&ncol,&nobs,&msta,&missing_dx.doubleval,
                             neval,eval,evec,pcvar,trace,&iopt,&jopt,dpcmsg,
                             tmp_x,evecx,teof,weval,cssm,&lcssm,work,&lwork,
                             iwork,&liwork,ifail,&lifail,&ier);

/*
 * Check various possible error messages.
 */
  if (ier != 0) {
    if (ier == -1) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov_tr_pcmsg: cssm contains one or more missing values\n" );
    }
    else if (ier == -88) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov_tr_pcmsg: trace is equal to zero\n" );
    }
    else if (ier < 0) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov_tr_pcmsg: The %d-th argument had an illegal value\n", abs(ier) );
    }
    else {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov_tr_pcmsg: %d eigenvectors failed to converge\n",ier);
    }
  }
/*
 * Free unneeded memory.
 */
  if((void*)dx != x) NclFree(dx);
  NclFree(work);
  NclFree(cssm);
  NclFree(weval);
  NclFree(teof);
  NclFree(iwork);
  NclFree(ifail);
  NclFree(tmp_x);
  NclFree(evecx);

/*
 * Return values. 
 */
  if(type_x != NCL_double) {
/*
 * Copy double values to float values.
 */
    revec = (float*)calloc(total_size_evec,sizeof(float));
    if( revec == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_tr_pcmsg: Unable to allocate memory for output array");
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
    reval = (float *)calloc(*neval,sizeof(float));
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
    rtrace = (float *)calloc(1,sizeof(float));
    *rtrace = (float)(*trace);
/*
 * Free double precision trace.
 */
    NclFree(trace);

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

/*
 * Return pcmsg as an attribute "pcrit" of type float or double.
 */
  dsizes[0] = 1;
  if(type_pcmsg != NCL_double) {
    NclFree(dpcmsg);
    if(type_pcmsg != NCL_float) {
      rpcmsg = coerce_input_float(pcmsg,type_pcmsg,1,0,NULL,NULL);
    }
    else {
      rpcmsg  = (float*)calloc(1,sizeof(float));
      *rpcmsg = ((float*)pcmsg)[0];
    }
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)rpcmsg,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
                           );
  }
  else {
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)dpcmsg,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypedoubleClass
                           );
  }
  _NclAddAtt(
             att_id,
             "pcrit",
             att_md,
             NULL
             );
/*
 * eof_function is returned to indicate which function was used.
 */
  eof_function = (int *)calloc(1,sizeof(int));
  *eof_function = 6;
  dsizes[0] = 1;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)eof_function,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypeintClass
                         );
  _NclAddAtt(
             att_id,
             "eof_function",
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

NhlErrorTypes eofcor_tr_pcmsg_W( void )
{
/*
 * Input array variables
 */
  void *x, *pcmsg;
  double *dx, *dpcmsg;
  float *rpcmsg;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  NclScalar missing_x, missing_rx, missing_dx;
  NclBasicDataTypes type_x, type_pcmsg;
  int nrow, ncol, nobs, msta, total_size_x;
  int *neval, iopt = 0, jopt = 1, i, ier = 0;
/*
 * Work array variables.
 */
  double *tmp_x, *cssm, *work, *teof, *weval, *evecx;
  int   *iwork, *ifail;
  int lwork, liwork, lifail;
  long long int lcssm, total_mem;
/*
 * Attribute variables
 */
  int att_id;
  int dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
  int *eof_function;
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
           3,
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
            3, 
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            2);

  pcmsg = (void *)NclGetArgValue(
            2,
            3, 
            NULL,
            NULL,
            NULL,
            NULL,
            &type_pcmsg,
            2);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_x < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_tr_pcmsg: The input array must be at least 2-dimensional");
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
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_tr_pcmsg: The dimensions of the input array must both be at least 1");
    return(NhlFATAL);
  }
/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Coerce x to double if necessary.
 */
  dx = coerce_input_double(x,type_x,total_size_x,has_missing_x,&missing_x,
                           &missing_dx);
  if( dx == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_tr_pcmsg: Unable to allocate memory for coercing x array to double precision");
    return(NhlFATAL);
  }

/*  
 * Go ahead and make a separate copy of pcmsg, even if it is already 
 * double, because we'll need to return that double later. We
 * don't want to return it as a pointer to the original double
 * value.
 */
  if(type_pcmsg != NCL_double) {
    dpcmsg = coerce_input_double(pcmsg,type_pcmsg,1,0,NULL,NULL);
  }
  else {
    dpcmsg  = (double*)calloc(1,sizeof(double));
    *dpcmsg = ((double*)pcmsg)[0];
  }
/*
 * Check dpcmsg
 */
  if(*dpcmsg < 0. || *dpcmsg > 100.) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_tr_pcmsg: pcmsg must be between 0 and 100 inclusive");
    return(NhlFATAL);
  }

/*
 * Allocate memory for return variable.
 */
  dsizes_evec[0] = *neval;
  for( i = 0; i <= ndims_x-2; i++ ) dsizes_evec[i+1] = dsizes_x[i];

  total_size_evec = *neval * ncol;

  evec = (double *)calloc(total_size_evec,sizeof(double));
  if(evec == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_tr_pcmsg: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Allocate memory for attributes.
 */
  trace = (double *)calloc(1,sizeof(double));
  eval  = (double *)calloc(*neval,sizeof(double));
  pcvar =  (float *)calloc(*neval,sizeof(float));
  if(trace == NULL || pcvar == NULL || eval == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_tr_pcmsg: Unable to allocate memory for attribute arrays");
    return(NhlFATAL);
  }

/*
 * Create a few more work arrays.  This is necessary to avoid having
 * these arrays created dynamically in the Fortran file (which makes
 * it Fortran 90, and unportable to some systems. 
 */
  lcssm  = nobs*(nobs+1)/2;
  lwork  = 8*nobs;
  liwork = 5*nobs;
  lifail = nobs;
  cssm   = (double *)calloc(lcssm,sizeof(double));
  work   = (double *)calloc(lwork,sizeof(double));
  teof   = (double *)calloc(*neval*nobs,sizeof(double));
  iwork  =    (int *)calloc(liwork,sizeof(int));
  ifail  =    (int *)calloc(lifail,sizeof(int));
  tmp_x  = (double *)calloc(total_size_x,sizeof(double));
  evecx  = (double *)calloc(total_size_evec,sizeof(double));
  weval  = (double *)calloc(lifail,sizeof(double));
  total_mem = 8*(lcssm+lwork+lifail+(*neval*nobs)+total_size_x + 
              total_size_evec)+4*(liwork+lifail);

  if(  cssm == NULL ||  work == NULL || weval == NULL || iwork == NULL || 
      ifail == NULL || tmp_x == NULL || evecx == NULL ||  teof == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_tr_pcmsg: Unable to allocate memory for work arrays. A total of %d bytes need to be allocated",total_mem);
    return(NhlFATAL);
  }
/*
 * Call the Fortran 77 version of 'drveof' with the full argument list.
 */
  NGCALLF(dtncleof,DTNCLEOF)(dx,&nrow,&ncol,&nobs,&msta,&missing_dx.doubleval,
                             neval,eval,evec,pcvar,trace,&iopt,&jopt,dpcmsg,
                             tmp_x,evecx,teof,weval,cssm,&lcssm,work,&lwork,
                             iwork,&liwork,ifail,&lifail,&ier);

/*
 * Check various possible error messages.
 */
  if (ier != 0) {
    if (ier == -1) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcor_tr_pcmsg: cssm contains one or more missing values\n" );
    }
    else if (ier == -88) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcor_tr_pcmsg: trace is equal to zero\n" );
    }
    else if (ier < 0) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcor_tr_pcmsg: The %d-th argument had an illegal value\n", abs(ier) );
    }
    else {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcor_tr_pcmsg: %d eigenvectors failed to converge\n",ier);
    }
  }
/*
 * Free unneeded memory.
 */
  if((void*)dx != x) NclFree(dx);
  NclFree(work);
  NclFree(cssm);
  NclFree(weval);
  NclFree(teof);
  NclFree(iwork);
  NclFree(ifail);
  NclFree(tmp_x);
  NclFree(evecx);

/*
 * Return values. 
 */
  if(type_x != NCL_double) {
/*
 * Copy double values to float values.
 */
    revec = (float*)calloc(total_size_evec,sizeof(float));
    if( revec == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_tr_pcmsg: Unable to allocate memory for output array");
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
    reval = (float *)calloc(*neval,sizeof(float));
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
    rtrace = (float *)calloc(1,sizeof(float));
    *rtrace = (float)(*trace);
/*
 * Free double precision trace.
 */
    NclFree(trace);

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

/*
 * Return pcmsg as an attribute "pcrit" of type float or double.
 */
  dsizes[0] = 1;
  if(type_pcmsg != NCL_double) {
    NclFree(dpcmsg);
    if(type_pcmsg != NCL_float) {
      rpcmsg = coerce_input_float(pcmsg,type_pcmsg,1,0,NULL,NULL);
    }
    else {
      rpcmsg  = (float*)calloc(1,sizeof(float));
      *rpcmsg = ((float*)pcmsg)[0];
    }
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)rpcmsg,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
                           );
  }
  else {
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)dpcmsg,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypedoubleClass
                           );
  }
  _NclAddAtt(
             att_id,
             "pcrit",
             att_md,
             NULL
             );
/*
 * eof_function is returned to indicate which function was used.
 */
  eof_function = (int *)calloc(1,sizeof(int));
  *eof_function = 7;
  dsizes[0] = 1;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)eof_function,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypeintClass
                         );
  _NclAddAtt(
             att_id,
             "eof_function",
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

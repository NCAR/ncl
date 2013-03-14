#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(dgeevxint,DGEEVXINT)(char *, char *, char *, char *, 
                                         int *, double *, double *, double *, 
                                         double *, logical *, double *, int *, 
                                         double *, double *, double *, double *,
                                         double *, int *, int *,int,int,int,int);

NhlErrorTypes dgeevx_lapack_W( void )
{

/*
 * Input variables
 */
/*
 * Argument # 0
 */
  void *Q;
  double *tmp_Q;
  ng_size_t dsizes_Q[2];
  int has_missing_Q;
  NclScalar missing_Q, missing_dbl_Q;
  NclBasicDataTypes type_Q;

/*
 * Arguments #1-5
 */
  NrmQuark *balanc, *jobvl, *jobvr, *sense;
  char   *sbalanc, *sjobvl, *sjobvr, *ssense;
  logical *opt;
/*
 * Return variable
 */
  void *evlr, *wr, *wi, *vl, *vr;
  double *tmp_evlr, *tmp_wr, *tmp_wi, *tmp_vl, *tmp_vr;
  int ndims_evlr;
  ng_size_t *dsizes_evlr;
  NclBasicDataTypes type_evlr;
  NclObjClass type_obj_evlr;
/*
 * Attribute and return variables
 */
  int att_id;
  ng_size_t dsizes[1], dsizes2[2];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
/*
 * Various
 */
  ng_size_t N, Nsqr, Nsqr4, lwork, liwork; 
  double *work, *scalem, *rconde, *rcondv;
  int *iwork, iN, ilwork, iliwork;

/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
/*
 * Get argument # Q. Note that this 
 */
  Q = (void*)NclGetArgValue(
           0,
           6,
           NULL,
           dsizes_Q,
           &missing_Q,
           &has_missing_Q,
           &type_Q,
           1);

/*
 * The Q array is both input and input, so it must be float or double.
 */
  if((type_Q != NCL_float && type_Q != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dgeevx_lapack: Q must be float or double");
    return(NhlFATAL);
  }

/*
 * Check dimension sizes.
 */
  N = dsizes_Q[0];
  if(dsizes_Q[1] != N) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dgeevx_lapack: Q must be an N x N array");
    return(NhlFATAL);
  }
  Nsqr  = N*N;
  Nsqr4 = 4*Nsqr;

/*
 * Coerce missing value to double if necessary.
 */
  coerce_missing(type_Q,has_missing_Q,&missing_Q,&missing_dbl_Q,NULL);

/*
 * Test array sizes.
 */
  lwork  = N*(N+6);
  liwork = 2*N-1;
  
  if((N > INT_MAX) || (lwork > INT_MAX) || (liwork > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dgeevx_lapack: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  iN      = (int) N;
  ilwork  = (int) lwork;
  iliwork = (int) liwork;

/*
 * Get string arguments #1-4
 */
  balanc = (NrmQuark *)NclGetArgValue(
           1,
           6,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
  jobvl = (NrmQuark *)NclGetArgValue(
           2,
           6,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
  jobvr = (NrmQuark *)NclGetArgValue(
           3,
           6,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
  sense = (NrmQuark *)NclGetArgValue(
           4,
           6,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

/*
 * Convert to character strings.
 */
  sbalanc = NrmQuarkToString(*balanc);
  sjobvl  = NrmQuarkToString(*jobvl);
  sjobvr  = NrmQuarkToString(*jobvr);
  ssense  = NrmQuarkToString(*sense);

/*
 * Check the strings to make sure they're valid.
 */
  if(strcmp(sbalanc,"N") && strcmp(sbalanc,"P") && 
     strcmp(sbalanc,"S") && strcmp(sbalanc,"B")) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dgeevx_lapack: 'balanc' must be set to 'N', 'P', 'S', or 'B'");
    return(NhlFATAL);
  }

  if(strcmp(sjobvl,"N") && strcmp(sjobvl,"V") && 
     strcmp(sjobvr,"N") && strcmp(sjobvr,"V")) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dgeevx_lapack: 'jobvl' and 'jobvr' must be set to 'N' or 'V'");
    return(NhlFATAL);
  }
  if(strcmp(ssense,"N") && strcmp(ssense,"E") && 
     strcmp(ssense,"V") && strcmp(ssense,"B")) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dgeevx_lapack: 'sense' must be set to 'N', 'E', 'V', or 'B'");
    return(NhlFATAL);
  }

  if((!strcmp(ssense,"E") || !strcmp(ssense,"B")) && 
     (strcmp(sjobvl,"V") || strcmp(sjobvr,"V"))) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dgeevx_lapack: if 'sense' is 'E' or 'B', then jobvl/jobvr must be set to 'V'");
    return(NhlFATAL);

  }

/*
 * Get (currently unsed) argument # 5
 */
  opt = (logical*)NclGetArgValue(
           5,
           6,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

/* 
 * Allocate space for coercing input arrays.  If any of the input
 * is already double, then we don't need to allocate space for
 * temporary arrays, because we'll just change the pointer into
 * the void array appropriately.
 */
  if(type_Q != NCL_double) {
    type_evlr     = NCL_float;
    type_obj_evlr = nclTypefloatClass;
  }
  else {
    type_evlr     = NCL_double;
    type_obj_evlr = nclTypedoubleClass; 
 }
  tmp_Q = coerce_input_double(Q,type_Q,Nsqr,0,NULL,NULL);
  if(tmp_Q == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dgeevx_lapack: Unable to allocate memory for coercing Q to double");
    return(NhlFATAL);
  }

/* 
 * Allocate space for output arrays.
 */
  if(type_evlr != NCL_double) {
    evlr     = (void *)calloc(Nsqr4, sizeof(float));
    wr       = (void *)calloc(N, sizeof(float));
    wi       = (void *)calloc(N, sizeof(float));
    vl       = (void *)calloc(Nsqr, sizeof(float));
    vr       = (void *)calloc(Nsqr, sizeof(float));
    tmp_evlr = (double *)calloc(Nsqr4,sizeof(double));
    tmp_wr   = (double *)calloc(Nsqr4,sizeof(double));
    tmp_wi   = (double *)calloc(Nsqr4,sizeof(double));
    tmp_vl   = (double *)calloc(Nsqr, sizeof(double));
    tmp_vr   = (double *)calloc(Nsqr, sizeof(double));
    if(evlr == NULL || tmp_evlr == NULL || wr   == NULL || tmp_wr == NULL ||
       wi   == NULL || tmp_wi   == NULL || vl   == NULL || tmp_vl == NULL ||
       vr   == NULL || tmp_vr   == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dgeevx_lapack: Unable to allocate memory for output arrays");
      return(NhlFATAL);
    }
  }
  else {
    evlr = (void *)calloc(Nsqr4,sizeof(double));
    wr   = (void *)calloc(N, sizeof(double));
    wi   = (void *)calloc(N, sizeof(double));
    vl   = (void *)calloc(Nsqr, sizeof(double));
    vr   = (void *)calloc(Nsqr, sizeof(double));
    if(evlr == NULL || wr == NULL || wi == NULL || vl == NULL || vr == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dgeevx_lapack: Unable to allocate memory for output arrays");
      return(NhlFATAL);
    }
    tmp_evlr = &((double*)evlr)[0];
    tmp_wr   = &((double*)wr)[0];
    tmp_wi   = &((double*)wi)[0];
    tmp_vl   = &((double*)vl)[0];
    tmp_vr   = &((double*)vr)[0];
  }

/* 
 * Allocate space for work arrays.
 */
  scalem = (double *)calloc(N, sizeof(double));
  rconde = (double *)calloc(N, sizeof(double));
  rcondv = (double *)calloc(N, sizeof(double));
  work   = (double *)calloc(ilwork, sizeof(double));
  iwork  = (int *)calloc(iliwork, sizeof(int));

  if(scalem == NULL || rconde == NULL || rcondv == NULL || 
     work == NULL || iwork == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dgeevx_lapack: Unable to allocate memory for work arrays");
      return(NhlFATAL);
  }

/* 
 * Allocate space for output dimension sizes and set them.
 */
  ndims_evlr = 4;
  dsizes_evlr = (ng_size_t*)calloc(ndims_evlr,sizeof(ng_size_t));
  if( dsizes_evlr == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dgeevx_lapack: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }
  dsizes_evlr[0] = 2;
  dsizes_evlr[1] = 2;
  dsizes_evlr[2] = N;
  dsizes_evlr[3] = N;

/*
 * Call the Fortran routine.
 */
  NGCALLF(dgeevxint,DGEEVXINT)(sbalanc, sjobvl, sjobvr, ssense, &iN, 
                               tmp_Q, tmp_evlr, tmp_wr, tmp_wi, opt, work, 
                               iwork, scalem, rconde, rcondv, 
                               tmp_vl, tmp_vr, &ilwork, &iliwork,
                               strlen(sbalanc),strlen(sjobvl), 
                               strlen(sjobvr), strlen(ssense));
  if(type_Q != NCL_double) {
    coerce_output_float_only(evlr,tmp_evlr,Nsqr4,0);
    coerce_output_float_only(Q,tmp_Q,Nsqr,0);
    coerce_output_float_only(wr,tmp_wr,N,0);
    coerce_output_float_only(wi,tmp_wi,N,0);
    coerce_output_float_only(vl,tmp_vl,Nsqr,0);
    coerce_output_float_only(vr,tmp_vr,Nsqr,0);
  }
/*
 * Free unneeded memory.
 */
  if(type_Q    != NCL_double) {
    NclFree(tmp_Q);
    NclFree(tmp_wr);
    NclFree(tmp_wi);
    NclFree(tmp_vl);
    NclFree(tmp_vr);
    NclFree(tmp_evlr);
  }
  NclFree(scalem);
  NclFree(rconde);
  NclFree(rcondv);
  NclFree(work);
  NclFree(iwork);

/*
 * Set up variable to return.
 */
  return_md = _NclCreateVal(
                            NULL,
                            NULL,
                            Ncl_MultiDValData,
                            0,
                            evlr,
                            NULL,
                            ndims_evlr,
                            dsizes_evlr,
                            TEMPORARY,
                            NULL,
                            type_obj_evlr
                            );

  free(dsizes_evlr);
/*
 * Set up attributes to return.
 */
  att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

/*
 * Create individual attributes.
 */
  dsizes[0] = N;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         wr,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         type_obj_evlr
                         );
  _NclAddAtt(
             att_id,
             "eigr",
             att_md,
             NULL
             );

  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         wi,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         type_obj_evlr
                         );
  _NclAddAtt(
             att_id,
             "eigi",
             att_md,
             NULL
             );

  dsizes2[0] = dsizes2[1] = N;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         vl,
                         NULL,
                         2,
                         dsizes2,
                         TEMPORARY,
                         NULL,
                         type_obj_evlr
                         );
  _NclAddAtt(
             att_id,
             "eigleft",
             att_md,
             NULL
             );

  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         vr,
                         NULL,
                         2,
                         dsizes2,
                         TEMPORARY,
                         NULL,
                         type_obj_evlr
                         );
  _NclAddAtt(
             att_id,
             "eigright",
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

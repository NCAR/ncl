#include <stdio.h>
#include <math.h>
#include "wrapper.h"

extern void NGCALLF(dspecx,DSPECX)(double *,int *,int *,int *,double *,
                                   double *,double *,int *,double *,double *,
                                   int *,double *,int *);


extern void NGCALLF(dspecxy,DSPECXY)(double *,double *,int *,int *,int *,
                                     double *,double *,double *,int *,
                                     double *,double *,double *,double *,
                                     double *,double *,double *,int *,
                                     double *,int *);

NhlErrorTypes specx_anal_W( void )
{
/*
 * Input array variables
 */
  void *x, *pct;
  double *dx, *dpct;
  ng_size_t dsizes[1];
  ng_size_t nx;
  int *iopt, *jave;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclScalar missing_x, missing_dx;
  NclBasicDataTypes type_x, type_pct;
  ng_size_t lwork;
  double scl, *work;
/*
 * Output variables
 */
  void *dof;
  NclBasicDataTypes type_dof;
  NclObjClass type_output;
/*
 * Attribute variables
 */
  int att_id;
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
  double *frq_tmp, *spcx_tmp, sinfo[50];
  void *spcx, *frq, *bw, *xavei, *xvari, *xvaro, *xlag1, *xslope;
/*
 * Declare variables for random purposes.
 */
  ng_size_t i, nspcmx, nspc, total_size_x;
  int ier;

/*
 * Retrieve arguments.
 */
  x = (void*)NclGetArgValue(
          0,
          4,
          &ndims_x,
          dsizes_x,
          &missing_x,
          &has_missing_x,
          &type_x,
          DONT_CARE);

  iopt = (int*)NclGetArgValue(
          1,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);
  
  jave = (int*)NclGetArgValue(
          2,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);


  pct = (void*)NclGetArgValue(
          3,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_pct,
          DONT_CARE);
/*
 * Check input.
 */
  if( *iopt > 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"specx_anal: 'iopt' must be <= 2");
    return(NhlFATAL);
  }

  nx = dsizes_x[0];
  nspc = nspcmx = nx/2 + 1;
  if( nx < 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"specx_anal: 'x' must have more than 3 elements");
    return(NhlFATAL);
  }

  if( abs(*jave) > nx/2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"specx_anal: 'jave' must be <= nx/2");
    return(NhlFATAL);
  }

/*
 * Compute the total number of elements in our array.
 */
  total_size_x = 1;
  for(i = 0; i < ndims_x; i++) total_size_x *= dsizes_x[i];
/*
 * Check for missing values.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,NULL);
/*
 * Coerce x to double precision if necessary.
 */
  dx = coerce_input_double(x,type_x,total_size_x,has_missing_x,&missing_x,
                           &missing_dx);
  if( dx == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"specx_anal: Unable to allocate memory for coercing x array to double precision");
    return(NhlFATAL);
  }
/*  
 * Check if x contains missing values.
 */
  if(contains_missing(dx,total_size_x,has_missing_x,missing_dx.doubleval)) {
     NhlPError(NhlFATAL,NhlEUNKNOWN,"specx_anal: 'x' cannot contain any missing values");
     return(NhlFATAL);
  }
/*
 * Coerce pct to double precision if necessary.
 */
  dpct = coerce_input_double(pct,type_pct,1,0,NULL,NULL);
  if( dpct == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"specx_anal: Unable to allocate memory for coercing pct array to double precision");
    return(NhlFATAL);
  }
/*
 * Check pct.
 */
  if( *dpct < 0.0 || *dpct > 1.0 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"specx_anal: 'pct' must be between 0 and 1 inclusive");
    return(NhlFATAL);
  }
/*
 * Check if any input is double.
 */
  if(type_x != NCL_double && type_pct != NCL_double) {
    type_dof    = NCL_float;
    type_output = nclTypefloatClass;
/*
 * Allocate space for float output variables.
 */
    dof     = (void *)calloc(1,sizeof(float));
    frq     = (void *)calloc(nspcmx-1,sizeof(float));
    spcx    = (void *)calloc(nspcmx-1,sizeof(float));
    bw      = (void *)calloc(1,sizeof(float));
    xavei   = (void *)calloc(1,sizeof(float));
    xvari   = (void *)calloc(1,sizeof(float));
    xvaro   = (void *)calloc(1,sizeof(float));
    xlag1   = (void *)calloc(1,sizeof(float));
    xslope  = (void *)calloc(1,sizeof(float));
  }
  else {
    type_dof    = NCL_double;
    type_output = nclTypedoubleClass;
/*
 * Allocate space for double output variables.
 */
    dof     = (void *)calloc(1,sizeof(double));
    frq     = (void *)calloc(nspcmx-1,sizeof(double));
    spcx    = (void *)calloc(nspcmx-1,sizeof(double));
    bw      = (void *)calloc(1,sizeof(double));
    xavei   = (void *)calloc(1,sizeof(double));
    xvari   = (void *)calloc(1,sizeof(double));
    xvaro   = (void *)calloc(1,sizeof(double));
    xlag1   = (void *)calloc(1,sizeof(double));
    xslope  = (void *)calloc(1,sizeof(double));
  }
  if(   dof == NULL ||    bw == NULL ||  spcx == NULL ||   frq == NULL ||
      xavei == NULL || xvari == NULL || xvaro == NULL || xlag1 == NULL ||
     xslope == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"specx_anal: Unable to allocate memory for output variables");
    return(NhlFATAL);
  }
/*
 * Allocate space for stuff to be returned by dspecx.
 */
  frq_tmp  = (double *)calloc(nspcmx,sizeof(double));
  spcx_tmp = (double *)calloc(nspcmx,sizeof(double));
  if( frq_tmp == NULL || spcx_tmp == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"specx_anal: Unable to allocate memory for output variables");
    return(NhlFATAL);
  }

/*
 * Allocate space for work array.
 */
  lwork = 5 * nx + 18 + abs(*jave);
  work  = (double *)calloc(lwork,sizeof(double));
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"specx_anal: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * Call the Fortran version of this routine.
 */
  scl = 2.0;
  if((nx <= INT_MAX) &&
     (lwork <= INT_MAX) &&
     (nspc <= INT_MAX))
  {
      int inx = (int) nx;
      int ilwork = (int) lwork;
      int inspc = (int) nspc;
      NGCALLF(dspecx,DSPECX)(dx,&inx,iopt,jave,dpct,&scl,work,&ilwork,
                             frq_tmp,spcx_tmp,&inspc,sinfo,&ier);
  }
  else
  {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"specx_anal: one or more input dimensions is greater than INT_MAX", nx);
    return(NhlFATAL);
  }


  if( ier > 700000 ) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"specx_anal: 'x' contains all constant values");
  }

  coerce_output_float_or_double(   dof,    &sinfo[0],type_dof,1,0);
  coerce_output_float_or_double( xlag1,    &sinfo[1],type_dof,1,0);
  coerce_output_float_or_double(    bw,    &sinfo[5],type_dof,1,0);
  coerce_output_float_or_double( xavei,   &sinfo[10],type_dof,1,0);
  coerce_output_float_or_double( xvari,   &sinfo[11],type_dof,1,0);
  coerce_output_float_or_double( xvaro,   &sinfo[12],type_dof,1,0);
  coerce_output_float_or_double(xslope,   &sinfo[31],type_dof,1,0);
  coerce_output_float_or_double(   frq,  &frq_tmp[1],type_dof,nspcmx-1,0);
  coerce_output_float_or_double(  spcx, &spcx_tmp[1],type_dof,nspcmx-1,0);

/*
 * Free up memory.
 */
  NclFree(frq_tmp);
  NclFree(spcx_tmp);
  NclFree(work);
  if((void*)dx   != x) NclFree(dx);
  if((void*)dpct != pct) NclFree(dpct);

/*
 * Set up variable to return.
 */
  dsizes[0] = 1;
  return_md = _NclCreateVal(
                            NULL,
                            NULL,
                            Ncl_MultiDValData,
                            0,
                            dof,
                            NULL,
                            1,
                            dsizes,
                            TEMPORARY,
                            NULL,
                            type_output
                            );
/*
 * Set up attributes to return.
 */
  att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);
    
  dsizes[0] = nspcmx-1;      /* returning nx/2 points, not nx/2 + 1 */
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         spcx,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         type_output
                         );
  _NclAddAtt(
             att_id,
             "spcx",
             att_md,
             NULL
             );
    
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         frq,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         type_output
                         );
  _NclAddAtt(
             att_id,
             "frq",
             att_md,
             NULL
             );
    
  dsizes[0] = 1;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         bw,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         type_output
                         );
  _NclAddAtt(
             att_id,
             "bw",
             att_md,
             NULL
             );
    
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         xavei,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         type_output
                         );
  _NclAddAtt(
             att_id,
             "xavei",
             att_md,
             NULL
             );
    
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         xvari,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         type_output
                         );
  _NclAddAtt(
             att_id,
             "xvari",
             att_md,
             NULL
             );
    
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         xvaro,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         type_output
                         );
  _NclAddAtt(
             att_id,
             "xvaro",
             att_md,
             NULL
             );
    
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         xlag1,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         type_output
                         );
  _NclAddAtt(
             att_id,
             "xlag1",
             att_md,
             NULL
             );

  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         xslope,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         type_output
                         );
  _NclAddAtt(
             att_id,
             "xslope",
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

NhlErrorTypes specxy_anal_W( void )
{
/*
 * Input array variables
 */
  void *x, *y, *pct;
  double *dx, *dy, *dpct;
  ng_size_t dsizes[1], nx;
  int *iopt, *jave;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int ndims_y;
  ng_size_t dsizes_y[NCL_MAX_DIMENSIONS];
  int has_missing_x, has_missing_y;
  NclScalar missing_x, missing_y, missing_dx, missing_dy;
  NclBasicDataTypes type_x, type_y, type_pct;
  ng_size_t lwork;
  double scl, *work;
/*
 * Output variables
 */
  void *dof;
  NclBasicDataTypes type_dof;
  NclObjClass type_output;
/*
 * Attribute variables
 */
  int att_id;
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
  double *frq_tmp, *spcx_tmp, *spcy_tmp;
  double *cospc_tmp, *quspc_tmp, *coher_tmp, *phase_tmp;
  double prob_tmp[4], sinfo[50];
  void *bw, *frq, *spcx, *spcy, *cospc, *quspc, *coher, *phase;
  void *xavei, *xvari, *xvaro, *xlag1, *xslope;
  void *yavei, *yvari, *yvaro, *ylag1, *yslope; 
  void *prob;
/*
 * Declare variables for random purposes.
 */
  ng_size_t i, nspc, nspcmx, total_size_x, total_size_y;
  int ier;
/*
 * Retrieve arguments.
 */
  x = (void*)NclGetArgValue(
          0,
          5,
          &ndims_x,
          dsizes_x,
          &missing_x,
          &has_missing_x,
          &type_x,
          DONT_CARE);

  y = (void*)NclGetArgValue(
          1,
          5,
          &ndims_y,
          dsizes_y,
          &missing_y,
          &has_missing_y,
          &type_y,
          DONT_CARE);

  iopt = (int*)NclGetArgValue(
          2,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

  jave = (int*)NclGetArgValue(
          3,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

  pct = (void*)NclGetArgValue(
          4,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_pct,
          DONT_CARE);
/*
 * Check input.
 */
  nx   = dsizes_x[0];
  nspc = nspcmx = nx/2 + 1;

  if( nx != dsizes_y[0]) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"specxy_anal: 'x' and 'y' must have the same number of elements");
    return(NhlFATAL);
  }
  if( nx < 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"specxy_anal: 'x' and 'y' must have more than 3 elements");
    return(NhlFATAL);
  }

  if( *iopt > 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"specxy_anal: 'iopt' must be <= 2");
    return(NhlFATAL);
  }

  if( abs(*jave) > nx/2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"specxy_anal: 'jave' must be <= nx/2");
    return(NhlFATAL);
  }

/*
 * Compute the total number of elements in our arrays.
 */
  total_size_x = 1;
  for(i = 0; i < ndims_x; i++) total_size_x *= dsizes_x[i];

  total_size_y = 1;
  for(i = 0; i < ndims_y; i++) total_size_y *= dsizes_y[i];
/*
 * Check for missing values and coerce data if necessary.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,NULL);
  coerce_missing(type_y,has_missing_y,&missing_y,&missing_dy,NULL);
/*
 * Coerce x/y to double precision if necessary.
 */
  dx = coerce_input_double(x,type_x,total_size_x,has_missing_x,&missing_x,
                           &missing_dx);
  dy = coerce_input_double(y,type_y,total_size_y,has_missing_y,&missing_y,
                           &missing_dy);
  if(dx == NULL|| dy == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"specxy_anal: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*  
 * Check if x or y contains missing values.
 */
  if(contains_missing(dx,total_size_x,has_missing_x,missing_dx.doubleval) ||
     contains_missing(dy,total_size_y,has_missing_y,missing_dy.doubleval)) {
     NhlPError(NhlFATAL,NhlEUNKNOWN,"specx_anal: x and y cannot contain any missing values");
     return(NhlFATAL);
  }
/*
 * Coerce pct to double precision if necessary.
 */
  dpct = coerce_input_double(pct,type_pct,1,0,NULL,NULL);
  if( dpct == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"specxy_anal: Unable to allocate memory for coercing pct array to double precision");
    return(NhlFATAL);
  }
/*
 * Check pct.
 */
  if( *dpct < 0.0 || *dpct > 1.0 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"specxy_anal: 'pct' must be between 0 and 1 inclusive");
    return(NhlFATAL);
  }
/*
 * Check if any input is double.
 */
  if(type_x != NCL_double && type_y != NCL_double && 
     type_pct != NCL_double) {

    type_dof = NCL_float;
    type_output = nclTypefloatClass;
/* 
 * Allocate space for float output variables.
 */
    dof      = (void *)calloc(1,sizeof(float));
    frq      = (void *)calloc(nspcmx-1,sizeof(float));
    spcx     = (void *)calloc(nspcmx-1,sizeof(float));
    spcy     = (void *)calloc(nspcmx-1,sizeof(float));
    cospc    = (void *)calloc(nspcmx-1,sizeof(float));
    quspc    = (void *)calloc(nspcmx-1,sizeof(float));
    coher    = (void *)calloc(nspcmx-1,sizeof(float));
    phase    = (void *)calloc(nspcmx-1,sizeof(float));
    bw       = (void *)calloc(1,sizeof(float));
    prob     = (void *)calloc(4,sizeof(float));
    xavei    = (void *)calloc(1,sizeof(float));
    xvari    = (void *)calloc(1,sizeof(float));
    xvaro    = (void *)calloc(1,sizeof(float));
    xlag1    = (void *)calloc(1,sizeof(float));
    xslope   = (void *)calloc(1,sizeof(float));
    yavei    = (void *)calloc(1,sizeof(float));
    yvari    = (void *)calloc(1,sizeof(float));
    yvaro    = (void *)calloc(1,sizeof(float));
    ylag1    = (void *)calloc(1,sizeof(float));
    yslope   = (void *)calloc(1,sizeof(float));
  }
  else {
    type_dof = NCL_double;
    type_output = nclTypedoubleClass;
/*
 * Allocate space for double output variables.
 */
    dof     = (void *)calloc(1,sizeof(double));
    bw      = (void *)calloc(1,sizeof(double));
    prob    = (void *)calloc(4,sizeof(double));
    frq     = (void *)calloc(nspcmx-1,sizeof(double));
    spcx    = (void *)calloc(nspcmx-1,sizeof(double));
    spcy    = (void *)calloc(nspcmx-1,sizeof(double));
    cospc   = (void *)calloc(nspcmx-1,sizeof(double));
    quspc   = (void *)calloc(nspcmx-1,sizeof(double));
    coher   = (void *)calloc(nspcmx-1,sizeof(double));
    phase   = (void *)calloc(nspcmx-1,sizeof(double));
    xavei   = (void *)calloc(1,sizeof(double));
    xvari   = (void *)calloc(1,sizeof(double));
    xvaro   = (void *)calloc(1,sizeof(double));
    xlag1   = (void *)calloc(1,sizeof(double));
    xslope  = (void *)calloc(1,sizeof(double));
    yavei   = (void *)calloc(1,sizeof(double));
    yvari   = (void *)calloc(1,sizeof(double));
    yvaro   = (void *)calloc(1,sizeof(double));
    ylag1   = (void *)calloc(1,sizeof(double));
    yslope  = (void *)calloc(1,sizeof(double));
  }
  if(   dof == NULL ||    bw == NULL || xavei == NULL || xvari == NULL ||
        frq == NULL ||  spcx == NULL ||  spcy == NULL || cospc == NULL ||
      quspc == NULL || coher == NULL || phase == NULL || xvaro == NULL ||
      xlag1 == NULL ||xslope == NULL || yavei == NULL || yvari == NULL ||
      yvaro == NULL || ylag1 == NULL ||yslope == NULL ||  prob == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"specxy_anal: Unable to allocate memory for output variables");
    return(NhlFATAL);
  }

/*
 * Allocate space for stuff to be returned by dspecx.
 */
  frq_tmp   = (double *)calloc(nspcmx,sizeof(double));
  spcx_tmp  = (double *)calloc(nspcmx,sizeof(double));
  spcy_tmp  = (double *)calloc(nspcmx,sizeof(double));
  cospc_tmp = (double *)calloc(nspcmx,sizeof(double));
  quspc_tmp = (double *)calloc(nspcmx,sizeof(double));
  coher_tmp = (double *)calloc(nspcmx,sizeof(double));
  phase_tmp = (double *)calloc(nspcmx,sizeof(double));
  if(    frq_tmp == NULL ||  spcx_tmp == NULL ||  spcy_tmp == NULL || 
       cospc_tmp == NULL || quspc_tmp == NULL || coher_tmp == NULL || 
       phase_tmp == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"specxy_anal: Unable to allocate memory for output variables");
    return(NhlFATAL);
  }

/*
 * Allocate space for work array.
 */
  lwork = 10 * nx;
  work  = (double *)calloc(lwork,sizeof(double));
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"specxy_anal: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * Call the Fortran version of this routine.
 */
  scl = 2.0;
  if((nx <= INT_MAX) &&
     (lwork <= INT_MAX) &&
     (nspc <= INT_MAX))
  {
      int inx = (int) nx;
      int ilwork = (int) lwork;
      int inspc = (int) nspc;
      NGCALLF(dspecxy,DSPECXY)(dx,dy,&inx,iopt,jave,dpct,&scl,work,&ilwork,
                               frq_tmp,spcx_tmp,spcy_tmp,cospc_tmp,quspc_tmp,
                               coher_tmp,phase_tmp,&inspc,sinfo,&ier);
  }
  else
  {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"specxy_anal: one or more input dimensions is greater than INT_MAX", nx);
    return(NhlFATAL);
  }

  if( ier > 700000 ) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"specxy_anal: 'x' and/or 'y' contains all constant values");
  }
/*
 * Calculate coherence corresponding to the 90, 95, 99, and 99.9% levels.
 */
  prob_tmp[0] = 1.-pow((1.-0.900),(1./(sinfo[0]/2.-1.)));
  prob_tmp[1] = 1.-pow((1.-0.950),(1./(sinfo[0]/2.-1.)));
  prob_tmp[2] = 1.-pow((1.-0.990),(1./(sinfo[0]/2.-1.)));
  prob_tmp[3] = 1.-pow((1.-0.999),(1./(sinfo[0]/2.-1.)));


  coerce_output_float_or_double(   dof,    &sinfo[0],type_dof,1,0);
  coerce_output_float_or_double( xlag1,    &sinfo[1],type_dof,1,0);
  coerce_output_float_or_double( ylag1,    &sinfo[2],type_dof,1,0);
  coerce_output_float_or_double(    bw,    &sinfo[5],type_dof,1,0);
  coerce_output_float_or_double(  prob,     prob_tmp,type_dof,4,0);
  coerce_output_float_or_double( xavei,   &sinfo[10],type_dof,1,0);
  coerce_output_float_or_double( xvari,   &sinfo[11],type_dof,1,0);
  coerce_output_float_or_double( xvaro,   &sinfo[12],type_dof,1,0);
  coerce_output_float_or_double(xslope,   &sinfo[31],type_dof,1,0);
  coerce_output_float_or_double( yavei,   &sinfo[20],type_dof,1,0);
  coerce_output_float_or_double( yvari,   &sinfo[21],type_dof,1,0);
  coerce_output_float_or_double( yvaro,   &sinfo[22],type_dof,1,0);
  coerce_output_float_or_double(yslope,   &sinfo[34],type_dof,1,0);

  coerce_output_float_or_double(   frq,  &frq_tmp[1],type_dof,nspcmx-1,0);
  coerce_output_float_or_double(  spcx, &spcx_tmp[1],type_dof,nspcmx-1,0);
  coerce_output_float_or_double(  spcy, &spcy_tmp[1],type_dof,nspcmx-1,0);
  coerce_output_float_or_double( cospc,&cospc_tmp[1],type_dof,nspcmx-1,0);
  coerce_output_float_or_double( quspc,&quspc_tmp[1],type_dof,nspcmx-1,0);
  coerce_output_float_or_double( coher,&coher_tmp[1],type_dof,nspcmx-1,0);
  coerce_output_float_or_double( phase,&phase_tmp[1],type_dof,nspcmx-1,0);

/*
 * Set up variable to return.
 */
  dsizes[0] = 1;
  return_md = _NclCreateVal(
                            NULL,
                            NULL,
                            Ncl_MultiDValData,
                            0,
                            dof,
                            NULL,
                            1,
                            dsizes,
                            TEMPORARY,
                            NULL,
                            type_output
                            );
  /*
   * Set up attributes to return.
   */
  att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);
  
  dsizes[0] = nspcmx-1;      /* returning nx/2 points, not nx/2 + 1 */
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         spcx,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         type_output
                         );
  _NclAddAtt(
             att_id,
             "spcx",
             att_md,
             NULL
             );
  
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         spcy,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         type_output
                         );
  _NclAddAtt(
             att_id,
             "spcy",
             att_md,
             NULL
             );
  
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         frq,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         type_output
                         );
  _NclAddAtt(
             att_id,
             "frq",
             att_md,
             NULL
             );
  
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         cospc,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         type_output
                         );
  _NclAddAtt(
             att_id,
             "cospc",
             att_md,
             NULL
             );
  
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         quspc,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         type_output
                         );
  _NclAddAtt(
             att_id,
             "quspc",
             att_md,
             NULL
             );
  
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         coher,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         type_output
                         );
  _NclAddAtt(
             att_id,
             "coher",
             att_md,
             NULL
             );
  
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         phase,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         type_output
                         );
  _NclAddAtt(
             att_id,
             "phase",
             att_md,
             NULL
             );
  
  dsizes[0] = 1;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         bw,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         type_output
                         );
  _NclAddAtt(
             att_id,
             "bw",
             att_md,
             NULL
             );
  
  dsizes[0] = 4;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         prob,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         type_output
                         );
  _NclAddAtt(
             att_id,
             "coher_probability",
             att_md,
             NULL
             );
  
  dsizes[0] = 1;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         xavei,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         type_output
                         );
  _NclAddAtt(
             att_id,
             "xavei",
             att_md,
             NULL
             );
  
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         xvari,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         type_output
                         );
  _NclAddAtt(
             att_id,
             "xvari",
             att_md,
             NULL
             );
  
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         xvaro,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         type_output
                         );
  _NclAddAtt(
             att_id,
             "xvaro",
             att_md,
             NULL
             );
  
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         xlag1,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         type_output
                         );
  _NclAddAtt(
             att_id,
             "xlag1",
             att_md,
             NULL
             );
  
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         xslope,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         type_output
                         );
  _NclAddAtt(
             att_id,
             "xslope",
             att_md,
             NULL
             );
  
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         yavei,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         type_output
                         );
  
  _NclAddAtt(
             att_id,
             "yavei",
             att_md,
             NULL
             );
  
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         yvari,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         type_output
                         );
  _NclAddAtt(
             att_id,
             "yvari",
             att_md,
             NULL
             );
  
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         yvaro,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         type_output
                         );
  _NclAddAtt(
             att_id,
             "yvaro",
             att_md,
             NULL
             );
  
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         ylag1,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         type_output
                         );
  _NclAddAtt(
             att_id,
             "ylag1",
             att_md,
             NULL
             );
  
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         yslope,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         type_output
                         );
  _NclAddAtt(
             att_id,
             "yslope",
             att_md,
             NULL
             );
/*
 * Free up memory.
 */
  NclFree(work);
  if((void*)dx   != x) NclFree(dx);
  if((void*)dy   != y) NclFree(dy);
  if((void*)dpct != pct) NclFree(dpct);

  NclFree(frq_tmp);
  NclFree(spcx_tmp);
  NclFree(spcy_tmp);
  NclFree(cospc_tmp);
  NclFree(quspc_tmp);
  NclFree(coher_tmp);
  NclFree(phase_tmp);
/*
 * Return variable.
 */
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

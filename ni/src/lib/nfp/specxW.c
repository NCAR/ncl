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
  int dsizes[1], nx, *iopt, *jave;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclScalar missing_x, missing_dx;
  NclBasicDataTypes type_x, type_pct;
  int lwork;
  double scl, *work;
/*
 * Output variables
 */
  double *dof;
  float *rdof;
/*
 * Attribute variables
 */
  int att_id;
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
  double *frq, *spcx, *bw, *xavei, *xvari, *xvaro, *xlag1, *xslope;
  float *rfrq, *rspcx, *rbw, *rxavei, *rxvari, *rxvaro, *rxlag1;
  float *rxslope;
  double sinfo[50];
/*
 * Declare variables for random purposes.
 */
  int i, j, l, nspcmx, nspc, total_size_x, ier, any_double;

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
          2);

  iopt = (int*)NclGetArgValue(
          1,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);
  
  jave = (int*)NclGetArgValue(
          2,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);


  pct = (void*)NclGetArgValue(
          3,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_pct,
          2);
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
  if(has_missing_x) {
/*
 * Coerce missing value to double so we can check our data to see if
 * it contains any missing values.
 */
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               &missing_dx,
               &missing_x,
               1,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
  }

/*
 * Coerce x to double precision if necessary.
 */
  if(type_x != NCL_double) {
    dx = (double*)NclMalloc(sizeof(double)*total_size_x);
    if( dx == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"specx_anal: Unable to allocate memory for coercing x array to double precision");
      return(NhlFATAL);
    }
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               dx,
               x,
               total_size_x,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
  }
  else {
/*
 * Input is already double.
 */
    dx = (double*)x;
  }
/*
 * Coerce pct to double precision if necessary.
 */
  if(type_pct != NCL_double) {
    dpct = (double*)NclMalloc(sizeof(double));
    if( dpct == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"specx_anal: Unable to allocate memory for coercing pct array to double precision");
      return(NhlFATAL);
    }
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               dpct,
               pct,
               1,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_pct)));
  }
  else {
/*
 * Input is already double.
 */
    dpct = (double*)pct;
  }
/*
 * Check pct.
 */
  if( *dpct < 0.0 || *dpct > 1.0 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"specx_anal: 'pct' must be between 0 and 1 inclusive");
    return(NhlFATAL);
  }
/*  
 * Check if x contains missing values.
 */
  if( has_missing_x ) {
    for( i = 0; i < total_size_x; i++ ) {
      if( dx[i] == missing_dx.doubleval ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"specx_anal: 'x' cannot contain any missing values");
        return(NhlFATAL);
      }
    }
  }
/*
 * Check if any input is double.
 */
  if(type_x != NCL_double && type_pct != NCL_double) {
    any_double = 0;
  }
  else {
    any_double = 1;
  }
/*
 * Allocate space for output variables.
 */
  dof     = (double *)calloc(1,sizeof(double));
  frq     = (double *)calloc(nspcmx,sizeof(double));
  spcx    = (double *)calloc(nspcmx,sizeof(double));
  bw      = (double *)calloc(1,sizeof(double));
  xavei   = (double *)calloc(1,sizeof(double));
  xvari   = (double *)calloc(1,sizeof(double));
  xvaro   = (double *)calloc(1,sizeof(double));
  xlag1   = (double *)calloc(1,sizeof(double));
  xslope  = (double *)calloc(1,sizeof(double));
  if(   dof == NULL ||   frq == NULL ||  spcx == NULL ||    bw == NULL || 
      xavei == NULL || xvari == NULL || xvaro == NULL || xlag1 == NULL ||
     xslope == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"specx_anal: Unable to allocate memory for output variables");
    return(NhlFATAL);
  }

  if(!any_double) {
    rdof     = (float *)calloc(1,sizeof(float));
    rfrq     = (float *)calloc(nspcmx,sizeof(float));
    rspcx    = (float *)calloc(nspcmx,sizeof(float));
    rbw      = (float *)calloc(1,sizeof(float));
    rxavei   = (float *)calloc(1,sizeof(float));
    rxvari   = (float *)calloc(1,sizeof(float));
    rxvaro   = (float *)calloc(1,sizeof(float));
    rxlag1   = (float *)calloc(1,sizeof(float));
    rxslope  = (float *)calloc(1,sizeof(float));
    if(  rdof == NULL ||  rfrq == NULL || rspcx == NULL ||   rbw == NULL || 
       rxavei == NULL ||rxvari == NULL ||rxvaro == NULL ||rxlag1 == NULL ||
      rxslope == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"specx_anal: Unable to allocate memory for output variables");
      return(NhlFATAL);
    }
  }
/*
 * Allocate space for work array.
 */
  lwork = 5 * nx + 17 + abs(*jave);
  work   = (double *)calloc(lwork,sizeof(double));
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"specx_anal: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * Call the Fortran version of this routine.
 *
 */
  scl = 2.0;
  NGCALLF(dspecx,DSPECX)(dx,&nx,iopt,jave,dpct,&scl,work,&lwork,
                         frq,spcx,&nspc,sinfo,&ier);

  if( ier > 700000 ) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"specx_anal: 'x' contains all constant values");
  }
/*
 * Free up memory.
 */
  NclFree(work);
  if((void*)dx != x) NclFree(dx);
  if((void*)dpct != pct) NclFree(dpct);

  if(!any_double) {
/*
 * Copy double values to float values.
 */
    *rdof   = (float)sinfo[0]; /* deg. of freedom */
    *rxlag1 = (float)sinfo[1]; /* lag1 auto correltn after dtrending */
    *rbw    = (float)sinfo[5]; /* band width */
    *rxavei = (float)sinfo[10];/* mean prior to dtrending */

    *rxvari = (float)sinfo[11];/* variance prior to detrending*/
    *rxvaro = (float)sinfo[12];/* variance after detrending */
    *rxslope= (float)sinfo[31];/* slope of linear trend (if iopt=1) */
    for(i = 0; i < nspcmx; i++ ) {
      rfrq[i] = (float)frq[i];
      rspcx[i] = (float)spcx[i];
    }
/*
 * Set up variable to return.
 */
    dsizes[0] = 1;
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)rdof,
                              NULL,
                              1,
                              dsizes,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypefloatClass
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
                           rspcx,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
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
                           rfrq,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
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
                           rbw,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
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
                           rxavei,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
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
                           rxvari,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
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
                           rxvaro,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
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
                           rxlag1,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
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
                           rxslope,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
                           );
    _NclAddAtt(
               att_id,
               "xslope",
               att_md,
               NULL
               );
  }
  else {
    *dof    = sinfo[0];  /* Degrees of freedom [spcx@dof] */
    *xlag1  = sinfo[1];  /* lag1 auto correlation after detrending [@xlag1] */
    *bw     = sinfo[5];  /* band width         [spcx@band_width] */
    *xavei  = sinfo[10]; /* mean     prior to detrending [@xavei]  */
    *xvari  = sinfo[11]; /* variance prior to detrending [@xvari]  */
    *xvaro  = sinfo[12]; /* variance after detrending    [@xvaro] */
    *xslope = sinfo[31]; /* slope of linear trend (if iopt=1) [@xslope] */

/*
 * Set up variable to return.
 */
    dsizes[0] = 1;
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)dof,
                              NULL,
                              1,
                              dsizes,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypedoubleClass
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
                           (NclObjClass)nclTypedoubleClass
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
                           (NclObjClass)nclTypedoubleClass
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
                           (NclObjClass)nclTypedoubleClass
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
                           (NclObjClass)nclTypedoubleClass
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
                           (NclObjClass)nclTypedoubleClass
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
                           (NclObjClass)nclTypedoubleClass
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
                           (NclObjClass)nclTypedoubleClass
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
                           (NclObjClass)nclTypedoubleClass
                           );
    _NclAddAtt(
               att_id,
               "xslope",
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

NhlErrorTypes specxy_anal_W( void )
{
/*
 * Input array variables
 */
  void *x, *y, *pct;
  double *dx, *dy, *dpct;
  int dsizes[1], nx, *iopt, *jave;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS];
  int ndims_y, dsizes_y[NCL_MAX_DIMENSIONS];
  int has_missing_x, has_missing_y;
  NclScalar missing_x, missing_y, missing_dx, missing_dy;
  NclBasicDataTypes type_x, type_y, type_pct;
  int lwork;
  double scl, *work;
/*
 * Output variables
 */
  double *dof;
  float *rdof;
/*
 * Attribute variables
 */
  int att_id;
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
  double *frq, *spcx, *spcy, *cospc, *quspc, *coher, *phase, *bw;
  double *xavei, *xvari, *xvaro, *xlag1, *xslope;
  double *yavei, *yvari, *yvaro, *ylag1, *yslope; 
  float *rfrq, *rspcx, *rspcy, *rcospc, *rquspc, *rcoher, *rphase, *rbw;
  float *rxavei, *rxvari, *rxvaro, *rxlag1, *rxslope;
  float *ryavei, *ryvari, *ryvaro, *rylag1, *ryslope; 
  double sinfo[50];
/*
 * Declare variables for random purposes.
 */
  int i, j, l, nspc, nspcmx, total_size_x, total_size_y, ier;
  int any_double;
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
          2);

  y = (void*)NclGetArgValue(
          1,
          5,
          &ndims_y,
          dsizes_y,
          &missing_y,
          &has_missing_y,
          &type_y,
          2);

  iopt = (int*)NclGetArgValue(
          2,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);

  jave = (int*)NclGetArgValue(
          3,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);

  pct = (void*)NclGetArgValue(
          4,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_pct,
          2);
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
  if(has_missing_x) {
/*
 * Coerce missing value to double so we can check our data to see if
 * it contains any missing values.
 */
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               &missing_dx,
               &missing_x,
               1,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
  }

/*
 * Coerce x to double precision if necessary.
 */
  if(type_x != NCL_double) {
    dx = (double*)NclMalloc(sizeof(double)*total_size_x);
    if( dx == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"specxy_anal: Unable to allocate memory for coercing x array to double precision");
      return(NhlFATAL);
    }
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               dx,
               x,
               total_size_x,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
  }
  else {
/*
 * Input is already double.
 */
    dx = (double*)x;
  }
/*
 * Do same for y array.
 */
  if(has_missing_y) {
/*
 * Coerce missing value to double so we can check our data to see if
 * it contains any missing values.
 */
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               &missing_dy,
               &missing_y,
               1,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_y)));
  }

/*
 * Coerce y to double precision if necessary.
 */
  if(type_y != NCL_double) {
    dy = (double*)NclMalloc(sizeof(double)*total_size_y);
    if( dy == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"specxy_anal: Unable to allocate memory for coercing y array to double precision");
      return(NhlFATAL);
    }
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               dy,
               y,
               total_size_y,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_y)));
  }
  else {
/*
 * Input is already double.
 */
    dy = (double*)y;
  }
/*
 * Coerce pct to double precision if necessary.
 */
  if(type_pct != NCL_double) {
    dpct = (double*)NclMalloc(sizeof(double));
    if( dpct == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"specxy_anal: Unable to allocate memory for coercing pct array to double precision");
      return(NhlFATAL);
    }
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               dpct,
               pct,
               1,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_pct)));
  }
  else {
/*
 * Input is already double.
 */
    dpct = (double*)pct;
  }
/*
 * Check pct.
 */
  if( *dpct < 0.0 || *dpct > 1.0 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"specxy_anal: 'pct' must be between 0 and 1 inclusive");
    return(NhlFATAL);
  }
/*  
 * Check if x and/or y contains missing values.
 */
  if( has_missing_x ) {
    for( i = 0; i < total_size_x; i++ ) {
      if( dx[i] == missing_dx.doubleval ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"specxy_anal: 'x' cannot contain any missing values");
        return(NhlFATAL);
      }
    }
  }
  if( has_missing_y ) {
    for( i = 0; i < total_size_y; i++ ) {
      if( dy[i] == missing_dy.doubleval ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"specxy_anal: 'y' cannot contain any missing values");
        return(NhlFATAL);
      }
    }
  }
/*
 * Check if any input is double.
 */
  if(type_x != NCL_double && type_y != NCL_double && 
     type_pct != NCL_double) {
    any_double = 0;
  }
  else {
    any_double = 1;
  }
/*
 * Allocate space for output variables.
 */
  dof     = (double *)calloc(1,sizeof(double));
  frq     = (double *)calloc(nspcmx,sizeof(double));
  spcx    = (double *)calloc(nspcmx,sizeof(double));
  spcy    = (double *)calloc(nspcmx,sizeof(double));
  cospc   = (double *)calloc(nspcmx,sizeof(double));
  quspc   = (double *)calloc(nspcmx,sizeof(double));
  coher   = (double *)calloc(nspcmx,sizeof(double));
  phase   = (double *)calloc(nspcmx,sizeof(double));
  bw      = (double *)calloc(1,sizeof(double));
  xavei   = (double *)calloc(1,sizeof(double));
  xvari   = (double *)calloc(1,sizeof(double));
  xvaro   = (double *)calloc(1,sizeof(double));
  xlag1   = (double *)calloc(1,sizeof(double));
  xslope  = (double *)calloc(1,sizeof(double));
  yavei   = (double *)calloc(1,sizeof(double));
  yvari   = (double *)calloc(1,sizeof(double));
  yvaro   = (double *)calloc(1,sizeof(double));
  ylag1   = (double *)calloc(1,sizeof(double));
  yslope  = (double *)calloc(1,sizeof(double));
  
  if(   dof == NULL ||   frq == NULL ||  spcx == NULL ||  spcy == NULL || 
      cospc == NULL || quspc == NULL || coher == NULL || phase == NULL || 
         bw == NULL || xavei == NULL || xvari == NULL || xvaro == NULL || 
      xlag1 == NULL ||xslope == NULL || yavei == NULL || yvari == NULL ||
      yvaro == NULL || ylag1 == NULL | yslope == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"specxy_anal: Unable to allocate memory for output variables");
    return(NhlFATAL);
  }
  if(!any_double) {
    rdof     = (float *)calloc(1,sizeof(float));
    rfrq     = (float *)calloc(nspcmx,sizeof(float));
    rspcx    = (float *)calloc(nspcmx,sizeof(float));
    rspcy    = (float *)calloc(nspcmx,sizeof(float));
    rcospc   = (float *)calloc(nspcmx,sizeof(float));
    rquspc   = (float *)calloc(nspcmx,sizeof(float));
    rcoher   = (float *)calloc(nspcmx,sizeof(float));
    rphase   = (float *)calloc(nspcmx,sizeof(float));
    rbw      = (float *)calloc(1,sizeof(float));
    rxavei   = (float *)calloc(1,sizeof(float));
    rxvari   = (float *)calloc(1,sizeof(float));
    rxvaro   = (float *)calloc(1,sizeof(float));
    rxlag1   = (float *)calloc(1,sizeof(float));
    rxslope  = (float *)calloc(1,sizeof(float));
    ryavei   = (float *)calloc(1,sizeof(float));
    ryvari   = (float *)calloc(1,sizeof(float));
    ryvaro   = (float *)calloc(1,sizeof(float));
    rylag1   = (float *)calloc(1,sizeof(float));
    ryslope  = (float *)calloc(1,sizeof(float));

    if(  rdof == NULL ||   rfrq == NULL ||  rspcx == NULL ||  rspcy == NULL || 
       rcospc == NULL || rquspc == NULL || rcoher == NULL || rphase == NULL || 
          rbw == NULL || rxavei == NULL || rxvari == NULL || rxvaro == NULL || 
       rxlag1 == NULL ||rxslope == NULL || ryavei == NULL || ryvari == NULL ||
       ryvaro == NULL || rylag1 == NULL ||ryslope == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"specxy_anal: Unable to allocate memory for output variables");
      return(NhlFATAL);
    }
  }

/*
 * Allocate space for work array.
 */
  lwork = 10 * nx;
  work   = (double *)calloc(lwork,sizeof(double));
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"specxy_anal: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * Call the Fortran version of this routine.
 *
 */
  scl = 2.0;
  NGCALLF(dspecxy,DSPECXY)(dx,dy,&nx,iopt,jave,dpct,&scl,work,&lwork,
                           frq,spcx,spcy,cospc,quspc,coher,phase,
                           &nspc,sinfo,&ier);

  if( ier > 700000 ) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"specxy_anal: 'x' and/or 'y' contains all constant values");
  }

/*
 * Free up memory.
 */
  NclFree(work);
  if((void*)dx != x) NclFree(dx);
  if((void*)dy != y) NclFree(dy);
  if((void*)dpct != pct) NclFree(dpct);

  if(!any_double) {
/*
 * Copy double values to float values.
 */
    *rdof   = (float)sinfo[0]; /* Deg. of freedom  */
    *rxlag1 = (float)sinfo[1]; /* lag1 auto corrltn after dtrending */
    *rylag1 = (float)sinfo[2]; /* lag1 auto corrltn after dtrending */
    *rbw    = (float)sinfo[5]; /* band width */
    
    *rxavei = (float)sinfo[10];/* mean prior to detrending  */
    *rxvari = (float)sinfo[11];/* variance prior to detrending */
    *rxvaro = (float)sinfo[12];/* variance after detrending */
    *rxslope= (float)sinfo[31];/* slope of linear trend (if iopt=1) */
    
    *ryavei = (float)sinfo[20];/* mean     prior to detrending */
    *ryvari = (float)sinfo[21];/* variance prior to detrending */
    *ryvaro = (float)sinfo[22];/* variance after detrending */
    *ryslope= (float)sinfo[34];/* slope of linear trend (if iopt=1) */

    for(i = 0; i < nspcmx; i++ ) {
      rfrq[i]   = (float)frq[i];
      rspcx[i]  = (float)spcx[i];
      rspcy[i]  = (float)spcy[i];
      rcospc[i] = (float)cospc[i];
      rquspc[i] = (float)quspc[i];
      rcoher[i] = (float)coher[i];
      rphase[i] = (float)phase[i];
    }

/*
 * Set up variable to return.
 */
    dsizes[0] = 1;
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)rdof,
                              NULL,
                              1,
                              dsizes,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypefloatClass
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
                           rspcx,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
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
                           rspcy,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
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
                           rfrq,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
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
                           rcospc,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
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
                           rquspc,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
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
                           rcoher,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
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
                           rphase,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
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
                           rbw,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
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
                           rxavei,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
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
                           rxvari,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
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
                           rxvaro,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
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
                           rxlag1,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
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
                           rxslope,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
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
                           ryavei,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
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
                           ryvari,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
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
                           ryvaro,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
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
                           rylag1,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
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
                           ryslope,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
                           );
    _NclAddAtt(
               att_id,
               "yslope",
               att_md,
               NULL
               );
  }
  else {
/*
 * Copy double values to float values.
 */
    *dof    = sinfo[0];  /* Degrees of freedom [spcx@dof] */
    *xlag1  = sinfo[1];  /* lag1 auto correlation after detrending [@xlag1] */
    *ylag1  = sinfo[2];  /* lag1 auto correlation after detrending [@ylag1] */
    *bw     = sinfo[5];  /* band width         [spcx@band_width] */
    
    *xavei  = sinfo[10]; /* mean     prior to detrending [@xavei]  */
    *xvari  = sinfo[11]; /* variance prior to detrending [@xvari]  */
    *xvaro  = sinfo[12]; /* variance after detrending    [@xvaro] */
    *xslope = sinfo[31]; /* slope of linear trend (if iopt=1) [@xslope] */
    
    *yavei  = sinfo[20]; /* mean     prior to detrending [@yavei]  */
    *yvari  = sinfo[21]; /* variance prior to detrending [@yvari]  */
    *yvaro  = sinfo[22]; /* variance after detrending    [@yvaro] */
    *yslope = sinfo[34]; /* slope of linear trend (if iopt=1) [@yslope] */

/*
 * Set up variable to return.
 */
    dsizes[0] = 1;
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)dof,
                              NULL,
                              1,
                              dsizes,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypedoubleClass
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
                           (NclObjClass)nclTypedoubleClass
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
                           (NclObjClass)nclTypedoubleClass
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
                           (NclObjClass)nclTypedoubleClass
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
                           (NclObjClass)nclTypedoubleClass
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
                           (NclObjClass)nclTypedoubleClass
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
                           (NclObjClass)nclTypedoubleClass
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
                           (NclObjClass)nclTypedoubleClass
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
                           (NclObjClass)nclTypedoubleClass
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
                           (NclObjClass)nclTypedoubleClass
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
                           (NclObjClass)nclTypedoubleClass
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
                           (NclObjClass)nclTypedoubleClass
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
                           (NclObjClass)nclTypedoubleClass
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
                           (NclObjClass)nclTypedoubleClass
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
                           (NclObjClass)nclTypedoubleClass
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
                           (NclObjClass)nclTypedoubleClass
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
                           (NclObjClass)nclTypedoubleClass
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
                           (NclObjClass)nclTypedoubleClass
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
                           (NclObjClass)nclTypedoubleClass
                           );
    _NclAddAtt(
               att_id,
               "yslope",
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

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

NhlErrorTypes specx_anal_W( void )
{
/*
 * Input array variables
 */
  float *x, *pct;
  int dsizes[1], nx, *iopt, *jave;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclScalar missing_x;
  int lwork;
  float scl, *work;
/*
 * Output variables
 */
  float *dof;
/*
 * Attribute variables
 */
  int att_id;
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
  float *frq, *spcx, *bw, *xavei, *xvari, *xvaro, *xlag1, *xslope, sinfo[50];
/*
 * Declare variables for random purposes.
 */
  int i, j, l, nspcmx, nspc, total_elements, ier;

/*
 * Retrieve arguments.
 */
  x = (float*)NclGetArgValue(
          0,
          4,
          &ndims_x,
          dsizes_x,
		  &missing_x,
		  &has_missing_x,
          NULL,
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
  
  if( *iopt > 2) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"specx_anal: 'iopt' must be <= 2");
	return(NhlFATAL);
  }

  jave = (int*)NclGetArgValue(
          2,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);

  nx   = dsizes_x[0];
  if( nx < 3) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"specx_anal: 'x' must have more than 3 elements");
	return(NhlFATAL);
  }

  if( abs(*jave) > nx/2 ) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"specx_anal: 'jave' must be <= nx/2");
	return(NhlFATAL);
  }

  pct = (float*)NclGetArgValue(
          3,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);

  if( *pct < 0.0 || *pct > 1.0 ) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"specx_anal: 'pct' must be between 0 and 1 inclusive");
	return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */
  total_elements = 1;
  for(i = 0; i < ndims_x; i++) {
	total_elements *= dsizes_x[i];
  }

/*  
 * Check for missing values.
 */
  if( has_missing_x ) {
	for( i = 0; i < total_elements; i++ ) {
	  if( x[i] == missing_x.floatval ) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"specx_anal: 'x' cannot contain any missing values");
		return(NhlFATAL);
	  }
	}
  }
  nspc = nspcmx = nx/2 + 1;
/*
 * Allocate space for output variables.
 */
  dof     = (float *)calloc(1,sizeof(float));
  frq     = (float *)calloc(nspcmx,sizeof(float));
  spcx    = (float *)calloc(nspcmx,sizeof(float));
  bw      = (float *)calloc(1,sizeof(float));
  xavei   = (float *)calloc(1,sizeof(float));
  xvari   = (float *)calloc(1,sizeof(float));
  xvaro   = (float *)calloc(1,sizeof(float));
  xlag1   = (float *)calloc(1,sizeof(float));
  xslope  = (float *)calloc(1,sizeof(float));
  if(   dof == NULL ||   frq == NULL ||  spcx == NULL ||    bw == NULL || 
	  xavei == NULL || xvari == NULL || xvaro == NULL || xlag1 == NULL ||
     xslope == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"specx_anal: Unable to allocate memory for output variables");
    return(NhlFATAL);
  }

/*
 * Allocate space for work array.
 */
  lwork = 5 * nx + 17 + abs(*jave);
  work   = (float *)calloc(lwork,sizeof(float));
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"specx_anal: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * Call the Fortran version of this routine.
 *
 */
  scl = 2.0;
  NGCALLF(specx, SPECX)(x,&nx,iopt,jave,pct,&scl,work,&lwork,
						frq,spcx,&nspc,sinfo,&ier);

  if( ier > 700000 ) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"specx_anal: 'x' contains all constant values");
  }
  free((float*)work);

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
                         spcx,
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
                         frq,
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
                         bw,
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
                         xavei,
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
                         xvari,
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
                         xvaro,
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
                         xlag1,
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
                         xslope,
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
  float *x, *y, *pct;
  int dsizes[1], nx, *iopt, *jave;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS];
  int ndims_y, dsizes_y[NCL_MAX_DIMENSIONS];
  int has_missing_x, has_missing_y;
  NclScalar missing_x, missing_y;
  int lwork;
  float scl, *work;
/*
 * Output variables
 */
  float *dof;
/*
 * Attribute variables
 */
  int att_id;
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
  float *frq, *spcx, *spcy, *cospc, *quspc, *coher, *phase, *bw;
  float *xavei, *xvari, *xvaro, *xlag1, *xslope;
  float *yavei, *yvari, *yvaro, *ylag1, *yslope; 
  float sinfo[50];
/*
 * Declare variables for random purposes.
 */
  int i, j, l, nspc, nspcmx, total_elements_x, total_elements_y, ier;

/*
 * Retrieve arguments.
 */
  x = (float*)NclGetArgValue(
          0,
          5,
          &ndims_x,
          dsizes_x,
		  &missing_x,
		  &has_missing_x,
          NULL,
          2);

  y = (float*)NclGetArgValue(
          1,
          5,
          &ndims_y,
          dsizes_y,
		  &missing_y,
		  &has_missing_y,
          NULL,
          2);

  nx   = dsizes_x[0];
  if( nx != dsizes_y[0]) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"specxy_anal: 'x' and 'y' must have the same number of elements");
	return(NhlFATAL);
  }
  if( nx < 3) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"specxy_anal: 'x' and 'y' must have more than 3 elements");
	return(NhlFATAL);
  }
  nspc = nspcmx = nx/2 + 1;

  iopt = (int*)NclGetArgValue(
          2,
          5,
          NULL,
          NULL,
		  NULL,
		  NULL,
          NULL,
          2);

  if( *iopt > 2) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"specxy_anal: 'iopt' must be <= 2");
	return(NhlFATAL);
  }

  jave = (int*)NclGetArgValue(
          3,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);

  if( abs(*jave) > nx/2 ) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"specxy_anal: 'jave' must be <= nx/2");
	return(NhlFATAL);
  }

  pct = (float*)NclGetArgValue(
          4,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);

  if( *pct < 0.0 || *pct > 1.0 ) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"specxy_anal: 'pct' must be between 0 and 1 inclusive");
	return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our arrays.
 */
  total_elements_x = 1;
  for(i = 0; i < ndims_x; i++) {
	total_elements_x *= dsizes_x[i];
  }
  total_elements_y = 1;
  for(i = 0; i < ndims_y; i++) {
	total_elements_y *= dsizes_y[i];
  }
/*  
 * Check for missing values.
 */
  if( has_missing_x ) {
	for( i = 0; i < total_elements_x; i++ ) {
	  if( x[i] == missing_x.floatval ) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"specxy_anal: 'x' cannot contain any missing values");
		return(NhlFATAL);
	  }
	}
  }
  if( has_missing_y ) {
	for( i = 0; i < total_elements_y; i++ ) {
	  if( y[i] == missing_y.floatval ) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"specxy_anal: 'y' cannot contain any missing values");
		return(NhlFATAL);
	  }
	}
  }
/*
 * Allocate space for output variables.
 */
  dof     = (float *)calloc(1,sizeof(float));
  frq     = (float *)calloc(nspcmx,sizeof(float));
  spcx    = (float *)calloc(nspcmx,sizeof(float));
  spcy    = (float *)calloc(nspcmx,sizeof(float));
  cospc   = (float *)calloc(nspcmx,sizeof(float));
  quspc   = (float *)calloc(nspcmx,sizeof(float));
  coher   = (float *)calloc(nspcmx,sizeof(float));
  phase   = (float *)calloc(nspcmx,sizeof(float));
  bw      = (float *)calloc(1,sizeof(float));
  xavei   = (float *)calloc(1,sizeof(float));
  xvari   = (float *)calloc(1,sizeof(float));
  xvaro   = (float *)calloc(1,sizeof(float));
  xlag1   = (float *)calloc(1,sizeof(float));
  xslope  = (float *)calloc(1,sizeof(float));
  yavei   = (float *)calloc(1,sizeof(float));
  yvari   = (float *)calloc(1,sizeof(float));
  yvaro   = (float *)calloc(1,sizeof(float));
  ylag1   = (float *)calloc(1,sizeof(float));
  yslope  = (float *)calloc(1,sizeof(float));

  if(   dof == NULL ||   frq == NULL ||  spcx == NULL ||  spcy == NULL || 
	  cospc == NULL || quspc == NULL || coher == NULL || phase == NULL || 
  	     bw == NULL || xavei == NULL || xvari == NULL || xvaro == NULL || 
	  xlag1 == NULL ||xslope == NULL || yavei == NULL || yvari == NULL ||
	  yvaro == NULL || ylag1 == NULL | yslope == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"specxy_anal: Unable to allocate memory for output variables");
    return(NhlFATAL);
  }

/*
 * Allocate space for work array.
 */
  lwork = 10 * nx;
  work   = (float *)calloc(lwork,sizeof(float));
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"specxy_anal: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * Call the Fortran version of this routine.
 *
 */
  scl = 2.0;
  NGCALLF(specxy, SPECXY)(x,y,&nx,iopt,jave,pct,&scl,work,&lwork,
						  frq,spcx,spcy,cospc,quspc,coher,phase,
						  &nspc,sinfo,&ier);

  if( ier > 700000 ) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"specxy_anal: 'x' and/or 'y' contains all constant values");
  }

  free((float*)work);

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
                         spcx,
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
                         spcy,
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
                         frq,
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
                         cospc,
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
                         quspc,
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
                         coher,
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
                         phase,
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
                         bw,
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
                         xavei,
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
                         xvari,
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
                         xvaro,
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
                         xlag1,
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
                         xslope,
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
                         yavei,
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
                         yvari,
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
                         yvaro,
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
                         ylag1,
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
                         yslope,
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




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
#include <math.h>
#include <ncarg/gks.h>

NhlErrorTypes sindex_yrmo_W( void )
{
/*
 * Input array variables
 */
  float *x, *y, xmsg;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS];
  int ndims_y, dsizes_y[NCL_MAX_DIMENSIONS];
  NclScalar missing_x, missing_y, *missing;
  int has_missing_x, has_missing_y;
  int nyrs, nmos, *iprnt, jprnt;
/*
 * Attribute variables
 */
  int att_id;
  int nelem = 1;
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
  NclQuark *long_name, *short_name, *units;
/*
 * Output array variables
 */
  float *soi, *soi_noise;
  int *dsizes_soi;
/*
 * various
 */
  int i, lwork, ler = 0;
  float *work;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  x = (float*)NclGetArgValue(
           0,
           3,
           &ndims_x, 
           dsizes_x,
		   &missing_x,
		   &has_missing_x,
           NULL,
           2);
  y = (float*)NclGetArgValue(
           1,
           3,
           &ndims_y,
           dsizes_y,
		   &missing_y,
		   &has_missing_y,
           NULL,
           2);
  iprnt = (int*)NclGetArgValue(
           2,
           3,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           2);
  nyrs = dsizes_x[0];
  nmos = dsizes_x[1];
/*
 * The grids coming in must be 2-dimensional.
 */
  if( (ndims_x != 2 || ndims_y != 2) || (dsizes_x[0] != dsizes_y[0]) ||
	  (dsizes_x[1] != dsizes_y[1])) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"sindex_yrmo: The input arrays must be 2-dimensional and have the same dimension sizes");
    return(NhlFATAL);
  }
/*
 * Check for missing values.
 */
  missing = (NclScalar*)NclMalloc(sizeof(NclScalar));
  if(has_missing_x) {
	xmsg = (float)missing_x.floatval;
	missing = &missing_x;
  }
  else if(has_missing_y) {
	xmsg = (float)missing_y.floatval;
	missing = &missing_y;
  }
  else {
	missing = NULL;
  }
/*
 * Allocate space for output arrays.
 */
  soi = (float *)NclMalloc(nyrs*nmos*sizeof(float));
  soi_noise = (float *)NclMalloc(nyrs*nmos*sizeof(float));
  if( soi == NULL || soi_noise == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"sindex_yrmo: Unable to allocate memory for output array(s)");
    return(NhlFATAL);
  }
/*
 * Allocate memory for work array.
 */
  lwork = 2 * nyrs * nmos + 4*nmos + nyrs;
  work = (float *)NclMalloc(lwork*sizeof(float));
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"sindex_yrmo: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * Call the f77 version of 'sindex' with the full argument list.
 */
  NGCALLF(indx77,INDX77)(x,y,&nmos,&nyrs,&xmsg,iprnt,
						   work,&lwork,soi,soi_noise,&ler);
  if (ler == 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"sindex_yrmo: One or both of the input data arrays contains all missing values");
    return(NhlFATAL);
  }
/*
 * Free workspace array.
 */
  free(work);
/*
 * Set up variable to return.
 */
  return_md = _NclCreateVal(
                        NULL,
                        NULL,
                        Ncl_MultiDValData,
                        0,
                        (void*)soi,
                        missing,
                        2,
                        dsizes_x,
                        TEMPORARY,
                        NULL,
                        (NclObjClass)nclTypefloatClass
                        );
/*
 * Set up attributes to return.
 */
  att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

  long_name = (NclQuark*)NclMalloc(sizeof(NclQuark));
  *long_name = NrmStringToQuark("Southern Oscillation Index");
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         long_name,
                         NULL,
                         1,
                         &nelem,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypestringClass
                         );
  _NclAddAtt(
             att_id,
             "long_name",
             att_md,
             NULL
             );

  short_name = (NclQuark*)NclMalloc(sizeof(NclQuark));
  *short_name = NrmStringToQuark("SOI");
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         short_name,
                         NULL,
                         1,
                         &nelem,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypestringClass
                         );
  _NclAddAtt(
             att_id,
             "short_name",
             att_md,
             NULL
             );

  units = (NclQuark*)NclMalloc(sizeof(NclQuark));
  *units = NrmStringToQuark("none");
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         units,
                         NULL,
                         1,
                         &nelem,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypestringClass
                         );
  _NclAddAtt(
             att_id,
             "units",
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


NhlErrorTypes snindex_yrmo_W( void )
{
/*
 * Input array variables
 */
  float *x, *y, xmsg;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS];
  int ndims_y, dsizes_y[NCL_MAX_DIMENSIONS];
  NclScalar missing_x, missing_y, *missing;
  int has_missing_x, has_missing_y;
  int nyrs, nmos, *iprnt;
/*
 * Attribute variables
 */
  int att_id;
  int nelem = 1;
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
  NclQuark *long_name, *short_name, *units;
/*
 * Output array variables
 */
  float *soi, *soi_noise;
  int *dsizes_soi;
/*
 * various
 */
  int i, lwork, ler = 0;
  float *work;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
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
  y = (float*)NclGetArgValue(
           1,
           4,
           &ndims_y,
           dsizes_y,
		   &missing_y,
		   &has_missing_y,
           NULL,
           2);
  iprnt = (int*)NclGetArgValue(
           2,
           4,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           2);
  *iprnt = -(*iprnt);
  nyrs = dsizes_x[0];
  nmos = dsizes_x[1];
/*
 * The grids coming in must be 2-dimensional.
 */
  if( (ndims_x != 2 || ndims_y != 2) || (dsizes_x[0] != dsizes_y[0]) ||
	  (dsizes_x[1] != dsizes_y[1])) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"snindex_yrmo: The input arrays must be 2-dimensional and have the same dimension sizes");
    return(NhlFATAL);
  }
/*
 * Check for missing values.
 */
  missing = (NclScalar*)NclMalloc(sizeof(NclScalar));
  if(has_missing_x) {
	xmsg = (float)missing_x.floatval;
	missing = &missing_x;
  }
  else if(has_missing_y) {
	xmsg = (float)missing_y.floatval;
	missing = &missing_y;
  }
  else {
	missing = NULL;
  }
/*
 * Allocate space for output arrays.
 */
  soi = (float *)NclMalloc(nyrs*nmos*sizeof(float));
  if( soi == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"snindex_yrmo: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
  soi_noise = (float*)NclGetArgValue(
           3,
           4,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           1);
/*
 * Allocate memory for work array.
 */
  lwork = 2 * nyrs * nmos + 4*nmos + nyrs;
  work = (float *)NclMalloc(lwork*sizeof(float));
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"snindex_yrmo: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * Call the f77 version of 'sindex' with the full argument list.
 */
  NGCALLF(indx77,INDX77)(x,y,&nmos,&nyrs,&xmsg,iprnt,
						   work,&lwork,soi,soi_noise,&ler);
  if (ler == 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"snindex_yrmo: One or both of the input data arrays contains all missing values");
    return(NhlFATAL);
  }
/*
 * Free workspace array.
 */
  free(work);
/*
 * Set up variable to return.
 */
  return_md = _NclCreateVal(
                        NULL,
                        NULL,
                        Ncl_MultiDValData,
                        0,
                        (void*)soi,
                        missing,
                        2,
                        dsizes_x,
                        TEMPORARY,
                        NULL,
                        (NclObjClass)nclTypefloatClass
                        );
/*
 * Set up attributes to return.
 */
  att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

  long_name = (NclQuark*)NclMalloc(sizeof(NclQuark));
  *long_name = NrmStringToQuark("(Noise) Southern Oscillation Index");
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         long_name,
                         NULL,
                         1,
                         &nelem,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypestringClass
                         );
  _NclAddAtt(
             att_id,
             "long_name",
             att_md,
             NULL
             );

  short_name = (NclQuark*)NclMalloc(sizeof(NclQuark));
  *short_name = NrmStringToQuark("nSOI");
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         short_name,
                         NULL,
                         1,
                         &nelem,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypestringClass
                         );
  _NclAddAtt(
             att_id,
             "short_name",
             att_md,
             NULL
             );

  units = (NclQuark*)NclMalloc(sizeof(NclQuark));
  *units = NrmStringToQuark("none");
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         units,
                         NULL,
                         1,
                         &nelem,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypestringClass
                         );
  _NclAddAtt(
             att_id,
             "units",
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

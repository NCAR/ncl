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

extern void NGCALLF(dindx77,DINDX77)(double *,double *,int *, int *,double *,
                                     int *,double *,int *,double *,double *,
                                     int *);

NhlErrorTypes sindex_yrmo_W( void )
{
/*
 * Input array variables
 */
  void *x, *y;
  double *dx, *dy;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS];
  int ndims_y, dsizes_y[NCL_MAX_DIMENSIONS];
  NclScalar missing_x, missing_y, missing_rx, missing_dx, missing_dy;
  int has_missing_x, has_missing_y;
  int nyrs, nmos, *iprnt;
  NclBasicDataTypes type_x, type_y;
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
  double *soi, *soi_noise;
  float *rsoi;
  int *dsizes_soi;
/*
 * various
 */
  int i, lwork, ler = 0, total_size_xy;
  double *work;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
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
  y = (void*)NclGetArgValue(
           1,
           3,
           &ndims_y,
           dsizes_y,
           &missing_y,
           &has_missing_y,
           &type_y,
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
/*
 * The grids coming in must be the same size.
 */
  if( (dsizes_x[0] != dsizes_y[0]) || (dsizes_x[1] != dsizes_y[1])) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"sindex_yrmo: The input arrays must have the same dimension sizes");
    return(NhlFATAL);
  }
/*
 * Get years and months.
 */
  nyrs = dsizes_x[0];
  nmos = dsizes_x[1];
  total_size_xy = nyrs * nmos;
/*
 * Coerce missing values to double.
 *
 * Use the default missing value if one isn't set.
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
      missing_rx.floatval = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
      missing_dx.doubleval = (double)missing_rx.floatval;
    }
    else {
      missing_dx.doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
    }
  }

  if(has_missing_y) {
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               &missing_dy,
               &missing_y,
               1,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_y)));
  }
  else {
/*
 * Get the default missing value.
 */ 
    if(type_y != NCL_double) {
      missing_dy.doubleval = (double)((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
    }
    else {
      missing_dy.doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
    }
  }
/*
 * x and y missing values must be equal.
 */
  if( missing_dx.doubleval != missing_dy.doubleval ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"sindex_yrmo: The missing values for x and y must be the same");
    return(NhlFATAL);
  }
    
/*
 * Coerce data to double if necessary.
 */
  if(type_x != NCL_double) {
    dx = (double*)NclMalloc(sizeof(double)*total_size_xy);
    if( dx == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"sindex_yrmo: Unable to allocate memory for coercing x array to double precision");
      return(NhlFATAL);
    }
    if(has_missing_x) {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dx,
                 x,
                 total_size_xy,
                 &missing_dx,
                 &missing_x,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
    }
    else {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dx,
                 x,
                 total_size_xy,
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
 * Coerce y to double.
 */
  if(type_y != NCL_double) {
    dy = (double*)NclMalloc(sizeof(double)*total_size_xy);
    if( dy == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"sindex_yrmo: Unable to allocate memory for coercing y array to double precision");
      return(NhlFATAL);
    }
    if(has_missing_y) {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dy,
                 y,
                 total_size_xy,
                 &missing_dy,
                 &missing_y,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_y)));
    }
    else {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dy,
                 y,
                 total_size_xy,
                 NULL,
                 NULL,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_y)));
    }
  }
  else {
/*
 * Input is already double.
 */
    dy = (double*)y;
  }

/*
 * Allocate space for output arrays.
 */
  soi       = (double *)NclMalloc(total_size_xy*sizeof(double));
  soi_noise = (double *)NclMalloc(total_size_xy*sizeof(double));
  if( soi == NULL || soi_noise == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"sindex_yrmo: Unable to allocate memory for output array(s)");
    return(NhlFATAL);
  }
/*
 * Allocate memory for work array.
 */
  lwork = 2*nyrs*nmos + 4*nmos + nyrs;
  work = (double *)NclMalloc(lwork*sizeof(double));
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"sindex_yrmo: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * Call the f77 version of 'sindex' with the full argument list.
 */
  NGCALLF(dindx77,DINDX77)(dx,dy,&nmos,&nyrs,&missing_dx.doubleval,iprnt,
                           work,&lwork,soi,soi_noise,&ler);
  if (ler == 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"sindex_yrmo: One or both of the input data arrays contains all missing values");
    return(NhlFATAL);
  }
/*
 * free memory.
 */
  if((void*)dx != x) {
    NclFree(dx);
  }
  if((void*)dy != y) {
    NclFree(dy);
  }

  NclFree(work);
  NclFree(soi_noise);

/*
 * Return values. 
 */
  if(type_x != NCL_double && type_y != NCL_double) {
/*
 * Copy double values to float values.
 */
    rsoi = (float*)NclMalloc(sizeof(float)*total_size_xy);
    if( rsoi == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"sindex_yrmo: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    for( i = 0; i < total_size_xy; i++ ) rsoi[i] = (float)soi[i];

    NclFree(soi);
/*
 * Return float values with missing value set.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)rsoi,
                              &missing_rx,
                              2,
                              dsizes_x,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypefloatClass
                              );
  }
  else {
/*
 * Return double values with missing value set.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)soi,
                              &missing_dx,
                              2,
                              dsizes_x,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypedoubleClass
                              );
  }
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
  void *x, *y;
  double *dx, *dy;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS];
  int ndims_y, dsizes_y[NCL_MAX_DIMENSIONS];
  NclScalar missing_x, missing_y, missing_rx, missing_dx, missing_dy;
  int has_missing_x, has_missing_y;
  int nyrs, nmos, *iprnt;
  NclBasicDataTypes type_x, type_y, type_soi_noise;
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
  void *soi_noise;
  double *soi, *dsoi_noise;
  float *rsoi, *rsoi_noise;
  int *dsizes_soi;
/*
 * various
 */
  int i, lwork, ler = 0, total_size_xy;
  double *work;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
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
  y = (void*)NclGetArgValue(
           1,
           4,
           &ndims_y,
           dsizes_y,
           &missing_y,
           &has_missing_y,
           &type_y,
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
  soi_noise = (void*)NclGetArgValue(
           3,
           4,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_soi_noise,
           1);
/*
 * The grids coming in must be the same size.
 */
  if( (dsizes_x[0] != dsizes_y[0]) || (dsizes_x[1] != dsizes_y[1])) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"snindex_yrmo: The input arrays must have the same dimension sizes");
    return(NhlFATAL);
  }
/*
 * Get years and months.
 */
  *iprnt = -(*iprnt);
  nyrs = dsizes_x[0];
  nmos = dsizes_x[1];
  total_size_xy = nyrs * nmos;
/*
 * Coerce missing values to double.
 *
 * Use the default missing value if one isn't set.
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
      missing_rx.floatval = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
      missing_dx.doubleval = (double)missing_rx.floatval;
    }
    else {
      missing_dx.doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
    }
  }

  if(has_missing_y) {
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               &missing_dy,
               &missing_y,
               1,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_y)));
  }
  else {
/*
 * Get the default missing value.
 */ 
    if(type_y != NCL_double) {
      missing_dy.doubleval = (double)((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
    }
    else {
      missing_dy.doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
    }
  }
/*
 * x and y missing values must be equal.
 */
  if( missing_dx.doubleval != missing_dy.doubleval ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"snindex_yrmo: The missing values for x and y must be the same");
    return(NhlFATAL);
  }
    
/*
 * Coerce data to double if necessary.
 */
  if(type_x != NCL_double) {
    dx = (double*)NclMalloc(sizeof(double)*total_size_xy);
    if( dx == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"snindex_yrmo: Unable to allocate memory for coercing x array to double precision");
      return(NhlFATAL);
    }
    if(has_missing_x) {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dx,
                 x,
                 total_size_xy,
                 &missing_dx,
                 &missing_x,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
    }
    else {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dx,
                 x,
                 total_size_xy,
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
 * Coerce y to double.
 */
  if(type_y != NCL_double) {
    dy = (double*)NclMalloc(sizeof(double)*total_size_xy);
    if( dy == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"snindex_yrmo: Unable to allocate memory for coercing y array to double precision");
      return(NhlFATAL);
    }
    if(has_missing_y) {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dy,
                 y,
                 total_size_xy,
                 &missing_dy,
                 &missing_y,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_y)));
    }
    else {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dy,
                 y,
                 total_size_xy,
                 NULL,
                 NULL,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_y)));
    }
  }
  else {
/*
 * Input is already double.
 */
    dy = (double*)y;
  }

/*
 * Coerce soi_noise to double if necessary.
 *
 * soi_noise must be a float or double. It doesn't matter what the input
 * type is.
 */
  if(type_soi_noise != NCL_float && type_soi_noise != NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"snindex_yrmo: soi_noise must be of type float or double");
    return(NhlFATAL);
  }
/*
 * Allocate space for double precision soi_noise. There's no need to do a
 * coercion because soi_noise is an output-only variable (i.e, there are no
 * values coming in).  soi_noise can only be float or double, so only allocate
 * space for a d.p. array if soi_noise is float.
 */
  if(type_soi_noise == NCL_float) {
    dsoi_noise = (double*)NclMalloc(sizeof(double)*total_size_xy);
    if( dsoi_noise == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"snindex_yrmo: Unable to allocate memory for coercing soi_noise array to double precision");
      return(NhlFATAL);
    }
  }
  else {
/*
 * Input is already double.
 */
    dsoi_noise = (double*)soi_noise;
  }

/*
 * Allocate space for output arrays.
 */
  soi = (double *)NclMalloc(total_size_xy*sizeof(double));
  if( soi == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"snindex_yrmo: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Allocate memory for work array.
 */
  lwork = 2*nyrs*nmos + 4*nmos + nyrs;
  work = (double *)NclMalloc(lwork*sizeof(double));
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"snindex_yrmo: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * Call the f77 version of 'sindex' with the full argument list.
 */
  NGCALLF(dindx77,DINDX77)(dx,dy,&nmos,&nyrs,&missing_dx.doubleval,iprnt,
                           work,&lwork,soi,dsoi_noise,&ler);
  if (ler == 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"snindex_yrmo: One or both of the input data arrays contains all missing values");
    return(NhlFATAL);
  }
/*
 * Free memory.
 */
  if((void*)dx != x) {
    NclFree(dx);
  }
  if((void*)dy != y) {
    NclFree(dy);
  }

  NclFree(work);
/*
 * If returning float values, we need to copy the coerced float values
 * back to the original location of soi_noise. Do this by creating a
 * pointer of type float that points to the original location, and then
 * loop through the values and do the coercion.
 */
  if(type_soi_noise == NCL_float) {
    rsoi_noise = (float*)soi_noise;  /* Float ptr to original soi_noise */
    for( i = 0; i < total_size_xy; i++ ) {
      rsoi_noise[i]  = (float)dsoi_noise[i];
    }
    NclFree(dsoi_noise);   /* Free up the double array */
  }

/*
 * Set up variable to return.
 */
  if(type_x != NCL_double && type_y != NCL_double) {
/*
 * Copy double values to float values.
 */
    rsoi = (float*)NclMalloc(sizeof(float)*total_size_xy);
    if( rsoi == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"snindex_yrmo: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    for( i = 0; i < total_size_xy; i++ ) rsoi[i] = (float)soi[i];
/*
 * Free up d.p array.
 */
    NclFree(soi);
/*
 * Return float values with missing value set.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)rsoi,
                              &missing_rx,
                              2,
                              dsizes_x,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypefloatClass
                              );
  }
  else {
/*
 * Return double values with missing value set.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)soi,
                              &missing_dx,
                              2,
                              dsizes_x,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypedoubleClass
                              );
  }
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

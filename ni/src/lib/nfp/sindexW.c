#include <stdio.h>
#include <math.h>
#include "wrapper.h"

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
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int ndims_y;
  ng_size_t dsizes_y[NCL_MAX_DIMENSIONS];
  NclScalar missing_x, missing_y, missing_rx, missing_dx, missing_dy;
  int has_missing_x, has_missing_y;
  ng_size_t nyrs, nmos;
  int inyrs, inmos, *iprnt;
  NclBasicDataTypes type_x, type_y;
/*
 * Attribute variables
 */
  int att_id;
  ng_size_t nelem = 1;
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
  NclQuark *long_name, *short_name, *units;
/*
 * Output array variables
 */
  void *soi;
  double *tmp_soi, *soi_noise;
  NclBasicDataTypes type_soi;
/*
 * various
 */
  int ler = 0, ilwork;
  ng_size_t lwork, total_size_xy;
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
           DONT_CARE);
  y = (void*)NclGetArgValue(
           1,
           3,
           &ndims_y,
           dsizes_y,
           &missing_y,
           &has_missing_y,
           &type_y,
           DONT_CARE);
  iprnt = (int*)NclGetArgValue(
           2,
           3,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
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
  lwork = 2*nyrs*nmos + 4*nmos + nyrs;

/*
 * Test dimension sizes.
 */
  if((lwork > INT_MAX) || (nyrs > INT_MAX) || (nmos > INT_MAX)){
    NhlPError(NhlFATAL,NhlEUNKNOWN,"sindex_yrmo: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  ilwork = (int) lwork;
  inyrs  = (int) nyrs;
  inmos  = (int) nmos;

/*
 * Coerce missing values to double.
 *
 * Use the default missing value if one isn't set.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
  coerce_missing(type_y,has_missing_y,&missing_y,&missing_dy,NULL);
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
  dx = coerce_input_double(x,type_x,total_size_xy,has_missing_x,&missing_x,
                           &missing_dx);
  dy = coerce_input_double(y,type_y,total_size_xy,has_missing_y,&missing_y,
                           &missing_dy);

  if(dx == NULL || dy == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"sindex_yrmo: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate space for output arrays.
 */
  if(type_x != NCL_double && type_y != NCL_double) {
    type_soi = NCL_float;
    soi = (void *)calloc(total_size_xy,sizeof(float));
  }
  else {
    type_soi = NCL_double;
    soi = (void *)calloc(total_size_xy,sizeof(double));
  }
  tmp_soi   = coerce_output_double(soi,type_soi,total_size_xy);
  soi_noise = (double *)calloc(total_size_xy,sizeof(double));

  if( tmp_soi == NULL || soi == NULL || soi_noise == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"sindex_yrmo: Unable to allocate memory for output array(s)");
    return(NhlFATAL);
  }
/*
 * Allocate memory for work array.
 */
  work = (double *)calloc(lwork,sizeof(double));
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"sindex_yrmo: Unable to allocate memory for work array");
    return(NhlFATAL);
  }

/*
 * Call the f77 version of 'sindex' with the full argument list.
 */
  NGCALLF(dindx77,DINDX77)(dx,dy,&inmos,&inyrs,&missing_dx.doubleval,iprnt,
                           work,&ilwork,tmp_soi,soi_noise,&ler);

  if (ler == 2) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"sindex_yrmo: One or both of the input data arrays contains all missing values");
  }
  if(type_soi == NCL_float) {
    coerce_output_float_only(soi,tmp_soi,total_size_xy,0);
    NclFree(tmp_soi);
  }
/*
 * free memory.
 */
  if((void*)dx != x) NclFree(dx);
  if((void*)dy != y) NclFree(dy);
  NclFree(work);
  NclFree(soi_noise);

/*
 * Return values. 
 */
  if(type_soi == NCL_float) {
/*
 * Return float values with missing value set.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              soi,
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
                              soi,
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
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int ndims_y;
  ng_size_t dsizes_y[NCL_MAX_DIMENSIONS];
  NclScalar missing_x, missing_y, missing_rx, missing_dx, missing_dy;
  int has_missing_x, has_missing_y;
  int inyrs, inmos, *iprnt;
  ng_size_t nyrs, nmos;
  NclBasicDataTypes type_x, type_y, type_soi_noise;
/*
 * Attribute variables
 */
  int att_id;
  ng_size_t nelem = 1;
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
  NclQuark *long_name, *short_name, *units;
/*
 * Output array variables
 */
  void *soi, *soi_noise;
  double *tmp_soi, *tmp_soi_noise;
  NclBasicDataTypes type_soi;
/*
 * various
 */
  int ler = 0, ilwork;
  ng_size_t lwork;
  ng_size_t total_size_xy;
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
           DONT_CARE);
  y = (void*)NclGetArgValue(
           1,
           4,
           &ndims_y,
           dsizes_y,
           &missing_y,
           &has_missing_y,
           &type_y,
           DONT_CARE);
  iprnt = (int*)NclGetArgValue(
           2,
           4,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
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
  lwork = 2*nyrs*nmos + 4*nmos + nyrs;

/*
 * Test dimension sizes.
 */
  if((lwork > INT_MAX) || (nyrs > INT_MAX) || (nmos > INT_MAX)){
    NhlPError(NhlFATAL,NhlEUNKNOWN,"snindex_yrmo: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  ilwork = (int) lwork;
  inyrs  = (int) nyrs;
  inmos  = (int) nmos;

/*
 * Coerce missing values to double.
 *
 * Use the default missing value if one isn't set.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
  coerce_missing(type_y,has_missing_y,&missing_y,&missing_dy,NULL);
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
  dx = coerce_input_double(x,type_x,total_size_xy,has_missing_x,&missing_x,
                           &missing_dx);
  dy = coerce_input_double(y,type_y,total_size_xy,has_missing_y,&missing_y,
                           &missing_dy);

  if(dx == NULL || dy == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"snindex_yrmo: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
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
  tmp_soi_noise = coerce_output_double(soi_noise,type_soi_noise,
                                       total_size_xy);
  if( tmp_soi_noise == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"snindex_yrmo: Unable to allocate memory for coercing soi_noise array to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate space for output arrays.
 */
  if(type_x != NCL_double && type_y != NCL_double) {
    type_soi = NCL_float;
    soi = calloc(total_size_xy,sizeof(float));
  }
  else {
    type_soi = NCL_double;
    soi = calloc(total_size_xy,sizeof(double));
  }
  tmp_soi = coerce_output_double(soi,type_soi,total_size_xy);
  if( soi == NULL || tmp_soi == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"snindex_yrmo: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Allocate memory for work array.
 */
  work = (double *)calloc(lwork,sizeof(double));
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"snindex_yrmo: Unable to allocate memory for work array");
    return(NhlFATAL);
  }

/*
 * Call the f77 version of 'sindex' with the full argument list.
 */
  NGCALLF(dindx77,DINDX77)(dx,dy,&inmos,&inyrs,&missing_dx.doubleval,iprnt,
			   work,&ilwork,tmp_soi,tmp_soi_noise,&ler);

  if (ler == 2) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"snindex_yrmo: One or both of the input data arrays contains all missing values");
  }
/*
 * If returning float values, we need to coerce the double precision values.
 */
  if(type_soi_noise == NCL_float) {
    coerce_output_float_only(soi_noise,tmp_soi_noise,total_size_xy,0);
    NclFree(tmp_soi_noise);   /* Free up the double array */
  }
  if(type_soi == NCL_float) {
    coerce_output_float_only(soi,tmp_soi,total_size_xy,0);
    NclFree(tmp_soi);   /* Free up the double array */
  }
/*
 * Free memory.
 */
  if((void*)dx != x) NclFree(dx);
  if((void*)dy != y) NclFree(dy);
  NclFree(work);

/*
 * Set up variable to return.
 */
  if(type_soi == NCL_float) {
/*
 * Return float values with missing value set.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              soi,
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
                              soi,
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

#include <stdio.h>

/*
 * The following are the required NCAR Graphics include files.
 * They should be located in ${NCARG_ROOT}/include.
 */
#include "wrapper.h"
#include "Machine.h"
#include "NclAtt.h"
#include <ncarg/ncl/NclVar.h>
#include "DataSupport.h"
#include "AttSupport.h"
#include "VarSupport.h"
#include "NclCoordVar.h"
#include <ncarg/ncl/NclCallBacksI.h>
#include <math.h>

extern void NGCALLF(dlocalmn,DLOCALMN)(double *,int *,int *,double *,
                                       int *,int *,int *,int *,
                                       double *, double *, int *, int *);

extern void NGCALLF(dlocalmx,DLOCALMX)(double *,int *,int *,double *,
                                       int *,int *,int *,int *,
                                       double *, double *, int *, int *);

NhlErrorTypes local_min_W( void )
{
/*
 * Input array variables
 */
  void   *x, *delta;
  logical *cyclic;
  double *tmp_x, *tmp_delta;
  int dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclScalar missing_x, missing_dx, missing_rx;
  NclBasicDataTypes type_x, type_delta;
/*
 * Attribute variables
 */
  int att_id;
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
/*
 * Output array variables
 */
  int tmp_nmin, *tmp_xi, *tmp_yi;
  void *xi, *yi, *minvals, *nmin;
  double *tmp_minvals;
/*
 * Declare various variables for random purposes.
 */
  int i, nx, ny, nxny, ier, dsizes[1];
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */

/*
 * Retrieve argument #1
 */
  x = (void*)NclGetArgValue(
          0,
          3,
          NULL,
          dsizes_x,
          &missing_x,
          &has_missing_x,
          &type_x,
          2);

/*
 * Retrieve argument #2
 */
  cyclic = (logical*)NclGetArgValue(
          1,
          3,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);
/*
 * Retrieve argument #3
 */
  delta = (void*)NclGetArgValue(
          2,
          3,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_delta,
          2);
/*
 * Get size of input array.
 */
  nx = dsizes_x[1];
  ny = dsizes_x[0];
  if( nx < 3 || ny < 3 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"local_min: The input array must have more than 3 elements in each dimension");
    return(NhlFATAL);
  }
  nxny = nx * ny;
/*
 * Check for missing values.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Create arrays to hold X and Y coordinate arrays and min values.
 */
  tmp_xi      = (int*)calloc(nxny,sizeof(int));
  tmp_yi      = (int*)calloc(nxny,sizeof(int));
  tmp_minvals = (double*)calloc(nxny,sizeof(double));
  nmin        = (void*)calloc(1,sizeof(int));
  if( tmp_xi == NULL || tmp_yi == NULL || tmp_minvals == NULL || nmin == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"local_min: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }
/*
 * Coerce data to double if necessary.
 */
  if(type_x != NCL_double) {
    tmp_x = (double*)calloc(nxny,sizeof(double));
    if( tmp_x == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"local_min: Unable to allocate memory for coercing x to double precision");
      return(NhlFATAL);
    }
  }

  if(type_delta != NCL_double) {
    tmp_delta = (double*)calloc(1,sizeof(double));
    if( tmp_delta == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"local_min: Unable to allocate memory for coercing delta to double precision");
      return(NhlFATAL);
    }
  }

  if(type_x != NCL_double) {
/*
 * Coerce x (tmp_x) to double.
 */
    coerce_subset_input_double(x,tmp_x,0,type_x,nxny,0,NULL,NULL);
  }
  else {
/*
 * Point tmp_x to x.
 */
    tmp_x = &((double*)x)[0];
  }
  if(type_delta != NCL_double) {
/*
 * Coerce delta (tmp_delta) to double.
 */
    coerce_subset_input_double(delta,tmp_delta,0,type_delta,1,0,NULL,NULL);
  }
  else {
/*
 * Point tmp_delta to delta.
 */
    tmp_delta = &((double*)delta)[0];
  }
/*
 * Call the Fortran routine.
 */
  NGCALLF(dlocalmn,DLOCALMN)(tmp_x,&nx,&ny,&missing_dx.doubleval,cyclic,
                             tmp_xi,tmp_yi,&nxny,tmp_minvals,tmp_delta,
                             &tmp_nmin,&ier);
  ((int*)nmin)[0] = tmp_nmin;

/*
 * If number of local minimums is zero, then don't bother returning
 * any attributes.
 */
  if(tmp_nmin == 0) {
    dsizes[0] = 1;
    return(NclReturnValue(nmin,1,dsizes,NULL,NCL_int,0));
  }
/*
 * Allocate space for coorindate array indices and minimum values.
 */
  xi = (void*)calloc(tmp_nmin,sizeof(int));
  yi = (void*)calloc(tmp_nmin,sizeof(int));
  if( xi == NULL || yi == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"local_min: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }
  if(type_x != NCL_double) {
    minvals = (void*)calloc(tmp_nmin,sizeof(float));
  }
  else {
    minvals = (void*)calloc(tmp_nmin,sizeof(double));
  }
  if(minvals == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"local_min: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }
/*
 * Copy output values.
 */
  for(i = 0; i < tmp_nmin; i++) {
    ((int*)xi)[i] = tmp_xi[i];
    ((int*)yi)[i] = tmp_yi[i];
    if(type_x != NCL_double) {
      ((float*)minvals)[i] = (float)tmp_minvals[i];
    }
    else {
      ((double*)minvals)[i] = tmp_minvals[i];
    }
  }
/*
 * Free memory.
 */
  if(type_x     != NCL_double) NclFree(tmp_x);
  if(type_delta != NCL_double) NclFree(tmp_delta);
  NclFree(tmp_xi);
  NclFree(tmp_yi);
  NclFree(tmp_minvals);

/*
 * Set up return structure.
 */
  dsizes[0] = 1;
  return_md = _NclCreateVal(
                            NULL,
                            NULL,
                            Ncl_MultiDValData,
                            0,
                            nmin,
                            NULL,
                            1,
                            dsizes,
                            TEMPORARY,
                            NULL,
                            (NclObjClass)nclTypeintClass
                            );
/*
 * Set up attributes to return.
 */
  att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

  dsizes[0] = tmp_nmin;  

  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         yi,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypeintClass
                         );
  _NclAddAtt(
             att_id,
             "yi",
             att_md,
             NULL
             );

  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         xi,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypeintClass
                         );
  _NclAddAtt(
             att_id,
             "xi",
             att_md,
             NULL
             );

  dsizes[0] = tmp_nmin;  

  if(type_x != NCL_double) {
/*
 * Input array is not double, so return float min values.
 */
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           minvals,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
                           );
  }
  else {
/*
 * Input array is double, so return double min values.
 */
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           minvals,
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
             "minval",
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

NhlErrorTypes local_max_W( void )
{
/*
 * Input array variables
 */
  void   *x, *delta;
  logical *cyclic;
  double *tmp_x, *tmp_delta;
  int dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclScalar missing_x, missing_dx, missing_rx;
  NclBasicDataTypes type_x, type_delta;
/*
 * Attribute variables
 */
  int att_id;
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
/*
 * Output array variables
 */
  int tmp_nmax, *tmp_xi, *tmp_yi;
  void *xi, *yi, *maxvals, *nmax;
  double *tmp_maxvals;
/*
 * Declare various variables for random purposes.
 */
  int i, nx, ny, nxny, ier, dsizes[1];
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */

/*
 * Retrieve argument #1
 */
  x = (void*)NclGetArgValue(
          0,
          3,
          NULL,
          dsizes_x,
          &missing_x,
          &has_missing_x,
          &type_x,
          2);

/*
 * Retrieve argument #2
 */
  cyclic = (logical*)NclGetArgValue(
          1,
          3,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);
/*
 * Retrieve argument #3
 */
  delta = (void*)NclGetArgValue(
          2,
          3,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_delta,
          2);
/*
 * Get size of input array.
 */
  nx = dsizes_x[1];
  ny = dsizes_x[0];
  if( nx < 3 || ny < 3 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"local_max: The input array must have more than 3 elements in each dimension");
    return(NhlFATAL);
  }
  nxny = nx * ny;
/*
 * Check for missing values.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Create arrays to hold X and Y coordinate arrays and max values.
 */
  tmp_xi      = (int*)calloc(nxny,sizeof(int));
  tmp_yi      = (int*)calloc(nxny,sizeof(int));
  tmp_maxvals = (double*)calloc(nxny,sizeof(double));
  nmax        = (void*)calloc(1,sizeof(int));
  if( tmp_xi == NULL || tmp_yi == NULL || tmp_maxvals == NULL || nmax == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"local_max: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }
/*
 * Coerce data to double if necessary.
 */
  if(type_x != NCL_double) {
    tmp_x = (double*)calloc(nxny,sizeof(double));
    if( tmp_x == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"local_max: Unable to allocate memory for coercing x to double precision");
      return(NhlFATAL);
    }
  }

  if(type_delta != NCL_double) {
    tmp_delta = (double*)calloc(1,sizeof(double));
    if( tmp_delta == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"local_max: Unable to allocate memory for coercing delta to double precision");
      return(NhlFATAL);
    }
  }

  if(type_x != NCL_double) {
/*
 * Coerce x (tmp_x) to double.
 */
    coerce_subset_input_double(x,tmp_x,0,type_x,nxny,0,NULL,NULL);
  }
  else {
/*
 * Point tmp_x to x.
 */
    tmp_x = &((double*)x)[0];
  }
  if(type_delta != NCL_double) {
/*
 * Coerce delta (tmp_delta) to double.
 */
    coerce_subset_input_double(delta,tmp_delta,0,type_delta,1,0,NULL,NULL);
  }
  else {
/*
 * Point tmp_delta to delta.
 */
    tmp_delta = &((double*)delta)[0];
  }
/*
 * Call the Fortran routine.
 */
  NGCALLF(dlocalmx,DLOCALMX)(tmp_x,&nx,&ny,&missing_dx.doubleval,cyclic,
                             tmp_xi,tmp_yi,&nxny,tmp_maxvals,tmp_delta,
                             &tmp_nmax,&ier);
  ((int*)nmax)[0] = tmp_nmax;
/*
 * If number of local maximums is zero, then don't bother returning
 * any attributes.
 */
  if(tmp_nmax == 0) {
    dsizes[0] = 1;
    return(NclReturnValue(nmax,1,dsizes,NULL,NCL_int,0));
  }
/*
 * Allocate space for coorindate array indices and maximum values.
 */
  xi = (void*)calloc(tmp_nmax,sizeof(int));
  yi = (void*)calloc(tmp_nmax,sizeof(int));
  if( xi == NULL || yi == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"local_max: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }
  if(type_x != NCL_double) {
    maxvals = (void*)calloc(tmp_nmax,sizeof(float));
  }
  else {
    maxvals = (void*)calloc(tmp_nmax,sizeof(double));
  }
  if( maxvals == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"local_max: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }
/*
 * Copy output values.
 */
  for(i = 0; i < tmp_nmax; i++) {
    ((int*)xi)[i] = tmp_xi[i];
    ((int*)yi)[i] = tmp_yi[i];
    if(type_x != NCL_double) {
      ((float*)maxvals)[i] = (float)tmp_maxvals[i];
    }
    else {
      ((double*)maxvals)[i] = tmp_maxvals[i];
    }
  }
/*
 * Free memory.
 */
  if(type_x     != NCL_double) NclFree(tmp_x);
  if(type_delta != NCL_double) NclFree(tmp_delta);
  NclFree(tmp_xi);
  NclFree(tmp_yi);
  NclFree(tmp_maxvals);

/*
 * Set up return structure.
 */
  dsizes[0] = 1;
  return_md = _NclCreateVal(
                            NULL,
                            NULL,
                            Ncl_MultiDValData,
                            0,
                            nmax,
                            NULL,
                            1,
                            dsizes,
                            TEMPORARY,
                            NULL,
                            (NclObjClass)nclTypeintClass
                            );
/*
 * Set up attributes to return.
 */
  att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

  dsizes[0] = tmp_nmax;  

  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         yi,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypeintClass
                         );
  _NclAddAtt(
             att_id,
             "yi",
             att_md,
             NULL
             );

  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         xi,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypeintClass
                         );
  _NclAddAtt(
             att_id,
             "xi",
             att_md,
             NULL
             );

  dsizes[0] = tmp_nmax;  

  if(type_x != NCL_double) {
/*
 * Input array is not double, so return float max values.
 */
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           maxvals,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
                           );
  }
  else {
/*
 * Input array is double, so return double max values.
 */
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           maxvals,
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
             "maxval",
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


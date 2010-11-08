#include <stdio.h>
#include <math.h>
#include "wrapper.h"

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
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
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
  int i, ier, inx, iny, inxny;
  ng_size_t nx, ny, nxny;
  ng_size_t dsizes[1];
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
          DONT_CARE);

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
          DONT_CARE);
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
          DONT_CARE);
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
 * Check dimension sizes
 */
  if((nx > INT_MAX) || (ny > INT_MAX) || (nxny > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"local_min: one or more input dimension sizes are greater than INT_MAX");
    return(NhlFATAL);
  }
  inx = (int) nx;
  iny = (int) ny;
  inxny = (int) nxny;

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
 * Coerce input to double if necessary.
 */
  tmp_x     = coerce_input_double(x,type_x,nxny,0,NULL,NULL);
  tmp_delta = coerce_input_double(delta,type_delta,1,0,NULL,NULL);
  if(tmp_x == NULL || tmp_delta == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"local_min: Unable to coerce input to double precision");
    return(NhlFATAL);
  }

/*
 * Call the Fortran routine.
 */
  NGCALLF(dlocalmn,DLOCALMN)(tmp_x,&inx,&iny,&missing_dx.doubleval,cyclic,
                             tmp_xi,tmp_yi,&inxny,tmp_minvals,tmp_delta,
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
  coerce_output_float_or_double(minvals,tmp_minvals,type_x,tmp_nmin,0);
  for(i = 0; i < tmp_nmin; i++) {
    ((int*)xi)[i] = tmp_xi[i];
    ((int*)yi)[i] = tmp_yi[i];
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
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
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
  int i, ier, inx, iny, inxny;
  ng_size_t nx, ny, nxny;
  ng_size_t dsizes[1];
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
          DONT_CARE);

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
          DONT_CARE);
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
          DONT_CARE);
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
 * Check dimension sizes
 */
  if((nx > INT_MAX) || (ny > INT_MAX) || (nxny > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"local_max: one or more input dimension sizes are greater than INT_MAX");
    return(NhlFATAL);
  }
  inx = (int) nx;
  iny = (int) ny;
  inxny = (int) nxny;

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
  tmp_x     = coerce_input_double(x,type_x,nxny,0,NULL,NULL);
  tmp_delta = coerce_input_double(delta,type_delta,1,0,NULL,NULL);
  if( tmp_x == NULL || tmp_delta == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"local_max: Unable to coerce input to double precision");
    return(NhlFATAL);
  }

/*
 * Call the Fortran routine.
 */
  NGCALLF(dlocalmx,DLOCALMX)(tmp_x,&inx,&iny,&missing_dx.doubleval,cyclic,
                             tmp_xi,tmp_yi,&inxny,tmp_maxvals,tmp_delta,
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
  coerce_output_float_or_double(maxvals,tmp_maxvals,type_x,tmp_nmax,0);
  for(i = 0; i < tmp_nmax; i++) {
    ((int*)xi)[i] = tmp_xi[i];
    ((int*)yi)[i] = tmp_yi[i];
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


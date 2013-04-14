#include <stdio.h>
#include <math.h>
#include "wrapper.h"

extern void NGCALLF(dcovcormssm,DCOVCORMSSM)(int *, int *, double *, double *,
                                             int *, double *, int *, double *,
                                             int *);

extern void NGCALLF(dcovcorm,DCOVCORM)(int *, int *, double *, double *, 
                                       int *, double*, int *, double *, int *);

extern void NGCALLF(dcovarxy,DCOVARXY)(double *,double *,double *,double *,
                                       double *,int *,int *,int *,int *,int *);

NhlErrorTypes covcorm_W( void )
{
/*
 * Input array variables
 */
  void *x, *trace;
  int *iopt;
  double *dx, *dtrace;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int ndims_x, has_missing_x;
  NclScalar missing_x, missing_dx;
  ng_size_t size_x, nvar, ntim, lvcm;
  int ier;
  NclBasicDataTypes type_x;

/*
 * Output array variable
 */
  void  *vcm;
  double *dvcm;
  ng_size_t *dsizes_vcm;
  int ndims_vcm;
  ng_size_t size_vcm;
  NclBasicDataTypes type_vcm;
  NclTypeClass type_vcm_class;
  NclScalar missing_vcm;

/*
 * Variables for returning attributes.
 */
  int att_id;
  ng_size_t dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;

  int intim, invar, ilvcm;

/*
 * Retrieve x.
 */
  x = (void*)NclGetArgValue(
          0,
          2,
          &ndims_x,
          dsizes_x,
          &missing_x,
          &has_missing_x,
          &type_x,
          DONT_CARE);

  iopt = (int*)NclGetArgValue(
          1,
          2,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

  nvar = dsizes_x[0];
  ntim = dsizes_x[1];
  size_x = nvar * ntim;
  lvcm = (nvar*(nvar+1))/2;

/*
 * Test dimension sizes to make sure they are <= INT_MAX.
 */
  if((ntim > INT_MAX) ||
     (nvar > INT_MAX) ||
     (lvcm > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"covcorm: one or more dimension sizes are greater than INT_MAX");
    return(NhlFATAL);
  }
  intim = (int) ntim;
  invar = (int) nvar;
  ilvcm = (int) lvcm;

/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,NULL);

/*
 * Allocate space for input/output arrays.
 */
  if(!iopt[1]) {
    size_vcm      = lvcm;
    ndims_vcm     = 1;
    dsizes_vcm    = (ng_size_t*)malloc(sizeof(ng_size_t));
    dsizes_vcm[0] = size_vcm;
  }
  else {
    size_vcm      = nvar*nvar;
    ndims_vcm     = 2;
    dsizes_vcm    = (ng_size_t*)malloc(2*sizeof(ng_size_t));
    dsizes_vcm[0] = nvar;
    dsizes_vcm[1] = nvar;
  }
  dx = coerce_input_double(x,type_x,size_x,0,NULL,NULL);

  if(type_x == NCL_double) {
    type_vcm              = NCL_double;
    vcm                   = (void*)malloc(size_vcm*sizeof(double));
    trace                 = (void*)malloc(sizeof(double));
    if(vcm == NULL || trace == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"covcorm: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    dvcm                  = &((double*)vcm)[0];
    dtrace                = &((double*)trace)[0];
    missing_vcm.doubleval = missing_dx.doubleval;
  }
  else {
    type_vcm             = NCL_float;
    vcm                  = (void*)malloc(size_vcm*sizeof(float));
    trace                = (void*)malloc(sizeof(float));
    dvcm                 = (double*)malloc(size_vcm*sizeof(double));
    dtrace               = (double*)malloc(sizeof(double));
    missing_vcm.floatval = (float)missing_dx.doubleval;
    if(vcm == NULL || trace == NULL  || dvcm == NULL || dtrace == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"covcorm: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }


/*
 * Depending on iopt[1], call one of two Fortran routines.
 *    iopt[0]=0 --> covariance
 *    iopt[0]=1 --> correlation 
 *    iopt[1]=0 --> 1D array (symmetric storage)
 *    iopt[1]=1 --> 2D array
 */
  if(!iopt[1]) {
    NGCALLF(dcovcormssm,DCOVCORMSSM)(&intim,&invar,dx,&missing_dx.doubleval,
                                     &iopt[0],dvcm,&ilvcm,dtrace,&ier);
  }
  else {
    NGCALLF(dcovcorm,DCOVCORM)(&intim,&invar,dx,&missing_dx.doubleval,
                               &iopt[0],dvcm,&ilvcm,dtrace,&ier);
  }

  if(type_vcm == NCL_float) {
/*
 * Need to coerce output array back to float before we return it.
 */
    coerce_output_float_only(vcm,dvcm,size_vcm,0);
    coerce_output_float_only(trace,dtrace,1,0);

    NclFree(dx);
    if(type_x != NCL_double) NclFree(dvcm);
    NclFree(dtrace);
  }


/*
 * Set up return value.
 */
  type_vcm_class = (NclTypeClass)(_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(type_vcm))));
  return_md = _NclCreateVal(
                            NULL,
                            NULL,
                            Ncl_MultiDValData,
                            0,
                            vcm,
                            &missing_vcm,
                            ndims_vcm,
                            dsizes_vcm,
                            TEMPORARY,
                            NULL,
                            (NclObjClass)type_vcm_class
                            );

/*
 * Initialize att_id so we can return some attributes.
 */
  att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

  dsizes[0] = 1;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         trace,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)type_vcm_class
                         );
  _NclAddAtt(
             att_id,
             "trace",
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

NhlErrorTypes covcorm_xy_W( void )
{
/*
 * Input array variables
 */
  void *x, *y;
  int *iopt;
  double *dx, *dy;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS],  dsizes_y[NCL_MAX_DIMENSIONS];
  int ndims_x, has_missing_x, ndims_y, has_missing_y;
  NclScalar missing_x, missing_dx, missing_y, missing_dy;
  ng_size_t size_x, nvar, ntim;
  int invar, intim;
  NclBasicDataTypes type_x, type_y;

/*
 * Output array variable
 */
  void  *vcm;
  double *dvcm;
  ng_size_t *dsizes_vcm;
  int ndims_vcm, ret;
  ng_size_t size_vcm;
  NclBasicDataTypes type_vcm;
  NclScalar missing_vcm;

/*
 * Retrieve x.
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

  iopt = (int*)NclGetArgValue(
          2,
          3,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

  nvar = dsizes_x[0];
  ntim = dsizes_x[1];

  if(dsizes_y[0] != nvar || dsizes_y[1] != ntim) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"covcorm_xy: x and y must be the same size");
    return(NhlFATAL);
  }

  size_x = nvar * ntim;

/*
 * Test dimension sizes to make sure they are <= INT_MAX.
 */
  if((ntim > INT_MAX) || (nvar > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"covcorm_xy: one or more dimension sizes are greater than INT_MAX");
    return(NhlFATAL);
  }
  intim = (int) ntim;
  invar = (int) nvar;

/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,NULL);
  coerce_missing(type_y,has_missing_y,&missing_y,&missing_dy,NULL);

/*
 * Allocate space for input/output arrays.
 */
  size_vcm      = nvar*nvar;
  ndims_vcm     = 2;
  dsizes_vcm    = (ng_size_t*)malloc(2*sizeof(ng_size_t));
  dsizes_vcm[0] = nvar;
  dsizes_vcm[1] = nvar;

  dx = coerce_input_double(x,type_x,size_x,0,NULL,NULL);
  dy = coerce_input_double(y,type_y,size_x,0,NULL,NULL);

  if(type_x == NCL_double || type_y == NCL_double) {
    type_vcm = NCL_double;
    vcm      = (void*)malloc(size_vcm*sizeof(double));
    if(vcm == NULL) { 
      NhlPError(NhlFATAL,NhlEUNKNOWN,"covcorm_xy: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    dvcm                  = &((double*)vcm)[0];
    missing_vcm.doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
  }
  else {
    type_vcm = NCL_float;
    vcm      = (void*)malloc(size_vcm*sizeof(float));
    dvcm     = (double*)malloc(size_vcm*sizeof(double));
    if(vcm == NULL  || dvcm == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"covcorm_xy: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    missing_vcm.floatval = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
  }

/*
 * Call the fortran routine.
 *     iopt(0) --> iopt
 *     iopt(1) --> lag
 *     iopt(2) --> ncrit
 */
  NGCALLF(dcovarxy,DCOVARXY)(dx,dy,&missing_dx.doubleval,
                             &missing_dy.doubleval,dvcm,&intim,&invar,
                             &iopt[1],&iopt[2],&iopt[0]);

/* Coerce to float if necessary */
  if(type_vcm == NCL_float) coerce_output_float_only(vcm,dvcm,size_vcm,0);

/* Free memory */
  if(type_x   != NCL_double) NclFree(dx);
  if(type_y   != NCL_double) NclFree(dy);
  if(type_vcm != NCL_double) NclFree(dvcm);

/* Return */
  ret = NclReturnValue(vcm,ndims_vcm,dsizes_vcm,&missing_vcm,type_vcm,0);
  NclFree(dsizes_vcm);
  return(ret);
}


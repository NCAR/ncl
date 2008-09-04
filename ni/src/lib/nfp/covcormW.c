#include <stdio.h>
#include <math.h>
#include "wrapper.h"

extern void NGCALLF(dcovcormssm,DCOVCORMSSM)(int *, int *, double *, double *,
                                             int *, double *, int *, double *,
                                             int *);

extern void NGCALLF(dcovcorm,DCOVCORM)(int *, int *, double *, double *, 
                                       int *, double*, int *, double *, int *);

NhlErrorTypes covcorm_W( void )
{
/*
 * Input array variables
 */
  void *x, *trace;
  int *iopt;
  double *dx, *dtrace;
  int dsizes_x[NCL_MAX_DIMENSIONS], ndims_x, has_missing_x;
  NclScalar missing_x, missing_dx;
  int size_x, nvar, ntim, lvcm, ier;
  NclBasicDataTypes type_x;

/*
 * Output array variable
 */
  void  *vcm;
  double *dvcm;
  int *dsizes_vcm, ndims_vcm, size_vcm;
  NclBasicDataTypes type_vcm;
  NclTypeClass type_vcm_class;
  NclScalar missing_vcm;

/*
 * Variables for returning attributes.
 */
  int att_id;
  int dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
  int i;

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
          2);

  iopt = (int*)NclGetArgValue(
          1,
          2,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);

  nvar = dsizes_x[0];
  ntim = dsizes_x[1];
  size_x = nvar * ntim;

/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,NULL);

/*
 * Allocate space for input/output arrays.
 */
  lvcm = (nvar*(nvar+1))/2;
  if(!iopt[0]) {
    size_vcm      = lvcm;
    ndims_vcm     = 1;
    dsizes_vcm    = (int*)malloc(sizeof(int));
    dsizes_vcm[0] = size_vcm;
  }
  else {
    size_vcm      = nvar*nvar;
    ndims_vcm     = 2;
    dsizes_vcm    = (int*)malloc(2*sizeof(int));
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
 * Depending on iopt[0], call one of two Fortran routines.
 */
  if(!iopt[0]) {
    NGCALLF(dcovcormssm,DCOVCORMSSM)(&ntim,&nvar,dx,&missing_dx.doubleval,
                                     &iopt[1],dvcm,&lvcm,dtrace,&ier);
  }
  else {
    NGCALLF(dcovcorm,DCOVCORM)(&ntim,&nvar,dx,&missing_dx.doubleval,
                               &iopt[1],dvcm,&lvcm,dtrace,&ier);
  }

  if(type_vcm == NCL_float) {
/*
 * Need to coerce output array back to float before we return it.
 */
    coerce_output_float_only(vcm,dvcm,size_vcm,0);
    coerce_output_float_only(trace,dtrace,1,0);

    NclFree(dx);
    NclFree(dvcm);
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

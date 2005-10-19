#include <stdio.h>
#include <math.h>
#include "wrapper.h"

extern void NGCALLF(dcancorxy,DCANCORXY)(int *, int *, int *, int *, int *, 
                                         int *, int *, double *,double *,int *,
                                         double *, double *, double *,
                                         double *, double *,double *,
                                         double *, int *, int *);

NhlErrorTypes cancor_W( void )
{
/*
 * Input array variables
 */
  void *x, *y;
  double *tmp_x, *tmp_y;
  logical *opt;
  int dsizes_y[2], dsizes_x[2];
  NclBasicDataTypes type_x, type_y;
/*
 * Attribute variables.
 */
  int *ndf;
  void *chisq, *coefx, *coefy, *wlam;
  double *tmp_chisq, *tmp_coefx, *tmp_coefy, *tmp_wlam;

  int att_id;
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;

/*
 * Output variable.
 */
  void *canr;
  double *tmp_canr;
  int dsizes_canr[1], dsizes_coefx[2], dsizes_coefy[2];
  NclBasicDataTypes type_canr;
/*
 * various
 */
  int size_coefx, size_coefy;
  int nobs, nobsx, nobsy, nx, ny, nxy, minxy, maxxy, lrdim, lrr;
  double *eval, *rr;
  int i, ier;

/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  x = (void*)NclGetArgValue(
           0,
           3,
           NULL,
           dsizes_x,
           NULL,
           NULL,
           &type_x,
           2);

  y = (void*)NclGetArgValue(
           1,
           3,
           NULL,
           dsizes_y,
           NULL,
           NULL,
           &type_y,
           2);

  opt = (logical *)NclGetArgValue(
            2,
            3, 
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            2);
/*
 * Get array sizes.
 */
  nx    = dsizes_x[0];
  ny    = dsizes_y[0];
  nobs  = dsizes_x[1];
  minxy = min(nx,ny);
  maxxy = max(nx,ny);
  lrdim = 2 * (maxxy*maxxy);
  nxy   = nx + ny;
  nobsx = nobs * nx;
  nobsy = nobs * ny;
  size_coefx = nx * ny;
  size_coefy = ny * ny;
/*
 * Check array sizes.
 */
  if(dsizes_y[1] != nobs){
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cancor: The rightmost dimensions of x and y must be the same");
    return(NhlFATAL);
  }  

/*
 * Set the dimension sizes for the output arrays.
 */
  dsizes_canr[0]  = minxy;
  dsizes_coefx[0] = ny;
  dsizes_coefx[1] = nx;
  dsizes_coefy[0] = ny;
  dsizes_coefy[1] = ny;

/*
 * Allocate space for work arrays.
 */
  eval = (double*)calloc(minxy,sizeof(double));
  lrr  = ((nxy+1)*nxy)/2;
  rr   = (double*)calloc(lrr,sizeof(double));
  if(eval == NULL || rr == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cancor: Unable to allocate memory for miscellaneous arrays");
    return(NhlFATAL);
  }

/* 
 * Allocate space for output arrays.
 */
  if(type_x == NCL_double || type_y == NCL_double) {
    type_canr = NCL_double;
    canr  = (double *)calloc(minxy,sizeof(double));
    chisq = (double *)calloc(minxy,sizeof(double));
    wlam  = (double *)calloc(minxy,sizeof(double));
    coefx = (double *)calloc(size_coefx,sizeof(double));
    coefy = (double *)calloc(size_coefy,sizeof(double));
    if(canr == NULL || chisq == NULL || coefx == NULL || coefy == NULL ||
       wlam == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"cancor: Unable to allocate memory for output variables");
      return(NhlFATAL);
    }
  }
  else {
    type_canr = NCL_float;
    canr      = (float *)calloc(minxy,sizeof(float));
    chisq     = (float *)calloc(minxy,sizeof(float));
    wlam      = (float *)calloc(minxy,sizeof(float));
    coefx     = (float *)calloc(size_coefx,sizeof(float));
    coefy     = (float *)calloc(size_coefy,sizeof(float));
    tmp_canr  = (double *)calloc(minxy,sizeof(double));
    tmp_chisq = (double *)calloc(minxy,sizeof(double));
    tmp_wlam  = (double *)calloc(minxy,sizeof(double));
    if(canr  == NULL || tmp_canr  == NULL || 
       chisq == NULL || tmp_chisq == NULL ||
       wlam  == NULL || tmp_wlam  == NULL || 
       coefx == NULL || coefy     == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"cancor: Unable to allocate memory for output variables");
      return(NhlFATAL);
    }
  }
/*
 * Allocate space for tmp coefx and coefy no matter what, because the
 * input arrays will be a different size than what is eventually
 * returned.
 */
  tmp_coefx = (double *)calloc(lrdim,sizeof(double));
  tmp_coefy = (double *)calloc(lrdim,sizeof(double));
  ndf       =     (int*)calloc(minxy,sizeof(int));
  if(ndf == NULL || tmp_coefx == NULL || tmp_coefy == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cancor: Unable to allocate memory for attributes");
    return(NhlFATAL);
  }

/*
 * Coerce x and y to double.
 */
  tmp_x = coerce_input_double(x,type_x,nobsx,0,NULL,NULL);
  tmp_y = coerce_input_double(y,type_y,nobsy,0,NULL,NULL);

  if(type_canr == NCL_double) {
    tmp_canr  = &((double*)canr)[0];
    tmp_chisq = &((double*)chisq)[0];
    tmp_wlam  = &((double*)wlam)[0];
  }
  NGCALLF(dcancorxy,DCANCORXY)(&nobs,&nx,&ny,&nxy,&minxy,&maxxy,&lrdim,
                               tmp_x,tmp_y,ndf,tmp_canr,eval,tmp_wlam,
                               tmp_chisq,tmp_coefx,tmp_coefy,rr,&lrr,&ier);
/*
 * Coerce output to float if necessary.
 */
  if(type_canr == NCL_float) {
    coerce_output_float_only(canr,tmp_canr,minxy,0);
    coerce_output_float_only(chisq,tmp_chisq,minxy,0);
    coerce_output_float_only(wlam,tmp_wlam,minxy,0);
  }
  coerce_output_float_or_double(coefx,tmp_coefx,type_canr,size_coefx,0);
  coerce_output_float_or_double(coefy,tmp_coefy,type_canr,size_coefy,0);

/*
 * Free up memory.
 */
  if(type_x != NCL_double) NclFree(tmp_x);
  if(type_y != NCL_double) NclFree(tmp_y);
  if(type_canr != NCL_double) {
    NclFree(tmp_canr);
    NclFree(tmp_chisq);
    NclFree(tmp_wlam);
  }
  NclFree(tmp_coefx);
  NclFree(tmp_coefy);
  NclFree(eval);
  NclFree(rr);

/*
 * Get ready to return the data and some attributes.
 */
  if(type_canr == NCL_float) {
/*
 * Set up return structure.
 */
    return_md = _NclCreateVal(
                      NULL,
                      NULL,
                      Ncl_MultiDValData,
                      0,
                      canr,
                      NULL,
                      1,
                      dsizes_canr,
                      TEMPORARY,
                      NULL,
                      (NclObjClass)nclTypefloatClass
                      );
/*
 * Set up attributes to return.
 */
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   chisq,
                   NULL,
                   1,
                   dsizes_canr,
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypefloatClass
                   );

    _NclAddAtt(
               att_id,
               "chisq",
               att_md,
               NULL
               );

    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   wlam,
                   NULL,
                   1,
                   dsizes_canr,
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypefloatClass
                   );

    _NclAddAtt(
               att_id,
               "wlam",
               att_md,
               NULL
               );

    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   coefx,
                   NULL,
                   2,
                   dsizes_coefx,
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypefloatClass
                   );

    _NclAddAtt(
               att_id,
               "coefx",
               att_md,
               NULL
               );

    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   coefy,
                   NULL,
                   2,
                   dsizes_coefy,
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypefloatClass
                   );

    _NclAddAtt(
               att_id,
               "coefy",
               att_md,
               NULL
               );

  }
  else {
/*
 * Set up return structure.
 */
    return_md = _NclCreateVal(
                      NULL,
                      NULL,
                      Ncl_MultiDValData,
                      0,
                      canr,
                      NULL,
                      1,
                      dsizes_canr,
                      TEMPORARY,
                      NULL,
                      (NclObjClass)nclTypedoubleClass
                      );
/*
 * Set up attributes to return.
 */
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   chisq,
                   NULL,
                   1,
                   dsizes_canr,
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypedoubleClass
                   );
    _NclAddAtt(
               att_id,
               "chisq",
               att_md,
               NULL
               );

    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   wlam,
                   NULL,
                   1,
                   dsizes_canr,
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypedoubleClass
                   );
    _NclAddAtt(
               att_id,
               "wlam",
               att_md,
               NULL
               );

    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   coefx,
                   NULL,
                   2,
                   dsizes_coefx,
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypedoubleClass
                   );

    _NclAddAtt(
               att_id,
               "coefx",
               att_md,
               NULL
               );

    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   coefy,
                   NULL,
                   2,
                   dsizes_coefy,
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypedoubleClass
                   );

    _NclAddAtt(
               att_id,
               "coefy",
               att_md,
               NULL
               );

  }


  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         ndf,
                         NULL,
                         1,
                         dsizes_canr,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypeintClass
                         );
  _NclAddAtt(
             att_id,
             "ndof",
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

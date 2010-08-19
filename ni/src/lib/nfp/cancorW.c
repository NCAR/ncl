#include <stdio.h>
#include <math.h>
#include "wrapper.h"

extern void NGCALLF(dcancorxy,DCANCORXY)(int *, int *, int *, int *, int *, 
                                         int *, int *, double *,double *,int *,
                                         double *, double *, double *,
                                         double *, double *,double *,
                                         double *, double *, double *, int *,
					 int *);

NhlErrorTypes cancor_W( void )
{
/*
 * Input array variables
 */
  void *x, *y;
  double *tmp_x, *tmp_y;
  logical *opt;
  ng_size_t dsizes_y[2], dsizes_x[2];
  NclBasicDataTypes type_x, type_y;
/*
 * Attribute variables.
 */
  int *ndf, *tmp_ndf;
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
  ng_size_t dsizes_canr[1], dsizes_coefx[2], dsizes_coefy[2];
  NclBasicDataTypes type_canr;
/*
 * various
 */
  ng_size_t size_coefx, size_coefy;
  int nobs, nobsx, nobsy, nx, ny, nxy, minxy, maxxy, lrdim, lrr;
  double *eval, *rr, *yx, *rx;
  ng_size_t i;
  int ier;

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
           DONT_CARE);

  y = (void*)NclGetArgValue(
           1,
           3,
           NULL,
           dsizes_y,
           NULL,
           NULL,
           &type_y,
           DONT_CARE);

  opt = (logical *)NclGetArgValue(
            2,
            3, 
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            DONT_CARE);
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
  eval = (double*)calloc(nx,sizeof(double));
  lrr  = ((nxy+1)*nxy)/2;
  rr   = (double*)calloc(lrr,sizeof(double));
  yx   = (double*)calloc(nobs*nxy,sizeof(double));
  rx   = (double*)calloc(nxy*nxy,sizeof(double));
  if(eval == NULL || rr == NULL || yx == NULL || rx == NULL) {
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
  }
  else {
    type_canr = NCL_float;
    canr  = (float *)calloc(minxy,sizeof(float));
    chisq = (float *)calloc(minxy,sizeof(float));
    wlam  = (float *)calloc(minxy,sizeof(float));
    coefx = (float *)calloc(size_coefx,sizeof(float));
    coefy = (float *)calloc(size_coefy,sizeof(float));
  }
  if(canr  == NULL || chisq == NULL || wlam == NULL || 
     coefx == NULL || coefy == NULL) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"cancor: Unable to allocate memory for output variables");
	return(NhlFATAL);
  }
/*
 * Allocate space for the arrays below no matter what, because the
 * input arrays are a different size than what is eventually
 * returned.
 */
  tmp_canr  = (double *)calloc(nx,sizeof(double));
  tmp_chisq = (double *)calloc(nx,sizeof(double));
  tmp_wlam  = (double *)calloc(nx,sizeof(double));
  tmp_ndf   =     (int*)calloc(nx,sizeof(int));
  ndf       =     (int*)calloc(minxy,sizeof(int));
  tmp_coefx = (double *)calloc(lrdim,sizeof(double));
  tmp_coefy = (double *)calloc(lrdim,sizeof(double));
  if(tmp_canr == NULL || ndf == NULL || tmp_ndf == NULL || tmp_wlam == NULL ||
     tmp_chisq == NULL || tmp_coefx == NULL || tmp_coefy == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cancor: Unable to allocate memory for attributes");
    return(NhlFATAL);
  }

/*
 * Coerce input x and y to double.
 */
  tmp_x = coerce_input_double(x,type_x,nobsx,0,NULL,NULL);
  tmp_y = coerce_input_double(y,type_y,nobsy,0,NULL,NULL);

  NGCALLF(dcancorxy,DCANCORXY)(&nobs,&nx,&ny,&nxy,&minxy,&maxxy,&lrdim,
                               tmp_x,tmp_y,tmp_ndf,tmp_canr,eval,tmp_wlam,
                               tmp_chisq,tmp_coefx,tmp_coefy,rr,yx,rx,
			       &lrr,&ier);
/*
 * Copy temp output arrays to appropriate place in output arrays.
 */
  coerce_output_float_or_double(canr,  tmp_canr,  type_canr, minxy,0);
  coerce_output_float_or_double(chisq, tmp_chisq, type_canr, minxy,0);
  coerce_output_float_or_double(wlam,  tmp_wlam,  type_canr, minxy,0);
  coerce_output_float_or_double(coefx, tmp_coefx, type_canr, size_coefx,0);
  coerce_output_float_or_double(coefy, tmp_coefy, type_canr, size_coefy,0);
/*
 * Copy only minxy of the ndf values.
 */
  for(i = 0; i < minxy; i++ ) ndf[i] = tmp_ndf[i];

/*
 * Free up memory.
 */
  if(type_x != NCL_double)    NclFree(tmp_x);
  if(type_y != NCL_double)    NclFree(tmp_y);
  NclFree(tmp_canr);
  NclFree(tmp_chisq);
  NclFree(tmp_wlam);
  NclFree(tmp_coefx);
  NclFree(tmp_coefy);
  NclFree(tmp_ndf);
  NclFree(eval);
  NclFree(rr);
  NclFree(yx);
  NclFree(rx);

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

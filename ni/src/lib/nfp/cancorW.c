#include <stdio.h>
#include <math.h>
#include "wrapper.h"

extern void NGCALLF(dcancorxy,DCANCORXY)(int *, int *, int *, int *, int *, 
                                        int *, int *, double *,double *,int *,
                                        double *, double *, double *, double *,
                                        double *,double *,int *);

NhlErrorTypes cancor_W( void )
{
/*
 * Input array variables
 */
  void *x, *y;
  double *tmp_x, *tmp_y;
  logical *opt;
  int ndims_x, ndims_y;
  int dsizes_y[NCL_MAX_DIMENSIONS], dsizes_x[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_x, type_y;
/*
 * Attribute variables.
 */
  int *ndf;
  void *chisq, *coefx, *coefy;
  double *tmp_chisq, *tmp_coefx, *tmp_coefy;

  int att_id;
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;

/*
 * Output variable.
 */
  void *canr;
  double *tmp_canr;
  int *dsizes_canr, *dsizes_coefx, *dsizes_coefy, ndims_canr;
  NclBasicDataTypes type_canr;
/*
 * various
 */
  int size_x, size_y, size_coefx, size_coefy;
  int nobs, nobsx, nobsy, nx, ny, nxy, minxy, maxxy, lrdim;
  int index_x, index_y, index_canr, index_coefx, index_coefy, ier;
  double *eval, *wlam;
  int i, size_leftmost;

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
           NULL,
           NULL,
           &type_x,
           2);

  y = (void*)NclGetArgValue(
           1,
           3,
           &ndims_y,
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
 * The x and y must be at least 2-dimensional, and the rightmost dimension
 * must be nobs for both of them.
 */
  if(ndims_x != ndims_y) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cancor: x and y must have the same number of dimensions");
    return(NhlFATAL);
  }

/*
 * Get array sizes.
 */
  nx    = dsizes_x[ndims_x-2];
  ny    = dsizes_y[ndims_y-2];
  nobs  = dsizes_x[ndims_x-1];
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
  if(dsizes_y[ndims_y-1] != nobs){
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cancor: The lefmost dimensions of x and y must be the same");
    return(NhlFATAL);
  }  

/*
 * Set the dimension sizes for the output arrays, and also check leftmost
 * dimensions while we're at it.
 */
  ndims_canr   = ndims_x-1;
  dsizes_canr  = (int*)calloc(ndims_canr,sizeof(int));  
  dsizes_coefx = (int*)calloc(ndims_x,sizeof(int));  
  dsizes_coefy = (int*)calloc(ndims_y,sizeof(int));  
  if(dsizes_canr == NULL || dsizes_coefx == NULL || dsizes_coefy == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cancor: Unable to allocate memory for the dimension sizes for the output arrays");
    return(NhlFATAL);
  }
  size_leftmost = 1;
  for( i=0; i < ndims_x-2; i++ ) {
    if(dsizes_y[i] != dsizes_x[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"cancor: The leftmost dimensions of x and y must be the same");
      return(NhlFATAL);
    }  
    size_leftmost *= dsizes_x[i];
    dsizes_canr[i] = dsizes_coefx[i] = dsizes_coefy[i] = dsizes_x[i];
  }
  dsizes_canr[ndims_x-2]  = minxy;

  dsizes_coefx[ndims_x-2] = ny;
  dsizes_coefx[ndims_x-1] = nx;

  dsizes_coefy[ndims_y-2] = ny;
  dsizes_coefy[ndims_y-1] = ny;

/*
 * Create arrays to hold double precision subsections of x and y.
 */
  if(type_x != NCL_double) {
    tmp_x = (double *)calloc(nobsx,sizeof(double));
    if( tmp_x == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"cancor: Unable to allocate memory for coercing input arrays to double");
      return(NhlFATAL);
    }
  }
  if(type_y != NCL_double) {
    tmp_y = (double *)calloc(nobsy,sizeof(double));
    if( tmp_y == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"cancor: Unable to allocate memory for coercing input arrays to double");
      return(NhlFATAL);
    }
  }


/*
 * Allocate space for work arrays.
 */
  eval = (double*)calloc(minxy,sizeof(double));
  wlam = (double*)calloc(minxy,sizeof(double));
  if(eval == NULL || wlam == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cancor: Unable to allocate memory for miscellaneous arrays");
    return(NhlFATAL);
  }

/* 
 * Allocate space for output arrays.
 */
  if(type_x == NCL_double || type_y == NCL_double) {
    type_canr = NCL_double;
    canr  = (double *)calloc(size_leftmost*minxy,sizeof(double));
    chisq = (double *)calloc(size_leftmost*minxy,sizeof(double));
    coefx = (double *)calloc(size_leftmost*size_coefx,sizeof(double));
    coefy = (double *)calloc(size_leftmost*size_coefy,sizeof(double));
    if(canr  == NULL || chisq == NULL || coefx == NULL || coefy == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"cancor: Unable to allocate memory for output variables");
      return(NhlFATAL);
    }
  }
  else {
    type_canr = NCL_float;
    canr      = (float *)calloc(size_leftmost*minxy,sizeof(float));
    chisq     = (float *)calloc(size_leftmost*minxy,sizeof(float));
    coefx     = (float *)calloc(size_leftmost*ny*nx,sizeof(float));
    coefy     = (float *)calloc(size_leftmost*ny*ny,sizeof(float));
    tmp_canr  = (double *)calloc(minxy,sizeof(double));
    tmp_chisq = (double *)calloc(minxy,sizeof(double));
    if(canr  == NULL || tmp_canr  == NULL ||
       chisq == NULL || tmp_chisq == NULL ||
       coefx == NULL || coefy == NULL) {
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
  ndf = (int*)calloc(size_leftmost*minxy,sizeof(double));
  if(ndf == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cancor: Unable to allocate memory for 'ndf' attribute");
    return(NhlFATAL);
  }

/*
 * Loop through the leftmost dimensions and call the Fortran function
 * for each subsection of x and y.
 */
  index_x = index_y = index_canr = index_coefx = index_coefy = 0;
  for(i = 0; i < size_leftmost; i++) {
    if(type_x != NCL_double) {
/*
 * Coerce nobsx subsection of x (tmp_x) to double.
 */
      coerce_subset_input_double(x,tmp_x,index_x,type_x,nobsx,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_x to appropriate locations in x.
 */
      tmp_x = &((double*)x)[index_x];
    }

    if(type_y != NCL_double) {
/*
 * Coerce nobsy subsection of y (tmp_y) to double.
 */
      coerce_subset_input_double(y,tmp_y,index_y,type_y,nobsy,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_y to appropriate locations in y.
 */
      tmp_y = &((double*)y)[index_y];
    }

    if(type_canr == NCL_double) {
      tmp_canr  = &((double*)canr)[index_canr];
      tmp_chisq = &((double*)chisq)[index_canr];
    }

    NGCALLF(dcancorxy,DCANCORXY)(&nobs,&nx,&ny,&nxy,&minxy,&maxxy,&lrdim,
                                 tmp_x,tmp_y,&ndf[index_canr],tmp_canr,
                                 eval,wlam,tmp_chisq,tmp_coefx,tmp_coefy,&ier);
/*
 * Coerce output to float if necessary.
 */
    if(type_canr == NCL_float) {
      coerce_output_float_only(canr,tmp_canr,minxy,index_canr);
      coerce_output_float_only(chisq,tmp_chisq,minxy,index_canr);
    }
    coerce_output_float_or_double(coefx,tmp_coefx,type_canr,size_coefx,
                                  index_coefx);
    coerce_output_float_or_double(coefy,tmp_coefy,type_canr,size_coefy,
                                  index_coefy);

    index_x    += nobsx;
    index_y    += nobsy;
    index_canr += minxy;
    index_coefx += size_coefx;
    index_coefy += size_coefy;
  }
/*
 * Free up memory.
 */
  if(type_x != NCL_double) NclFree(tmp_x);
  if(type_y != NCL_double) NclFree(tmp_y);
  if(type_canr != NCL_double) {
    NclFree(tmp_canr);
    NclFree(tmp_chisq);
  }
  NclFree(tmp_coefx);
  NclFree(tmp_coefy);
  NclFree(eval);
  NclFree(wlam);

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
                      ndims_canr,
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
                   ndims_canr,
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
                   coefx,
                   NULL,
                   ndims_x,
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
                   ndims_x,
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
                      ndims_canr,
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
                   ndims_canr,
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
                   coefx,
                   NULL,
                   ndims_x,
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
                   ndims_x,
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
                         ndims_canr,
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

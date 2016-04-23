#include <stdio.h>
#include "wrapper.h"

extern double NGCALLF(drelhum,DRELHUM)(double*,double*,double*);
extern void NGCALLF(drelhumw,DRELHUMW)(double *, double *, double *, 
                                       double *);
extern void NGCALLF(drelhumi,DRELHUMI)(double *, double *, double *, 
                                       double *);

NhlErrorTypes relhum_W( void )
{
/*
 * Input variables
 */
  void *t, *w, *p;
  double *tmp_t = NULL;
  double *tmp_w = NULL;
  double *tmp_p = NULL;
  int ndims_t;
  ng_size_t dsizes_t[NCL_MAX_DIMENSIONS];
  int has_missing_t;
  int ndims_w;
  ng_size_t dsizes_w[NCL_MAX_DIMENSIONS];
  int has_missing_w;
  int ndims_p;
  ng_size_t dsizes_p[NCL_MAX_DIMENSIONS];
  int has_missing_p;
  NclScalar missing_t, missing_dt;
  NclScalar missing_w, missing_dw;
  NclScalar missing_p, missing_dp;
  NclBasicDataTypes type_t, type_w, type_p;
/*
 * Output variable
 */
  void *rh;
  double *tmp_rh = NULL;
  NclBasicDataTypes type_rh;
  NclScalar missing_rh, missing_drh;
/*
 * Various.
 */
  int found_missing_t, found_missing_w, found_missing_p;
  ng_size_t i, total_size_rh;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 *
 * Retrieve argument #1
 */
  t = (void*)NclGetArgValue(
          0,
          3,
          &ndims_t, 
          dsizes_t,
          &missing_t,
          &has_missing_t,
          &type_t,
          DONT_CARE);
/*
 * Retrieve argument #2
 */
  w = (void*)NclGetArgValue(
          1,
          3,
          &ndims_w, 
          dsizes_w,
          &missing_w,
          &has_missing_w,
          &type_w,
          DONT_CARE);
/*
 * Retrieve argument #3
 */
  p = (void*)NclGetArgValue(
          2,
          3,
          &ndims_p, 
          dsizes_p,
          &missing_p,
          &has_missing_p,
          &type_p,
          DONT_CARE);
/*
 * Check dimensions and calculate total size of arrays.
 */
  if( ndims_t != ndims_w || ndims_t != ndims_p ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"relhum: The input arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
/*
 * Determine type of output.
 */
  if(type_t != NCL_double && type_w != NCL_double && 
     type_p != NCL_double) {
    type_rh = NCL_float;
  }
  else {
    type_rh = NCL_double;
  }
/*
 * Prior to V6.2.0, this function required all input arrays to have missing
 * values, but it only used t's missing value as the return missing value.
 * This requirement has been dropped in V6.2.0, and the default missing
 * value for the type being returned is now the missing value.
 */
  if(type_rh == NCL_double) {
    missing_rh.doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
    missing_drh.doubleval = missing_rh.doubleval;
  }
  else {
    missing_rh.floatval = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
    missing_drh.doubleval = (double)missing_rh.floatval;
  }

/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_t,has_missing_t,&missing_t,&missing_dt,NULL);
  coerce_missing(type_w,has_missing_w,&missing_w,&missing_dw,NULL);
  coerce_missing(type_p,has_missing_p,&missing_p,&missing_dp,NULL);
/*
 * Calculate total size of arrays.
 */
  total_size_rh = 1;
  for( i = 0; i < ndims_t; i++ ) {
    if( dsizes_t[i] != dsizes_p[i] || dsizes_t[i] != dsizes_w[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"relhum: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
    total_size_rh *= dsizes_t[i];
  }
/*
 * Coerce data to double if necessary.
 */
  if(type_t != NCL_double) {
    tmp_t = (double*)calloc(1,sizeof(double));
    if( tmp_t == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"relhum: Unable to allocate memory for coercing t array to double precision");
      return(NhlFATAL);
    }
  }
/*
 * Coerce w.
 */
  if(type_w != NCL_double) {
    tmp_w = (double*)calloc(1,sizeof(double));
    if( tmp_w == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"relhum: Unable to allocate memory for coercing w array to double precision");
      return(NhlFATAL);
    }
  }
/*
 * Coerce p.
 */
  if(type_p != NCL_double) {
    tmp_p = (double*)calloc(1,sizeof(double));
    if( tmp_p == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"relhum: Unable to allocate memory for coercing p array to double precision");
      return(NhlFATAL);
    }
  }
/*
 * Allocate space for output array.
 */
  if(type_rh == NCL_float) {
    rh     = (void*)calloc(total_size_rh,sizeof(float));
    tmp_rh = (void*)calloc(1,sizeof(double));
    if(tmp_rh == NULL || rh == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"relhum: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    rh = (void*)calloc(total_size_rh,sizeof(double));
    if(rh == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"relhum: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }

/*
 * Call function.
 */
  for( i = 0; i < total_size_rh; i++ ) {
    if(type_t != NCL_double) {
/*
 * Coerce subsection of t (tmp_t) to double.
 */
      coerce_subset_input_double(t,tmp_t,i,type_t,1,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_t to appropriate location in t.
 */
      tmp_t = &((double*)t)[i];
    }

    if(type_w != NCL_double) {
/*
 * Coerce subsection of w (tmp_w) to double.
 */
      coerce_subset_input_double(w,tmp_w,i,type_w,1,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_w to appropriate location in w.
 */
      tmp_w = &((double*)w)[i];
    }

    if(type_p != NCL_double) {
/*
 * Coerce subsection of p (tmp_p) to double.
 */
      coerce_subset_input_double(p,tmp_p,i,type_p,1,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_p to appropriate location in p.
 */
      tmp_p = &((double*)p)[i];
    }

    if(type_rh == NCL_double) tmp_rh = &((double*)rh)[i];

/*
 * Check for a missing value.
 */
    found_missing_t = contains_missing(tmp_t,1,has_missing_t,
                                       missing_dt.doubleval);
    found_missing_w = contains_missing(tmp_w,1,has_missing_w,
                                       missing_dw.doubleval);
    found_missing_p = contains_missing(tmp_p,1,has_missing_p,
                                       missing_dp.doubleval);
    if(found_missing_t || found_missing_w || found_missing_p) {
      *tmp_rh = missing_drh.doubleval;
    }
    else {
      *tmp_rh = NGCALLF(drelhum,DRELHUM)(tmp_t,tmp_w,tmp_p);
    }
/*
 * Copy output values from temporary tmp_zh to zh.
 */
    if(type_rh != NCL_double) ((float*)rh)[i] = (float)(*tmp_rh);
  }
/*
 * Free memory.
 */
  if(type_t  != NCL_double) NclFree(tmp_t);
  if(type_w  != NCL_double) NclFree(tmp_w);
  if(type_p  != NCL_double) NclFree(tmp_p);
  if(type_rh != NCL_double) NclFree(tmp_rh);

/*
 * Return.
 */
  if(has_missing_t || has_missing_w || has_missing_p) {
    return(NclReturnValue(rh,ndims_t,dsizes_t,&missing_rh,type_rh,0));
  }
  else {
    return(NclReturnValue(rh,ndims_t,dsizes_t,NULL,type_rh,0));
  }
}


NhlErrorTypes relhum_water_W( void )
{
/*
 * Input variables
 *
 * Argument # 0
 */
  void *pres;
  double *tmp_pres = NULL;
  int       ndims_pres;
  ng_size_t dsizes_pres[NCL_MAX_DIMENSIONS];
  int has_missing_pres;
  NclScalar missing_pres, missing_dpres;
  NclBasicDataTypes type_pres;

/*
 * Argument # 1
 */
  void *tk;
  double *tmp_tk = NULL;
  int       ndims_tk;
  ng_size_t dsizes_tk[NCL_MAX_DIMENSIONS];
  int has_missing_tk;
  NclScalar missing_tk, missing_dtk;
  NclBasicDataTypes type_tk;

/*
 * Argument # 2
 */
  void *qw;
  double *tmp_qw = NULL;
  int       ndims_qw;
  ng_size_t dsizes_qw[NCL_MAX_DIMENSIONS];
  int has_missing_qw;
  NclScalar missing_qw, missing_dqw;
  NclBasicDataTypes type_qw;

/*
 * Return variable
 */
  void *rh;
  double *tmp_rh = NULL;
  NclScalar missing_rh, missing_drh;
  NclBasicDataTypes type_rh;

/*
 * Various
 */
  int ret;
  ng_size_t i, size_output;
  int found_missing_pres, found_missing_tk, found_missing_qw;

/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
/*
 * Get argument # 0
 */
  tk = (void*)NclGetArgValue(
           0,
           3,
           &ndims_tk,
           dsizes_tk,
           &missing_tk,
           &has_missing_tk,
           &type_tk,
           DONT_CARE);

/*
 * Get argument # 1
 */
  qw = (void*)NclGetArgValue(
           1,
           3,
           &ndims_qw,
           dsizes_qw,
           &missing_qw,
           &has_missing_qw,
           &type_qw,
           DONT_CARE);

/*
 * Get argument # 2
 */
  pres = (void*)NclGetArgValue(
           2,
           3,
           &ndims_pres,
           dsizes_pres,
           &missing_pres,
           &has_missing_pres,
           &type_pres,
           DONT_CARE);

/*
 * Check dimension sizes.
 */
  if(ndims_pres != ndims_tk || ndims_pres != ndims_qw) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"relhum_water: The pres, tk, and qw arrays must have the same dimensionality");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_tk; i++) {
    if(dsizes_pres[i] != dsizes_tk[i] || 
       dsizes_pres[i] != dsizes_qw[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"relhum_water: The pres, tk, and qw arrays must have the same dimensionality");
      return(NhlFATAL);
    }
  }

/*
 * Calculate size of arrays.
 */
  size_output = 1;
  for(i = 0; i < ndims_tk; i++) size_output *= dsizes_pres[i];

/* 
 * Allocate space for coercing input arrays.  If the input arrays
 * are already double, then we don't need to allocate space for
 * temporary arrays, because we'll just change the pointer into
 * the void array appropriately.
 *
 * The output type defaults to float, unless any of the input
 * arrays are double.
 */
  type_rh = NCL_float;
  if(type_pres != NCL_double) {
    tmp_pres = (double *)calloc(1,sizeof(double));
    if(tmp_pres == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"relhum_water Unable to allocate memory for coercing 'pres' to double");
      return(NhlFATAL);
    }
  }
  else {
    type_rh = NCL_double;
  }
  if(type_tk != NCL_double) {
    tmp_tk = (double *)calloc(1,sizeof(double));
    if(tmp_tk == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"relhum_water Unable to allocate memory for coercing 'tk' to double");
      return(NhlFATAL);
    }
  }
  else {
    type_rh = NCL_double;
  }
  if(type_qw != NCL_double) {
    tmp_qw = (double *)calloc(1,sizeof(double));
    if(tmp_qw == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"relhum_water Unable to allocate memory for coercing 'qw' to double");
      return(NhlFATAL);
    }
  }
  else {
    type_rh = NCL_double;
  }


/*
 * Coerce missing values to double if necessary.
 */
  coerce_missing(type_pres,has_missing_pres,&missing_pres,
                 &missing_dpres,NULL);

  coerce_missing(type_tk,has_missing_tk,&missing_tk,
                 &missing_dtk,NULL);

  coerce_missing(type_qw,has_missing_qw,&missing_qw,
                 &missing_dqw,NULL);

  if(type_rh == NCL_double) {
    missing_rh.doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
    missing_drh.doubleval = missing_rh.doubleval;
  }
  else {
    missing_rh.floatval = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
    missing_drh.doubleval = (double)missing_rh.floatval;
  }

/* 
 * Allocate space for output array.
 */
  if(type_rh != NCL_double) {
    rh     = (void *)calloc(size_output, sizeof(float));
    tmp_rh = (double *)calloc(1,sizeof(double));
    if(tmp_rh == NULL || rh == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"relhum_water: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    
  }
  else {
    rh = (void *)calloc(size_output, sizeof(double));
    if(rh == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"relhum_water: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }

/*
 * Loop across dimensions and call the Fortran routine for each value.
 */
  for(i = 0; i < size_output; i++) {
/*
 * Coerce subsection of pres (tmp_pres) to double if necessary.
 */
    if(type_pres != NCL_double) {
      coerce_subset_input_double(pres,&tmp_pres[0],i,
                                 type_pres,1,0,NULL,NULL);
    }
    else {
      tmp_pres = &((double*)pres)[i];
    }

/*
 * Coerce subsection of tk (tmp_tk) to double if necessary.
 */
    if(type_tk != NCL_double) {
      coerce_subset_input_double(tk,&tmp_tk[0],i,
                                 type_tk,1,0,NULL,NULL);
    }
    else {
      tmp_tk = &((double*)tk)[i];
    }

/*
 * Coerce subsection of qw (tmp_qw) to double if necessary.
 */
    if(type_qw != NCL_double) {
      coerce_subset_input_double(qw,&tmp_qw[0],i,
                                 type_qw,1,0,NULL,NULL);
    }
    else {
      tmp_qw = &((double*)qw)[i];
    }

/*
 * Point temporary output array to void output array if appropriate.
 */
    if(type_rh == NCL_double) tmp_rh = &((double*)rh)[i];

/*
 * Check for a missing value.
 */
    found_missing_pres = contains_missing(tmp_pres,1,has_missing_pres,
                                          missing_dpres.doubleval);
    found_missing_tk = contains_missing(tmp_tk,1,has_missing_tk,
                                       missing_tk.doubleval);
    found_missing_qw = contains_missing(tmp_qw,1,has_missing_qw,
                                       missing_qw.doubleval);
    if(found_missing_pres || found_missing_tk || found_missing_qw) {
      *tmp_rh = missing_drh.doubleval;
    }
    else {
      NGCALLF(drelhumw,DRELHUMW)(tmp_pres, tmp_tk, tmp_qw, tmp_rh);
    }
/*
 * Coerce output back to float if necessary.
 */
    if(type_rh == NCL_float) {
      coerce_output_float_only(rh,tmp_rh,1,i);
    }
  }

/*
 * Free unneeded memory.
 */
  if(type_pres != NCL_double) NclFree(tmp_pres);
  if(type_tk   != NCL_double) NclFree(tmp_tk);
  if(type_qw   != NCL_double) NclFree(tmp_qw);
  if(type_rh   != NCL_double) NclFree(tmp_rh);

/*
 * Return value back to NCL script.  Will return missing value no matter
 * what, just to stay consistent with relhum_ice which does the same.
 */
  ret = NclReturnValue(rh,ndims_pres,dsizes_pres,&missing_rh,
                       type_rh,0);
  return(ret);
}


NhlErrorTypes relhum_ice_W( void )
{
/*
 * Input variables
 *
 * Argument # 0
 */
  void *pres;
  double *tmp_pres = NULL;
  int       ndims_pres;
  ng_size_t dsizes_pres[NCL_MAX_DIMENSIONS];
  int has_missing_pres;
  NclScalar missing_pres, missing_dpres;
  NclBasicDataTypes type_pres;

/*
 * Argument # 1
 */
  void *tk;
  double *tmp_tk = NULL;
  int       ndims_tk;
  ng_size_t dsizes_tk[NCL_MAX_DIMENSIONS];
  int has_missing_tk;
  NclScalar missing_tk, missing_dtk;
  NclBasicDataTypes type_tk;

/*
 * Argument # 2
 */
  void *qw;
  double *tmp_qw = NULL;
  int       ndims_qw;
  ng_size_t dsizes_qw[NCL_MAX_DIMENSIONS];
  int has_missing_qw;
  NclScalar missing_qw, missing_dqw;
  NclBasicDataTypes type_qw;

/*
 * Return variable
 */
  void *rh;
  double *tmp_rh = NULL;
  NclScalar missing_rh, missing_drh;
  NclBasicDataTypes type_rh;

/*
 * Various
 */
  int ret;
  ng_size_t i, size_output;
  int found_missing_pres, found_missing_tk, found_missing_qw;

/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
/*
 * Get argument # 0
 */
  tk = (void*)NclGetArgValue(
           0,
           3,
           &ndims_tk,
           dsizes_tk,
           &missing_tk,
           &has_missing_tk,
           &type_tk,
           DONT_CARE);

/*
 * Get argument # 1
 */
  qw = (void*)NclGetArgValue(
           1,
           3,
           &ndims_qw,
           dsizes_qw,
           &missing_qw,
           &has_missing_qw,
           &type_qw,
           DONT_CARE);

/*
 * Get argument # 2
 */
  pres = (void*)NclGetArgValue(
           2,
           3,
           &ndims_pres,
           dsizes_pres,
           &missing_pres,
           &has_missing_pres,
           &type_pres,
           DONT_CARE);

/*
 * Check dimension sizes.
 */
  if(ndims_pres != ndims_tk || ndims_pres != ndims_qw) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"relhum_ice: The pres, tk, and qw arrays must have the same dimensionality");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_tk; i++) {
    if(dsizes_pres[i] != dsizes_tk[i] || 
       dsizes_pres[i] != dsizes_qw[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"relhum_ice: The pres, tk, and qw arrays must have the same dimensionality");
      return(NhlFATAL);
    }
  }

/*
 * Calculate size of arrays.
 */
  size_output = 1;
  for(i = 0; i < ndims_tk; i++) size_output *= dsizes_pres[i];

/* 
 * Allocate space for coercing input arrays.  If the input arrays
 * are already double, then we don't need to allocate space for
 * temporary arrays, because we'll just change the pointer into
 * the void array appropriately.
 *
 * The output type defaults to float, unless any of the input
 * arrays are double.
 */
  type_rh = NCL_float;
  if(type_pres != NCL_double) {
    tmp_pres = (double *)calloc(1,sizeof(double));
    if(tmp_pres == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"relhum_ice Unable to allocate memory for coercing 'pres' to double");
      return(NhlFATAL);
    }
  }
  else {
    type_rh = NCL_double;
  }
  if(type_tk != NCL_double) {
    tmp_tk = (double *)calloc(1,sizeof(double));
    if(tmp_tk == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"relhum_ice Unable to allocate memory for coercing 'tk' to double");
      return(NhlFATAL);
    }
  }
  else {
    type_rh = NCL_double;
  }
  if(type_qw != NCL_double) {
    tmp_qw = (double *)calloc(1,sizeof(double));
    if(tmp_qw == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"relhum_ice Unable to allocate memory for coercing 'qw' to double");
      return(NhlFATAL);
    }
  }
  else {
    type_rh = NCL_double;
  }

/*
 * Coerce missing values to double if necessary.
 */
  coerce_missing(type_pres,has_missing_pres,&missing_pres,
                 &missing_dpres,NULL);

  coerce_missing(type_tk,has_missing_tk,&missing_tk,
                 &missing_dtk,NULL);

  coerce_missing(type_qw,has_missing_qw,&missing_qw,
                 &missing_dqw,NULL);

  if(type_rh == NCL_double) {
    missing_rh.doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
    missing_drh.doubleval = missing_rh.doubleval;
  }
  else {
    missing_rh.floatval = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
    missing_drh.doubleval = (double)missing_rh.floatval;
  }

/* 
 * Allocate space for output array.
 */
  if(type_rh != NCL_double) {
    rh     = (void *)calloc(size_output, sizeof(float));
    tmp_rh = (double *)calloc(1,sizeof(double));
    if(tmp_rh == NULL || rh == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"relhum_ice: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    
  }
  else {
    rh = (void *)calloc(size_output, sizeof(double));
    if(rh == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"relhum_ice: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }

/*
 * Loop across dimensions and call the Fortran routine for each value.
 */
  for(i = 0; i < size_output; i++) {
/*
 * Coerce subsection of pres (tmp_pres) to double if necessary.
 */
    if(type_pres != NCL_double) {
      coerce_subset_input_double(pres,&tmp_pres[0],i,
                                 type_pres,1,0,NULL,NULL);
    }
    else {
      tmp_pres = &((double*)pres)[i];
    }

/*
 * Coerce subsection of tk (tmp_tk) to double if necessary.
 */
    if(type_tk != NCL_double) {
      coerce_subset_input_double(tk,&tmp_tk[0],i,
                                 type_tk,1,0,NULL,NULL);
    }
    else {
      tmp_tk = &((double*)tk)[i];
    }

/*
 * Coerce subsection of qw (tmp_qw) to double if necessary.
 */
    if(type_qw != NCL_double) {
      coerce_subset_input_double(qw,&tmp_qw[0],i,
                                 type_qw,1,0,NULL,NULL);
    }
    else {
      tmp_qw = &((double*)qw)[i];
    }

/*
 * Point temporary output array to void output array if appropriate.
 */
    if(type_rh == NCL_double) tmp_rh = &((double*)rh)[i];

/*
 * Check for a missing value.
 */
    found_missing_pres = contains_missing(tmp_pres,1,has_missing_pres,
                                          missing_dpres.doubleval);
    found_missing_tk = contains_missing(tmp_tk,1,has_missing_tk,
                                       missing_tk.doubleval);
    found_missing_qw = contains_missing(tmp_qw,1,has_missing_qw,
                                       missing_qw.doubleval);
    if(found_missing_pres || found_missing_tk || found_missing_qw || *tmp_tk > 273.15 ) {
      *tmp_rh = missing_drh.doubleval;
    }
    else {
      NGCALLF(drelhumi,DRELHUMI)(tmp_pres, tmp_tk, tmp_qw, tmp_rh);
    }

/*
 * Coerce output back to float if necessary.
 */
    if(type_rh == NCL_float) {
      coerce_output_float_only(rh,tmp_rh,1,i);
    }
  }

/*
 * Free unneeded memory.
 */
  if(type_pres != NCL_double) NclFree(tmp_pres);
  if(type_tk   != NCL_double) NclFree(tmp_tk);
  if(type_qw   != NCL_double) NclFree(tmp_qw);
  if(type_rh   != NCL_double) NclFree(tmp_rh);

/*
 * Return value back to NCL script. Will attach a 
 * missing value no matter what, becasuse missing
 * values might be returned based on input values
 * (i.e. temp > 0.0 degC).
 */
  ret = NclReturnValue(rh,ndims_pres,dsizes_pres,&missing_rh,
                       type_rh,0);
  return(ret);
}


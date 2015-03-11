#include <stdio.h>
#include <math.h>
#include "wrapper.h"

extern void NGCALLF(dezffti,DEZFFTI)(int*,double*);
extern void NGCALLF(dezfftf,DEZFFTF)(int*,double*,double*,double*,double*,
                                     double*);
extern void NGCALLF(dezfftb,DEZFFTB)(int*,double*,double*,double*,double*,
                                     double*);

NhlErrorTypes ezfftf_W( void )
{
/*
 * Input array variables
 */
  void *x;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_x;
  NclScalar missing_x, missing_dx, missing_rx, missing_cf;
  int has_missing_x;
  double *tmp_x = NULL;
/*
 * Output array variables
 */
  void *cf, *xbar;
  int ndims_cf;
  ng_size_t dsizes_cf[NCL_MAX_DIMENSIONS];
  double *tmp_cf1, *tmp_cf2, *tmp_xbar;
  NclBasicDataTypes type_cf;
  NclTypeClass type_cf_class;
/*
 * Attribute variables
 */
  void *N;
  int att_id;
  ng_size_t dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
/*
 * various
 */
  double *work;
  ng_size_t index_x, index_cf1, index_cf2;
  ng_size_t i, npts, npts2, lnpts2, npts22;
  int found_missing, any_missing;
  ng_size_t size_leftmost, size_cf;
  int inpts;

/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  x = (void*)NclGetArgValue(
           0,
           1,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);
/*
 * Calculate number of leftmost elements.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_x-1; i++ ) size_leftmost *= dsizes_x[i];
/*
 * Test input array size
 */
  npts = dsizes_x[ndims_x-1];

  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftf: npts = %d is greater than INT_MAX", npts);
    return(NhlFATAL);
  }
  inpts = (int) npts;

/*
 * Calculate size of output array.
 */
  if((npts % 2) == 0) {
    npts2 = npts/2;
  }
  else {
    npts2 = (npts-1)/2;
  }
  lnpts2 = npts2 * size_leftmost;
  npts22 = 2*npts2;
  size_cf = size_leftmost * npts22;

  ndims_cf           = ndims_x + 1;
  dsizes_cf[0]       = 2;
  for(i = 1; i < ndims_x; i++ ) dsizes_cf[i] = dsizes_x[i-1];
  dsizes_cf[ndims_x] = npts2;
/*
 * Coerce missing values.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Create space for temporary input array if necessary.
 */
  if(type_x != NCL_double) {
    tmp_x = (double*)calloc(npts,sizeof(double));
    if(tmp_x == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftf: Unable to allocate memory for coercing input array to double precision");
      return(NhlFATAL);
    }
  }
/*
 * Allocate space for output arrays.
 */
  tmp_xbar = (double*)calloc(1,sizeof(double));
  tmp_cf1  = (double*)calloc(npts2,sizeof(double));
  tmp_cf2  = (double*)calloc(npts2,sizeof(double));
  if ( tmp_cf1 == NULL || tmp_cf2 == NULL || tmp_xbar == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftf: Cannot allocate memory for temporary output arrays" );
    return(NhlFATAL);
  }
  if(type_x == NCL_double) {
    cf   = (void*)calloc(size_cf,sizeof(double));
    xbar = (void*)calloc(size_leftmost,sizeof(double));
    type_cf = NCL_double;
    if(has_missing_x) missing_cf = missing_dx;
  }
  else {
    cf   = (void*)calloc(size_cf,sizeof(float));
    xbar = (void*)calloc(size_leftmost,sizeof(float));
    type_cf = NCL_float;
    if(has_missing_x) missing_cf = missing_rx;
  }
  N = (void*)calloc(1,sizeof(int));
  if ( cf == NULL || xbar == NULL || N == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftf: Cannot allocate memory for output arrays" );
    return(NhlFATAL);
  }

/*
 * Allocate memory for work array
 */
  work = (double*)calloc((4*npts+15),sizeof(double));
  if ( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftf: Cannot allocate memory for work array" );
    return(NhlFATAL);
  }
/*
 * Call the f77 version of 'dezfftf' with the full argument list.
 */
  index_x   = 0;
  index_cf1 = 0;
  index_cf2 = lnpts2;
  any_missing = 0;
  for(i = 0; i < size_leftmost; i++) {
    if(type_x != NCL_double) { 
      coerce_subset_input_double(x,tmp_x,index_x,type_x,npts,0,NULL,NULL);
    }
    else {
      tmp_x = &((double*)x)[index_x];
    }
/*
 * Check for missing values in x.  If any, then coerce that section of
 * the output to missing.
 */
    found_missing = contains_missing(tmp_x,npts,has_missing_x,
                                     missing_dx.doubleval);
    if(found_missing) {
      any_missing++;
      set_subset_output_missing(xbar,i,type_cf,1,missing_dx.doubleval);
      set_subset_output_missing(cf,index_cf1,type_cf,npts2,
                                missing_dx.doubleval);
      set_subset_output_missing(cf,index_cf2,type_cf,npts2,
                                missing_dx.doubleval);
    }
    else {
      NGCALLF(dezffti,DEZFFTI)(&inpts,work);
      NGCALLF(dezfftf,DEZFFTF)(&inpts,tmp_x,tmp_xbar,tmp_cf1,tmp_cf2,work);
/*
 * Copy results back into xbar and cf.
 */
      coerce_output_float_or_double(xbar,tmp_xbar,type_cf,1,i);
      coerce_output_float_or_double(cf,tmp_cf1,type_cf,npts2,index_cf1);
      coerce_output_float_or_double(cf,tmp_cf2,type_cf,npts2,index_cf2);
    }
    index_x   += npts;
    index_cf1 += npts2;
    index_cf2 += npts2;
  }

/*
 * Free up memory.
 */
  if(type_x != NCL_double) free(tmp_x);
  free(work);
  free(tmp_cf1);
  free(tmp_cf2);
  free(tmp_xbar);
/*
 * Set up variable to return.
 */
  type_cf_class = (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(type_cf)));

/*
 * Set up return values.
 */
  if(any_missing) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"ezfftf: %d input arrays contained missing values. No calculations performed on these arrays.",any_missing);

    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              cf,
                              &missing_cf,
                              ndims_cf,
                              dsizes_cf,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)type_cf_class
                              );
  }
  else {
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              cf,
                              NULL,
                              ndims_cf,
                              dsizes_cf,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)type_cf_class
                              );
  }
/*
 * Attributes "xbar" and "npts".
 */
  att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

  dsizes[0] = size_leftmost;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         xbar,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)type_cf_class
                         );
  _NclAddAtt(
             att_id,
             "xbar",
             att_md,
             NULL
             );

  (*(int*)N) = npts;
  dsizes[0] = 1;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         N,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypeintClass
                         );
  _NclAddAtt(
             att_id,
             "npts",
             att_md,
             NULL
             );

/*
 * Set up variable to hold return array and attributes.
 */
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


NhlErrorTypes ezfftf_n_W( void )
{
/*
 * Input array variables
 */
  void *x;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_x;
  NclScalar missing_x, missing_dx, missing_rx, missing_cf;
  int has_missing_x;
  double *tmp_x = NULL;
/*
 * Argument # 1
 */
  int *dim;
/*
 * Output array variables
 */
  void *cf, *xbar;
  int ndims_cf;
  ng_size_t dsizes_cf[NCL_MAX_DIMENSIONS];
  double *tmp_cf1, *tmp_cf2, *tmp_xbar;
  NclBasicDataTypes type_cf;
  NclTypeClass type_cf_class;
/*
 * Attribute variables
 */
  void *N;
  int att_id;
  ng_size_t dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;

/*
 * various
 */
  double *work;
  ng_size_t index_x, index_cf1, index_cf2, index_xbar;
  ng_size_t i, j, npts, npts2, lnpts2, npts22;
  ng_size_t index_nrnpts, index_nrnpts2;
  int found_missing, any_missing;
  ng_size_t total_nl, total_nr, total_nlr, size_cf;
  int inpts;

/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
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

/*
 * Get argument # 2
 */
  dim = (int *)NclGetArgValue(1,2,NULL,NULL,NULL,NULL,NULL,0);

/*
 * Make sure "dim" is a valid dimension.
 */
  if (*dim < 0 || *dim >= ndims_x) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftf_n: Invalid dimension index");
    return(NhlFATAL);
  }

  npts = dsizes_x[*dim];
  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftf_n: npts = %d is greater than INT_MAX", npts);
    return(NhlFATAL);
  }
  inpts = (int) npts;

/*
 * Calculate number of leftmost and rightmost elements.
 */
  total_nl = total_nr = 1;
  for( i =      0; i < *dim;    i++ ) total_nl *= dsizes_x[i];
  for( i = *dim+1; i < ndims_x; i++ ) total_nr *= dsizes_x[i];
  total_nlr = total_nl * total_nr;

/*
 * Calculate size of output array.
 */
  if((npts % 2) == 0) {
    npts2 = npts/2;
  }
  else {
    npts2 = (npts-1)/2;
  }
  lnpts2 = npts2 * total_nlr;
  npts22 = 2*npts2;
  size_cf = total_nlr * npts22;

  ndims_cf          = ndims_x + 1;
  dsizes_cf[0]      = 2;
  dsizes_cf[*dim+1] = npts2;

  for( i =      0; i < *dim;    i++ ) dsizes_cf[i+1] = dsizes_x[i];
  for( i = *dim+1; i < ndims_x; i++ ) dsizes_cf[i+1] = dsizes_x[i];

/*
 * Coerce missing values.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Create space for temporary input array.
 */
  tmp_x = (double*)calloc(npts,sizeof(double));
  if(tmp_x == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftf_n: Unable to allocate memory for coercing input array to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate space for output arrays.
 */
  tmp_xbar = (double*)calloc(1,sizeof(double));
  tmp_cf1  = (double*)calloc(npts2,sizeof(double));
  tmp_cf2  = (double*)calloc(npts2,sizeof(double));
  if ( tmp_cf1 == NULL || tmp_cf2 == NULL || tmp_xbar == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftf_n: Cannot allocate memory for temporary output arrays" );
    return(NhlFATAL);
  }
  if(type_x == NCL_double) {
    cf   = (void*)calloc(size_cf,sizeof(double));
    xbar = (void*)calloc(total_nlr,sizeof(double));
    type_cf = NCL_double;
    if(has_missing_x) missing_cf = missing_dx;
  }
  else {
    cf   = (void*)calloc(size_cf,sizeof(float));
    xbar = (void*)calloc(total_nlr,sizeof(float));
    type_cf = NCL_float;
    if(has_missing_x) missing_cf = missing_rx;
  }
  N = (void*)calloc(1,sizeof(int));
  if ( cf == NULL || xbar == NULL || N == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftf_n: Cannot allocate memory for output arrays" );
    return(NhlFATAL);
  }

/*
 * Allocate memory for work array
 */
  work = (double*)calloc((4*npts+15),sizeof(double));
  if ( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftf_n: Cannot allocate memory for work array" );
    return(NhlFATAL);
  }
/*
 * Call the f77 version of 'dezfftf' with the full argument list.
 */
  any_missing = 0;

  index_xbar = 0;
  for( i = 0; i < total_nl; i++ ) {
    index_nrnpts  = i*total_nr * npts;
    index_nrnpts2 = i*total_nr * npts2;
    for( j = 0; j < total_nr; j++ ) {
      index_x   = index_nrnpts + j;
      index_cf1 = index_nrnpts2 + j;
      index_cf2 = lnpts2 + index_nrnpts2 + j;

      coerce_subset_input_double_step(x,tmp_x,index_x,total_nr,
                                      type_x,npts,0,NULL,NULL);
/*
 * Check for missing values in x.  If any, then coerce that section of
 * the output to missing.
 */
      found_missing = contains_missing(tmp_x,npts,has_missing_x,
                                       missing_dx.doubleval);
      if(found_missing) {
        any_missing++;
        set_subset_output_missing(xbar,index_xbar,type_cf,1,missing_dx.doubleval);
        set_subset_output_missing_step(cf,index_cf1,total_nr,type_cf,npts2,
                                       missing_dx.doubleval);
        set_subset_output_missing_step(cf,index_cf2,total_nr,type_cf,npts2,
                                       missing_dx.doubleval);
      }
      else {
        NGCALLF(dezffti,DEZFFTI)(&inpts,work);
        NGCALLF(dezfftf,DEZFFTF)(&inpts,tmp_x,tmp_xbar,tmp_cf1,tmp_cf2,work);
/*
 * Copy results back into xbar and cf.
 */
        coerce_output_float_or_double(xbar,tmp_xbar,type_cf,1,index_xbar);
        coerce_output_float_or_double_step(cf,tmp_cf1,type_cf,npts2,index_cf1,total_nr);
        coerce_output_float_or_double_step(cf,tmp_cf2,type_cf,npts2,index_cf2,total_nr);
      }
      index_xbar++;
    }
  }

/*
 * Free up memory.
 */
  free(tmp_x);
  free(work);
  free(tmp_cf1);
  free(tmp_cf2);
  free(tmp_xbar);
/*
 * Set up variable to return.
 */
  type_cf_class = (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(type_cf)));

/*
 * Set up return values.
 */
  if(any_missing) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"ezfftf_n: %d input arrays contained missing values. No calculations performed on these arrays.",any_missing);

    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              cf,
                              &missing_cf,
                              ndims_cf,
                              dsizes_cf,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)type_cf_class
                              );
  }
  else {
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              cf,
                              NULL,
                              ndims_cf,
                              dsizes_cf,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)type_cf_class
                              );
  }
/*
 * Attributes "xbar" and "npts".
 */
  att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

  dsizes[0] = total_nlr;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         xbar,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)type_cf_class
                         );
  _NclAddAtt(
             att_id,
             "xbar",
             att_md,
             NULL
             );

  (*(int*)N) = npts;
  dsizes[0] = 1;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         N,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypeintClass
                         );
  _NclAddAtt(
             att_id,
             "npts",
             att_md,
             NULL
             );

/*
 * Set up variable to hold return array and attributes.
 */
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


NhlErrorTypes ezfftb_W( void )
{
/*
 * Input array variables
 *
 * Argument # 1
 */
  void *cf;
  double *tmp_cf1 = NULL;
  double *tmp_cf2 = NULL;
  int ndims_cf;
  ng_size_t dsizes_cf[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_cf;
  NclScalar missing_cf, missing_dcf, missing_rcf;
  int has_missing_cf;
/*
 * Argument # 2
 */
  void *xbar;
  double *tmp_xbar = NULL;
  ng_size_t dsizes_xbar[1];
  NclBasicDataTypes type_xbar;
/*
 * Some variables we need to retrieve the "npts" atttribute (if it exists).
 */
  NclAttList *att_list;
  NclAtt tmp_attobj;
  NclStackEntry data;
/*
 * Output array variables
 */
  void *x;
  double *tmp_x;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_x;
  NclScalar missing_x;
/*
 * various
 */
  double *work;
  ng_size_t index_cf, index_x;
  ng_size_t i, *tmp_npts, npts, npts2, lnpts2, size_x, size_leftmost;
  int found_missing1, found_missing2, any_missing, scalar_xbar;
  int inpts;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  cf = (void*)NclGetArgValue(
           0,
           2,
           &ndims_cf, 
           dsizes_cf,
           &missing_cf,
           &has_missing_cf,
           &type_cf,
           DONT_CARE);
  xbar = (void*)NclGetArgValue(
           1,
           2,
           NULL,
           dsizes_xbar,
           NULL,
           NULL,
           &type_xbar,
           DONT_CARE);
/*
 * Calculate number of leftmost elements.
 */
  size_leftmost = 1;
  for( i = 1; i < ndims_cf-1; i++ ) size_leftmost *= dsizes_cf[i];
/*
 * Check xbar dimension sizes.
 */
  scalar_xbar = is_scalar(1,dsizes_xbar);

  if(!scalar_xbar) {
/*
 * If xbar is not a scalar, it must be an array of the same dimension
 * sizes as the leftmost dimensions of cf (except the first dimension
 * of '2').
 */ 
    if(dsizes_xbar[0] != size_leftmost) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftb: If xbar is not a scalar, then it must be a single vector of the length of the product of the leftmost dimensions of 'cf' (not including the '2' dimension)") ;
      return(NhlFATAL);
    }
  }

/*
 * Coerce missing values.
 */
  coerce_missing(type_cf,has_missing_cf,&missing_cf,&missing_dcf,&missing_rcf);
/*
 * Okay, what follows here is some code for retrieving the "npts"
 * attribute if it exists. This attribute is one that should have been
 * set when "ezfftf_n" was called, and it indicates the length of the
 * original series.
 */
  npts2  = dsizes_cf[ndims_cf-1];     /* Calculate the length in case  */
                                      /* it is not set explicitly. */
  npts = 2*npts2;

  data = _NclGetArg(0,2,DONT_CARE);
  switch(data.kind) {
  case NclStk_VAR:
    if(data.u.data_var->var.att_id != -1) {
      tmp_attobj = (NclAtt)_NclGetObj(data.u.data_var->var.att_id);
      if(tmp_attobj == NULL) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftb: Bad attribute list, can't continue");
        return(NhlFATAL);
      }
      if(tmp_attobj->att.n_atts == 0) {
        break;
      }
      att_list = tmp_attobj->att.att_list;
      i = 0;
      while(att_list != NULL) {
        if(att_list->quark == NrmStringToQuark("npts")) {
          tmp_npts = get_dimensions(att_list->attvalue->multidval.val,1,
                                    att_list->attvalue->multidval.data_type,
                                    "ezfftb");
          npts = *tmp_npts;
          free(tmp_npts);
          if((npts % 2) == 0) {
            npts2 = npts/2;
          }
          else {
            npts2 = (npts-1)/2;
          }
          break;
        }
        att_list = att_list->next;
      }
    }
    break;
  default:
        NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftb: data.kind, can't continue");
        return(NhlFATAL);
  }
/*
 * Test input array size
 */
  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftb: npts = %d is greater than INT_MAX", npts);
    return(NhlFATAL);
  }
  inpts = (int) npts;

/*
 * Calculate size of output array.
 */
  lnpts2 = npts2 * size_leftmost;
  size_x = size_leftmost * npts;

  ndims_x = ndims_cf - 1;
  for(i = 0; i < ndims_x-1; i++ ) dsizes_x[i] = dsizes_cf[i+1];
  dsizes_x[ndims_x-1] = npts;
/*
 * Create arrays to coerce input to double if necessary.
 */
  if(type_cf != NCL_double) {
    tmp_cf1 = (double*)calloc(npts2,sizeof(double));
    tmp_cf2 = (double*)calloc(npts2,sizeof(double));
    if(tmp_cf1 == NULL || tmp_cf2 == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftb: Unable to allocate memory for coercing input array to double precision");
      return(NhlFATAL);
    }
  }

  if(type_xbar != NCL_double) {
    tmp_xbar = (double*)calloc(1,sizeof(double));
    if(tmp_xbar == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftb: Unable to allocate memory for coercing input array to double precision");
      return(NhlFATAL);
    }
  }

/*
 * Allocate memory for output array.
 */
  tmp_x = (double *)calloc(npts,sizeof(double));
  if (tmp_x == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftb: Cannot allocate memory for temporary output array" );
    return(NhlFATAL);
  }
  if(type_cf == NCL_double) {
    type_x = NCL_double;
    x = (void*)calloc(size_x,sizeof(double));
    if(has_missing_cf) missing_x = missing_dcf;
  }
  else {
    type_x = NCL_float;
    x = (void*)calloc(size_x,sizeof(float));
    if(has_missing_cf) missing_x = missing_rcf;
  }
  if (x == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftb: Cannot allocate memory for output array" );
    return(NhlFATAL);
  }
/*
 * Allocate memory for work array
 */
  work = (double*)calloc(4*npts+15,sizeof(double));
  if ( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftb: Cannot allocate memory for work array" );
    return(NhlFATAL);
  }
/*
 * If xbar is a scalar, coerce it outside the loop.
 */
  if(scalar_xbar) {
    if(type_xbar != NCL_double) { 
      coerce_subset_input_double(xbar,tmp_xbar,0,type_xbar,1,0,NULL,NULL);
    }
    else {
      tmp_xbar = &((double*)xbar)[0];
    }
  }

/*
 * Call the f77 version of 'dezfftb' with the full argument list.
 */
  index_x = index_cf = 0;
  any_missing = 0;
  for(i = 0; i < size_leftmost; i++) {
    if(type_cf != NCL_double) { 
      coerce_subset_input_double(cf,tmp_cf1,index_cf,type_cf,npts2,0,
                                 NULL,NULL);
      coerce_subset_input_double(cf,tmp_cf2,lnpts2+index_cf,type_cf,npts2,0,
                                 NULL,NULL);
    }
    else {
      tmp_cf1 = &((double*)cf)[index_cf];
      tmp_cf2 = &((double*)cf)[lnpts2+index_cf];
    }
/*
 * Check for missing values in cf.  If any, then coerce that section of
 * the output to missing.
 */
    found_missing1 = contains_missing(tmp_cf1,npts2,has_missing_cf,
                                      missing_dcf.doubleval);
    found_missing2 = contains_missing(tmp_cf2,npts2,has_missing_cf,
                                      missing_dcf.doubleval);
    if(found_missing1 || found_missing2) {
      any_missing++;
      set_subset_output_missing(x,index_x,type_x,npts,missing_dcf.doubleval);
    }
    else {
/*
 * If xbar is not a scalar, then we need to coerce each element
 * to double or else just grab its value.
 */
      if(!scalar_xbar) {
        if(type_xbar != NCL_double) { 
          coerce_subset_input_double(xbar,tmp_xbar,i,type_xbar,1,0,NULL,NULL);
        }
        else {
          tmp_xbar = &((double*)xbar)[i];
        }
      }

      NGCALLF(dezffti,DEZFFTI)(&inpts,work);
      NGCALLF(dezfftb,DEZFFTB)(&inpts,tmp_x,tmp_xbar,tmp_cf1,tmp_cf2,work);
/*
 * Copy results back into x.
 */
      coerce_output_float_or_double(x,tmp_x,type_cf,npts,index_x);
    }
    index_x  += npts;
    index_cf += npts2;
  }

/*
 * Free up memory.
 */
  if(type_cf != NCL_double) {
    free(tmp_cf1);
    free(tmp_cf2);
  }
  if(type_xbar != NCL_double) free(tmp_xbar);
  free(tmp_x);
  free(work);

  if(any_missing) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"ezfftb: %d input arrays contained missing values. No calculations performed on these arrays.",any_missing);

    return(NclReturnValue(x,ndims_x,dsizes_x,&missing_x,type_x,0));
  }
  else {
    return(NclReturnValue(x,ndims_x,dsizes_x,NULL,type_x,0));
  }
}


NhlErrorTypes ezfftb_n_W( void )
{
/*
 * Input array variables
 *
 * Argument # 1
 */
  void *cf;
  double *tmp_cf1 = NULL;
  double *tmp_cf2 = NULL;
  int ndims_cf;
  ng_size_t dsizes_cf[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_cf;
  NclScalar missing_cf, missing_dcf, missing_rcf;
  int has_missing_cf;
/*
 * Argument # 2
 */
  void *xbar;
  double *tmp_xbar = NULL;
  ng_size_t dsizes_xbar[1];
  NclBasicDataTypes type_xbar;
/*
 * Argument # 3
 */
  int *dim;
/*
 * Some variables we need to retrieve the "npts" atttribute (if it exists).
 */
  NclAttList *att_list;
  NclAtt tmp_attobj;
  NclStackEntry data;
/*
 * Output array variables
 */
  void *x;
  double *tmp_x;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_x;
  NclScalar missing_x;
/*
 * various
 */
  double *work;
  ng_size_t index_cf1, index_cf2, index_x, index_xbar;
  ng_size_t index_nrnpts, index_nrnpts2;
  ng_size_t i, j, *tmp_npts, npts, npts2, lnpts2, size_x;
  ng_size_t total_nl, total_nr, total_nlr;
  int found_missing1, found_missing2, any_missing, scalar_xbar;
  int inpts;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  cf = (void*)NclGetArgValue(
           0,
           3,
           &ndims_cf, 
           dsizes_cf,
           &missing_cf,
           &has_missing_cf,
           &type_cf,
           DONT_CARE);
  xbar = (void*)NclGetArgValue(
           1,
           3,
           NULL,
           dsizes_xbar,
           NULL,
           NULL,
           &type_xbar,
           DONT_CARE);

  dim = (int *)NclGetArgValue(1,2,NULL,NULL,NULL,NULL,NULL,0);
/*
 * Make sure "dim" is a valid dimension. It can't be equal to 0, because
 * this dimension is the "2" that represents the real and imaginary
 * parts.
 */
  if (*dim == 0) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftb_n: The dimension index cannot be 0. The leftmost dimension of cf represents the real/imaginary parts");
    return(NhlFATAL);
  }

  if (*dim < 0 || *dim >= ndims_cf) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftb_n: Invalid dimension index");
    return(NhlFATAL);
  }

/*
 * Calculate number of leftmost and rightmost elements.
 */
  total_nl = total_nr = 1;
  for( i =      1; i < *dim;     i++ ) total_nl *= dsizes_cf[i];
  for( i = *dim+1; i < ndims_cf; i++ ) total_nr *= dsizes_cf[i];
  total_nlr = total_nl * total_nr;

/*
 * Check xbar dimension sizes.
 */
  scalar_xbar = is_scalar(1,dsizes_xbar);

  if(!scalar_xbar) {
/*
 * If xbar is not a scalar, it must be an array of the same dimension
 * sizes as the leftmost dimensions of cf (except the first dimension
 * of 'if').
 */ 
    if(dsizes_xbar[0] != total_nlr) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftb_n: If xbar is not a scalar, then it must be a single vector of the length of the product of the dimensions of 'cf' (not including the '2' or the dimension that was transformed)") ;
      return(NhlFATAL);
    }
  }

/*
 * Coerce missing values.
 */
  coerce_missing(type_cf,has_missing_cf,&missing_cf,&missing_dcf,&missing_rcf);
/*
 * Okay, what follows here is some code for retrieving the "npts"
 * attribute if it exists. This attribute is one that should have been
 * set when "ezfftf_n" was called, and it indicates the length of the
 * original series.
 */
  npts2  = dsizes_cf[*dim];         /* Calculate the length in case  */
                                      /* it is not set explicitly. */
  npts = 2*npts2;

  data = _NclGetArg(0,3,DONT_CARE);
  switch(data.kind) {
  case NclStk_VAR:
    if(data.u.data_var->var.att_id != -1) {
      tmp_attobj = (NclAtt)_NclGetObj(data.u.data_var->var.att_id);
      if(tmp_attobj == NULL) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftb_n: Bad attribute list, can't continue");
        return(NhlFATAL);
      }
      if(tmp_attobj->att.n_atts == 0) {
        break;
      }
      att_list = tmp_attobj->att.att_list;
      i = 0;
      while(att_list != NULL) {
        if(att_list->quark == NrmStringToQuark("npts")) {
          tmp_npts = get_dimensions(att_list->attvalue->multidval.val,1,
                                    att_list->attvalue->multidval.data_type,
                                    "ezfftb_n");
          npts = *tmp_npts;
          free(tmp_npts);
          if((npts % 2) == 0) {
            npts2 = npts/2;
          }
          else {
            npts2 = (npts-1)/2;
          }
          break;
        }
        att_list = att_list->next;
      }
    }
    break;
  default:
        NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftb_n: data.kind, can't continue");
        return(NhlFATAL);
  }
/*
 * Test input array size
 */
  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftb_n: npts = %d is greater than INT_MAX", npts);
    return(NhlFATAL);
  }
  inpts = (int) npts;

/*
 * Calculate size of output array.
 */
  lnpts2 = npts2 * total_nlr;
  size_x = total_nlr * npts;

  ndims_x = ndims_cf - 1;
  for(i = 0; i < ndims_x-1; i++ ) dsizes_x[i] = dsizes_cf[i+1];
  dsizes_x[*dim-1] = npts;
  for( i =      1; i < *dim;     i++ ) dsizes_x[i-1] = dsizes_cf[i];
  for( i = *dim+1; i < ndims_cf; i++ ) dsizes_x[i-1] = dsizes_cf[i];
/*
 * Create arrays to coerce input to double.
 */
  tmp_cf1 = (double*)calloc(npts2,sizeof(double));
  tmp_cf2 = (double*)calloc(npts2,sizeof(double));
  if(tmp_cf1 == NULL || tmp_cf2 == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftb_n: Unable to allocate memory for coercing input array to double precision");
    return(NhlFATAL);
  }

  if(type_xbar != NCL_double) {
    tmp_xbar = (double*)calloc(1,sizeof(double));
    if(tmp_xbar == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftb_n: Unable to allocate memory for coercing input array to double precision");
      return(NhlFATAL);
    }
  }

/*
 * Allocate memory for output array.
 */
  tmp_x = (double *)calloc(npts,sizeof(double));
  if (tmp_x == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftb_n: Cannot allocate memory for temporary output array" );
    return(NhlFATAL);
  }
  if(type_cf == NCL_double) {
    type_x = NCL_double;
    x = (void*)calloc(size_x,sizeof(double));
    if(has_missing_cf) missing_x = missing_dcf;
  }
  else {
    type_x = NCL_float;
    x = (void*)calloc(size_x,sizeof(float));
    if(has_missing_cf) missing_x = missing_rcf;
  }
  if (x == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftb_n: Cannot allocate memory for output array" );
    return(NhlFATAL);
  }
/*
 * Allocate memory for work array
 */
  work = (double*)calloc(4*npts+15,sizeof(double));
  if ( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftb_n: Cannot allocate memory for work array" );
    return(NhlFATAL);
  }
/*
 * If xbar is a scalar, coerce it outside the loop.
 */
  if(scalar_xbar) {
    if(type_xbar != NCL_double) { 
      coerce_subset_input_double(xbar,tmp_xbar,0,type_xbar,1,0,NULL,NULL);
    }
    else {
      tmp_xbar = &((double*)xbar)[0];
    }
  }

/*
 * Call the f77 version of 'dezfftb' with the full argument list.
 */
  any_missing = 0;
  index_xbar = 0;
  for( i = 0; i < total_nl; i++ ) {
    index_nrnpts  = i*total_nr * npts;
    index_nrnpts2 = i*total_nr * npts2;
    for( j = 0; j < total_nr; j++ ) {
      index_x   = index_nrnpts + j;
      index_cf1 = index_nrnpts2 + j;
      index_cf2 = lnpts2 + index_nrnpts2 + j;

      coerce_subset_input_double_step(cf,tmp_cf1,index_cf1,total_nr,type_cf,npts2,0,
                                      NULL,NULL);
      coerce_subset_input_double_step(cf,tmp_cf2,index_cf2,total_nr,type_cf,npts2,0,
                                      NULL,NULL);
/*
 * Check for missing values in cf.  If any, then coerce that section of
 * the output to missing.
 */
      found_missing1 = contains_missing(tmp_cf1,npts2,has_missing_cf,
                                        missing_dcf.doubleval);
      found_missing2 = contains_missing(tmp_cf2,npts2,has_missing_cf,
                                        missing_dcf.doubleval);
      if(found_missing1 || found_missing2) {
        any_missing++;
        set_subset_output_missing_step(x,index_x,total_nr,type_x,npts,missing_dcf.doubleval);
      }
      else {
/*
 * If xbar is not a scalar, then we need to coerce each element
 * to double or else just grab its value.
 */
        if(!scalar_xbar) {
          if(type_xbar != NCL_double) { 
            coerce_subset_input_double(xbar,tmp_xbar,index_xbar,type_xbar,1,0,NULL,NULL);
          }
          else {
            tmp_xbar = &((double*)xbar)[index_xbar];
          }
        }
        
        NGCALLF(dezffti,DEZFFTI)(&inpts,work);
        NGCALLF(dezfftb,DEZFFTB)(&inpts,tmp_x,tmp_xbar,tmp_cf1,tmp_cf2,work);
/*
 * Copy results back into x.
 */
        coerce_output_float_or_double_step(x,tmp_x,type_cf,npts,index_x,total_nr);
      }
      index_xbar++;
    }
  }

/*
 * Free up memory.
 */
  free(tmp_cf1);
  free(tmp_cf2);
  if(type_xbar != NCL_double) free(tmp_xbar);
  free(tmp_x);
  free(work);

  if(any_missing) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"ezfftb_n: %d input arrays contained missing values. No calculations performed on these arrays.",any_missing);

    return(NclReturnValue(x,ndims_x,dsizes_x,&missing_x,type_x,0));
  }
  else {
    return(NclReturnValue(x,ndims_x,dsizes_x,NULL,type_x,0));
  }
}


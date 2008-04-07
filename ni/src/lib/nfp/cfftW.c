#include <stdio.h>
#include <math.h>
#include "wrapper.h"

extern void NGCALLF(cfftfdriver,CFFTFDRIVER)(int*,double*,double*,double*,
                                             double*,int*);

extern void NGCALLF(cfftbfdriver,CFFTBDRIVER)(int*,double*,double*,double*,
                                              double*,int*);

extern void NGCALLF(frqcfft,FRQCFFT)(int*,double*);

extern void NGCALLF(cfftffrqreorder,CFFTFFRQREORDER)(int *,double*,double*,
                                                     double*,double*,double*,
                                                     double*);
NhlErrorTypes cfftf_W( void )
{
/*
 * Input array variables
 */
  void *x;
  int *opt;
  int size_x, ndims_x, dsizes_x[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_x;
  NclScalar missing_x, missing_dx, missing_rx;
  int has_missing_x;
  double *tmp_x;
/*
 * Output array variables
 */
  void *cf;
  int ndims_cf, *dsizes_cf;
  double *tmp_cfa, *tmp_cfb;
  NclBasicDataTypes type_cf;
  NclTypeClass type_cf_class;
  NclScalar missing_cf;
/*
 * Attribute variables
 */
  void *frq;
  double *tmp_frq;
  int *N;
  int att_id;
  int dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
/*
 * various
 */
  double *work;
  int i, npts, npts2, nwrk, index_x, index_cfb;
  int found_missing, any_missing, size_leftmost, size_cf;
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
           2);

  opt = (int*)NclGetArgValue(
           1,
           2,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           2);
/*
 * Calculate number of leftmost elements and dimension sizes of output
 */
  ndims_cf     = ndims_x + 1;
  dsizes_cf = (int *)malloc(ndims_cf*sizeof(int));
  dsizes_cf[0] = 2;
  size_leftmost = 1;
  for( i = 0; i < ndims_x-1; i++ ) {
    size_leftmost *= dsizes_x[i];
    dsizes_cf[i+1] = dsizes_x[i];
  }
  dsizes_cf[ndims_x] = npts = dsizes_x[ndims_x-1];

/* Calculate size of output array. */
  npts2   = 2*npts;
  size_x  = size_leftmost * npts;
  size_cf = 2 * size_x;

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
      NhlPError(NhlFATAL,NhlEUNKNOWN,"cfftf: Unable to allocate memory for coercing input array to double precision");
      return(NhlFATAL);
    }
  }
/*
 * Allocate space for other arrays.
 */
  nwrk     = (4*npts) + 25;
  tmp_frq  = (double*)calloc(npts,sizeof(double));
  tmp_cfa  = (double*)calloc(npts,sizeof(double));
  tmp_cfb  = (double*)calloc(npts,sizeof(double));
  work     = (double*)calloc(nwrk,sizeof(double));
  N        = (void*)calloc(1,sizeof(int));
  if ( tmp_frq == NULL || tmp_cfa == NULL || tmp_cfb == NULL || \
       work == NULL || N == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cfftf: Cannot allocate memory for temporary arrays" );
    return(NhlFATAL);
  }
  if(type_x == NCL_double) {
    cf   = (void*)calloc(size_cf,sizeof(double));
    frq  = (void*)calloc(size_x,sizeof(double));
    type_cf = NCL_double;
    if(has_missing_x) missing_cf = missing_dx;
  }
  else {
    cf   = (void*)calloc(size_cf,sizeof(float));
    frq  = (void*)calloc(size_x,sizeof(float));
    type_cf = NCL_float;
    if(has_missing_x) missing_cf = missing_rx;
  }
  if ( cf == NULL || frq == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cfftf: Cannot allocate memory for output arrays" );
    return(NhlFATAL);
  }

/*
 * Call the f77 version of 'cfftfdriver' with the full argument list.
 */
  index_x   = 0;
  index_cfb = size_x;
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
      set_subset_output_missing(cf,index_x,type_cf,npts,
                                missing_dx.doubleval);
      set_subset_output_missing(cf,index_cfb,type_cf,npts,
                                missing_dx.doubleval);
    }
    else {
      NGCALLF(cfftfdriver,CFFTFDRIVER)(&npts,tmp_x,tmp_cfa,tmp_cfb,work,&nwrk);
/*
 * Copy results back into cf.
 */
      coerce_output_float_or_double(cf,tmp_cfa,type_cf,npts,index_x);
      coerce_output_float_or_double(cf,tmp_cfb,type_cf,npts,index_cfb);
    }
    index_x   += npts;
    index_cfb += npts;
  }

/*
 * Free up memory.
 */
  if(type_x != NCL_double) free(tmp_x);
  free(work);
  free(tmp_cfa);
  free(tmp_cfb);

/*
 * Set up return values.
 */
  type_cf_class = (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(type_cf)));
  if(any_missing) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"cfftf: %d input array(s) contained missing values. No calculations performed on these arrays.",any_missing);

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
 * Attributes "frq" and "npts".
 */
  att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

/* Calculate frequences */
  NGCALLF(frqcfft,FRQCFFT)(&npts,tmp_frq);
  coerce_output_float_or_double(frq,tmp_frq,type_cf,npts,0);
  free(tmp_frq);

  dsizes[0] = npts;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         frq,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)type_cf_class
                         );
  _NclAddAtt(
             att_id,
             "frq",
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

NhlErrorTypes cfftb_W( void )
{
/*
 * Input array variables
 */
  void *cf;
  int *opt;
  int size_cf, ndims_cf, dsizes_cf[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_cf;
  NclScalar missing_cf, missing_dcf, missing_rcf;
  int has_missing_cf;
  double *tmp_cfa, *tmp_cfb;
/*
 * Output array variables
 */
  void *x;
  double *tmp_x;
  int ndims_x, *dsizes_x;
  NclBasicDataTypes type_x;
  NclTypeClass type_x_class;
  NclScalar missing_x;
/*
 * various
 */
  double *work;
  int i, j, ret, npts, npts2, nwrk, index_cfa, index_cfb;
  int found_missing_cfa, found_missing_cfb, any_missing, size_leftmost, size_x;
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
           2);

  if(dsizes_cf[0] != 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cfftb: Leftmost dimension of 'cf' must be 2");
    return(NhlFATAL);
  }
  opt = (int*)NclGetArgValue(
           1,
           2,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           2);

/*
 * Calculate number of leftmost elements and dimension sizes of output
 */
  if(ndims_cf > 1) {
    ndims_x = ndims_cf - 1;
  }
  else {
    ndims_x = 1;
  }
  dsizes_x = (int *)malloc(ndims_x*sizeof(int));
  size_leftmost = 1;
  for( i = 1; i < ndims_cf-1; i++ ) {
    size_leftmost *= dsizes_cf[i];
    dsizes_x[i-1]  = dsizes_cf[i];
  }
  dsizes_x[ndims_x-1] = npts = dsizes_cf[ndims_cf-1];

/* Calculate size of output array. */
  npts2   = 2*npts;
  size_x  = size_leftmost * npts;
  size_cf = 2 * size_x;

/*
 * Coerce missing values.
 */
  coerce_missing(type_cf,has_missing_cf,&missing_cf,&missing_dcf,&missing_rcf);
/*
 * Create space for temporary input arrays if necessary.
 */
  if(type_cf != NCL_double) {
    tmp_cfa  = (double*)calloc(npts,sizeof(double));
    tmp_cfb  = (double*)calloc(npts,sizeof(double));
    if(tmp_cfa == NULL || tmp_cfb == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"cfftb: Unable to allocate memory for coercing input array to double precision");
      return(NhlFATAL);
    }
  }
/*
 * Allocate space for other arrays.
 */
  nwrk  = (4*npts) + 25;
  work  = (double*)calloc(nwrk,sizeof(double));
  tmp_x = (double*)calloc(npts,sizeof(double));
  if ( tmp_x == NULL || work == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cfftb: Cannot allocate memory for temporary arrays" );
    return(NhlFATAL);
  }
  if(type_cf == NCL_double) {
    x      = (void*)calloc(size_x,sizeof(double));
    type_x = NCL_double;
    if(has_missing_cf) missing_x = missing_dcf;
  }
  else {
    x      = (void*)calloc(size_x,sizeof(float));
    type_x = NCL_float;
    if(has_missing_cf) missing_x = missing_rcf;
  }
  if ( x == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cfftb: Cannot allocate memory for output array" );
    return(NhlFATAL);
  }

/*
 * Call the f77 version of 'cfftbdriver' with the full argument list.
 */
  index_cfa = 0;
  index_cfb = size_x;
  any_missing = 0;
  for(i = 0; i < size_leftmost; i++) {
    if(type_cf != NCL_double) { 
      coerce_subset_input_double(cf,tmp_cfa,index_cfa,type_cf,npts,0,NULL,NULL);
      coerce_subset_input_double(cf,tmp_cfb,index_cfb,type_cf,npts,0,NULL,NULL);
    }
    else {
      tmp_cfa = &((double*)cf)[index_cfa];
      tmp_cfb = &((double*)cf)[index_cfb];
    }
/*
 * Check for missing values in cf.  If any, then coerce that section of
 * the output to missing.
 */
    found_missing_cfa = contains_missing(tmp_cfa,npts,has_missing_cf,
                                         missing_dcf.doubleval);
    found_missing_cfb = contains_missing(tmp_cfb,npts,has_missing_cf,
                                         missing_dcf.doubleval);
    if(found_missing_cfa || found_missing_cfb) {
      any_missing++;
      set_subset_output_missing(x,index_cfa,type_x,npts,
                                missing_dcf.doubleval);
    }
    else {
      NGCALLF(cfftbdriver,CFFTBDRIVER)(&npts,tmp_x,tmp_cfa,tmp_cfb,work,&nwrk);
/*
 * Copy results back into x.
 */
      coerce_output_float_or_double(x,tmp_x,type_x,npts,index_cfa);
    }
    index_cfa += npts;
    index_cfb += npts;
  }

  if(any_missing) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"cfftb: %d input array(s) contained missing values. No calculations performed on these arrays.",any_missing);
  }

/*
 * Free up memory.
 */
  if(type_cf != NCL_double) {
     free(tmp_cfa);
     free(tmp_cfb);
  }
  free(work);
  free(tmp_x);

/*
 * Set up variable to return.
 */
  if(any_missing) {
    ret = NclReturnValue(x,ndims_x,dsizes_x,&missing_x,type_x,0);
  }
  else {
    ret = NclReturnValue(x,ndims_x,dsizes_x,NULL,type_x,0);
  }
  return(ret);
}


NhlErrorTypes cfftf_frq_reorder_W( void )
{
/*
 * Input array variables
 */
  void *cf;
  int size_cf, ndims_cf, dsizes_cf[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_cf;
  NclScalar missing_cf, missing_dcf, missing_rcf;
  int has_missing_cf;
/*
 * Output array variables
 */
  void *cfr, *frqr;
  NclTypeClass type_cfr_class;
  NclBasicDataTypes type_cfr;
/*
 * Variables for retrieving attributes from "cf".
 */
  void *frq;
  int size_frq;
  NclBasicDataTypes type_frq;  
  NclAttList  *attr_list;
  NclAtt  attr_obj;
  NclStackEntry   stack_entry;
/*
 * Attribute variables
 */
  int att_id;
  int dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
  double *dfrq;
  float *rfrq;

/*
 * various
 */
  double *tmp_cfa, *tmp_cfb, *tmp_cfar, *tmp_cfbr, *tmp_frq, *tmp_frqr;
  int i, size_x, ret, *N, npts, index_cfa, index_cfb, size_leftmost;
  logical found_frq;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  cf = (void*)NclGetArgValue(
           0,
           1,
           &ndims_cf, 
           dsizes_cf,
           &missing_cf,
           &has_missing_cf,
           &type_cf,
           2);

  if(dsizes_cf[0] != 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cfftf_frq_reorder: Leftmost dimension of 'cf' must be 2");
    return(NhlFATAL);
  }

/*
 * Calculate size of input array.
 */
  size_leftmost = 1;
  for( i = 1; i < ndims_cf-1; i++ ) size_leftmost *= dsizes_cf[i];
  npts    = dsizes_cf[ndims_cf-1];
  size_x  = size_leftmost * npts;
  size_cf = 2 * size_x;

/*
 * Look for "frq" attribute. If it isn't there, we can't continue.
 */
  found_frq = False;
  stack_entry = _NclGetArg(0, 1, DONT_CARE);
  switch (stack_entry.kind) {
  case NclStk_VAR:
    if (stack_entry.u.data_var->var.att_id != -1) {
      attr_obj = (NclAtt) _NclGetObj(stack_entry.u.data_var->var.att_id);
      if (attr_obj == NULL) {
        break;
      }
    }
    else {
/*
 * att_id == -1 ==> no optional args given.
 */
      break;
    }
/* 
 * Get optional arguments.
 */
    if (attr_obj->att.n_atts > 0) {
/*
 * Get list of attributes.
 */
      attr_list = attr_obj->att.att_list;
/*
 * Loop through attributes and check them. We are looking for "frq".
 */
      while (attr_list != NULL) {
        if(!strcmp(attr_list->attname, "frq")) {
          frq       = attr_list->attvalue->multidval.val;
          type_frq  = attr_list->attvalue->multidval.data_type;
          size_frq  = attr_list->attvalue->multidval.dim_sizes[0];
          found_frq = True;
        }
        attr_list = attr_list->next;
      }
    default:
      break;
    }
  }

  if(!found_frq) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cfftf_frq_reorder: No 'frq' attribute is present. Can't continue.");
    return(NhlFATAL);
  }
  if(size_frq != npts) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cfftf_frq_reorder: The length of the 'frq' attribute must be the same as the rightmost dimension of 'cf'. Can't continue.");
    return(NhlFATAL);
  }

/*
 * Create space for output arrays, and temporary input arrays if necessary.
 */
  if(type_cf != NCL_double) {
    type_cfr = NCL_float;
    tmp_cfa  = (double*)calloc(npts,sizeof(double));
    tmp_cfb  = (double*)calloc(npts,sizeof(double));
    cfr      = (void*)calloc(size_cf,sizeof(float));
    frqr     = (void*)calloc(npts,sizeof(float));
    if(cfr == NULL || frqr == NULL || tmp_cfa == NULL || tmp_cfb == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"cfftf_frq_reorder: Unable to allocate memory for temporary input/output arrays");
      return(NhlFATAL);
    }
  }
  else {
    type_cfr = NCL_double;
    cfr  = (void*)calloc(size_cf,sizeof(double));
    frqr = (void*)calloc(npts,sizeof(double));
    if(cfr == NULL || frqr == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"cfftf_frq_reorder: Unable to allocate memory for output arrays");
      return(NhlFATAL);
    }
  }

/*
 * Coerce frq to double if necessary.
 */
  tmp_frq = coerce_input_double(frq,type_frq,npts,0,NULL,NULL);

/*
 * Allocate space for other arrays.
 */
  tmp_cfar = (double*)calloc(npts,sizeof(double));
  tmp_cfbr = (double*)calloc(npts,sizeof(double));
  tmp_frqr = (double*)calloc(npts,sizeof(double));
  if ( tmp_cfar == NULL || tmp_cfbr == NULL || tmp_frqr == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cfftf_frq_reorder: Cannot allocate memory for temporary arrays" );
    return(NhlFATAL);
  }

/*
 * Call the f77 version of 'cfftffrqreorder' with the full argument list.
 */
  index_cfa = 0;
  index_cfb = size_x;
  for(i = 0; i < size_leftmost; i++) {
    if(type_cf != NCL_double) { 
      coerce_subset_input_double(cf,tmp_cfa,index_cfa,type_cf,npts,0,
                                 NULL,NULL);
      coerce_subset_input_double(cf,tmp_cfb,index_cfb,type_cf,npts,0,
                                 NULL,NULL);
    }
    else {
      tmp_cfa = &((double*)cf)[index_cfa];
      tmp_cfb = &((double*)cf)[index_cfb];
    }

    NGCALLF(cfftffrqreorder,CFFTFFRQREORDER)(&npts,tmp_frq,tmp_cfa,tmp_cfb,
                                             tmp_frqr,tmp_cfar,tmp_cfbr);
/*
 * Copy results back into cfr.
 */
    if(i == 0) {
      coerce_output_float_or_double(frqr,tmp_frqr,type_cfr,npts,0);
    }

    coerce_output_float_or_double(cfr,tmp_cfar,type_cfr,npts,index_cfa);
    coerce_output_float_or_double(cfr,tmp_cfbr,type_cfr,npts,index_cfb);

    index_cfa += npts;
    index_cfb += npts;
  }

/*
 * Free up memory.
 */
  if(type_cf != NCL_double) {
     free(tmp_cfa);
     free(tmp_cfb);
  }

  if(type_frq != NCL_double) free(tmp_frq);

  free(tmp_cfar);
  free(tmp_cfbr);
  free(tmp_frqr);

/*
 * Set up return values.
 */
  type_cfr_class = (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(type_cfr)));

  if(has_missing_cf) {
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              cfr,
                              &missing_cf,
                              ndims_cf,
                              dsizes_cf,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)type_cfr_class
                              );
  }
  else {
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              cfr,
                              NULL,
                              ndims_cf,
                              dsizes_cf,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)type_cfr_class
                              );
  }
/*
 * Attributes "frq" and "npts".
 */
  att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

  dsizes[0] = npts;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         frqr,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)type_cfr_class
                         );
  _NclAddAtt(
             att_id,
             "frq",
             att_md,
             NULL
             );

  N = (void*)calloc(1,sizeof(int));
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


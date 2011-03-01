#include <stdio.h>
#include <math.h>
#include "wrapper.h"

extern void NGCALLF(vors,VORS)(int *, int *, double *, double *, double *, 
                               double *, int *);

extern void NGCALLF(roteof,ROTEOF)(int *, int *, double *, int *, double*,
                                   double *, double *, double *, int *, int *);

extern void NGCALLF(zroteof,ZROTEOF)(int *, int *, double *, int *, double*,
                                     double *, double *, double *, int *, int *);

NhlErrorTypes eof_varimax_W( void )
{
/*
 * Input array variables
 */
  NclStackEntry data;
  NclMultiDValData tmp_md = NULL;
  NclAttList  *attr_list;
  NclAtt  attr_obj;

  void *evec = NULL, *trace = NULL;
  double *devec;
  ng_size_t *dsizes_evec;
  int ndims_evec, has_missing_evec;
  NclScalar missing_evec, missing_devec;
  int nvar, nfac, ldevec;
  ng_size_t total_size_evec;
  NclBasicDataTypes type_evec;
  NclTypeClass type_trace_class = NCL_none;

/*
 * Work array variables.
 */
  double *a, *b, *w;
/*
 * Output array variable
 */
  void  *evec_out;
  NclBasicDataTypes type_evec_out;
  NclTypeClass type_evec_out_class;

/*
 * Variables for returning attributes.
 */
  int att_id;
  ng_size_t dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
  int i, invar, infac, ildevec;
  float *pcvar;
  logical return_trace;

/*
 * Retrieve evec.
 */
  data = _NclGetArg(0,1,DONT_CARE);

  switch(data.kind) {
  case NclStk_VAR:
    tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
    break;
  case NclStk_VAL:
    tmp_md = (NclMultiDValData)data.u.data_obj;
    break;
  default:
    break;
  }

/*
 * Initialize info about evec.
 */
  evec             = tmp_md->multidval.val;
  ndims_evec       = tmp_md->multidval.n_dims;
  dsizes_evec      = tmp_md->multidval.dim_sizes;
  has_missing_evec = tmp_md->multidval.missing_value.has_missing;
  missing_evec     = tmp_md->multidval.missing_value.value;
  type_evec        = tmp_md->multidval.data_type;

/*
 * Check dimensions.
 */
  if( ndims_evec < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eof_varimax: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }

/*
 * Check for "trace" attribute. If exists, then make sure it is returned
 * with this function. 
 */
  return_trace = False;

  switch(data.kind) {
  case NclStk_VAR:
    if (data.u.data_var->var.att_id != -1) {
      attr_obj = (NclAtt) _NclGetObj(data.u.data_var->var.att_id);
      if (attr_obj == NULL) {
        break;
      }
    }
    else {
/*
 * att_id == -1, no attributes.
 */
      break;
    }
/* 
 * Check attributes for "trace". If none, then just proceed as normal.
 */
    if (attr_obj->att.n_atts == 0) {
      break;
    }
    else {
/* 
 * att_n_atts > 0, retrieve optional arguments 
 */
      attr_list = attr_obj->att.att_list;
      while (attr_list != NULL && !return_trace) {
        if ((strcmp(attr_list->attname, "trace")) == 0) {
          return_trace = True;
          trace        = attr_list->attvalue->multidval.val;
          type_trace_class = (NclTypeClass)(_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(attr_list->attvalue->multidval.data_type))));
        }
        attr_list = attr_list->next;
      }
    }
    
  default:
    break;
  }

/*
 * Calculate size of output array.
 */
  nfac = dsizes_evec[0];

  nvar = 1;
  for( i = 1; i <= ndims_evec-1; i++ ) {
    nvar *= dsizes_evec[i];
  }
  ldevec = nvar;

/*
 * Test dimension sizes. 
 */
  if((nvar > INT_MAX) || (nfac > INT_MAX) || (ldevec > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eof_varimax: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  invar = (int) nvar;
  infac = (int) nfac;
  ildevec = (int) ldevec;

  if( nvar < 1 || nfac < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eof_varimax: The dimensions of the input array must both be at least 1");
    return(NhlFATAL);
  }
  total_size_evec = nvar * nfac;

/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_evec,has_missing_evec,&missing_evec,
                 &missing_devec,NULL);
/*
 * Coerce evec to double no matter what, since we need to make a copy of
 * the input array anyway.
 */
  devec = (double *)calloc(total_size_evec,sizeof(double));
  a     = (double *)calloc(nvar,sizeof(double));
  b     = (double *)calloc(nvar,sizeof(double));
  pcvar = (float *)calloc(nfac,sizeof(float));
  if( devec == NULL || a == NULL || b == NULL || pcvar == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eof_varimax: Unable to allocate memory for  input/output arrays");
    return(NhlFATAL);
  }
  coerce_subset_input_double(evec,devec,0,type_evec,total_size_evec,
                             has_missing_evec,&missing_evec,&missing_devec);
/*
 * Check for a missing value.
 */
  if(contains_missing(devec,total_size_evec,has_missing_evec,
                      missing_devec.doubleval)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eof_varimax: The input array contains missing values.");
    return(NhlFATAL);
  }

/*
 * Allocate space for output arrays.
 */
  if(type_evec != NCL_double) {
    type_evec_out = NCL_float;
    evec_out      = (void*)calloc(total_size_evec,sizeof(float));
    if(evec_out == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eof_varimax: Unable to allocate memory for work array");
      return(NhlFATAL);
    }
  }
  else {
/*
 * We've already allocated a double precision output variable, so
 * just point to it.
 */ 
    type_evec_out = NCL_double;
    evec_out      = (void*)devec;
  }
/*
 * Allocate memory for work array.
 */
  w = (double *)calloc(nvar,sizeof(double));
  if(w == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eof_varimax: Unable to allocate memory for work array");
    return(NhlFATAL);
  }

/*
 * Call the Fortran 77 version of 'vors' with the full argument list.
 */
  NGCALLF(vors,VORS)(&invar, &infac, devec, a, b, w, &ildevec);

/*
 * Free unneeded memory.
 */
  NclFree(w);
  NclFree(b);

/*
 * Return pcvar as float no matter what.
 */
  for( i = 0; i < nfac; i++ ) pcvar[i] = (float)a[i];
  NclFree(a);

  if(type_evec_out == NCL_float) {
/*
 * Need to coerce output array back to float before we return it.
 */
    coerce_output_float_only(evec_out,devec,total_size_evec,0);
    NclFree(devec);
  }

/*
 * Set up return value.
 */
  type_evec_out_class = (NclTypeClass)(_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(type_evec_out))));
  return_md = _NclCreateVal(
                            NULL,
                            NULL,
                            Ncl_MultiDValData,
                            0,
                            evec_out,
                            NULL,
                            ndims_evec,
                            dsizes_evec,
                            TEMPORARY,
                            NULL,
                            (NclObjClass)type_evec_out_class
                            );

/*
 * Initialize att_id so we can return some attributes.
 */
  att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

  dsizes[0] = nfac;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         pcvar,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypefloatClass
                         );
  _NclAddAtt(
             att_id,
             "pcvar_varimax",
             att_md,
             NULL
             );


/*
 * Return "trace" attribute, if it exists.
 */
  if(return_trace) {
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
                           (NclObjClass)type_trace_class
                           );
    _NclAddAtt(
               att_id,
               "trace",
               att_md,
               NULL
               );

  }

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

NhlErrorTypes eofunc_varimax_W( void )
{
/*
 * Input array variables
 */
  void *evec = NULL, *trace = NULL, *opt = NULL;
  double *devec;
  double *deval = NULL; 
  double *dpcvar = NULL; 
  ng_size_t dsizes_evec[NCL_MAX_DIMENSIONS];
  int ndims_evec, has_missing_evec;
  NclScalar missing_evec, missing_devec, missing_revec, missing_evec_out;
  NclBasicDataTypes type_evec, type_eval, type_pcvar, type_opt;
  NclTypeClass type_trace_class = NCL_none;

/*
 * Various and work array variables.
 */
  double *drotvar;
  int iopt, kflag;
  ng_size_t nvar, nfac, ldevec;
  ng_size_t total_size_evec;

/*
 * Variables for retrieving attributes from "opt".
 */
  NclAttList  *attr_list;
  NclAtt  attr_obj;
  NclStackEntry   stack_entry;

/*
 * Output array variables
 */
  void  *evec_out;
  NclBasicDataTypes type_evec_out;
  NclTypeClass type_evec_out_class;

/*
 * Variables for returning attributes.
 */
  int att_id;
  ng_size_t dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
  int i, invar, infac, ildevec;
  void *rotvar;
  float *pcvar;
  logical return_trace = False, found_eval = False, found_pcvar = False;

/*
 * Retrieve evec.
 */
  evec = (void*)NclGetArgValue(
           0,
           2,
           &ndims_evec, 
           dsizes_evec,
           &missing_evec,
           &has_missing_evec,
           &type_evec,
           DONT_CARE);

  nfac = dsizes_evec[0];

/*
 * Retrieve opt. As of version 4.3.0, it is now used to indicate
 * if the eigenvectors should be scaled (and maybe rotated).
 */
  opt = (void*)NclGetArgValue(
           1,
           2,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_opt,
           DONT_CARE);

  if(type_opt != NCL_int && type_opt != NCL_logical) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_varimax: 'opt' must be an integer or a logical scalar");
    return(NhlFATAL);
  }
  if(type_opt == NCL_logical) {
    if( ((logical*)opt)[0])  {
      iopt = 1;
    }
    else {
      iopt = 0;
    }
  }
  else {
    iopt = ((int*)opt)[0];
  }
/*
 * Check dimensions.
 */
  if( ndims_evec < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_varimax: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }

/*
 * Loop through the attributes that may be attached to the "evec" variable.
 * Check if the input data array has "trace" set as an attribute.
 * If so, make sure it gets returned.
 * 
 * Attributes "eval" and "pcvar" *must* be found, or an error will result.
 */
  stack_entry = _NclGetArg(0, 2, DONT_CARE);
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
 * Loop through attributes and check them. The only three recognized are
 * "trace", "pcvar", and "eval".
 */
      while (attr_list != NULL) {
/*
 * Check for "trace".
 */
        if (!strcmp(attr_list->attname, "trace")) {
          return_trace = True;
          trace        = attr_list->attvalue->multidval.val;
          type_trace_class = (NclTypeClass)(_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(attr_list->attvalue->multidval.data_type))));
        }
/*
 * Check for "eval".
 */
        else if (!strcmp(attr_list->attname, "eval")) {
          found_eval = True;
          type_eval = attr_list->attvalue->multidval.data_type;
          deval = coerce_input_double(attr_list->attvalue->multidval.val,
                                      type_eval,nfac,0,NULL,NULL);
        }
/*
 * Check for "pcvar".
 */
        else if (!strcmp(attr_list->attname, "pcvar")) {
          found_pcvar = True;
          type_pcvar = attr_list->attvalue->multidval.data_type;
          dpcvar = coerce_input_double(attr_list->attvalue->multidval.val,
                                       type_pcvar,nfac,0,NULL,NULL);
        }
        attr_list = attr_list->next;
      }
    }
  default:
    break;
  }
/*
 * Return an error if we didn't find eval or pcvar attributes.
 */
  if( !found_eval || !found_pcvar ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_varimax: Could not find attributes 'pcvar' or 'eval' attached to the input array. These should be present if you used eofunc.");
    return(NhlFATAL);
  }

/*
 * Calculate size of output array.  "nvar" represents all but the first
 * dimension.
 */
  nvar = 1;
  for( i = 1; i <= ndims_evec-1; i++ ) {
    nvar *= dsizes_evec[i];
  }
  ldevec = nvar;

  if( nvar < 1 || nfac < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_varimax: The dimensions of the input array must both be at least 1");
    return(NhlFATAL);
  }
  total_size_evec = nvar * nfac;

/*
 * Test input dimension sizes.
 */
  if((nvar > INT_MAX) || (nfac > INT_MAX) || (ldevec > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_varimax: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  invar = (int) nvar;
  infac = (int) nfac;
  ildevec = (int) ldevec;

/*
 * Coerce missing values, if any. Prior to 4.2.0.a034, this routine
 * didn't handle missing values.
 */
  coerce_missing(type_evec,has_missing_evec,&missing_evec,&missing_devec,
                 &missing_revec);
/*
 * Coerce evec to double no matter what, since we need to make a copy of
 * the input array anyway.
 */
  devec   = (double *)calloc(total_size_evec,sizeof(double));
  drotvar = (double *)calloc(nfac,sizeof(double));
  pcvar   = (float *)calloc(nfac,sizeof(float));
  if(type_evec != NCL_double) {
    rotvar = (float *)calloc(nfac,sizeof(float));
  }
  else {
    rotvar = (double *)calloc(nfac,sizeof(double));
  }
  if( devec == NULL || drotvar == NULL || pcvar == NULL || rotvar == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_varimax: Unable to allocate memory for input/output arrays");
    return(NhlFATAL);
  }
  coerce_subset_input_double(evec,devec,0,type_evec,total_size_evec,
                             has_missing_evec,&missing_evec,&missing_devec);
/*
 * Allocate space for output array.
 */
  if(type_evec != NCL_double) {
    type_evec_out = NCL_float;
    evec_out      = (void*)calloc(total_size_evec,sizeof(float));
    if(evec_out == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_varimax: Unable to allocate memory for work array");
      return(NhlFATAL);
    }
  }
  else {
/*
 * We've already allocated a double precision output variable, so
 * just point to it.
 */ 
    type_evec_out = NCL_double;
    evec_out      = (void*)devec;
  }

/*
 * Call the Fortran 77 version of 'roteof' with the full argument list.
 */
  NGCALLF(roteof,ROTEOF)(&invar, &infac, devec, &ildevec, deval, dpcvar, drotvar,
                         &missing_devec.doubleval,&iopt,&kflag);

  if(type_evec_out == NCL_float) {
/*
 * Need to coerce evec and rotvar back to float before we return them.
 */
    coerce_output_float_only(evec_out,devec,total_size_evec,0);
    coerce_output_float_only(rotvar,drotvar,nfac,0);
   
    if(kflag == 1) {
      missing_evec_out = missing_revec;
    }
    NclFree(devec);
    NclFree(drotvar);
  }
  else {
    rotvar = (void*)drotvar;
    if(kflag == 1) {
      missing_evec_out = missing_devec;
    }
  }

/*
 * Return pcvar as float no matter what.
 */
  for( i = 0; i < nfac; i++ ) pcvar[i] = (float)dpcvar[i];

/*
 * Set up return value. If kflag is 1, then this means missing values
 * are in the input, and we need to return a missing value.
 */
  type_evec_out_class = (NclTypeClass)(_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(type_evec_out))));
  if(kflag == 1) {
    return_md = _NclCreateVal(
                            NULL,
                            NULL,
                            Ncl_MultiDValData,
                            0,
                            evec_out,
                            &missing_evec_out,
                            ndims_evec,
                            dsizes_evec,
                            TEMPORARY,
                            NULL,
                            (NclObjClass)type_evec_out_class
                            );
  }
  else { 
    return_md = _NclCreateVal(
                            NULL,
                            NULL,
                            Ncl_MultiDValData,
                            0,
                            evec_out,
                            NULL,
                            ndims_evec,
                            dsizes_evec,
                            TEMPORARY,
                            NULL,
                            (NclObjClass)type_evec_out_class
                            );
  }

/*
 * Initialize att_id so we can return some attributes.
 */
  att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

  dsizes[0] = nfac;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         pcvar,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypefloatClass
                         );
  _NclAddAtt(
             att_id,
             "pcvar_varimax",
             att_md,
             NULL
             );

  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         rotvar,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)type_evec_out_class
                         );
  _NclAddAtt(
             att_id,
             "variance_varimax",
             att_md,
             NULL
             );


/*
 * Return "trace" attribute, if it exists.
 *
 */
  if(return_trace) {
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
                           (NclObjClass)type_trace_class
                           );
    _NclAddAtt(
               att_id,
               "trace",
               att_md,
               NULL
               );

  }

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
 * Free memory.
 */
  if(type_eval  != NCL_double) NclFree(deval);
  if(type_pcvar != NCL_double) NclFree(dpcvar);

/*
 * Return output grid and attributes to NCL.
 */
  return_data.kind = NclStk_VAR;
  return_data.u.data_var = tmp_var;
  _NclPlaceReturn(return_data);
  return(NhlNOERROR);

}

NhlErrorTypes eofunc_varimax_jl_W( void )
{
/*
 * Input array variables
 */
  void *evec = NULL, *trace = NULL, *opt = NULL;
  double *devec;
  double *deval = NULL;
  double *dpcvar = NULL;
  ng_size_t dsizes_evec[NCL_MAX_DIMENSIONS];
  int ndims_evec, has_missing_evec;
  NclScalar missing_evec, missing_devec, missing_revec, missing_evec_out;
  NclBasicDataTypes type_evec, type_eval, type_pcvar, type_opt;
  NclTypeClass type_trace_class = NCL_none;

/*
 * Various and work array variables.
 */
  double *drotvar;
  int iopt, kflag;
  ng_size_t nvar, nfac, ldevec;
  ng_size_t total_size_evec;

/*
 * Variables for retrieving attributes from "opt".
 */
  NclAttList  *attr_list;
  NclAtt  attr_obj;
  NclStackEntry   stack_entry;

/*
 * Output array variables
 */
  void  *evec_out;
  NclBasicDataTypes type_evec_out;
  NclTypeClass type_evec_out_class;

/*
 * Variables for returning attributes.
 */
  int att_id;
  ng_size_t dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
  int i, invar, infac, ildevec;
  void *rotvar;
  float *pcvar;
  logical return_trace = False, found_eval = False, found_pcvar = False;

/*
 * Retrieve evec.
 */
  evec = (void*)NclGetArgValue(
           0,
           2,
           &ndims_evec, 
           dsizes_evec,
           &missing_evec,
           &has_missing_evec,
           &type_evec,
           DONT_CARE);

  nfac = dsizes_evec[0];

/*
 * Retrieve opt. As of version 4.3.0, it is now used to indicate
 * if the eigenvectors should be scaled (and maybe rotated).
 */
  opt = (void*)NclGetArgValue(
           1,
           2,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_opt,
           DONT_CARE);

  if(type_opt != NCL_int && type_opt != NCL_logical) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_varimax_jl: 'opt' must be an integer or a logical scalar");
    return(NhlFATAL);
  }
  if(type_opt == NCL_logical) {
    if( ((logical*)opt)[0])  {
      iopt = 1;
    }
    else {
      iopt = 0;
    }
  }
  else {
    iopt = ((int*)opt)[0];
  }
/*
 * Check dimensions.
 */
  if( ndims_evec < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_varimax_jl: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }

/*
 * Loop through the attributes that may be attached to the "evec" variable.
 * Check if the input data array has "trace" set as an attribute.
 * If so, make sure it gets returned.
 * 
 * Attributes "eval" and "pcvar" *must* be found, or an error will result.
 */
  stack_entry = _NclGetArg(0, 2, DONT_CARE);
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
 * Loop through attributes and check them. The only three recognized are
 * "trace", "pcvar", and "eval".
 */
      while (attr_list != NULL) {
/*
 * Check for "trace".
 */
        if (!strcmp(attr_list->attname, "trace")) {
          return_trace = True;
          trace        = attr_list->attvalue->multidval.val;
          type_trace_class = (NclTypeClass)(_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(attr_list->attvalue->multidval.data_type))));
        }
/*
 * Check for "eval".
 */
        else if (!strcmp(attr_list->attname, "eval")) {
          found_eval = True;
          type_eval = attr_list->attvalue->multidval.data_type;
          deval = coerce_input_double(attr_list->attvalue->multidval.val,
                                      type_eval,nfac,0,NULL,NULL);
        }
/*
 * Check for "pcvar".
 */
        else if (!strcmp(attr_list->attname, "pcvar")) {
          found_pcvar = True;
          type_pcvar = attr_list->attvalue->multidval.data_type;
          dpcvar = coerce_input_double(attr_list->attvalue->multidval.val,
                                       type_pcvar,nfac,0,NULL,NULL);
        }
        attr_list = attr_list->next;
      }
    }
  default:
    break;
  }
/*
 * Return an error if we didn't find eval or pcvar attributes.
 */
  if( !found_eval || !found_pcvar ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_varimax_jl: Could not find attributes 'pcvar' or 'eval' attached to the input array. These should be present if you used eofunc.");
    return(NhlFATAL);
  }

/*
 * Calculate size of output array.  "nvar" represents all but the first
 * dimension.
 */
  nvar = 1;
  for( i = 1; i <= ndims_evec-1; i++ ) {
    nvar *= dsizes_evec[i];
  }
  ldevec = nvar;

  if( nvar < 1 || nfac < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_varimax_jl: The dimensions of the input array must both be at least 1");
    return(NhlFATAL);
  }
  total_size_evec = nvar * nfac;

/*
 * Test input dimension sizes.
 */
  if((nvar > INT_MAX) || (nfac > INT_MAX) || (ldevec > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_varimax_jl: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  invar = (int) nvar;
  infac = (int) nfac;
  ildevec = (int) ldevec;

/*
 * Coerce missing values, if any. Prior to 4.2.0.a034, this routine
 * didn't handle missing values.
 */
  coerce_missing(type_evec,has_missing_evec,&missing_evec,&missing_devec,
                 &missing_revec);
/*
 * Coerce evec to double no matter what, since we need to make a copy of
 * the input array anyway.
 */
  devec   = (double *)calloc(total_size_evec,sizeof(double));
  drotvar = (double *)calloc(nfac,sizeof(double));
  pcvar   = (float *)calloc(nfac,sizeof(float));
  if(type_evec != NCL_double) {
    rotvar = (float *)calloc(nfac,sizeof(float));
  }
  else {
    rotvar = (double *)calloc(nfac,sizeof(double));
  }
  if( devec == NULL || drotvar == NULL || pcvar == NULL || rotvar == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_varimax_jl: Unable to allocate memory for input/output arrays");
    return(NhlFATAL);
  }
  coerce_subset_input_double(evec,devec,0,type_evec,total_size_evec,
                             has_missing_evec,&missing_evec,&missing_devec);
/*
 * Allocate space for output array.
 */
  if(type_evec != NCL_double) {
    type_evec_out = NCL_float;
    evec_out      = (void*)calloc(total_size_evec,sizeof(float));
    if(evec_out == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_varimax_jl: Unable to allocate memory for work array");
      return(NhlFATAL);
    }
  }
  else {
/*
 * We've already allocated a double precision output variable, so
 * just point to it.
 */ 
    type_evec_out = NCL_double;
    evec_out      = (void*)devec;
  }

/*
 * Call the Fortran 77 version of 'roteof' with the full argument list.
 */
  NGCALLF(zroteof,ZROTEOF)(&invar, &infac, devec, &ildevec, deval, dpcvar, drotvar,
                           &missing_devec.doubleval,&iopt,&kflag);

  if(type_evec_out == NCL_float) {
/*
 * Need to coerce evec and rotvar back to float before we return them.
 */
    coerce_output_float_only(evec_out,devec,total_size_evec,0);
    coerce_output_float_only(rotvar,drotvar,nfac,0);
   
    if(kflag == 1) {
      missing_evec_out = missing_revec;
    }
    NclFree(devec);
    NclFree(drotvar);
  }
  else {
    rotvar = (void*)drotvar;
    if(kflag == 1) {
      missing_evec_out = missing_devec;
    }
  }

/*
 * Return pcvar as float no matter what.
 */
  for( i = 0; i < nfac; i++ ) pcvar[i] = (float)dpcvar[i];

/*
 * Set up return value. If kflag is 1, then this means missing values
 * are in the input, and we need to return a missing value.
 */
  type_evec_out_class = (NclTypeClass)(_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(type_evec_out))));
  if(kflag == 1) {
    return_md = _NclCreateVal(
                            NULL,
                            NULL,
                            Ncl_MultiDValData,
                            0,
                            evec_out,
                            &missing_evec_out,
                            ndims_evec,
                            dsizes_evec,
                            TEMPORARY,
                            NULL,
                            (NclObjClass)type_evec_out_class
                            );
  }
  else { 
    return_md = _NclCreateVal(
                            NULL,
                            NULL,
                            Ncl_MultiDValData,
                            0,
                            evec_out,
                            NULL,
                            ndims_evec,
                            dsizes_evec,
                            TEMPORARY,
                            NULL,
                            (NclObjClass)type_evec_out_class
                            );
  }

/*
 * Initialize att_id so we can return some attributes.
 */
  att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

  dsizes[0] = nfac;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         pcvar,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypefloatClass
                         );
  _NclAddAtt(
             att_id,
             "pcvar_varimax",
             att_md,
             NULL
             );

  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         rotvar,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)type_evec_out_class
                         );
  _NclAddAtt(
             att_id,
             "variance_varimax",
             att_md,
             NULL
             );


/*
 * Return "trace" attribute, if it exists.
 *
 */
  if(return_trace) {
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
                           (NclObjClass)type_trace_class
                           );
    _NclAddAtt(
               att_id,
               "trace",
               att_md,
               NULL
               );

  }

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


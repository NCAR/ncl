#include <stdio.h>
#include <math.h>
#include "wrapper.h"

extern void NGCALLF(vors,VORS)(int *, int *, double *, double *, double *, 
                               double *, int *);

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
  int *dsizes_evec, ndims_evec, has_missing_evec;
  NclScalar missing_evec, missing_devec;
  int nvar, nfac, ldevec, total_size_evec;
  NclBasicDataTypes type_evec;
  NclTypeClass type_trace_class;

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
  int dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
  int i;
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
  devec = (double*)calloc(total_size_evec,sizeof(double));
  if( devec == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eof_varimax: Unable to allocate memory for coercing evec array to double precision");
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
 * Allocate space for output array.
 */
  if(type_evec != NCL_double) {
    type_evec_out = NCL_float;
    evec_out      = (void*)calloc(total_size_evec,sizeof(float));
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
 * Allocate memory for work arrays.
 */
  a = (double *)calloc(nvar,sizeof(double));
  b = (double *)calloc(nvar,sizeof(double));
  w = (double *)calloc(nvar,sizeof(double));
  if( a == NULL || b == NULL || w == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eof_varimax: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
/*
 * Call the Fortran 77 version of 'vors' with the full argument list.
 */
  NGCALLF(vors,VORS)(&nvar, &nfac, devec, a, b, w, &ldevec);

/*
 * Free unneeded memory.
 */
  NclFree(w);
  NclFree(a);
  NclFree(b);

  if(type_evec_out == NCL_float) {
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
 * Return "trace" attribute, if it exists.
 *
 * Be sure to initialize att_id, even if there's no 
 * attribute to return.
 */
  att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);
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

NhlErrorTypes eof_varimax2_W( void )
{
/*
 * Input array variables
 */
  void *evec = NULL, *trace = NULL;
  double *devec;
  int dsizes_evec[NCL_MAX_DIMENSIONS], ndims_evec, has_missing_evec;
  logical *opt;
  NclScalar missing_evec, missing_devec;
  NclBasicDataTypes type_evec;
  NclTypeClass type_trace_class;

/*
 * Various and work array variables.
 */
  double *a, *b, *w;
  int nvar, nfac, ldevec, total_size_evec;

/*
 * Variables for retrieving attributes from "opt".
 */
  NclAttList  *attr_list;
  NclAtt  attr_obj;
  NclStackEntry   stack_entry;

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
  int dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
  int i;
  logical return_trace = False;

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
           2);

/*
 * Retrieve opt. Currently it is not used for anything.
 */
  opt = (logical*)NclGetArgValue(
           1,
           2,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           2);

/*
 * Check dimensions.
 */
  if( ndims_evec < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_varimax: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }

/*
 * Check if the input data array has "trace" set as an attribute.
 * If so, make sure it gets returned.
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
 * Loop through attributes and check them. The only one recognized is
 * "trace".
 */
      while (attr_list != NULL && !return_trace) {
/*
 * Check for "trace".
 */
        if (!strcmp(attr_list->attname, "trace")) {
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

  if( nvar < 1 || nfac < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_varimax: The dimensions of the input array must both be at least 1");
    return(NhlFATAL);
  }
  total_size_evec = nvar * nfac;

/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_evec,has_missing_evec,&missing_evec,&missing_devec,
                 NULL);
/*
 * Coerce evec to double no matter what, since we need to make a coyp of
 * the input array anyway.
 */
  devec = (double*)calloc(total_size_evec,sizeof(double));
  if( devec == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_varimax: Unable to allocate memory for coercing evec array to double precision");
    return(NhlFATAL);
  }
  coerce_subset_input_double(evec,devec,0,type_evec,total_size_evec,
                             has_missing_evec,&missing_evec,&missing_devec);
/*
 * Check for a missing value.
 */
  if(contains_missing(devec,total_size_evec,has_missing_evec,
                      missing_devec.doubleval)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_varimax: The input array contains missing values.");
    return(NhlFATAL);
  }

/*
 * Allocate space for output array.
 */
  if(type_evec != NCL_double) {
    type_evec_out = NCL_float;
    evec_out      = (void*)calloc(total_size_evec,sizeof(float));
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
 * Allocate memory for work arrays.
 */
  a = (double *)calloc(nvar,sizeof(double));
  b = (double *)calloc(nvar,sizeof(double));
  w = (double *)calloc(nvar,sizeof(double));
  if( a == NULL || b == NULL || w == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_varimax: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
/*
 * Call the Fortran 77 version of 'vors' with the full argument list.
 */
  NGCALLF(vors,VORS)(&nvar, &nfac, devec, a, b, w, &ldevec);

/*
 * Free unneeded memory.
 */
  NclFree(w);
  NclFree(a);
  NclFree(b);

  if(type_evec_out == NCL_float) {
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
 * Return "trace" attribute, if it exists.
 *
 * Be sure to initialize att_id, even if there's no 
 * attribute to return.
 */
  att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);
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

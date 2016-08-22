#include <stdio.h>
#include <strings.h>
#include "wrapper.h"

extern void NGCALLF(buttfilt,BUTTFILT)(double *, double *,double *, 
                                       double *, double *, double *, 
                                       int *, int *, int *, int *);

NhlErrorTypes bw_bandpass_filter_W( void )
{

/*
 * Input variables
 */
/*
 * Argument # 0
 */
  void *xr;
  double *tmp_xr;
  int       ndims_xr;
  ng_size_t dsizes_xr[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_xr;

/*
 * Argument # 1
 */
  void *fca;
  double *tmp_fca;
  NclBasicDataTypes type_fca;

/*
 * Argument # 2
 */
  void *fcb;
  double *tmp_fcb;
  NclBasicDataTypes type_fcb;

/*
 * Argument # 3
 */
  logical *opt;

/*
 * Argument # 4
 */
  int *dims;
  ng_size_t ndims;
/*
 * Return variable
 */
  void *bf;
  int ndims_bf;
  double *tmp_yr, *tmp_er;
  ng_size_t *dsizes_bf;
  NclBasicDataTypes type_bf;

/*
 * Variables for retrieving attributes from "opt".
 */
  NclAttList  *attr_list;
  NclAtt  attr_obj;
  NclStackEntry stack_entry;
  logical set_dt = False, rmv_mean = True, ret_filt = True, ret_env = False;
  int m=6, iflag;
  void *dt;
  double *tmp_dt;
  NclBasicDataTypes type_dt;

/*
 * Various
 */
  ng_size_t i, nx, total_nl, total_nr, nrnx;
  ng_size_t index_xr, index_nrx, size_xr, size_output;
  int j, inx, ret, ier;

/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
/*
 * Get argument # 0
 */
  xr = (void*)NclGetArgValue(
           0,
           5,
           &ndims_xr,
           dsizes_xr,
           NULL,
           NULL,
           &type_xr,
           DONT_CARE);

/*
 * Get argument # 1
 */
  fca = (void*)NclGetArgValue(
           1,
           5,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_fca,
           DONT_CARE);
/*
 * Get argument # 2
 */
  fcb = (void*)NclGetArgValue(
           2,
           5,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_fcb,
           DONT_CARE);
/*
 * Get argument # 3
 */
  opt = (logical*)NclGetArgValue(
           3,
           5,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

/*
 * Get argument # 4
 */
  dims = (int *)NclGetArgValue(4,5,NULL,&ndims,NULL,NULL,NULL,DONT_CARE);

/*
 * Some error checking. Make sure input dimension is valid.
 */
  if(ndims > ndims_xr) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"bw_bandpass_filter: too many dimensions in dimension argument, can't continue");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims; i++ ) {
    if(dims[i] < 0 || dims[i] >= ndims_xr) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"bw_bandpass_filter: Invalid dimension argument, can't continue");
      return(NhlFATAL);
    }
    if(i > 0 && dims[i] != (dims[i-1]+1)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"bw_bandpass_filter: Input dimension sizes must be monotonically increasing, can't continue");
      return(NhlFATAL);
    }
  }

/*
 * Check for attributes attached to "opt"
 *
 *   "m"               - 6
 *   "dt"              - 1.0
 *   "remove_mean"     - True
 *   "return_filtered" - True
 *   "return_envelope" - False
 */
  if(*opt) {
    stack_entry = _NclGetArg(3, 5, DONT_CARE);
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
 * Loop through attributes and check them.
 */
        while (attr_list != NULL) {
          if(!strcasecmp(attr_list->attname, "remove_mean")) {
            rmv_mean = *(logical *) attr_list->attvalue->multidval.val;
          }
          else if(!strcasecmp(attr_list->attname, "return_filtered")) {
            ret_filt = *(logical *) attr_list->attvalue->multidval.val;
          }
          else if(!strcasecmp(attr_list->attname, "return_envelope")) {
            ret_env = *(logical *) attr_list->attvalue->multidval.val;
          }
          else if(!strcasecmp(attr_list->attname, "dt")) {
            dt      = attr_list->attvalue->multidval.val;
            type_dt = attr_list->attvalue->multidval.data_type;
            set_dt  = True;
          }
          else if(!strcasecmp(attr_list->attname, "m")) {
            m = *(int *) attr_list->attvalue->multidval.val;
          }
          attr_list = attr_list->next;
        }
      default:
        break;
      }
    }
  }

/*
 * Provide default for dt if not specified by user.
 */
  if(set_dt) {
    tmp_dt = coerce_input_double(dt,type_dt,1,0,NULL,NULL);
  }
  else {
    type_dt = NCL_double;
    tmp_dt  = (double *)calloc(1,sizeof(double));
    *tmp_dt = 1.0;
  }

  if(!ret_filt && !ret_env) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"bw_bandpass_filter: both return_filtered and return_envelope are False. One of these must be True");
    return(NhlFATAL);
  }

/*
 * Calculate size and dimension sizes of output array.
 *
 * If both ret_filt and ret_env are True, then the
 * return array will be 2 x k x ...
 * Otherwise it will be k x ...
 *
 */
  if(ret_filt && ret_env) ndims_bf = ndims_xr + 1;
  else                    ndims_bf = ndims_xr;

  dsizes_bf = (ng_size_t*)calloc(ndims_bf,sizeof(ng_size_t));  
  if( dsizes_bf == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"bw_bandpass_filter: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }

  if(ret_filt && ret_env) dsizes_bf[0] = 2;
  for(i = 0; i < ndims_xr; i++) 
    dsizes_bf[i+(ndims_bf-ndims_xr)] = dsizes_xr[i];

/*
 * Calculate number of leftmost, rightmost, and middle elements.
 */
  nx = total_nl = total_nr = 1;
  for(i = 0; i < ndims ; i++)                 nx = nx*dsizes_xr[dims[i]];
  for(i = 0; i < dims[0]; i++)                total_nl *= dsizes_xr[i];
  for(i = dims[ndims-1]+1; i < ndims_xr; i++) total_nr *= dsizes_xr[i];

/*
 * Calculate xr and output sizes.
 */
  size_xr = total_nr * total_nl * nx;
  if(ret_filt && ret_env) size_output = size_xr * 2;
  else                    size_output = size_xr;

  if(nx > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"bw_bandpass_filter: nx = %ld is greater than INT_MAX", nx);
    return(NhlFATAL);
  }
  inx = (int) nx;

/*
 * Coerce fca, fcb to double, if needed.
 */
  tmp_fca = coerce_input_double(fca,type_fca,1,0,NULL,NULL);
  tmp_fcb = coerce_input_double(fcb,type_fcb,1,0,NULL,NULL);
  if(tmp_fca == NULL || tmp_fcb == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"bw_bandpass_filter: Unable to allocate memory for coercing input scalars to double");
    return(NhlFATAL);
  }

/*
 * Allocate space for input array no matter what, because it 
 * may not be contiguous in memory.
 */
  tmp_xr = (double *)calloc(nx,sizeof(double));
  if(tmp_xr == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"bw_bandpass_filter: Unable to allocate memory for coercing input array to double");
    return(NhlFATAL);
  }
/*
 * Return type.
 */
  if(type_xr != NCL_double) type_bf = NCL_float;
  else                      type_bf = NCL_double;

/* 
 * Allocate space for output array.
 */
  if(type_bf != NCL_double) bf = (void *)calloc(size_output, sizeof(float));
  else                      bf = (void *)calloc(size_output, sizeof(double));
  if(bf == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"bw_bandpass_filter: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Allocate space for subset of output array.
 */
  tmp_yr = (double *)calloc(nx, sizeof(double));
  tmp_er = (double *)calloc(nx, sizeof(double));
  if(tmp_yr == NULL || tmp_er == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"bw_bandpass_filter: Unable to allocate memory for temporary output arrays");
    return(NhlFATAL);
  }

/*
 * Loop across leftmost/rightmost dimensions and call 
 * the Fortran routine for each subsection of the 
 * input arrays.
 */
  nrnx = total_nr * nx;
  if(rmv_mean) iflag = 1;
  else         iflag = 0;
  for(i = 0; i < total_nl; i++) {
    index_nrx = i*nrnx;
    for(j = 0; j < total_nr; j++) {
      index_xr = index_nrx + j;
/*
 * Coerce subsection of x (tmp_xr) to double if necessary.
 */
      coerce_subset_input_double_step(xr,tmp_xr,index_xr,total_nr,type_xr,
                                      nx,0,NULL,NULL);
/*
 * Call the Fortran routine.
 */
      NGCALLF(buttfilt,BUTTFILT)(tmp_xr, tmp_yr, tmp_er, tmp_fca, tmp_fcb, 
                                 tmp_dt, &m, &inx, &iflag, &ier);
/*
 * Copy/coerce back to output array
 */
      if(ret_filt && !ret_env) {
        coerce_output_float_or_double_step(bf,tmp_yr,type_bf,nx,
                                           index_xr,total_nr);
      }
      else if(!ret_filt && ret_env) {
        coerce_output_float_or_double_step(bf,tmp_er,type_bf,nx,
                                           index_xr,total_nr);
      }
      else {
        coerce_output_float_or_double_step(bf,tmp_yr,type_bf,nx,
                                           index_xr,total_nr);
        coerce_output_float_or_double_step(bf,tmp_er,type_bf,nx,
                                           index_xr+size_xr,total_nr);
      }
    }
  }

/*
 * Free unneeded memory.
 */
  NclFree(tmp_xr);
  NclFree(tmp_er);
  NclFree(tmp_yr);
  if(type_fca != NCL_double) NclFree(tmp_fca);
  if(type_fcb != NCL_double) NclFree(tmp_fcb);
  if(!set_dt || type_dt != NCL_double) NclFree(tmp_dt);

/*
 * Return value back to NCL script.
 */
  ret = NclReturnValue(bf,ndims_bf,dsizes_bf,NULL,type_bf,0);
  NclFree(dsizes_bf);
  return(ret);
}

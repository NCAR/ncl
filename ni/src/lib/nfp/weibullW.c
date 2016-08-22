#include <stdio.h>
#include <strings.h>
#include "wrapper.h"

extern void NGCALLF(weibfit,WEIBFIT)(int *, double *, double *, int *, 
                                     double *, double *, int *);

NhlErrorTypes weibull_W( void )
{

/*
 * Input variables
 */
/*
 * Argument # 0
 */
  void *x;
  double *tmp_x;
  int       ndims_x;
  ng_size_t nx, dsizes_x[NCL_MAX_DIMENSIONS];
  int inx, has_missing_x;
  NclScalar missing_x, missing_dbl_x;
  NclBasicDataTypes type_x;

/*
 * Argument # 1
 */
  int *dims;
  ng_size_t ndims;
/*
 * Argument # 2
 */
  logical *opt;
/*
 * Return variable
 */
  void *wb;
  double tmp_wb[6];
  int       ndims_wb;
  ng_size_t *dsizes_wb;
  NclScalar missing_wb;
  NclBasicDataTypes type_wb;

/*
 * Variables for retrieving attributes from "opt".
 */
  NclAttList  *attr_list;
  NclAtt  attr_obj;
  NclStackEntry stack_entry;
  int nmin, set_nmin, set_confi;
  void *confi;
  double *tmp_confi;
  NclBasicDataTypes type_confi;

/*
 * Various
 */
  ng_size_t i, j, nrnx, total_nl, total_nr, total_elements, size_output;
  ng_size_t index_nrx, index_nr, index_x, index_wb;
  int ier, ret;

/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
/*
 * Get argument # 0
 */
  x = (void*)NclGetArgValue(
           0,
           3,
           &ndims_x,
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);

/*
 * Coerce missing value to double if necessary.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dbl_x,NULL);

/*
 * Get argument # 1
 */
  opt = (logical*)NclGetArgValue(
           1,
           3,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

/*
 * Get argument # 2
 */
  dims = (int *)NclGetArgValue(2,3,NULL,&ndims,NULL,NULL,NULL,DONT_CARE);
/*
 * Some error checking. Make sure input dimensions are valid.
 */
  for(i = 0; i < ndims; i++ ) {
    if(dims[i] < 0 || dims[i] >= ndims_x) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"weibull: Invalid dimension sizes to do calculation on, can't continue");
      return(NhlFATAL);
    }
    if(i > 0 && dims[i] != (dims[i-1]+1)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"weibull: Input dimension sizes must be monotonically increasing, can't continue");
      return(NhlFATAL);
    }
  }

/*
 * Check for attributes attached to "opt"
 */
  set_nmin = set_confi = False;

  if(*opt) {
    stack_entry = _NclGetArg(1, 3, DONT_CARE);
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
 * Loop through attributes and check them. We are looking for:
 *
 *  nmin or confi
 */
        while (attr_list != NULL) {
          if(!strcasecmp(attr_list->attname, "nmin")) {
            nmin      = *(int *) attr_list->attvalue->multidval.val;
            set_nmin  = True;
          }
          else if( !strcasecmp(attr_list->attname, "confidence") || 
                   !strcasecmp(attr_list->attname, "confi")){
            confi      = attr_list->attvalue->multidval.val;
            type_confi = attr_list->attvalue->multidval.data_type;
            set_confi  = True;
          }
          attr_list = attr_list->next;
        }
      default:
        break;
      }
    }
  }

/* 
 * Allocate space for output dimension sizes and set them.
 */
  ndims_wb = ndims_x - ndims + 1;
  dsizes_wb = (ng_size_t*)calloc(ndims_wb,sizeof(ng_size_t));  
  if( dsizes_wb == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"weibull: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }

  nx = total_nl = total_nr = 1;
  
  if(ndims_wb == 1) j = 0;
  else              j = 1;

  if(set_confi) dsizes_wb[0] = 6;
  else          dsizes_wb[0] = 2;

  for(i = 0; i < dims[0]; i++) {
    total_nl *= dsizes_x[i];
    dsizes_wb[j+i] = dsizes_x[i];
  }
  for(i = 0; i < ndims ; i++) {
    nx = nx*dsizes_x[dims[i]];
  }
  for(i = dims[ndims-1]+1; i < ndims_x; i++) {
    total_nr *= dsizes_x[i];
    dsizes_wb[j+i-ndims] = dsizes_x[i];
  }
  if(nx > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"weibull: nx = %ld is greater than INT_MAX", nx);
    return(NhlFATAL);
  }
  inx = (int) nx;

/*
 * Provide defaults for nmin and confidence if not specified by user.
 */
  if(!set_nmin) {
    nmin = (int)(0.75*(float)inx);    /* 75% of the total number of points */
  }
  if(set_confi) {
    tmp_confi = coerce_input_double(confi,type_confi,1,0,NULL,NULL);
  }
  else {
    type_confi = NCL_double;
    tmp_confi  = (double *)calloc(1,sizeof(double));
    *tmp_confi = 0.95;
  }
/*
 * Allocate space for tmp_x.
 */
  tmp_x = (double *)calloc(nx,sizeof(double));
  if(tmp_x == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"weibull: Unable to allocate memory for coercing input array to double");
    return(NhlFATAL);
  }

/* 
 * Allocate space for output array.
 */
  total_elements = total_nr * total_nl;
  size_output = dsizes_wb[0] * total_elements;
  if(type_x != NCL_double) {
    type_wb = NCL_float;
    missing_wb.floatval = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
    wb = (void *)calloc(size_output, sizeof(float));
  }
  else {
    type_wb = NCL_double;
    missing_wb.doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
    wb = (void *)calloc(size_output, sizeof(double));
  }
  if(wb == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"weibull: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Loop across leftmost dimensions and call the Fortran routine for each
 * subsection of the input arrays.
 */
  nrnx = total_nr * nx;
  for(i = 0; i < total_nl; i++) {
    index_nrx = i*nrnx;
    index_nr  = i*total_nr;
    for(j = 0; j < total_nr; j++) {
      index_x  = index_nrx + j;
      index_wb = index_nr + j;
/*
 * Coerce subsection of x (tmp_x) to double.
 */
      coerce_subset_input_double_step(x,tmp_x,index_x,total_nr,type_x,
                                      nx,0,NULL,NULL);

/*
 * Call the Fortran routine.
 */
      NGCALLF(weibfit,WEIBFIT)(&inx, tmp_x, &missing_dbl_x.doubleval, 
                               &nmin, tmp_confi, tmp_wb, &ier);

/*
 * Coerce output array to appropriate type
 */
      coerce_output_float_or_double_step(wb,tmp_wb,type_wb,dsizes_wb[0],
                                         index_wb,total_elements);
    }
  }

/*
 * Free unneeded memory.
 */
  NclFree(tmp_x);
  if(type_confi != NCL_double) NclFree(tmp_confi);

/*
 * Return value back to NCL script.
 */
  ret = NclReturnValue(wb,ndims_wb,dsizes_wb,&missing_wb,type_wb,0);

  NclFree(dsizes_wb);
  return(ret);
}

#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(gamfitd3,GAMFITD3)(double *, int *, double *, double *, 
                                       int *, double *, double *, double *,
                                       double *, int *);

NhlErrorTypes dim_gamfit_n_W( void )
{

/*
 * Input variables
 */
/*
 * Argument # 0
 */
  void *x;
  double *tmp_x;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclScalar missing_x, missing_flt_x, missing_dbl_x;
  NclBasicDataTypes type_x;

/*
 * Argument # 1
 */
  logical *optgam;

/*
 * Argument # 2
 */
  int *dims;
  ng_size_t dsizes_dims;

/*
 * Return variable
 */
  void *xpar;
  int ndims_xpar;
  ng_size_t *dsizes_xpar;
  NclScalar missing_xpar;
  NclBasicDataTypes type_xpar;

/*
 * Variables for retrieving attributes from "optgam";
 */
  NclAttList  *attr_list;
  NclAtt  attr_obj;
  NclStackEntry   stack_entry;

/*
 * Various
 */
  ng_size_t npts;
  int inpts;
  ng_size_t index_x, index_xpar, index_nrx, index_nr;
  double *pcrit = NULL;
  logical set_pcrit;
  double alpha, scale, shape, pzero;
  int inv_scale, ier, ret;
  ng_size_t i, j, nrnx, total_nr, total_nl, total_nlnr, size_output;

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
 * Get argument # 1
 */
  optgam = (logical*)NclGetArgValue(
           1,
           3,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

/*
 * Get dimension(s) to do computation on.
 */
  dims = (int*)NclGetArgValue(
           2,
           3,
           NULL,
           &dsizes_dims,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

/*
 * Some error checking. Make sure input dimensions are valid.
 */
  for(i = 0; i < dsizes_dims; i++ ) {
    if(dims[i] < 0 || dims[i] >= ndims_x) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_gamfit_n: Invalid dimension sizes to do calculations across, can't continue");
      return(NhlFATAL);
    }
    if(i > 0 && dims[i] != (dims[i-1]+1)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_gamfit_n: Input dimension sizes must be monotonically increasing, can't continue");
      return(NhlFATAL);
    }
  }

/*
 * Calculate size of leftmost dimensions (nl) up to the dims[0]-th
 *   dimensions.
 *
 * Calculate number of points that will be passed to Fortran
 *   routine (npts).
 *
 * Calculate size of rightmost dimensions (nr) from the
 *   ndims[ndims-1]-th dimension.
 *
 * The dimension(s) to do the calculations across are "dims".
 */
  total_nl = total_nr = npts = 1;
  if(ndims_x > 1) {
    ndims_xpar  = ndims_x-dsizes_dims+1;   
    dsizes_xpar = NclMalloc(ndims_xpar * sizeof(ng_size_t));
    dsizes_xpar[0] = 3;
    for(i = 0; i < dims[0] ; i++) {
      total_nl = total_nl*dsizes_x[i];
      dsizes_xpar[i+1] = dsizes_x[i];
    }
    for(i = 0; i < dsizes_dims ; i++) {
      npts = npts*dsizes_x[dims[i]];
    }
    for(i = dims[dsizes_dims-1]+1; i < ndims_x; i++) {
      total_nr = total_nr*dsizes_x[i];
      dsizes_xpar[i-dsizes_dims+1] = dsizes_x[i];
    }
  } else {
    dsizes_xpar = NclMalloc(sizeof(ng_size_t));
    *dsizes_xpar = 3;
    ndims_xpar   = 1;
    npts         = dsizes_x[dims[0]];
  }
  total_nlnr  = total_nl * total_nr;
  size_output = 3 * total_nlnr;

  if( npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_gamfit_n: npts is greater than INT_MAX");
    return(NhlFATAL);
  }
  inpts = (int) npts;

/*
 * Allocate space for tmp_x.
 */
  tmp_x = (double *)calloc(npts,sizeof(double));
  if(tmp_x == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_gamfit_n: Unable to allocate memory for coercing input array to double");
    return(NhlFATAL);
  }

/*
 * Coerce missing value to double if necessary.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,
                 &missing_dbl_x,&missing_flt_x);

/* 
 * Allocate space for output array.
 */
  if(type_x != NCL_double) {
    type_xpar = NCL_float;
    xpar      = (void *)calloc(size_output, sizeof(float));
  }
  else {
    type_xpar = NCL_double;
    xpar      = (void *)calloc(size_output, sizeof(double));
  }
  if(xpar == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_gamfit_n: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
  if(has_missing_x) {
    if(type_xpar == NCL_double) missing_xpar = missing_dbl_x;
    else                        missing_xpar = missing_flt_x;
  }

/*
 * Retrieve attributes from optgam, if any.
 */
  set_pcrit = False;
  inv_scale = 0;

  if(*optgam) {
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
 * att_id == -1 ==> no attributes specified.
 */
        break;
      }
/* 
 * Check for attributes. If none are set, then use default values.
 */
      if (attr_obj->att.n_atts == 0) {
        break;
      }
      else {
/*
 * Get list of attributes.
 */
        attr_list = attr_obj->att.att_list;
/*
 * Loop through attributes and check them.
 */
        while (attr_list != NULL) {
/*
 * pcrit
 */
          if ((strcmp(attr_list->attname, "pcrit")) == 0) {
            pcrit = coerce_input_double(attr_list->attvalue->multidval.val,
                                        attr_list->attvalue->multidval.data_type,
                                        1,0,NULL,NULL);
            set_pcrit = True;
          }
/*
 * inv_scale
 */
          if ((strcmp(attr_list->attname, "inv_scale")) == 0) {
            if(attr_list->attvalue->multidval.data_type != NCL_logical) {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"dim_gamfit_n: the 'inv_scale' attribute must be a logical, defaulting to False.");
            }
            else if(*(logical*)attr_list->attvalue->multidval.val) {
              inv_scale = 1;
            }
          }
          attr_list = attr_list->next;
        }
      }
    default:
      break;
    }
  }

  if(!set_pcrit) {
    pcrit = (double *)calloc(1,sizeof(double));
    *pcrit = 0.0;
  }

/*
 * Loop across leftmost dimensions and call the Fortran routine for each
 * subsection of the input arrays.
 */
  nrnx = total_nr * npts;
  for(i = 0; i < total_nl; i++) {
    index_nrx = i*nrnx;
    index_nr  = i*total_nr;
    for(j = 0; j < total_nr; j++) {
      index_x    = index_nrx + j;
      index_xpar = index_nr + j;
/*
 * Coerce subsection of x (tmp_x) to double.
 */
      coerce_subset_input_double_step(x,tmp_x,index_x,total_nr,type_x,
                                      npts,0,NULL,NULL);
/*
 * Call the Fortran routine.
 */
      NGCALLF(gamfitd3,GAMFITD3)(tmp_x, &inpts, &missing_dbl_x.doubleval, 
                                 pcrit, &inv_scale, &alpha, &scale, 
                                 &shape, &pzero, &ier);
/*
 * Coerce output back to float or double
 */
      coerce_output_float_or_double(xpar,&shape,type_xpar,1,index_xpar);
      coerce_output_float_or_double(xpar,&scale,type_xpar,1,
                                    index_xpar+total_nlnr);
      coerce_output_float_or_double(xpar,&pzero,type_xpar,1,
                                    index_xpar+(2*total_nlnr));
    }
  }

/*
 * Free unneeded memory.
 */
  NclFree(tmp_x);

/*
 * Return value back to NCL script.
 */
  if(has_missing_x) {
    ret = NclReturnValue(xpar,ndims_xpar,dsizes_xpar,&missing_xpar,
                         type_xpar,0);
  }
  else {
    ret = NclReturnValue(xpar,ndims_xpar,dsizes_xpar,NULL,type_xpar,0);
  }
  NclFree(dsizes_xpar);
  return(ret);
}

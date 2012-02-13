#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(xy2pdf77,XY2PDF77)(int *, double *, double *, double *,
                                       double *, int *, int *, double *, 
                                       int *, int *, double *, double *, 
                                       int *, int *);


extern void NGCALLF(x1pdf77,X1PDF77)(int *, double *, double *, int *,
                                     double *, int *, double *, int *, 
                                     int *);

NhlErrorTypes pdfxy_bin_W( void )
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
  NclScalar missing_x, missing_dbl_x;
  NclBasicDataTypes type_x;

/*
 * Argument # 1
 */
  void *y;
  double *tmp_y;
  int ndims_y;
  ng_size_t dsizes_y[NCL_MAX_DIMENSIONS];
  int has_missing_y;
  NclScalar missing_y, missing_dbl_y;
  NclBasicDataTypes type_y;

/*
 * Argument # 2
 */
  void *binxbnd;
  double *tmp_binxbnd;
  ng_size_t dsizes_binxbnd[1];
  NclBasicDataTypes type_binxbnd;

/*
 * Argument # 3
 */
  void *binybnd;
  double *tmp_binybnd;
  ng_size_t dsizes_binybnd[1];
  NclBasicDataTypes type_binybnd;

/*
 * Argument # 4
 */
  logical *opt;

/*
 * Return variable
 */
  void *pdf;
  double *tmp_pdf = NULL;
  ng_size_t dsizes_pdf[2];
  NclBasicDataTypes type_pdf;

/*
 * Various
 */
  ng_size_t i, nxy, mbxp1, nbyp1, nby, mbx, nbymbx;
  int ier, ret;
  int inxy, imbx, inby, imbxp1, inbyp1;

/*
 * Variables for retrieving attributes from "opt".
 */
  NclAttList  *attr_list;
  NclAtt  attr_obj;
  NclStackEntry   stack_entry;
  logical fraction = False;
  int ipcnt;
 
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
           5,
           &ndims_x,
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);

/*
 * Get argument # 1
 */
  y = (void*)NclGetArgValue(
           1,
           5,
           &ndims_y,
           dsizes_y,
           &missing_y,
           &has_missing_y,
           &type_y,
           DONT_CARE);

  if(ndims_x != ndims_y) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pdfxy_bin: The x and y arrays must have the same number of dimensions");
    return(NhlFATAL);
  }

  nxy = 1;
  for(i = 0; i < ndims_x; i++) {
    nxy *= dsizes_x[i];
    if(dsizes_x[i] != dsizes_y[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pdfxy_bin: The x and y arrays must have the same dimensionality");
      return(NhlFATAL);
    }
  }

/*
 * Get argument # 2
 */
  binxbnd = (void*)NclGetArgValue(
           2,
           5,
           NULL,
           dsizes_binxbnd,
           NULL,
           NULL,
           &type_binxbnd,
           DONT_CARE);
  mbxp1 = dsizes_binxbnd[0];
  mbx   = mbxp1 - 1;
  if(mbxp1 < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pdfxy_bin: The binxbnd array must have at least two values");
    return(NhlFATAL);
  }

/*
 * Get argument # 3
 */
  binybnd = (void*)NclGetArgValue(
           3,
           5,
           NULL,
           dsizes_binybnd,
           NULL,
           NULL,
           &type_binybnd,
           DONT_CARE);
  nbyp1 = dsizes_binybnd[0];
  nby   = nbyp1 - 1;
  if(nbyp1 < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pdfxy_bin: The binybnd array must have at least two values");
    return(NhlFATAL);
  }

/*
 * Test input dimension sizes.
 */
  if((nxy > INT_MAX) || (mbx > INT_MAX) || (nby > INT_MAX) ||
     (mbxp1 > INT_MAX) || (nbyp1 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pdfxy_bin: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inxy = (int) nxy;
  imbx = (int) mbx;
  inby = (int) nby;
  imbxp1 = (int) mbxp1;
  inbyp1 = (int) nbyp1;

/*
 * Get argument # 4
 */
  opt = (logical*)NclGetArgValue(
           4,
           5,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

/* 
 * If "opt" is True, then check if any attributes have been set.
 * 
 * There's only one recognized right now:
 *
 *   "fraction" : whether to return fraction (True) or percent (False)
 *                (False by default)
 */
  if(*opt) {
    stack_entry = _NclGetArg(4, 5, DONT_CARE);
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
/*
 * Check for "fraction".
 */
          if (!strcmp(attr_list->attname, "fraction")) {
            if(attr_list->attvalue->multidval.data_type != NCL_logical) {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"pdfxy_bin: The 'fraction' attribute must be a logical; defaulting to False.");
            }
            else {
              fraction = *(logical*) attr_list->attvalue->multidval.val;
            }
          }
          attr_list = attr_list->next;
        }
      }
    default:
      break;
    }
  }

  if(fraction) ipcnt = 0;
  else         ipcnt = 1;

/*
 * Coerce missing values to double if necessary.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dbl_x,NULL);
  coerce_missing(type_y,has_missing_y,&missing_y,&missing_dbl_y,NULL);

/*
 * The output type defaults to float, unless any input arrays are double.
 */
  if(type_x       == NCL_double || type_y       == NCL_double ||
     type_binxbnd == NCL_double || type_binybnd == NCL_double) {
    type_pdf = NCL_double;
  }
  else {
    type_pdf = NCL_float;
  }

/* 
 * Coerce input arrays to double if necessary.
 */
  tmp_x = coerce_input_double(x,type_x,nxy,0,NULL,NULL);
  if(tmp_x == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pdfxy_bin: Unable to allocate memory for coercing x to double");
    return(NhlFATAL);
  }

  tmp_y = coerce_input_double(y,type_y,nxy,0,NULL,NULL);
  if(tmp_y == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pdfxy_bin: Unable to allocate memory for coercing y to double");
    return(NhlFATAL);
  }

  tmp_binxbnd = coerce_input_double(binxbnd,type_binxbnd,mbxp1,0,NULL,NULL);
  if(tmp_binxbnd == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pdfxy_bin: Unable to allocate memory for coercing binxbnd to double");
    return(NhlFATAL);
  }

  tmp_binybnd = coerce_input_double(binybnd,type_binybnd,nbyp1,0,NULL,NULL);
  if(tmp_binybnd == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pdfxy_bin: Unable to allocate memory for coercing binybnd to double");
    return(NhlFATAL);
  }

/*
 * Calculate size of output array.
 */
  nbymbx = nby * mbx;

/* 
 * Allocate space for output array.
 */
  if(type_pdf != NCL_double) {
    pdf     = (void *)calloc(nbymbx, sizeof(float));
    tmp_pdf = (double *)calloc(nbymbx,sizeof(double));
    if(tmp_pdf == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pdfxy_bin: Unable to allocate memory for temporary output array");
      return(NhlFATAL);
    }
  }
  else {
    pdf = (void *)calloc(nbymbx, sizeof(double));
  }
  if(pdf == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pdfxy_bin: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
  if(type_pdf == NCL_double) tmp_pdf = &((double*)pdf)[0];

/*
 * Call the Fortran routine.
 */
  NGCALLF(xy2pdf77,XY2PDF77)(&inxy, tmp_x, tmp_y, &missing_dbl_x.doubleval,
                             &missing_dbl_y.doubleval, &inby, &imbx, 
                             tmp_pdf, &imbxp1, &inbyp1, tmp_binxbnd, 
                             tmp_binybnd, &ipcnt, &ier);
/*
 * Coerce output back to float if necessary.
 */
  if(type_pdf == NCL_float) coerce_output_float_only(pdf,tmp_pdf,nbymbx,0);

/*
 * Free unneeded memory.
 */
  if(type_x       != NCL_double) NclFree(tmp_x);
  if(type_y       != NCL_double) NclFree(tmp_y);
  if(type_binxbnd != NCL_double) NclFree(tmp_binxbnd);
  if(type_binybnd != NCL_double) NclFree(tmp_binybnd);
  if(type_pdf     != NCL_double) NclFree(tmp_pdf);

/*
 * Return value back to NCL script.
 */
  dsizes_pdf[0] = nby;
  dsizes_pdf[1] = mbx;
  ret = NclReturnValue(pdf,2,dsizes_pdf,NULL,type_pdf,0);
  return(ret);
}


NhlErrorTypes pdfx_bin_W( void )
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
  NclScalar missing_x, missing_dbl_x;
  NclBasicDataTypes type_x;

/*
 * Argument # 1
 */
  void *binxbnd;
  double *tmp_binxbnd;
  ng_size_t dsizes_binxbnd[1];
  NclBasicDataTypes type_binxbnd;

/*
 * Argument # 2
 */
  logical *opt;

/*
 * Return variable
 */
  void *pdf;
  double *tmp_pdf = NULL;
  ng_size_t dsizes_pdf[1];
  NclBasicDataTypes type_pdf;

/*
 * Various
 */
  ng_size_t i, nx, mbxp1, mbx;
  int ier, ret;
  int inx, imbx, imbxp1;

/*
 * Variables for retrieving attributes from "opt".
 */
  NclAttList  *attr_list;
  NclAtt  attr_obj;
  NclStackEntry   stack_entry;
  logical fraction = False;
  int ipcnt;
 
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

  nx = 1;
  for(i = 0; i < ndims_x; i++) nx *= dsizes_x[i];

/*
 * Get argument # 1
 */
  binxbnd = (void*)NclGetArgValue(
           1,
           3,
           NULL,
           dsizes_binxbnd,
           NULL,
           NULL,
           &type_binxbnd,
           DONT_CARE);
  mbxp1 = dsizes_binxbnd[0];
  mbx   = mbxp1 - 1;
  if(mbxp1 < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pdfx_bin: The binxbnd array must have at least two values");
    return(NhlFATAL);
  }

/*
 * Test input dimension sizes.
 */
  if((nx > INT_MAX) || (mbx > INT_MAX) || (mbxp1 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pdfx_bin: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inx    = (int) nx;
  imbx   = (int) mbx;
  imbxp1 = (int) mbxp1;

/*
 * Get argument # 2
 */
  opt = (logical*)NclGetArgValue(
           2,
           3,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

/* 
 * If "opt" is True, then check if any attributes have been set.
 * 
 * There's only one recognized right now:
 *
 *   "fraction" : whether to return fraction (True) or percent (False)
 *                (False by default)
 */
  if(*opt) {
    stack_entry = _NclGetArg(2, 3, DONT_CARE);
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
/*
 * Check for "fraction".
 */
          if (!strcmp(attr_list->attname, "fraction")) {
            if(attr_list->attvalue->multidval.data_type != NCL_logical) {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"pdfx_bin: The 'fraction' attribute must be a logical; defaulting to False.");
            }
            else {
              fraction = *(logical*) attr_list->attvalue->multidval.val;
            }
          }
          attr_list = attr_list->next;
        }
      }
    default:
      break;
    }
  }

  if(fraction) ipcnt = 0;
  else         ipcnt = 1;

/*
 * Coerce missing values to double if necessary.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dbl_x,NULL);

/*
 * The output type defaults to float, unless any input arrays are double.
 */
  if(type_x == NCL_double || type_binxbnd == NCL_double) {
    type_pdf = NCL_double;
  }
  else {
    type_pdf = NCL_float;
  }

/* 
 * Coerce input arrays to double if necessary.
 */
  tmp_x = coerce_input_double(x,type_x,nx,0,NULL,NULL);
  if(tmp_x == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pdfx_bin: Unable to allocate memory for coercing x to double");
    return(NhlFATAL);
  }

  tmp_binxbnd = coerce_input_double(binxbnd,type_binxbnd,mbxp1,0,NULL,NULL);
  if(tmp_binxbnd == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pdfx_bin: Unable to allocate memory for coercing binxbnd to double");
    return(NhlFATAL);
  }

/* 
 * Allocate space for output array.
 */
  if(type_pdf != NCL_double) {
    pdf     = (void *)calloc(mbx, sizeof(float));
    tmp_pdf = (double *)calloc(mbx,sizeof(double));
    if(tmp_pdf == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pdfx_bin: Unable to allocate memory for temporary output array");
      return(NhlFATAL);
    }
  }
  else {
    pdf = (void *)calloc(mbx, sizeof(double));
  }
  if(pdf == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pdfx_bin: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
  if(type_pdf == NCL_double) tmp_pdf = &((double*)pdf)[0];

/*
 * Call the Fortran routine.
 */
  NGCALLF(x1pdf77,X1PDF77)(&inx, tmp_x, &missing_dbl_x.doubleval,
                           &imbx, tmp_pdf, &imbxp1, tmp_binxbnd, 
                           &ipcnt, &ier);
/*
 * Coerce output back to float if necessary.
 */
  if(type_pdf == NCL_float) coerce_output_float_only(pdf,tmp_pdf,mbx,0);

/*
 * Free unneeded memory.
 */
  if(type_x       != NCL_double) NclFree(tmp_x);
  if(type_binxbnd != NCL_double) NclFree(tmp_binxbnd);
  if(type_pdf     != NCL_double) NclFree(tmp_pdf);

/*
 * Return value back to NCL script.
 */
  dsizes_pdf[0] = mbx;
  ret = NclReturnValue(pdf,1,dsizes_pdf,NULL,type_pdf,0);
  return(ret);
}


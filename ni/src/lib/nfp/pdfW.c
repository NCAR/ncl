#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(xy2pdf77,XY2PDF77)(int *, double *, double *, double *,
                                       double *, int *, int *, double *, 
                                       int *, int *, double *, double *, 
                                       int *, int *);

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
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclScalar missing_x, missing_dbl_x;
  NclBasicDataTypes type_x;

/*
 * Argument # 1
 */
  void *y;
  double *tmp_y;
  int ndims_y, dsizes_y[NCL_MAX_DIMENSIONS];
  int has_missing_y;
  NclScalar missing_y, missing_dbl_y;
  NclBasicDataTypes type_y;

/*
 * Argument # 2
 */
  void *binxbnd;
  double *tmp_binxbnd;
  int dsizes_binxbnd[1];
  NclBasicDataTypes type_binxbnd;

/*
 * Argument # 3
 */
  void *binybnd;
  double *tmp_binybnd;
  int dsizes_binybnd[1];
  NclBasicDataTypes type_binybnd;

/*
 * Argument # 4
 */
  logical *opt;
/*
 * Return variable
 */
  void *pdf;
  double *tmp_pdf;
  int ndims_pdf, *dsizes_pdf;
  NclBasicDataTypes type_pdf;

/*
 * Various
 */
  int nxy, mbxp1, nbyp1, nby, mbx, nbymbx;
  int index_xy, index_pdf, ier;
  int i, ndims_leftmost, size_leftmost, size_output, ret;

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
           2);

  nxy = dsizes_x[ndims_x-1];
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
           2);

  if(ndims_x != ndims_y) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pdfxy_bin: The x and y arrays must have the same number of dimensions");
    return(NhlFATAL);
  }

  for(i = 0; i < ndims_x; i++) {
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
           2);
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
           2);
  nbyp1 = dsizes_binybnd[0];
  nby   = nbyp1 - 1;
  if(nbyp1 < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pdfxy_bin: The binybnd array must have at least two values");
    return(NhlFATAL);
  }

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
           2);

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
 * Calculate size of leftmost dimensions.
 */
  if(ndims_x == 1) {
    ndims_leftmost = 0;
  }
  else {
    ndims_leftmost = ndims_x-1;
  }
  size_leftmost = 1;
  for(i = 0; i < ndims_leftmost; i++) size_leftmost *= dsizes_x[i];


/*
 * The output type defaults to float, unless any input arrays are double.
 */
  if(type_x       == NCL_double || type_y       == NCL_double ||
     type_binxbnd == NCL_double || type_binxbnd == NCL_double) {
    type_pdf = NCL_double;
  }
  else {
    type_pdf = NCL_float;
  }

/* 
 * Allocate space for coercing input arrays.  If any of the input
 * is already double, then we don't need to allocate space for
 * temporary arrays, because we'll just change the pointer into
 * the void array appropriately.
 */
/*
 * Allocate space for tmp_x.
 */
  if(type_x != NCL_double) {
    tmp_x = (double *)calloc(nxy,sizeof(double));
    if(tmp_x == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pdfxy_bin: Unable to allocate memory for coercing x to double");
      return(NhlFATAL);
    }
  }

/*
 * Allocate space for tmp_y.
 */
  if(type_y != NCL_double) {
    tmp_y = (double *)calloc(nxy,sizeof(double));
    if(tmp_y == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pdfxy_bin: Unable to allocate memory for coercing y to double");
      return(NhlFATAL);
    }
  }

/*
 * Allocate space for tmp_binxbnd.
 */
  tmp_binxbnd = coerce_input_double(binxbnd,type_binxbnd,mbxp1,0,NULL,NULL);
  if(tmp_binxbnd == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pdfxy_bin: Unable to allocate memory for coercing binxbnd to double");
    return(NhlFATAL);
  }

/*
 * Allocate space for tmp_binybnd.
 */
  tmp_binybnd = coerce_input_double(binybnd,type_binybnd,nbyp1,0,NULL,NULL);
  if(tmp_binybnd == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pdfxy_bin: Unable to allocate memory for coercing binybnd to double");
    return(NhlFATAL);
  }

/*
 * Calculate size of output array.
 */
  nbymbx = nby * mbx;
  size_output = size_leftmost * nbymbx;

/* 
 * Allocate space for output array.
 */
  if(type_pdf != NCL_double) {
    pdf = (void *)calloc(size_output, sizeof(float));
    tmp_pdf = (double *)calloc(nbymbx,sizeof(double));
    if(tmp_pdf == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pdfxy_bin: Unable to allocate memory for temporary output array");
      return(NhlFATAL);
    }
  }
  else {
    pdf = (void *)calloc(size_output, sizeof(double));
  }
  if(pdf == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pdfxy_bin: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/* 
 * Allocate space for output dimension sizes and set them.
 */
  ndims_pdf = ndims_leftmost + 2;
  dsizes_pdf = (int*)calloc(ndims_pdf,sizeof(int));  
  if( dsizes_pdf == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pdfxy_bin: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_pdf-2; i++) dsizes_pdf[i] = dsizes_x[i];
  dsizes_pdf[ndims_pdf-2] = nby;
  dsizes_pdf[ndims_pdf-1] = mbx;

/*
 * Loop across leftmost dimensions and call the Fortran routine for each
 * subsection of the input arrays.
 */
  index_xy = index_pdf = 0;

  for(i = 0; i < size_leftmost; i++) {
/*
 * Coerce subsection of x (tmp_x) to double if necessary.
 */
    if(type_x != NCL_double) {
      coerce_subset_input_double(x,tmp_x,index_xy,type_x,nxy,0,NULL,NULL);
    }
    else {
      tmp_x = &((double*)x)[index_xy];
    }

/*
 * Coerce subsection of y (tmp_y) to double if necessary.
 */
    if(type_y != NCL_double) {
      coerce_subset_input_double(y,tmp_y,index_xy,type_y,nxy,0,NULL,NULL);
    }
    else {
      tmp_y = &((double*)y)[index_xy];
    }

/*
 * Point temporary output array to void output array if appropriate.
 */
    if(type_pdf == NCL_double) tmp_pdf = &((double*)pdf)[index_pdf];

/*
 * Call the Fortran routine.
 */
    NGCALLF(xy2pdf77,XY2PDF77)(&nxy, tmp_x, tmp_y, &missing_dbl_x.doubleval,
                               &missing_dbl_y.doubleval, &nby, &mbx, 
                               tmp_pdf, &mbxp1, &nbyp1, tmp_binxbnd, 
                               tmp_binybnd, &ipcnt, &ier);

/*
 * Coerce output back to float if necessary.
 */
    if(type_pdf == NCL_float) {
      coerce_output_float_only(pdf,tmp_pdf,nbymbx,index_pdf);
    }
    index_xy  += nxy;
    index_pdf += nbymbx;
  }

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
  ret = NclReturnValue(pdf,ndims_pdf,dsizes_pdf,NULL,type_pdf,0);
  NclFree(dsizes_pdf);
  return(ret);
}

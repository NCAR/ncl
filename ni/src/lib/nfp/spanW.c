#include <stdio.h>
#include <stdlib.h>
#include "wrapper.h"

NhlErrorTypes testspan_W( void )
{

/*
 * Input variables
 */
/*
 * Argument # 0
 */
  NclMultiDValData start = NULL;
  NclStackEntry data0;
  double *tmp_start;
  NclBasicDataTypes type_start;

/*
 * Argument # 1
 */
  NclMultiDValData finish = NULL;
  NclStackEntry data1;
  double *tmp_finish;
  NclBasicDataTypes type_finish;

/*
 * Argument # 2
 */
  NclMultiDValData spacing = NULL;
  NclStackEntry data2;
  double *tmp_spacing;
  NclBasicDataTypes type_spacing;

/*
 * Return variable
 */
  void *xspan;
  ng_size_t dsizes_xspan[1];
  NclBasicDataTypes type_xspan;

/*
 * Various
 */
  ng_size_t i, npts;

/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
/*
 * Get argument # 0
 */
  data0 = _NclGetArg(0, 3, DONT_CARE);
  switch (data0.kind) {
  case NclStk_VAR:
    start = _NclVarValueRead(data0.u.data_var, NULL, NULL);
    break;

  case NclStk_VAL:
    start = (NclMultiDValData) data0.u.data_obj;
    break;
  default:
    break;
  }

  data1 = _NclGetArg(1, 3, DONT_CARE);
  switch (data1.kind) {
  case NclStk_VAR:
    finish = _NclVarValueRead(data1.u.data_var, NULL, NULL);
    break;

  case NclStk_VAL:
    finish = (NclMultiDValData) data1.u.data_obj;
    break;
  default:
    break;
  }

  if (finish == NULL)
    return NhlFATAL;

  data2 = _NclGetArg(2, 3, DONT_CARE);
  switch (data2.kind) {
  case NclStk_VAR:
    spacing = _NclVarValueRead(data2.u.data_var,NULL,NULL);
    break;

  case NclStk_VAL:
    spacing = (NclMultiDValData)data2.u.data_obj;
    break;
  default:
    break;
  }

  if (spacing == NULL)
    return NhlFATAL;

  if (_NclIsMissing(start,start->multidval.val) ||
      _NclIsMissing(finish,finish->multidval.val) ||
      _NclIsMissing(spacing,spacing->multidval.val)) {

    NhlPError(NhlFATAL, NhlEUNKNOWN, "testspan: Missing value detected in input, can't continue");
    return NhlFATAL;
  }

/*
 * Coerce input to double if necessary.
 */
  type_start   = start->multidval.data_type;
  type_finish  = finish->multidval.data_type;
  type_spacing = spacing->multidval.data_type;

  tmp_start   = coerce_input_double(start->multidval.val,type_start,
                                    1,0,NULL,NULL);
  tmp_finish  = coerce_input_double(finish->multidval.val,type_finish,
                                    1,0,NULL,NULL);
  tmp_spacing = coerce_input_double(spacing->multidval.val,type_spacing,
                                    1,0,NULL,NULL);

  if(tmp_start == NULL || tmp_finish == NULL || tmp_spacing == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"testspan: Unable to allocate memory for coercing input values to double");
    return(NhlFATAL);
  }
  if(type_start == NCL_double || type_finish == NCL_double || 
     type_spacing == NCL_double) {  
    type_xspan = NCL_double;
  }
  else {
    type_xspan = NCL_float;
  }
  
  npts = fabs(*tmp_finish - *tmp_start)/(*tmp_spacing) + 1;
  
  if(type_xspan == NCL_double) {
    xspan = (void *)calloc(npts, sizeof(double));
  }
  else {
    xspan = (void *)calloc(npts, sizeof(float));
  }
  if(xspan == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"testspan: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
  
/*
 * Increasing values.
 */
  if((*tmp_finish - *tmp_start) > 0.0) {
    if(type_xspan == NCL_double) {
      for(i = 0; i < npts; i++) {
        ((double*)xspan)[i] = (*tmp_start + (i * (*tmp_spacing)));
      }
    }
    else {
      for(i = 0; i < npts; i++) {
        ((float*)xspan)[i] = (float)(*tmp_start + (i * (*tmp_spacing)));
      }
    }
  }
/*
 * Decreasing values.
 */
  else if((*tmp_finish - *tmp_start) < 0.0) {
    if(type_xspan == NCL_double) {
      for(i = 0; i < npts; i++) {
        ((double*)xspan)[i] = (*tmp_start - (i * (*tmp_spacing)));
      }
    }
    else {
      for(i = 0; i < npts; i++) {
        ((float*)xspan)[i] = (float)(*tmp_start - (i * (*tmp_spacing)));
      }
    }
  }
/*
 * One value.
 */
  else {
    if(type_xspan == NCL_double) {
      ((double*)xspan)[0] = *tmp_start;
    }
    else {
      ((float*)xspan)[0] = (float)*tmp_start;
    }
    npts = 1;
  }

/*
 * Free unneeded memory.
 */
  if(type_start   != NCL_double) NclFree(tmp_start);
  if(type_finish  != NCL_double) NclFree(tmp_finish);
  if(type_spacing != NCL_double) NclFree(tmp_spacing);

/*
 * Return value back to NCL script.
 */
  dsizes_xspan[0] = npts;
  return(NclReturnValue(xspan,1,dsizes_xspan,NULL,type_xspan,0));
}

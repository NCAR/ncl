#include <stdio.h>
#include <math.h>
#include <ncarg/hlu/hluutil.h>
#include "wrapper.h"

NhlErrorTypes nice_mnmxintvl_W( void )
{
/*
 * Input array variables
 */
  void *xmin, *xmax;
  double *tmp_xmin, *tmp_xmax;
  int *max_levels;
  logical *outside;
  int has_missing_xmin, has_missing_xmax;
  NclScalar missing_xmin, missing_dxmin, missing_rxmin;
  NclScalar missing_xmax, missing_dxmax, missing_rxmax;
  NclBasicDataTypes type_xmin, type_xmax;
/*
 * Output array variables
 */
  double min_out, max_out, step_size;
  void *mnmxintvl;
  NclScalar missing_dmnmxintvl, missing_rmnmxintvl;
  ng_size_t dsizes_mnmxintvl[1];
  NclBasicDataTypes type_mnmxintvl;
/*
 * Declare various variables for random purposes.
 */
  int found_missing_xmin, found_missing_xmax, output_contains_msg;

/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  xmin = (void*)NclGetArgValue(
          0,
          4,
          NULL,
          NULL,
          &missing_xmin,
          &has_missing_xmin,
          &type_xmin,
          DONT_CARE);

  xmax = (void*)NclGetArgValue(
          1,
          4,
          NULL,
          NULL,
          &missing_xmax,
          &has_missing_xmax,
          &type_xmax,
          DONT_CARE);

  max_levels = (int*)NclGetArgValue(
          2,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

  outside = (logical*)NclGetArgValue(
          3,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);
/*
 * Check for missing values.  We also want to create a float and double
 * precision version of the missing value in case we need it for the
 * return value.
 */
  coerce_missing(type_xmin,has_missing_xmin,&missing_xmin,&missing_dxmin,
                 &missing_rxmin);
  coerce_missing(type_xmax,has_missing_xmax,&missing_xmax,&missing_dxmax,
                 &missing_rxmax);
/*  
 * Set type of output based on the input types.
 */
  if(type_xmin == NCL_double || type_xmax == NCL_double) {
    type_mnmxintvl = NCL_double;
  }
  else {
    type_mnmxintvl = NCL_float;
  }
/*
 * Allocate variable to hold output.
 */
  if(type_mnmxintvl == NCL_float) mnmxintvl = (void*)calloc(3,sizeof(float));
  else                          mnmxintvl = (void*)calloc(3,sizeof(double));

  if(mnmxintvl == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"nice_mnmxintvl: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Call the C routine that calculates our "nice" values. If xmin or xmax
 * is a missing value, then return a missing value in the output.
 */
  output_contains_msg = 0;   /* Flag to keep track of whether output contains
                                any missing values.*/

/*
 * Coerce xmin/xmax to double if necessary.
 */
  tmp_xmin = coerce_input_double(xmin,type_xmin,1,0,NULL,NULL);
  tmp_xmax = coerce_input_double(xmax,type_xmax,1,0,NULL,NULL);
  if(tmp_xmin == NULL || tmp_xmax == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"nice_mnmxintvl: Unable to coerce input to double precision");
    return(NhlFATAL);
  }

/*
 * Check for missing values. If encountered, then set output
 * to a missing value.
 */
  found_missing_xmin = (*tmp_xmin == missing_dxmin.doubleval) ? 1 : 0;
  found_missing_xmax = (*tmp_xmax == missing_dxmax.doubleval) ? 1 : 0;

  if(found_missing_xmin || found_missing_xmax) {
    output_contains_msg = 1;
    if(found_missing_xmin) {
      set_subset_output_missing(mnmxintvl,0,type_mnmxintvl,3,
                                missing_dxmin.doubleval);
      missing_dmnmxintvl = missing_dxmin;
      missing_rmnmxintvl = missing_rxmin;
    }
    else {
      set_subset_output_missing(mnmxintvl,0,type_mnmxintvl,3,
                                missing_dxmax.doubleval);
      missing_dmnmxintvl = missing_dxmax;
      missing_rmnmxintvl = missing_rxmax;
    }
  }
/*
 * Call the internal C routine.
 */
  if(!output_contains_msg) {
    _NhlGetEndpointsAndStepSize(*tmp_xmin,*tmp_xmax,*max_levels,
                                *outside,&min_out,&max_out,&step_size);

    coerce_output_float_or_double(mnmxintvl,&min_out,type_mnmxintvl,1,0);
    coerce_output_float_or_double(mnmxintvl,&max_out,type_mnmxintvl,1,1);
    coerce_output_float_or_double(mnmxintvl,&step_size,type_mnmxintvl,1,2);
  }
/*
 * Free tmp arrays.
 */
  if(type_xmin != NCL_double) NclFree(tmp_xmin);
  if(type_xmax != NCL_double) NclFree(tmp_xmax);

/*
 * Return values. If any of the min/max input contained a missing value, 
 * then this means that the output will also contain a missing value, so
 * we need to set it if this is the case.
 */
  dsizes_mnmxintvl[0] = 3;
  if(output_contains_msg) {
    if(type_mnmxintvl == NCL_float) {
      return(NclReturnValue(mnmxintvl,1,dsizes_mnmxintvl,&missing_rmnmxintvl,
                            type_mnmxintvl,0));
    }
    else {
      return(NclReturnValue(mnmxintvl,1,dsizes_mnmxintvl,&missing_dmnmxintvl,
                            type_mnmxintvl,0));
    }
  }
  else {
    return(NclReturnValue(mnmxintvl,1,dsizes_mnmxintvl,NULL,type_mnmxintvl,0));
  }
} 


#include <stdio.h>

/*
 * The following are the required NCAR Graphics include files.
 */
#include "wrapper.h"
#include <ncarg/hlu/hluutil.h>
#include <math.h>

NhlErrorTypes nice_mnmxintvl_W( void )
{
/*
 * Input array variables
 */
  void *min, *max;
  double tmp_min, tmp_max;
  int *max_levels;
  logical *outside;
  int has_missing_min, has_missing_max;
  NclScalar missing_min, missing_dmin, missing_rmin;
  NclScalar missing_max, missing_dmax, missing_rmax;
  NclBasicDataTypes type_min, type_max;
/*
 * Output array variables
 */
  double min_out, max_out, step_size;
  void *mnmxintvl;
  NclScalar missing_dmnmxintvl, missing_rmnmxintvl;
  int dsizes_mnmxintvl[1];
  NclBasicDataTypes type_mnmxintvl;
/*
 * Declare various variables for random purposes.
 */
  int found_missing_min, found_missing_max, output_contains_msg;

/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  min = (void*)NclGetArgValue(
          0,
          4,
          NULL,
          NULL,
          &missing_min,
          &has_missing_min,
          &type_min,
          2);

  max = (void*)NclGetArgValue(
          1,
          4,
          NULL,
          NULL,
          &missing_max,
          &has_missing_max,
          &type_max,
          2);

  max_levels = (int*)NclGetArgValue(
          2,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);

  outside = (logical*)NclGetArgValue(
          3,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);
/*
 * Check for missing values.  We also want to create a float and double
 * precision version of the missing value in case we need it for the
 * return value.
 */
  coerce_missing(type_min,has_missing_min,&missing_min,&missing_dmin,
                 &missing_rmin);
  coerce_missing(type_max,has_missing_max,&missing_max,&missing_dmax,
                 &missing_rmax);
/*  
 * Set type of output based on the input types.
 */
  if(type_min == NCL_double || type_max == NCL_double) {
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
 * Call the C routine that calculates our "nice" values. If min or max
 * is a missing value, then return a missing value in the output.
 */
  output_contains_msg = 0;   /* Flag to keep track of whether output contains
                                any missing values.*/

/*
 * If type_min is not double, then coerce subsection of min (tmp_min)
 * to double. Otherwise, point tmp_min to appropriate location in min.
 */
  if(type_min != NCL_double) {
    coerce_subset_input_double(min,&tmp_min,0,type_min,1,0,NULL,NULL);
  }
  else {
    tmp_min = ((double*)min)[0];
  }
/*
 * If type_max is not double, then coerce subsection of max (tmp_max)
 * to double. Otherwise, point tmp_max to appropriate location in max.
 */
  if(type_max != NCL_double) {
    coerce_subset_input_double(max,&tmp_max,0,type_max,1,0,NULL,NULL);
  }
  else {
    tmp_max = ((double*)max)[0];
  }
/*
 * Check for missing values. If encountered, then set output
 * to a missing value.
 */
  found_missing_min = (tmp_min == missing_dmin.doubleval) ? 1 : 0;
  found_missing_max = (tmp_max == missing_dmax.doubleval) ? 1 : 0;

  if(found_missing_min || found_missing_max) {
    output_contains_msg = 1;
    if(found_missing_min) {
      set_subset_output_missing(mnmxintvl,0,type_mnmxintvl,3,
				missing_dmin.doubleval);
      missing_dmnmxintvl = missing_dmin;
      missing_rmnmxintvl = missing_rmin;
    }
    else {
      set_subset_output_missing(mnmxintvl,0,type_mnmxintvl,3,
				missing_dmax.doubleval);
      missing_dmnmxintvl = missing_dmax;
      missing_rmnmxintvl = missing_rmax;
    }
  }
/*
 * Call the internal C routine.
 */
  if(!output_contains_msg) {
    _NhlGetEndpointsAndStepSize(tmp_min,tmp_max,*max_levels,
				*outside,&min_out,&max_out,&step_size);

    if(type_mnmxintvl == NCL_float) {
      ((float*)mnmxintvl)[0] = (float)min_out;
      ((float*)mnmxintvl)[1] = (float)max_out;
      ((float*)mnmxintvl)[2] = (float)step_size;
    }
    else {
      ((double*)mnmxintvl)[0] = min_out;
      ((double*)mnmxintvl)[1] = max_out;
      ((double*)mnmxintvl)[2] = step_size;
    }
  }
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


#include <stdio.h>

/*
 * The following are the required NCAR Graphics include files.
 */
#include "wrapper.h"
#include <ncarg/hlu/hluutil.h>
#include <math.h>

NhlErrorTypes common_graphic_mnmxstp_W( void )
{
/*
 * Input array variables
 */
  void *min, *max;
  double *tmp_min, *tmp_max;
  int *max_levels, tmp_max_levels;
  logical *outside, tmp_outside;
  int has_missing_min, has_missing_max;
  int ndims_min, dsizes_min[NCL_MAX_DIMENSIONS];
  int ndims_max, dsizes_max[NCL_MAX_DIMENSIONS];
  int ndims_max_levels, dsizes_max_levels[NCL_MAX_DIMENSIONS];
  int ndims_outside, dsizes_outside[NCL_MAX_DIMENSIONS];
  NclScalar missing_min, missing_dmin, missing_rmin;
  NclScalar missing_max, missing_dmax, missing_rmax;
  NclBasicDataTypes type_min, type_max;
/*
 * Output array variables
 */
  double min_out, max_out, step_size;
  void *mnmxstp;
  NclScalar missing_dmnmxstp, missing_rmnmxstp;
  int ndims_mnmxstp, *dsizes_mnmxstp;
  NclBasicDataTypes type_mnmxstp;
/*
 * Declare various variables for random purposes.
 */
  int i, index_m, size_leftmost, found_missing_min, found_missing_max;
  int scalar_min, scalar_max_levels, scalar_outside;
  int subset_contains_msg, output_contains_msg;

/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  min = (void*)NclGetArgValue(
          0,
          4,
          &ndims_min,
          dsizes_min,
          &missing_min,
          &has_missing_min,
          &type_min,
          2);

  max = (void*)NclGetArgValue(
          1,
          4,
          &ndims_max,
          dsizes_max,
          &missing_max,
          &has_missing_max,
          &type_max,
          2);

  max_levels = (int*)NclGetArgValue(
          2,
          4,
          &ndims_max_levels,
          dsizes_max_levels,
          NULL,
          NULL,
          NULL,
          2);

  outside = (logical*)NclGetArgValue(
          3,
          4,
          &ndims_outside,
          dsizes_outside,
          NULL,
          NULL,
          NULL,
          2);
/*
 * Check dimension sizes. min and max must be the same size.
 * max_levels and outside must either be scalars or same size as min/max.
 */
  if(ndims_min != ndims_max) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"common_graphic_mnmxstp: min and max must have the same number of dimensions");
    return(NhlFATAL);
  }

  for(i = 0; i < ndims_min; i++ ) {
    if(dsizes_min[i] != dsizes_max[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"common_graphic_mnmxstp: min and max must have the same dimensions");
      return(NhlFATAL);
    }
  }

  scalar_max_levels = is_scalar(ndims_max_levels,dsizes_max_levels);
  scalar_outside    = is_scalar(ndims_outside,dsizes_outside);

  if(!scalar_max_levels) {
    if(ndims_max_levels != ndims_min) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"common_graphic_mnmxstp: max_levels must either be a scalar or the same number of dimensions as min and max");
      return(NhlFATAL);
    }
    for(i = 0; i < ndims_min; i++ ) {
      if(dsizes_min[i] != dsizes_max_levels[i]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"common_graphic_mnmxstp: max_levels must either be a scalar or the same dimension sizes as min and max");
        return(NhlFATAL);
      }
    }
  }
  if(!scalar_outside) {
    if(ndims_outside != ndims_min) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"common_graphic_mnmxstp: outside must either be a scalar or the same number of dimensions as min and max");
      return(NhlFATAL);
    }
    for(i = 0; i < ndims_min; i++ ) {
      if(dsizes_min[i] != dsizes_outside[i]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"common_graphic_mnmxstp: outside must either be a scalar or the same dimension sizes as min and max");
        return(NhlFATAL);
      }
    }
  }
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
    type_mnmxstp = NCL_double;
  }
  else {
    type_mnmxstp = NCL_float;
  }
/*
 * Create temporary arrays to hold input data.  If the input is already
 * double, then we don't need to create any space.  We'll just point it
 * to the output variable.
 */
  if(type_min != NCL_double) {
    tmp_min = (double*)calloc(1,sizeof(double));
    if( tmp_min == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"common_graphic_mnmxstp: Unable to allocate memory for coercing min to double precision");
      return(NhlFATAL);
    }
  }

  if(type_max != NCL_double) {
    tmp_max = (double*)calloc(1,sizeof(double));
    if( tmp_max == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"common_graphic_mnmxstp: Unable to allocate memory for coercing max to double precision");
      return(NhlFATAL);
    }
  }
/*
 * Calculate size of min/max arrays.
 */
  size_leftmost = 1;
  for(i = 0; i < ndims_min; i++ ) size_leftmost *= dsizes_min[i];
/*
 * Allocate variable to hold size of output.  It will be 3 elements if
 * min/max are scalars, or n x 3 where n is the leftmost dimensions of
 * min/max.
 */
  scalar_min = is_scalar(ndims_min,dsizes_min);

  if(scalar_min) ndims_mnmxstp = 1;
  else           ndims_mnmxstp = ndims_min + 1;

  dsizes_mnmxstp = (int*)calloc(ndims_mnmxstp,sizeof(int));
  if(dsizes_mnmxstp == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"common_graphic_mnmxstp: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
  dsizes_mnmxstp[ndims_mnmxstp-1] = 3;
  if(!scalar_min) {
    for(i = 0; i < ndims_min; i++ ) dsizes_mnmxstp[i] = dsizes_min[i];
  }
/*
 * Allocate variable to hold output.
 */
  if(type_mnmxstp == NCL_float) {
    mnmxstp  = (void*)calloc(3*size_leftmost,sizeof(float));
  }
  else {
    mnmxstp  = (void*)calloc(3*size_leftmost,sizeof(double));
  }
  if(mnmxstp == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"common_graphic_mnmxstp: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Loop through leftmost dimensions and call the C routine that 
 * calculates our "nice" values. If min or max is a missing value, 
 * then return a missing value in the output.
 */
  output_contains_msg = 0;   /* Flag to keep track of whether output contains
                                any missing values.*/

  if(scalar_max_levels) tmp_max_levels = *max_levels;
  if(scalar_outside)    tmp_outside    = *outside;

  index_m = 0;    /* Index for output */

  for( i = 0; i < size_leftmost; i++ ) {
    subset_contains_msg = 0;   /* Flag to keep track of whether subset
                                  data contains any missing values.*/

/*
 * If type_min is not double, then coerce subsection of min (tmp_min)
 * to double. Otherwise, point tmp_min to appropriate location in min.
 */
    if(type_min != NCL_double) {
      coerce_subset_input_double(min,tmp_min,i,type_min,1,0,NULL,NULL);
    }
    else {
      tmp_min = &((double*)min)[i];
    }
/*
 * If type_max is not double, then coerce subsection of max (tmp_max)
 * to double. Otherwise, point tmp_max to appropriate location in max.
 */
    if(type_max != NCL_double) {
      coerce_subset_input_double(max,tmp_max,i,type_max,1,0,NULL,NULL);
    }
    else {
      tmp_max = &((double*)max)[i];
    }
/*
 * Check for missing values. If encountered, then set output
 * to a missing value.
 */
    found_missing_min = (*tmp_min == missing_dmin.doubleval) ? 1 : 0;
    found_missing_max = (*tmp_max == missing_dmax.doubleval) ? 1 : 0;

    if(found_missing_min || found_missing_max) {
      subset_contains_msg = output_contains_msg = 1;
      if(found_missing_min) {
        set_subset_output_missing(mnmxstp,i,type_mnmxstp,3,
                                  missing_dmin.doubleval);
        missing_dmnmxstp = missing_dmin;
        missing_rmnmxstp = missing_rmin;
      }
      else {
        set_subset_output_missing(mnmxstp,i,type_mnmxstp,3,
                                  missing_dmax.doubleval);
        missing_dmnmxstp = missing_dmax;
        missing_rmnmxstp = missing_rmax;
      }
    }
/* 
 * If max_levels and/or outside are not scalars, then get the next
 * value.
 */ 
    if(!scalar_max_levels) tmp_max_levels = max_levels[i];
    if(!scalar_outside)    tmp_outside    = outside[i];

/*
 * Call the internal C routine.
 */
    if(!subset_contains_msg) {
      _NhlGetEndpointsAndStepSize(*tmp_min,*tmp_max,tmp_max_levels,
                                  tmp_outside,&min_out,&max_out,&step_size);

      if(type_mnmxstp == NCL_float) {
        ((float*)mnmxstp)[index_m]   = (float)min_out;
        ((float*)mnmxstp)[index_m+1] = (float)max_out;
        ((float*)mnmxstp)[index_m+2] = (float)step_size;
      }
      else {
        ((double*)mnmxstp)[index_m]   = min_out;
        ((double*)mnmxstp)[index_m+1] = max_out;
        ((double*)mnmxstp)[index_m+2] = step_size;
      }
    }
    index_m += 3;
  }
/*
 * Free memory.
 */
  if(type_min != NCL_double) NclFree(tmp_min);
  if(type_max != NCL_double) NclFree(tmp_max);
/*
 * Return values. If any of the min/max input contained a missing value, 
 * then this means that the output will also contain a missing value, so
 * we need to set it if this is the case.
 */
  if(output_contains_msg) {
    if(type_mnmxstp == NCL_float) {
      return(NclReturnValue(mnmxstp,ndims_mnmxstp,dsizes_mnmxstp,
                            &missing_rmnmxstp,type_mnmxstp,0));
    }
    else {
      return(NclReturnValue(mnmxstp,ndims_mnmxstp,dsizes_mnmxstp,
                            &missing_dmnmxstp,type_mnmxstp,0));
    }
  }
  else {
    return(NclReturnValue(mnmxstp,ndims_mnmxstp,dsizes_mnmxstp,NULL,
                          type_mnmxstp,0));
  }
} 


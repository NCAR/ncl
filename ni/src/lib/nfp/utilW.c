#include <stdio.h>

/*
 * The following are the required NCAR Graphics include files.
 * They should be located in ${NCARG_ROOT}/include.
 */
#include "wrapper.h"

extern void NGCALLF(rndncl,RNDNCL)(int*,double*,int*,double*,double*,int*);

NhlErrorTypes round_W( void )
{
/*
 * Input array variables
 */
  void *x;
  double *tmp_x;
  int has_missing_x, ndims_x, dsizes_x[NCL_MAX_DIMENSIONS];
  int *iopt;
  NclScalar missing_x, missing_dx, missing_xout;
  NclBasicDataTypes type_x;
/*
 * Output array variables
 */
  void *xout;
  double *tmp_xout;
  NclBasicDataTypes type_xout;
/*
 * Declare various variables for random purposes.
 */
  int i, size_x;
/*
 * Retrieve argument.
 */
  x = (void*)NclGetArgValue(
          0,
          2,
          &ndims_x,
          dsizes_x,
          &missing_x,
          &has_missing_x,
          &type_x,
          2);

/*
 * Retrieve iopt.  Currently, the value of iopt specifies the following:
 *
 *   0 -> depending on input, return float or double
 *   1 -> send the output back as float
 *   2 -> send the output back as double
 *   3 -> send the output back as integer
 */
  iopt = (int*)NclGetArgValue(
          1,
          2,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);

  if(*iopt < 0 || *iopt > 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"round: 'iopt' can only have the values 0-3");
    return(NhlFATAL);
  }

/*
 * Compute the total size of the input array.
 */
  size_x = 1;
  for( i = 0; i < ndims_x; i++ ) size_x *= dsizes_x[i];

/*
 * Coerce input and missing value to double if necessary.
 */
  tmp_x = coerce_input_double(x,type_x,size_x,0,NULL,NULL);
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,NULL);

/*
 * The type of the output array depends on iopt and possibly the
 * type of the input.
 */
    switch(*iopt) {
    case  0:
      if(type_x != NCL_double) {
        type_xout = NCL_float;
      }
      else {
        type_xout = NCL_double;
      }
      break;
    case  1:
      type_xout = NCL_float;
      break;
    case  2:
      type_xout = NCL_double;
      break;
    case  3:
      type_xout = NCL_int;
      break;
    }
/*
 * Allocate memory for output.
 */
    switch(type_xout) {
    case  NCL_double:
      xout = (void*)calloc(size_x,sizeof(double));
      break;
    case  NCL_float:
      xout = (void*)calloc(size_x,sizeof(float));
      break;
    case  NCL_int:
      xout = (void*)calloc(size_x,sizeof(int));
      break;
    }
/*
 * Allocate space for temporary output which must be double. If the output
 * is already double, then just point tmp_xout to xout.
 */
    if(type_xout == NCL_double) {
      tmp_xout = (double*)xout;
    }
    else {
      tmp_xout = (double*)calloc(size_x,sizeof(double));
    }
    if(tmp_xout == NULL || xout == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"round: Unable to allocate memory for output arrays");
      return(NhlFATAL);
    }
/*
 * Call the Fortran version of this routine.
 */
    NGCALLF(rndncl,RNDNCL)(&size_x,tmp_x,&has_missing_x,
                           &missing_dx.doubleval,tmp_xout,iopt);
/*
 * Figure out if we need to coerce output back to float or int.
 */
    if(type_xout == NCL_float) {
      coerce_output_float_only(xout,tmp_xout,size_x,0);
    }
    if(type_xout == NCL_int) {
      coerce_output_int_only(xout,tmp_xout,size_x,0);
    }
/*
 * Return correct missing value type for output.
 */
    switch(type_xout) {
    case  NCL_double:
      missing_xout.doubleval = missing_dx.doubleval;
      break;
    case  NCL_float:
      missing_xout.floatval = (float)missing_dx.doubleval;
      break;
    case  NCL_int:
      missing_xout.intval = (int)missing_dx.doubleval;
      break;
    }

/*
 * Free memory.
 */
  if(type_x  != NCL_double)   NclFree(tmp_x);
  if(type_xout != NCL_double) NclFree(tmp_xout);
/*
 * Return.
 */
  if(has_missing_x) {
    return(NclReturnValue(xout,ndims_x,dsizes_x,&missing_xout,type_xout,0));
  }
  else{
    return(NclReturnValue(xout,ndims_x,dsizes_x,NULL,type_xout,0));
  }
}

NhlErrorTypes isnan_ieee_W( void )
{
/*
 * Input array variables
 */
  void *x;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_x;
/*
 * Output array variables
 */
  logical *isnan;
/*
 * Declare various variables for random purposes.
 */
  int i, size_x;
/*
 * Retrieve argument.
 */
  x = (void*)NclGetArgValue(
          0,
          1,
          &ndims_x,
          dsizes_x,
          NULL,
          NULL,
          &type_x,
          2);

  if(type_x != NCL_float && type_x != NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"isnan_ieee: the input must be of type float or double");
    return(NhlFATAL);
  }
/*
 * Compute the total size of the input array.
 */
  size_x = 1;
  for( i = 0; i < ndims_x; i++ ) size_x *= dsizes_x[i];

  isnan = (logical*)calloc(size_x,sizeof(logical));
/*
 * A poor man's test for NaN: if the number is not less than or equal to
 * 0 or greater than or equal to zero, then it must be NaN.
 */
  if(type_x == NCL_float) {
    for(i = 0; i < size_x; i++) {
      if( ((float*)x)[i] <= 0. || ((float*)x)[i] >= 0. ) {
        isnan[i] = False;
      }
      else {
        isnan[i] = True;
      }
    }
  }
  else {
    for(i = 0; i < size_x; i++) {
      if( ((double*)x)[i] <= 0. || ((double*)x)[i] >= 0. ) {
        isnan[i] = False;
      }
      else {
        isnan[i] = True;
      }
    }
  }

/*
 * Return.
 */
  return(NclReturnValue(isnan,ndims_x,dsizes_x,NULL,NCL_logical,0));
}

NhlErrorTypes replace_ieeenan_W( void )
{
/*
 * Input array variables
 */
  void *x, *value;
  int *iopt, has_missing_x;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_x, type_value;
  NclScalar missing_x;
/*
 * Declare various variables for random purposes.
 */
  int i, size_x;
/*
 * Retrieve argument.
 */
  x = (void*)NclGetArgValue(
          0,
          3,
          &ndims_x,
          dsizes_x,
          &missing_x,
          &has_missing_x,
          &type_x,
          2);

  if(type_x != NCL_float && type_x != NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"replace_ieeenan: the first argument must be of type float or double");
    return(NhlFATAL);
  }

  value = (void*)NclGetArgValue(
          1,
          3,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_value,
          2);

  if(type_value != NCL_float && type_value != NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"replace_ieeenan: the second argument must be of type float or double");
    return(NhlFATAL);
  }

  iopt = (int*)NclGetArgValue(
          2,
          3,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);
/*
 * Compute the total size of the input array.
 */
  size_x = 1;
  for( i = 0; i < ndims_x; i++ ) size_x *= dsizes_x[i];

/*
 * A poor man's test for NaN: if the number is not less than or equal to
 * 0 or greater than or equal to zero, then it must be NaN.
 */
  if(type_x == NCL_float) {
    for(i = 0; i < size_x; i++) {
      if(((float*)x)[i] <= 0. || ((float*)x)[i] >= 0.) {
        continue;
      }
      else {
        ((float*)x)[i] = ((float*)value)[0];
      }
    }
  }
  else {
    for(i = 0; i < size_x; i++) {
      if(((double*)x)[i] <= 0. || ((double*)x)[i] >= 0.) {
        continue;
      }
      else {
        ((double*)x)[i] = ((double*)value)[0];
      }
    }
  }
/*
 * Return.
 */
  if(*iopt == 1) {
    if(has_missing_x) {
      if(type_x == NCL_float) {
        missing_x.floatval = ((float*)value)[0];
      }
      else {
        missing_x.doubleval = ((double*)value)[0];
      }
      return(NclReturnValue(x,ndims_x,dsizes_x,&missing_x,type_x,0));
    }
    else {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"replace_ieeenan: the first argument does not have a _FillValue attribute set, so it cannot be set to the replacement value");
    }
  }
  return(NclReturnValue(x,ndims_x,dsizes_x,NULL,type_x,0));
}


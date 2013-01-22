#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(rndncl,RNDNCL)(int*,double*,int*,double*,double*,int*);

extern void NGCALLF(dgendat,DGENDAT)(double *,int *,int *,int *,int *,int *,
                                     double *,double *,int *);

NhlErrorTypes round_W( void )
{
/*
 * Input array variables
 */
  void *x;
  double *tmp_x;
  int has_missing_x, ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int *iopt, isx;
  NclScalar missing_x, missing_dx, missing_xout;
  NclBasicDataTypes type_x;
/*
 * Output array variables
 */
  void *xout = NULL;
  double *tmp_xout;
  NclBasicDataTypes type_xout = NCL_none;
/*
 * Declare various variables for random purposes.
 */
  ng_size_t i, size_x;
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
          DONT_CARE);

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
          DONT_CARE);

  if(*iopt < 0 || *iopt > 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"round: 'iopt' can only have the values 0-3");
    return(NhlFATAL);
  }

/*
 * Compute the total size of the input array.
 */
  size_x = 1;
  for( i = 0; i < ndims_x; i++ ) size_x *= dsizes_x[i];

  if(size_x > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"round: size_x = %ld is greater than INT_MAX", size_x);
    return(NhlFATAL);
  }
  isx = (int) size_x;

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
    default:
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
    NGCALLF(rndncl,RNDNCL)(&isx,tmp_x,&has_missing_x,
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
    default:
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
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_x;
/*
 * Output array variables
 */
  logical *isnan;
/*
 * Declare various variables for random purposes.
 */
  ng_size_t i, size_x;
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
          DONT_CARE);

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

NhlErrorTypes get_ncl_version_W(void)
{
  char *version;
  NrmQuark *sversion;
  int len;
  ng_size_t ret_size = 1;

/*
 * There are no input arguments to retrieve.
 * Just get the version number and return it.
 */
  len     = strlen(GetNCLVersion());
  version = (char *)calloc(len+1,sizeof(char));
  strcpy(version,GetNCLVersion());
  sversion  = (NrmQuark *)calloc(1,sizeof(NrmQuark));
  *sversion = NrmStringToQuark(version);
  free(version);
  return(NclReturnValue((void *)sversion, 1, &ret_size, NULL, NCL_string, 0));
}

NhlErrorTypes replace_ieeenan_W( void )
{
/*
 * Input array variables
 */
  void *x, *value;
  double *dvalue = NULL;
  int *iopt, has_missing_x;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_x, type_value;
  NclScalar missing_x;
/*
 * Declare various variables for random purposes.
 */
  ng_size_t i, size_x;
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
          DONT_CARE);

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
          DONT_CARE);

  if(type_value != NCL_float && type_value != NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"replace_ieeenan: the second argument must be of type float or double");
    return(NhlFATAL);
  }
/*
 * iopt isn't used for anything yet.
 */
  iopt = (int*)NclGetArgValue(
          2,
          3,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);
/*
 * Compute the total size of the input array.
 */
  size_x = 1;
  for( i = 0; i < ndims_x; i++ ) size_x *= dsizes_x[i];

/*
 * Coerce value to double if necessary.
 */
  if(type_x == NCL_double) {
    dvalue = coerce_input_double(value,type_value,1,0,NULL,NULL);
  }
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
        ((double*)x)[i] = *dvalue;
      }
    }
  }
/*
 * Return.
 */
  return(NhlNOERROR);
}

NhlErrorTypes generate_2d_array_W( void )
{
/*
 * Input array variables
 */
  void *tmp_dsizes_data;
  ng_size_t *dsizes_data;
  int *mlow, *mhigh, *iseed;
  void *dlow, *dhigh;
  double *tmp_dlow, *tmp_dhigh;
  NclBasicDataTypes type_dlow, type_dhigh, type_dsizes_data;
/*
 * Output variables.
 */
  void *data;
  double *tmp_data;
  NclBasicDataTypes type_data;
  int ret, id0, id1;
/*
 * Declare various variables for random purposes.
 */
  ng_size_t size_data;
/*
 * Retrieve arguments.
 *
 *
 * Get number of lows and highs. These two values will be forced to
 * be between 1 and 25.
 */
  mlow = (int*)NclGetArgValue(
          0,
          6,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

  mhigh = (int*)NclGetArgValue(
          1,
          6,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

/*
 * Retrieve minimum and maximum values that the data is supposed to have.
 */
  dlow = (void*)NclGetArgValue(
          2,
          6,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_dlow,
          DONT_CARE);

  dhigh = (void*)NclGetArgValue(
          3,
          6,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_dhigh,
          DONT_CARE);

/*
 * Get size of output array.
 */
  iseed = (int*)NclGetArgValue(
          4,
          6,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);
/*
 * Get size of output array.
 */
  tmp_dsizes_data = (void*)NclGetArgValue(
          5,
          6,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_dsizes_data,
          DONT_CARE);

/*
 * Error checking.
 */
  dsizes_data = get_dimensions(tmp_dsizes_data,2,type_dsizes_data,"generate_2d_array");
  if(dsizes_data == NULL) 
    return(NhlFATAL);

  if(dsizes_data[0] <= 1 && dsizes_data[1] <= 1) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"generate_2d_array: the dimensions of the output array must be such that it has at least two elements");
    return(NhlFATAL);
  }
  if((dsizes_data[0] > INT_MAX) ||
     (dsizes_data[1] > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"generate_2d_array: input dimensions are greater than INT_MAX");
    return(NhlFATAL);
  }
  id0 = (int) dsizes_data[0];
  id1 = (int) dsizes_data[1];

  if(*iseed < 0 || *iseed > 100) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"generate_2d_array: iseed must be between 0 and 100. Will reset to 0.");
    *iseed = 0;
  }  
  if(*mlow < 1) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"generate_2d_array: mlow must be between 1 and 25. Will reset to 1.");
    *mlow = 1;
  }
  if(*mlow > 25) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"generate_2d_array: mlow must be between 1 and 25. Will reset to 25.");
    *mlow = 25;
  }

  if(*mhigh < 1) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"generate_2d_array: mhigh must be between 1 and 25. Will reset to 1.");
    *mhigh = 1;
  }
  if(*mhigh > 25) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"generate_2d_array: mhigh must be between 1 and 25. Will reset to 25.");
    *mhigh = 25;
  }

/*
 * Coerce dlow and dhigh to double.
 */
  tmp_dlow  = coerce_input_double(dlow, type_dlow, 1,0,NULL,NULL);
  tmp_dhigh = coerce_input_double(dhigh,type_dhigh,1,0,NULL,NULL);

/*
 * Compute the size of the 2D output array.
 */
  size_data = dsizes_data[0] * dsizes_data[1];

/*
 * The type of the output array depends on dlow and dhigh.
 */
  if(type_dlow == NCL_double || type_dhigh == NCL_double) {
    type_data = NCL_double;
  }
  else {
    type_data = NCL_float;
  }

/*
 * Allocate memory for output.
 */
  if(type_data == NCL_double) {
    data     = (void*)malloc(size_data*sizeof(double));
    tmp_data = (double *)data;
    if(data == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"generate_2d_array: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    data     = (void*)malloc(size_data*sizeof(float));
    tmp_data = (double*)malloc(size_data*sizeof(double));
    if(tmp_data == NULL || data == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"generate_2d_array: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
/*
 * Call the Fortran version of this routine.
 */
  NGCALLF(dgendat,DGENDAT)(tmp_data,&id1,&id1,
			   &id0,mlow,mhigh,tmp_dlow,tmp_dhigh,
			   iseed);

/*
 * Figure out if we need to coerce output back to float.
 */
  if(type_data == NCL_float) {
    coerce_output_float_only(data,tmp_data,size_data,0);
  }
/*
 * Free memory.
 */
  if(type_data  != NCL_double) NclFree(tmp_data);
  if(type_dlow  != NCL_double) NclFree(tmp_dlow);
  if(type_dhigh != NCL_double) NclFree(tmp_dhigh);

  ret = NclReturnValue(data,2,dsizes_data,NULL,type_data,0);
  NclFree(dsizes_data);
  return(ret);
}


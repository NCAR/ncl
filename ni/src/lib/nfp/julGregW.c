#include <stdio.h>
#include "wrapper.h"

extern double NGCALLF(greg2juld,GREG2JULD)(int *,int *,int *,int *);
extern int    NGCALLF(greg2juli,GREG2JULI)(int *,int *,int *);
extern void   NGCALLF(juld2greg,JULD2GREG)(double *,int *,int *,int *,int *);
extern void   NGCALLF(juli2greg,JULI2GREG)(int *,int *,int *,int *);

NhlErrorTypes greg2jul_W( void )
{
  int i, j, total;
/*
 * Input variables
 */
  int *year, *month, *day, *hour;
  int ndims_year, dsizes_year[NCL_MAX_DIMENSIONS];
  int ndims_month, dsizes_month[NCL_MAX_DIMENSIONS];
  int ndims_day, dsizes_day[NCL_MAX_DIMENSIONS];
  int ndims_hour, dsizes_hour[NCL_MAX_DIMENSIONS];
/*
 * Output variables
 */
  int *julian_i;
  double *julian_d;
/*
 * Other variables
 */
  int use_hour;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  year = (int*)NclGetArgValue(
          0,
          4,
          &ndims_year, 
          dsizes_year,
          NULL,
          NULL,
          NULL,
          2);

  month = (int*)NclGetArgValue(
          1,
          4,
          &ndims_month, 
          dsizes_month,
          NULL,
          NULL,
          NULL,
          2);

  day = (int*)NclGetArgValue(
          2,
          4,
          &ndims_day, 
          dsizes_day,
          NULL,
          NULL,
          NULL,
          2);

  hour = (int*)NclGetArgValue(
          3,
          4,
          &ndims_hour,
          dsizes_hour,
          NULL,
          NULL,
          NULL,
          2);
/*
 * Check dimensions and calculate total size of arrays.
 */
  if( ndims_year != ndims_month || ndims_year != ndims_day || 
      ndims_year != ndims_hour) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"greg2jul: The input arrays must all be the same size");
    return(NhlFATAL);
  }

  total = 1;
  for( i = 0; i < ndims_year; i++ ) {
    if( dsizes_year[i] != dsizes_day[i] || dsizes_year[i] != dsizes_month[i] ||
		dsizes_year[i] != dsizes_hour[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"greg2jul: The input arrays must all have the same dimension sizes");
      return(NhlFATAL);
    }
    total *= dsizes_year[i];
  }
/*
 * Check input.
 */
  for( i = 0; i < total; i++ ) {
    if(!(1 <= month[i] && month[i] <= 12)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"greg2jul: The month values must fall between 1 and 12");
      return(NhlFATAL);
    }
    if(!(1 <= day[i] && day[i] <= 31)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"greg2jul: The day values must fall between 1 and 31");
      return(NhlFATAL);
    }
  }
/*
 * The hour values must all either be negative, indicating they are not to
 * to be used in the calculation, or they must all fall between 0 and 23.
 */
  use_hour = 0;
  if(hour[0] >= 0) use_hour = 1;
  if(use_hour) {
    for( i = 0; i < total; i++ ) {
      if(!(0 <= hour[i] && hour[i] <= 23)) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"greg2jul: The hour values must fall between 0 and 23");
	return(NhlFATAL);
      }
    }
  }
  else {
    for( i = 0; i < total; i++ ) {
      if(hour[i] >= 0) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"greg2jul: The hour values must all either be negative, or all fall between 0 and 23");
	return(NhlFATAL);
      }
    }
  }
/*
 * Allocate space for output array.  The output will be of type double if
 * hour values are to be returned, otherwise the output is of type integer.
 */
  if(use_hour) {
    julian_d = (double*)NclMalloc(total*sizeof(double));
    if( julian_d == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"greg2jul: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    julian_i = (int*)NclMalloc(total*sizeof(int));
    if( julian_i == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"greg2jul: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
/*
 * Call appropriate function, depending on whether hours are part of the
 * calculation or not.
 */
  if(use_hour) {
    for( i = 0; i < total; i++ ) {
	  julian_d[i] = NGCALLF(greg2juld,GREG2JULD)(&year[i],&month[i],
												 &day[i],&hour[i]);
	}
    return(NclReturnValue((void*)julian_d,ndims_year,dsizes_year,
						  NULL,NCL_double,0));
  }
  else {
    for( i = 0; i < total; i++ ) {
	  julian_i[i] = NGCALLF(greg2juli,GREG2JULI)(&year[i],&month[i],&day[i]);
	}
    return(NclReturnValue((void*)julian_i,ndims_year,dsizes_year,
						  NULL,NCL_int,0));
  }
}


NhlErrorTypes jul2greg_W( void )
{
/*
 * Input variables
 */
  NclStackEntry data;
  NclMultiDValData tmp_md = NULL, tmp1_md;
  int ndims_jul;
/*
 * Output variables
 */
  int *date;
  int ndims_date, dsizes_date[NCL_MAX_DIMENSIONS];
/*
 * Other variables
 */
  int i, j, total, num_elems, is_double;
/*
 * Retrieve argument.
 *
 */
  data = _NclGetArg(0,1,DONT_CARE);
  switch(data.kind) {
  case NclStk_VAR:
	tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
	break;
  case NclStk_VAL:
	tmp_md = (NclMultiDValData)data.u.data_obj;
	break;
  }
/*
 * Calculate total size of array.
 */
  total = 1;
  ndims_jul = tmp_md->multidval.n_dims;
  for( i = 0; i < ndims_jul; i++ ) {
	total *= tmp_md->multidval.dim_sizes[i];
  }
/*
 * Allocate space for output array.
 */
  if( tmp_md->multidval.data_type == NCL_double ||
      tmp_md->multidval.data_type == NCL_float) {

/*
 * Promote to double if necessary.
 */
	if(tmp_md->multidval.data_type == NCL_float) {
	  tmp1_md = _NclCoerceData(tmp_md,Ncl_Typedouble,NULL);
	  if(tmp1_md == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"jul2greg: Unable to convert input to double");
		return(NhlFATAL);
	  }	
	}
	else {
	  tmp1_md = tmp_md;
	}
	is_double = 1;
	num_elems = 4;
  }
  else {
	is_double = 0;
	num_elems = 3;
  }
  date = (int*)NclMalloc(num_elems*total*sizeof(int));
  if( date == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"jul2greg: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Call conversion procedure.
 */
  j = 0;
  for( i = 0; i < total; i++ ) {
	if(is_double) {
	  NGCALLF(juld2greg,JULD2GREG)(&((double*)tmp1_md->multidval.val)[i],
								   &date[j],&date[j+1],&date[j+2],&date[j+3]);
	}
	else {
	  NGCALLF(juli2greg,JULI2GREG)(&((int*)tmp_md->multidval.val)[i],
								   &date[j],&date[j+1],&date[j+2]);
	}
	j += num_elems;
  }
/*
 * Return information.
 */
  if(ndims_jul == 1 && tmp_md->multidval.dim_sizes[0] == 1) {
	ndims_date     = 1;
	dsizes_date[0] = num_elems;
  }
  else {
	ndims_date = ndims_jul + 1;
	for( i = 0; i < ndims_jul; i++ ) {
	  dsizes_date[i] = tmp_md->multidval.dim_sizes[i];
	}
	dsizes_date[ndims_jul] = num_elems;
  }
  return(NclReturnValue((void*)date,ndims_date,dsizes_date,NULL,NCL_int,0));
}

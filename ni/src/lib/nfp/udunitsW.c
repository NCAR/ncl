#include <stdio.h>
#include <udunits.h>
#include "wrapper.h"

/*
 * Function for initializing Udunits package.  If UDUNITS_PATH is
 * set, then this path is used for the "udunits.dat" file. Otherwise,
 * the path within NCL ($NCARG_ROOT/lib/ncarg/udunits) is used.
 */
int utopen()
{
  const char *path = NULL;
  char udunits_file[_NhlMAXFNAMELEN];
  int utret;
/*
 * Initialize the Udunits package.
 *
 * The default is to use $NCARG_ROOT/lib/ncarg/udunits/udunits.dat
 * for the initialization file, unless UDUNITS_PATH is set by the
 * user, then it will try to use this path. 
 */
  path = getenv("UDUNITS_PATH");
  if ((void *)path == (void *)NULL) {
    path = _NGGetNCARGEnv("udunits");
    if ((void *)path != (void *)NULL) {
      strcpy(udunits_file,path);
      strcat(udunits_file,_NhlPATHDELIMITER);
      strcat(udunits_file,"udunits.dat");
      utret = utInit(udunits_file);
    }
    else {
/*
 * Use path built-in at compile time. It's not a good thing if we reach
 * this point, because the "_NGGetNCARGEnv" call above should have
 * returned a valid path.
 */
      utret = utInit("");
    }
  }
  else {
/*
 * Use UDUNITS_PATH.
 */
    utret = utInit(path);
  }
  return(utret);
}

NhlErrorTypes ut_calendar_W( void )
{
/*
 * Input array variables
 */
  void *x;
  double *tmp_x;
  string *sspec;
  int *option;
  char *cspec;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  NclScalar missing_x, missing_dx;
  NclBasicDataTypes type_x;
/*
 * Variables for Udunits package.
 */
  int utopen();
  utUnit unit;
/*
 * Output variables.
 */
  int year, month, day, hour, minute;
  float second;
  void *date;
  int ndims_date, *dsizes_date;
  NclScalar missing_date;
/*
 * various
 */
  int i, total_size_x;

/*
 * Before we do anything, initialize the Udunits package.
 */
  if (utopen() != 0) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ut_calendar: Could not initialize Udunits package.");
    return(NhlFATAL);
  }

/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
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
/*
 * Get spec string.
 */
  sspec = (string*)NclGetArgValue(
           1,
           3,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           1);
/*
 * Get option.
 */
  option = (int*)NclGetArgValue(
           2,
           3,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           1);
/*
 * Convert sspec to character string.
 */
  cspec = NrmQuarkToString(*sspec);

/*
 * Make sure cspec is a valid udunits string.
 */
  if(utScan(cspec, &unit) != 0) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ut_calendar: Invalid specification string");
    return(NhlFATAL);
  }
/*
 * Calculate total size of input array, and size and dimensions for
 * output array, and alloc memory for output array.
 *
 * Since we are returning 6 separate values for each input value
 * (year, month, day, hour, minute, second), and since
 * seconds are float and everybody else is integer, return a float
 * array dimensioned (6,total_size_x array).
 */
  total_size_x = 1;
  for( i = 0; i < ndims_x; i++ ) total_size_x *= dsizes_x[i];

  ndims_date  = ndims_x + 1;
  dsizes_date = (int *)calloc(ndims_date,sizeof(int));
  date        = (void *)calloc(6*total_size_x,sizeof(float));

  if( date == NULL || dsizes_date == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ut_calendar: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  dsizes_date[0] = 6;
  for( i = 0; i < ndims_x; i++ ) dsizes_date[i+1] = dsizes_x[i];

/*
 * Coerce missing value to double, and get the default missing
 * value for a float type.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,NULL);

  missing_date = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis;

/*
 * Convert input to double if necessary.
 */
  tmp_x = coerce_input_double(x,type_x,total_size_x,has_missing_x,&missing_x,
                  &missing_dx);
/* 
 * Loop through each element and get the 6 values.
 */
  for( i = 0; i < total_size_x; i++ ) {
    if(!has_missing_x ||
       (has_missing_x && tmp_x[i] != missing_dx.doubleval)) {
      (void) utCalendar(tmp_x[i],&unit,&year,&month,&day,
            &hour,&minute,&second);
    
      ((float*)date)[i]                  = (float)year;
      ((float*)date)[i+total_size_x]     = (float)month;
      ((float*)date)[i+(2*total_size_x)] = (float)day;
      ((float*)date)[i+(3*total_size_x)] = (float)hour;
      ((float*)date)[i+(4*total_size_x)] = (float)minute;
      ((float*)date)[i+(5*total_size_x)] = second;
    }
    else {
      ((float*)date)[i]                  = missing_date.floatval;
      ((float*)date)[i+total_size_x]     = missing_date.floatval;
      ((float*)date)[i+(2*total_size_x)] = missing_date.floatval;
      ((float*)date)[i+(3*total_size_x)] = missing_date.floatval;
      ((float*)date)[i+(4*total_size_x)] = missing_date.floatval;
      ((float*)date)[i+(5*total_size_x)] = missing_date.floatval;
    }
  }

/*
 * Free the work arrays.
 */
  if(type_x != NCL_double) NclFree(tmp_x);
/*
 * Close up Udunits.
 */
  utTerm();
/*
 * Return.
 */ 
  if(has_missing_x) {
    return(NclReturnValue(date,ndims_date,dsizes_date,&missing_date,NCL_float,0));
  }
  else {
    return(NclReturnValue(date,ndims_date,dsizes_date,NULL,NCL_float,0));
  }
}

NhlErrorTypes ut_inv_calendar_W( void )
{
/*
 * Input array variables
 */
  int *year, *month, *day, *hour, *minute;
  double *second;
  string *sspec;
  int *option;
  char *cspec;
  int ndims_year,   dsizes_year[NCL_MAX_DIMENSIONS],   has_missing_year;
  int ndims_month,  dsizes_month[NCL_MAX_DIMENSIONS],  has_missing_month;
  int ndims_day,    dsizes_day[NCL_MAX_DIMENSIONS],    has_missing_day;
  int ndims_hour,   dsizes_hour[NCL_MAX_DIMENSIONS],   has_missing_hour;
  int ndims_minute, dsizes_minute[NCL_MAX_DIMENSIONS], has_missing_minute;
  int ndims_second, dsizes_second[NCL_MAX_DIMENSIONS], has_missing_second;
  NclScalar missing_year;
  NclScalar missing_month;
  NclScalar missing_day;
  NclScalar missing_hour;
  NclScalar missing_minute;
  NclScalar missing_second;
/*
 * Variables for Udunits package.
 */
  int utopen();
  utUnit unit;
/*
 * Output variables.
 */
  double *x;
  int has_missing_x;
  NclScalar missing_x;
/*
 * various
 */
  int i, total_size_input;
/*
 * Before we do anything, initialize the Udunits package.
 */
  if (utopen() != 0) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ut_inv_calendar: Could not initialize Udunits package.");
    return(NhlFATAL);
  }

/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 * The first size input arrays must be the same dimension sizes.
 */
  year = (int*)NclGetArgValue(
           0,
           8,
           &ndims_year, 
           dsizes_year,
           &missing_year,
           &has_missing_year,
           NULL,
           2);
  month = (int*)NclGetArgValue(
           1,
           8,
           &ndims_month, 
           dsizes_month,
           &missing_month,
           &has_missing_month,
           NULL,
           2);
  day = (int*)NclGetArgValue(
           2,
           8,
           &ndims_day, 
           dsizes_day,
           &missing_day,
           &has_missing_day,
           NULL,
           2);
  hour = (int*)NclGetArgValue(
           3,
           8,
           &ndims_hour, 
           dsizes_hour,
           &missing_hour,
           &has_missing_hour,
           NULL,
           2);
  minute = (int*)NclGetArgValue(
           4,
           8,
           &ndims_minute, 
           dsizes_minute,
           &missing_minute,
           &has_missing_minute,
           NULL,
           2);
  second = (double*)NclGetArgValue(
           5,
           8,
           &ndims_second, 
           dsizes_second,
           &missing_second,
           &has_missing_second,
           NULL,
           2);

  if(ndims_year != ndims_month || ndims_year != ndims_day    || 
     ndims_year != ndims_hour  || ndims_year != ndims_minute ||
     ndims_year != ndims_second) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ut_inv_calendar: The first six arguments must have the same dimensionality");
    return(NhlFATAL);
  }

  for(i = 0; i < ndims_year; i++ ) {
    if(dsizes_year[i] != dsizes_month[i]  ||
       dsizes_year[i] != dsizes_day[i]    || 
       dsizes_year[i] != dsizes_hour[i]   || 
       dsizes_year[i] != dsizes_minute[i] ||
       dsizes_year[i] != dsizes_second[i]) {
      
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ut_inv_calendar: The first six arguments must have the same dimensionality");
      return(NhlFATAL);
    }
  }
/* 
 * x will contain a _FillValue attribute if any of the input
 * has a _FillValue attribute set.
 */
  if(has_missing_year || has_missing_month || has_missing_day ||
     has_missing_hour || has_missing_minute || has_missing_second) {
    has_missing_x = 1;
/*
 * Get the default missing value for a double type.
 */
    missing_x = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis;
  }
  else {
    has_missing_x = 0;
  }
/*
 * Get spec string.
 */
  sspec = (string*)NclGetArgValue(
           6,
           8,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           1);
/*
 * Get option.
 */
  option = (int*)NclGetArgValue(
           7,
           8,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           1);
/*
 * Convert sspec to character string.
 */
  cspec = NrmQuarkToString(*sspec);

/*
 * Make sure cspec is a valid Udunits string.
 */
  if(utScan(cspec, &unit) != 0) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ut_inv_calendar: Invalid specification string");
    return(NhlFATAL);
  }
/*
 * Calculate total size of input arrays, and size and dimensions for
 * output array, and alloc memory for output array.
 */
  total_size_input = 1;
  for( i = 0; i < ndims_year; i++ ) total_size_input *= dsizes_year[i];

  x = (double *)calloc(total_size_input,sizeof(double));

  if( x == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ut_inv_calendar: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/* 
 * Loop through each data value, and call Udunits routine.
 */ 
  for( i = 0; i < total_size_input; i++ ) {
    if((!has_missing_year   ||
        (has_missing_year  && year[i]    != missing_year.intval))   &&
       (!has_missing_month  ||
         (has_missing_month && month[i]  != missing_month.intval))  &&
       (!has_missing_day    ||
         (has_missing_day   && day[i]    != missing_day.intval))    &&
       (!has_missing_hour   ||
         (has_missing_hour  && hour[i]   != missing_hour.intval))   &&
       (!has_missing_minute ||
         (has_missing_minute&& minute[i] != missing_minute.intval)) &&
       (!has_missing_second ||
        (has_missing_second&& second[i]  != missing_second.doubleval)) ) {

       (void)utInvCalendar(year[i],month[i],day[i],hour[i],minute[i],
                           second[i],&unit,&x[i]);
    }
    else {
      x[i]  = missing_x.doubleval;
    }
  }

/*
 * Close up Udunits.
 */
  utTerm();
/*
 * Return.
 */ 
  if(has_missing_x) {
    return(NclReturnValue((void*)x,ndims_year,dsizes_year,&missing_x,
                          NCL_double,0));
  }
  else {
    return(NclReturnValue((void*)x,ndims_year,dsizes_year,NULL,
                          NCL_double,0));
  }
}

#include <stdio.h>
#include <udunits.h>
#include "wrapper.h"
#include   "NclAtt.h"
#include   <ncarg/ncl/NclVar.h>
#include   "DataSupport.h"
#include   "AttSupport.h"
#include   "VarSupport.h"

/*
 * Function for initializing Udunits package.  If UDUNITS_PATH is
 * set, then this path is used for the "udunits.dat" file. Otherwise,
 * the path within NCL ($NCARG_ROOT/lib/ncarg/udunits/) is used.
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

/*
 * The NCAR/CSM convention states the following:
 *
 *    The calendar calculations done by the udunits package use a mixed
 *    Gregorian/Julian calendar, i.e., dates prior to 1582-10-15 are
 *    assumed to use the Julian calendar. Time coordinates that use
 *    other calendars are thus not able to make use of the udunits
 *    library for this purpose. However, it is still required to use the
 *    time unit format described above as this contains all the
 *    information required to make calendar calculations once the
 *    calendar has been specified. We describe a calendar attribute for
 *    the time coordinate variable below that may be used for this
 *    purpose.
 *
 * The ut_calendar function below depends on udunits, so it is the 
 * user's responsibility to make sure that the input refers to
 * the mixed Gregorian/Julian calendar.
 *
 * The NCL function below checks for a "calendar" attribute. If
 * it is not present or has the values "standard" or "gregorian",
 * then this function will return the dates as returned
 * from the udunits package. If the "calendar" attribute is present
 * and it has other values, i.e. "n kyr B.P.", "common_year",
 * "no_leap", "365_day", then _FillValue will be returned.
 */

NhlErrorTypes ut_calendar_W( void )
{
/*
 * Input array variables
 */
  void *x;
  double *tmp_x;
  string *sspec;
  char *cspec;
  int *option;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  NclScalar missing_x, missing_dx;
  NclBasicDataTypes type_x;
/*
 * Variables for retrieving attributes from "options".
 */
  NclAttList  *attr_list;
  NclAtt  attr_obj;
  NclStackEntry   stack_entry;
  string *calendar;
  char   *ccal;
/*
 * Variables for Udunits package.
 */
  int utopen();
  utUnit unit;
/*
 * Output variables.
 */
  int year, month, day, hour, minute;
  float second, *date;
  int ndims_date, *dsizes_date;
  NclScalar missing_date;
/*
 * various
 */
  int i, total_size_x, index_date, return_missing;

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
           2,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           2);
/*
 * Get option.
 */
  option = (int*)NclGetArgValue(
           1,
           2,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           1);
/*
 * Coerce missing value to double, and get the default missing
 * value for a float type.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,NULL);

  missing_date = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis;

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
  date        = (float *)calloc(6*total_size_x,sizeof(float));

  if( date == NULL || dsizes_date == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ut_calendar: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_x; i++ ) dsizes_date[i] = dsizes_x[i];
  dsizes_date[ndims_x] = 6;

/* 
 * The "units" attribute of "time" must be set, otherwise missing
 * values will be returned.
 *
 * The "calendar" option may optionally be set, but it must be equal to
 * "standard" or "gregorian".
 */
  return_missing = 0;

  stack_entry = _NclGetArg(0, 2, DONT_CARE);
  switch (stack_entry.kind) {
  case NclStk_VAR:
    if (stack_entry.u.data_var->var.att_id != -1) {
      attr_obj = (NclAtt) _NclGetObj(stack_entry.u.data_var->var.att_id);
      if (attr_obj == NULL) {
		return_missing = 1;
        break;
      }
    }
    else {
/*
 * att_id == -1 ==> no optional args given; use default calendar.
 */
	  return_missing = 1;
      break;
    }
/* 
 * Get optional arguments. If none are specified, then return
 * missing values.
 */
    if (attr_obj->att.n_atts == 0) {
	  return_missing = 1;
      break;
    }
    else {
/*
 * Get list of attributes.
 */
      attr_list = attr_obj->att.att_list;
/*
 * Loop through attributes and check them.
 */
      while (attr_list != NULL) {
        if ((strcmp(attr_list->attname, "calendar")) == 0) {
          calendar = (string *) attr_list->attvalue->multidval.val;
          ccal     = NrmQuarkToString(*calendar);
          if(strcmp(ccal,"standard") && strcmp(ccal,"gregorian")) {
            NhlPError(NhlWARNING,NhlEUNKNOWN,"ut_calendar: the 'calendar' attribute is not equal to 'standard' or 'gregorian'. This function only understands a mixed Julian/Gregorian calendar. Returning all missing values.");
			return_missing = 1;
          }
        }
        if ((strcmp(attr_list->attname, "units")) == 0) {
          sspec = (string *) attr_list->attvalue->multidval.val;
          cspec = NrmQuarkToString(*sspec);
/*
 * Make sure cspec is a valid udunits string.
 */
		  if(utScan(cspec, &unit) != 0) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"ut_calendar: Invalid specification string");
			return_missing = 1;
          }
        }
        attr_list = attr_list->next;
      }
    }
  default:
    break;
  }

/* 
 * If we reach this point and return_missing is not 0, then either
 * "units" was invalid or wasn't set , or "calendar" was not a
 * recoginized calendar. We return all missing values in this case
 */
  if(return_missing) {
	for(i = 0; i < 6*total_size_x; i++) {
	  date[i] = missing_date.floatval;
	}
/*
 * Close up Udunits.
 */
	utTerm();
/*
 * Return all missing values.
 */
	return(NclReturnValue(date,ndims_date,dsizes_date,
						  &missing_date,NCL_float,0));
  }
            
/*
 * Convert input to double if necessary.
 */
  tmp_x = coerce_input_double(x,type_x,total_size_x,has_missing_x,&missing_x,
                  &missing_dx);
/* 
 * Loop through each element and get the 6 values.
 */
  index_date = 0;
  for( i = 0; i < total_size_x; i++ ) {
    if(!has_missing_x ||
       (has_missing_x && tmp_x[i] != missing_dx.doubleval)) {
      (void) utCalendar(tmp_x[i],&unit,&year,&month,&day,
            &hour,&minute,&second);
    
      date[index_date]   = (float)year;
      date[index_date+1] = (float)month;
      date[index_date+2] = (float)day;
      date[index_date+3] = (float)hour;
      date[index_date+4] = (float)minute;
      date[index_date+5] = second;
    }
    else {
      date[index_date]   = missing_date.floatval;
      date[index_date+1] = missing_date.floatval;
      date[index_date+2] = missing_date.floatval;
      date[index_date+3] = missing_date.floatval;
      date[index_date+4] = missing_date.floatval;
      date[index_date+5] = missing_date.floatval;
    }
	index_date += 6;
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

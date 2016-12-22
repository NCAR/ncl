#include <stdio.h>
#include "wrapper.h"

extern int day_of_year (int, int, int, const char*);
extern int days_in_month (int, int, const char*);
extern int day_of_week (int, int, int);
extern int monthday (int, int, const char*);
extern int isleapyear(int,const char*);
extern const char *get_calendar_attribute(int,int);

NhlErrorTypes day_of_year_W( void )
{
  int i;
  ng_size_t total;
/*
 * Input variables
 */
  int *year, *month, *day;
  int ndims_year, has_missing_year;
  ng_size_t dsizes_year[NCL_MAX_DIMENSIONS];
  int ndims_month, has_missing_month;
  ng_size_t dsizes_month[NCL_MAX_DIMENSIONS];
  int ndims_day, has_missing_day;
  ng_size_t dsizes_day[NCL_MAX_DIMENSIONS];
  NclScalar missing_year, missing_month, missing_day;
/*
 * Output variables
 */
  int *dayofyear, has_missing_dayofyear;
  NclScalar missing_dayofyear;
  const char *calendar;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */

/*
 * Retrieve argument #1
 */
  year = (int*)NclGetArgValue(
          0,
          3,
          &ndims_year, 
          dsizes_year,
          &missing_year,
          &has_missing_year,
          NULL,
          DONT_CARE);
/*
 * Retrieve argument #2
 */
  month = (int*)NclGetArgValue(
          1,
          3,
          &ndims_month, 
          dsizes_month,
          &missing_month,
          &has_missing_month,
          NULL,
          DONT_CARE);
/*
 * Retrieve argument #3
 */
  day = (int*)NclGetArgValue(
          2,
          3,
          &ndims_day, 
          dsizes_day,
          &missing_day,
          &has_missing_day,
          NULL,
          DONT_CARE);
/*
 * Check dimensions and calculate total size of arrays.
 */
  if( ndims_year != ndims_month || ndims_year != ndims_day ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"day_of_year: The input arrays must be the same size");
    return(NhlFATAL);
  }

  total = 1;
  for( i = 0; i < ndims_year; i++ ) {
    if( dsizes_year[i] != dsizes_day[i] || dsizes_year[i] != dsizes_month[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"day_of_year: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
    total *= dsizes_year[i];
  }
/*
 * Allocate space for output array.
 */
  dayofyear = (int*)NclMalloc(total*sizeof(int));
  if( dayofyear == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"day_of_year: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Check for potential missing values. 
 */
  missing_dayofyear.intval = -9999;
  if(has_missing_year || has_missing_month || has_missing_day) {
    has_missing_dayofyear = 1;
  }
  else {
    has_missing_dayofyear = 0;
  }
    
/*
 * Check for a calendar attribute. If it comes back NULL, then 
 * something is wrong and we need to return all missing values.
 */
  calendar = get_calendar_attribute(0,3);
  if(calendar == NULL) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"day_of_year: Invalid calendar. Returning all missing values");
    for( i = 0; i < total; i++ ) dayofyear[i] = missing_dayofyear.intval;
    return(NclReturnValue((void*)dayofyear,ndims_year,dsizes_year,
                          &missing_dayofyear,NCL_int,0));
  }

/*
 * Loop through each value and calculate the value.
 * 
 * If any of the input is potentially missing, then check for this. If
 * none of the input is missing, it's still possible for the function to
 * return missing, if a bad value was input (like month = 14).
 */
  if(has_missing_dayofyear) {
    for( i = 0; i < total; i++ ) {
      if( (has_missing_year  && year[i]  == missing_year.intval) ||
          (has_missing_month && month[i] == missing_month.intval) ||
          (has_missing_day   && day[i]   == missing_day.intval)) {
        dayofyear[i] = missing_dayofyear.intval;
      }
      else {
        dayofyear[i] = day_of_year(year[i],month[i],day[i],calendar);
      }
    }
  }
  else {    
    for( i = 0; i < total; i++ ) {
      dayofyear[i] = day_of_year(year[i],month[i],day[i],calendar);
      if(dayofyear[i] == missing_dayofyear.intval) has_missing_dayofyear = 1;
    }
  }
/*
 * Return.
 */
  if(has_missing_dayofyear) {
    return(NclReturnValue((void*)dayofyear,ndims_year,dsizes_year,
                          &missing_dayofyear,NCL_int,0));
    
  }
  else {
    return(NclReturnValue((void*)dayofyear,ndims_year,dsizes_year,
                          NULL,NCL_int,0));
  }
}

NhlErrorTypes days_in_month_W( void )
{
  int i;
  ng_size_t total;
/*
 * Input variables
 */
  int *year, *month;
  int ndims_year, has_missing_year;
  ng_size_t dsizes_year[NCL_MAX_DIMENSIONS];
  int ndims_month, has_missing_month;
  ng_size_t dsizes_month[NCL_MAX_DIMENSIONS];
  NclScalar missing_year, missing_month;
/*
 * Output variables
 */
  int *daysinmonth, has_missing_daysinmonth;
  NclScalar missing_daysinmonth;
  const char *calendar;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */

/*
 * Retrieve argument #1
 */
  year = (int*)NclGetArgValue(
          0,
          2,
          &ndims_year, 
          dsizes_year,
          &missing_year,
          &has_missing_year,
          NULL,
          DONT_CARE);
/*
 * Retrieve argument #2
 */
  month = (int*)NclGetArgValue(
          1,
          2,
          &ndims_month, 
          dsizes_month,
          &missing_month,
          &has_missing_month,
          NULL,
          DONT_CARE);
/*
 * Check dimensions and calculate total size of arrays.
 */
  if( ndims_year != ndims_month ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"days_in_month: The input arrays must be the same size");
    return(NhlFATAL);
  }

  total = 1;
  for( i = 0; i < ndims_year; i++ ) {
    if( dsizes_year[i] != dsizes_month[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"days_in_month: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
    total *= dsizes_year[i];
  }
/*
 * Allocate space for output array.
 */
  daysinmonth = (int*)NclMalloc(total*sizeof(int));
  if( daysinmonth == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"days_in_month: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Check for potential missing values
 */
  missing_daysinmonth.intval = -9999;
  if(has_missing_year || has_missing_month) {
    has_missing_daysinmonth = 1;
  }
  else {
    has_missing_daysinmonth = 0;
  }

/*
 * Check for a calendar attribute. If it comes back NULL, then 
 * something is wrong and we need to return all missing values.
 */
  calendar = get_calendar_attribute(0,2);
  if(calendar == NULL) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"days_in_month: Invalid calendar. Returning all missing values");
    for( i = 0; i < total; i++ ) daysinmonth[i] = missing_daysinmonth.intval;
    return(NclReturnValue((void*)daysinmonth,ndims_year,dsizes_year,
                          &missing_daysinmonth,NCL_int,0));
  }

/*
 * Loop through each value and calculate the value.
 * 
 * If any of the input is potentially missing, then check for this. If
 * none of the input is missing, it's still possible for the function to
 * return missing, if a bad value was input (like month = 14).
 */
  if(has_missing_daysinmonth) {
    for( i = 0; i < total; i++ ) {
      if( (has_missing_year  && year[i]  == missing_year.intval) ||
          (has_missing_month && month[i] == missing_month.intval)) {
        daysinmonth[i] = missing_daysinmonth.intval;
      }
      else {
        daysinmonth[i] = days_in_month(year[i],month[i],calendar);
      }
    }
  }
  else {
    for( i = 0; i < total; i++ ) {
      daysinmonth[i] = days_in_month(year[i],month[i],calendar);
      if(daysinmonth[i] == missing_daysinmonth.intval) has_missing_daysinmonth = 1;
    }
  }
/*
 * Return.
 */
  if(has_missing_daysinmonth) {
    return(NclReturnValue((void*)daysinmonth,ndims_year,dsizes_year,
                          &missing_daysinmonth,NCL_int,0));
  }
  else {
    return(NclReturnValue((void*)daysinmonth,ndims_year,dsizes_year,
                          NULL,NCL_int,0));
  }
}

NhlErrorTypes day_of_week_W( void )
{
  int i;
  ng_size_t total;
/*
 * Input variables
 */
  int *year, *month, *day;
  int ndims_year, has_missing_year;
  ng_size_t dsizes_year[NCL_MAX_DIMENSIONS];
  int ndims_month, has_missing_month;
  ng_size_t dsizes_month[NCL_MAX_DIMENSIONS];
  int ndims_day, has_missing_day;
  ng_size_t dsizes_day[NCL_MAX_DIMENSIONS];
  NclScalar missing_year, missing_month, missing_day;
/*
 * Output variables
 */
  int *dayofweek, has_missing_dayofweek;
  NclScalar missing_dayofweek;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 *
 * Retrieve argument #1
 */
  year = (int*)NclGetArgValue(
          0,
          3,
          &ndims_year, 
          dsizes_year,
          &missing_year,
          &has_missing_year,
          NULL,
          DONT_CARE);

  month = (int*)NclGetArgValue(
          1,
          3,
          &ndims_month, 
          dsizes_month,
          &missing_month,
          &has_missing_month,
          NULL,
          DONT_CARE);

  day = (int*)NclGetArgValue(
          2,
          3,
          &ndims_day, 
          dsizes_day,
          &missing_day,
          &has_missing_day,
          NULL,
          DONT_CARE);
/*
 * Check dimensions and calculate total size of arrays.
 */
  if( ndims_year != ndims_month || ndims_year != ndims_day ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"day_of_week: The input arrays must be the same size");
    return(NhlFATAL);
  }

  total = 1;
  for( i = 0; i < ndims_year; i++ ) {
    if( dsizes_year[i] != dsizes_day[i] || dsizes_year[i] != dsizes_month[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"day_of_week: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
    total *= dsizes_year[i];
  }
/*
 * Allocate space for output array.
 */
  dayofweek = (int*)NclMalloc(total*sizeof(int));
  if( dayofweek == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"day_of_week: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Check for potential missing values
 */
  missing_dayofweek.intval = -9999;
  if(has_missing_year || has_missing_month || has_missing_day) {
    has_missing_dayofweek = 1;
  }
  else {
    has_missing_dayofweek = 0;
  }

/*
 * Loop through each value and calculate the value.
 * 
 * If any of the input is potentially missing, then check for this. If
 * none of the input is missing, it's still possible for the function to
 * return missing, if a bad value was input (like month = 14).
 */
  if(has_missing_dayofweek) {
    for( i = 0; i < total; i++ ) {
      if( (has_missing_year  && year[i]  == missing_year.intval) ||
          (has_missing_month && month[i] == missing_month.intval) ||
          (has_missing_day   && day[i]   == missing_day.intval)) {
        dayofweek[i] = missing_dayofweek.intval;
      }
      else {
        dayofweek[i] = day_of_week(year[i],month[i],day[i]);
      }
    }
  }
  else {
    for( i = 0; i < total; i++ ) {
      dayofweek[i] = day_of_week(year[i],month[i],day[i]);
      if(dayofweek[i] == missing_dayofweek.intval) has_missing_dayofweek = 1;
    }
  }
/*
 * Return.
 */
  if(has_missing_dayofweek) {
    return(NclReturnValue((void*)dayofweek,ndims_year,dsizes_year,
                          &missing_dayofweek,NCL_int,0));
  }
  else {
    return(NclReturnValue((void*)dayofweek,ndims_year,dsizes_year,
                          NULL,NCL_int,0));
  }
}


NhlErrorTypes isleapyear_W( void )
{
  int i;
  ng_size_t total;
/*
 * Input variables
 */
  int *year;
  int ndims_year, has_missing_year;
  ng_size_t dsizes_year[NCL_MAX_DIMENSIONS];
  NclScalar missing_year;
/*
 * Output variables
 */
  int has_missing_isleap, iret;
  logical *isleap;
  NclScalar missing_isleap;
  const char *calendar;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */

/*
 * Retrieve argument #1
 */
  year = (int*)NclGetArgValue(
          0,
          1,
          &ndims_year, 
          dsizes_year,
          &missing_year,
          &has_missing_year,
          NULL,
          DONT_CARE);
/*
 * Calculate total size of array.
 */
  total = 1;
  for( i = 0; i < ndims_year; i++ )     total *= dsizes_year[i];
/*
 * Allocate space for output array.
 */
  isleap = (logical*)NclMalloc(total*sizeof(logical));
  if( isleap == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"isleapyear: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Check for potential missing values
 */
  missing_isleap.logicalval = ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval;
  if(has_missing_year) {
    has_missing_isleap = 1;
  }
  else {
    has_missing_isleap = 0;
  }    

/*
 * Check for a calendar attribute. If it comes back NULL, then 
 * something is wrong and we need to return all missing values.
 */
  calendar = get_calendar_attribute(0,1);
  if(calendar == NULL) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"isleapyear: Invalid calendar. Returning all missing values");
    for( i = 0; i < total; i++ ) isleap[i] = missing_isleap.logicalval;
    return(NclReturnValue((void*)isleap,ndims_year,dsizes_year,
                          &missing_isleap,NCL_logical,0));
  }
/*
 * Loop through each value and calculate the value.
 * 
 * If any of the input is potentially missing, then check for this. If
 * none of the input is missing, it's still possible for the function to
 * return missing, if a bad value was input (like year = -1990).
 */
  if(has_missing_isleap) {
    for( i = 0; i < total; i++ ){ 
      if(year[i] == missing_year.intval) {
        isleap[i] = missing_isleap.logicalval;
      }
      else {
        iret = (logical)isleapyear(year[i],calendar);
        if(iret != -9999) isleap[i] = (logical)iret;
        else              isleap[i] = missing_isleap.logicalval;
      }
    }
  }
  else {
    for( i = 0; i < total; i++ ) {
      iret = (logical)isleapyear(year[i],calendar);
      if(iret != -9999) {
        isleap[i] = (logical)iret;
      }
      else {
        has_missing_isleap = 1;
        isleap[i] = missing_isleap.logicalval;
      }
    }
  }
/*
 * Return.
 */
  if(has_missing_isleap) {
    return(NclReturnValue((void*)isleap,ndims_year,dsizes_year,
                          &missing_isleap,NCL_logical,0));
  }
  else {
    return(NclReturnValue((void*)isleap,ndims_year,dsizes_year,
                          NULL,NCL_logical,0));
  }
}

NhlErrorTypes monthday_W( void )
{
  int i;
  ng_size_t total;
/*
 * Input variables
 */
  int *year, *day;
  int ndims_year, has_missing_year;
  ng_size_t dsizes_year[NCL_MAX_DIMENSIONS];
  int ndims_day, has_missing_day;
  ng_size_t dsizes_day[NCL_MAX_DIMENSIONS];
  NclScalar missing_year, missing_day;
/*
 * Output variables
 */
  int *mnthdy, has_missing_mnthdy;
  NclScalar missing_mnthdy;
  const char *calendar;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */

/*
 * Retrieve argument #1
 */
  year = (int*)NclGetArgValue(
          0,
          2,
          &ndims_year, 
          dsizes_year,
          &missing_year,
          &has_missing_year,
          NULL,
          DONT_CARE);
/*
 * Retrieve argument #2
 */
  day = (int*)NclGetArgValue(
          1,
          2,
          &ndims_day, 
          dsizes_day,
          &missing_day,
          &has_missing_day,
          NULL,
          DONT_CARE);
/*
 * Check dimensions and calculate total size of arrays.
 */
  if( ndims_year != ndims_day ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"monthday: The input arrays must be the same size");
    return(NhlFATAL);
  }

  total = 1;
  for( i = 0; i < ndims_year; i++ ) {
    if( dsizes_year[i] != dsizes_day[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"monthday: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
    total *= dsizes_year[i];
  }
/*
 * Allocate space for output array.
 */
  mnthdy = (int*)NclMalloc(total*sizeof(int));
  if( mnthdy == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"monthday: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Check for potential missing values
 */
  missing_mnthdy.intval = -9999;
  if(has_missing_year || has_missing_day) {
    has_missing_mnthdy = 1;
  }
  else {
    has_missing_mnthdy = 0;
  }

/*
 * Check for a calendar attribute. If it comes back NULL, then 
 * something is wrong and we need to return all missing values.
 */
  calendar = get_calendar_attribute(0,2);
  if(calendar == NULL) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"monthday: Invalid calendar. Returning all missing values");
    for( i = 0; i < total; i++ ) mnthdy[i] = missing_mnthdy.intval;
    return(NclReturnValue((void*)mnthdy,ndims_year,dsizes_year,
                          &missing_mnthdy,NCL_int,0));
  }

/*
 * Loop through each value and calculate the value.
 * 
 * If any of the input is potentially missing, then check for this. If
 * none of the input is missing, it's still possible for the function to
 * return missing, if a bad value was input (like year = 14).
 */
  if(has_missing_mnthdy) {
    for( i = 0; i < total; i++ ) {
      if( (has_missing_year  && year[i]  == missing_year.intval) ||
          (has_missing_day   && day[i]   == missing_day.intval)) {
        mnthdy[i] = missing_mnthdy.intval;
      }
      else {
        mnthdy[i] = monthday(year[i],day[i],calendar);
      }
    }
  }
  else {
    for( i = 0; i < total; i++ ) {
      mnthdy[i] = monthday(year[i],day[i],calendar);
      if(mnthdy[i] == missing_mnthdy.intval) has_missing_mnthdy = 1;
    }
  }
/*
 * Return.
 */
  if(has_missing_mnthdy) {
    return(NclReturnValue((void*)mnthdy,ndims_year,dsizes_year,
                          &missing_mnthdy,NCL_int,0));
  }
  else {
    return(NclReturnValue((void*)mnthdy,ndims_year,dsizes_year,
                          NULL,NCL_int,0));
  }
}

int day_of_year (int year, int month, int day, const char *calendar)
{ 
  int dofy;
/* 
 * This function will return the day of the year.
 *
 * An error will result in day_of_year being set to -9999
 * 
 * In NCL V6.1.0, this function was updated to recognize the
 * "calendar" attribute.
 */ 
  int yrday[12]       = { 1,32,60,91,121,152,182,213,244,274,305,335};
  int daysinmonth[12] = {31,28,31,30, 31, 30, 31, 31, 30, 31, 30, 31};
  int yrday360[12]       = { 1,31,61,91,121,151,181,211,241,271,301,331};
  int daysinmonth360[12] = {30,30,30,30, 30, 30, 30, 30, 30, 30, 30, 30};
  int msg = -9999;

  if (calendar == NULL) {
    fprintf (stderr,"day_of_year: calendar is NULL\n");
        return(msg);
  }

  if (month < 1 || month > 12) {
        fprintf(stderr,"day_of_year: illegal month, %d\n", month);
        return(msg);
  }
  if (year < 0) {
        fprintf(stderr,"day_of_year: illegal year, year = %d\n", year);
        return(msg);
  }
  if (day < 1) {
        fprintf(stderr,"day_of_year: illegal day, day = %d\n", day);
        return(msg);
  }
  if(!strcasecmp(calendar,"standard")  || 
     !strcasecmp(calendar,"gregorian") ||
     !strcasecmp(calendar,"julian") ||
     !strcasecmp(calendar,"none")) {
    if (  (isleapyear(year,calendar) && month == 2 && day > 29) ||
          (!(isleapyear(year,calendar) && month == 2) && 
           day > daysinmonth[month-1])) {
      fprintf(stderr,"day_of_year: illegal arguments, year = %d, month = %d, day = %d\n", year, month, day);
      return(msg);
    }
    else {
      dofy = yrday[month-1] + day - 1;
      if (isleapyear(year,calendar) && month > 2) dofy++;
    }
  }
  else if(!strcasecmp(calendar,"allleap")  ||
          !strcasecmp(calendar,"all_leap") ||
          !strcasecmp(calendar,"366_day")  ||
          !strcasecmp(calendar,"366")) {
    dofy = yrday[month-1] + day - 1;
    if(month > 2) dofy++;
  }
  else if(!strcasecmp(calendar,"noleap")  ||
          !strcasecmp(calendar,"no_leap") ||
          !strcasecmp(calendar,"365_day") ||
          !strcasecmp(calendar,"365")) {
    dofy = yrday[month-1] + day - 1;
  }
  else if(!strcasecmp(calendar,"360_day") ||
          !strcasecmp(calendar,"360")) {
    if(day > daysinmonth360[month-1]) {
      fprintf(stderr,"day_of_year: illegal arguments for 360 calendar, year = %d, month = %d, day = %d\n", year, month, day);
      return(msg);
    }
    else {
      dofy = yrday360[month-1] + day - 1;
    }
  }
  else  {
    fprintf (stderr,"day_of_year: illegal calendar = '%s'\n", calendar);
    return(msg);
  }
  
  return(dofy);
}

int days_in_month (int year, int month, const char *calendar)
{ 
  int dinm;
/* 
 * This function will return the number of days in a particular month.
 *
 * An error will result in days_in_month being set to -9999
 * 
 * In NCL V6.1.0, this function was updated to recognize the
 * "calendar" attribute.
 */ 
  int daysinmonth[12] = {31,28,31,30,31,30,31,31,30,31,30,31};
  int msg = -9999;  

  if (calendar == NULL) {
    fprintf (stderr,"days_in_month: calendar is NULL\n");
    return(msg);
  }

  if (month < 1 || month > 12 || year < 0) {
    fprintf(stderr,"days_in_month: illegal argument, year = %d, month = %d\n", year, month);
    return(msg);
  }
  if(!strcasecmp(calendar,"standard")  || 
     !strcasecmp(calendar,"gregorian") ||
     !strcasecmp(calendar,"julian") ||
     !strcasecmp(calendar,"none")) {
    dinm = daysinmonth[month-1];
    if (isleapyear(year,calendar) && (month == 2)) dinm = 29;
  }
  else if(!strcasecmp(calendar,"allleap")  ||
          !strcasecmp(calendar,"all_leap") ||
          !strcasecmp(calendar,"366_day")  ||
          !strcasecmp(calendar,"366")) {
    dinm = daysinmonth[month-1];
    if(month == 2) dinm = 29;
  }
  else if(!strcasecmp(calendar,"noleap")  ||
          !strcasecmp(calendar,"no_leap") ||
          !strcasecmp(calendar,"365_day") ||
          !strcasecmp(calendar,"365")) {
    dinm = daysinmonth[month-1];
  }
  else if(!strcasecmp(calendar,"360_day") ||
          !strcasecmp(calendar,"360")) {
    dinm = 30;
  }
  else  {
    fprintf (stderr,"days_in_month: illegal calendar = '%s'\n", calendar);
    return(msg);
  }
  return(dinm);
}

int day_of_week (int year, int month, int day)
{ 
  int dow;
  int daysinmonth[12] = {31,28,31,30,31,30,31,31,30,31,30,31};
  int msg=-9999;
/* 
 * This function will return the day of the week (0-6) given year, month,
 * and day of month.
 *
 * An error will result in day_of_week being set to msg
 *
 *    Day_Of_Week: (0=Sunday,1=Monday...6=Saturday)
 *    cf J.D.Robertson, CACM 15(10):918
 */
  if (month < 1 || month > 12) {
        fprintf(stderr,"day_of_week: illegal month, %d\n", month);
        dow = msg;
  }
  else if (year < 0) {
        fprintf(stderr,"day_of_week: illegal year, year = %d\n", year);
        dow = msg;
  }
  else if (day < 1 || (isleapyear(year,"standard") && month == 2 && day > 29) ||
           (!(isleapyear(year,"standard") && month == 2) && 
            day > daysinmonth[month-1])) {
        fprintf(stderr,"day_of_week: illegal arguments, year = %d, month = %d, day = %d\n", year, month, day);
        dow = msg;
  }
  else {
    dow = ((13*(month+10-(month+10)/13*12)-1)/5+day+77           
           +5*(year+(month-14)/12-(year+(month-14)/12)/100*100)/4   
           +(year+(month-14)/12)/400-(year+(month-14)/12)/100*2) % 7;
  }
  return(dow);
}

int monthday (int year, int dayofyear, const char *calendar)
{ 
/*
 * Given the year and day of year this function will return
 * the concatenated month and day (integer).
 * 
 * For example: if year = 1933 and dayofyear = 245
 * then monthday= 902 for September 2.
 *
 * An error will result in monthday being set to -9999
 * 
 * In NCL V6.1.0, this function was updated to recognize the
 * "calendar" attribute.
 */ 
  int i, work[13];
  int mday, msg=-9999;
  int yearday[13]    = {1,32,60,91,121,152,182,213,244,274,305,335,367};
  int yearday360[13] = {1,31,61,91,121,151,181,211,241,271,301,331,361};

  mday = msg;

  if (calendar == NULL) {
    fprintf (stderr,"monthday: calendar is NULL\n");
    return(msg);
  }

  if (dayofyear < 1 || year < 0) {
    fprintf (stderr,"monthday: illegal argument, year = %d, dayofyear = %d\n", year, dayofyear);
    return(msg);
  }

  if(!strcasecmp(calendar,"standard")  || 
     !strcasecmp(calendar,"gregorian") ||
     !strcasecmp(calendar,"julian") ||
     !strcasecmp(calendar,"none")) {
/* 
 * Error check
 */
    if ((isleapyear(year,calendar) && dayofyear > 366) || (!isleapyear(year,calendar) && dayofyear > 365)) {
      fprintf (stderr,"monthday: illegal argument, year = %d, dayofyear = %d\n", year, dayofyear);
      return(msg);
    }
/*
 * Easy way around leap year problem.
 */
    for( i = 0; i < 13; i++ ) work[i] = yearday[i];
/*
 * Add one day to normal year vector.
 */
    if (isleapyear(year,calendar)) {
      for( i=2; i < 12; i++ ) work[i] = yearday[i]+1;
    }
    for( i = 1; i <= 12; i++ ) {
      if (dayofyear >= work[i-1] && dayofyear < work[i]) {
        mday = i*100 + (dayofyear - work[i-1]+1);
      }
    }
    return(mday);
  }
  else if(!strcasecmp(calendar,"allleap")  ||
          !strcasecmp(calendar,"all_leap") ||
          !strcasecmp(calendar,"366_day")  ||
          !strcasecmp(calendar,"366")) {
/* 
 * Error check
 */
    if (dayofyear > 366) {
      fprintf (stderr,"monthday: illegal argument for all leap calendar: dayofyear = %d\n", dayofyear);
      return(msg);
    }

    for( i = 0; i < 13; i++ ) work[i] = yearday[i];
/*
 * Add one day to normal year vector.
 */
    for( i=2; i < 12; i++ ) work[i] = yearday[i]+1;
    for( i = 1; i <= 12; i++ ) {
      if (dayofyear >= work[i-1] && dayofyear < work[i]) {
        mday = i*100 + (dayofyear - work[i-1]+1);
      }
    }
    return(mday);
  }
  else if(!strcasecmp(calendar,"noleap")  ||
          !strcasecmp(calendar,"no_leap") ||
          !strcasecmp(calendar,"365_day") ||
          !strcasecmp(calendar,"365")) {
/* 
 * Error check
 */
    if (dayofyear > 365) {
      fprintf (stderr,"monthday: illegal argument for no leap calendar: dayofyear = %d\n", dayofyear);
      return(msg);
    }
    for( i = 0; i < 13; i++ ) work[i] = yearday[i];
    for( i = 1; i <= 12; i++ ) {
      if (dayofyear >= work[i-1] && dayofyear < work[i]) {
        mday = i*100 + (dayofyear - work[i-1]+1);
      }
    }
    return(mday);
  }
  else if(!strcasecmp(calendar,"360_day") ||
          !strcasecmp(calendar,"360")) {
/* 
 * Error check
 */
    if (dayofyear > 360) {
      fprintf (stderr,"monthday: illegal argument for 360-day calendar: dayofyear = %d\n", dayofyear);
      return(msg);
    }

    for( i = 0; i < 13; i++ ) work[i] = yearday360[i];
    for( i = 1; i <= 12; i++ ) {
      if (dayofyear >= work[i-1] && dayofyear < work[i]) {
        mday = i*100 + (dayofyear - work[i-1]+1);
      }
    }
    return(mday);
  }
  else  {
    fprintf (stderr,"monthday: illegal calendar = '%s'\n", calendar);
    return(msg);
  }
}

int seconds_in_year (int year, const char *calendar)
{
/*      
 * This function will return the number of seconds in a year,
 * given a calendar. This is meant as an internal function for
 * option 4 of cd_calendar/ut_calendar.
 */
  int nsid, tsiy, msg=-9999;

  nsid = 86400;      /* num seconds in a day */
  if(!strcasecmp(calendar,"standard")  || 
          !strcasecmp(calendar,"gregorian") ||
          !strcasecmp(calendar,"julian") ||
          !strcasecmp(calendar,"none")) {
    tsiy = isleapyear(year,calendar) ? 366*nsid : 365*nsid;
  }
  else if(!strcasecmp(calendar,"noleap")  ||
          !strcasecmp(calendar,"no_leap") ||
          !strcasecmp(calendar,"365_day") ||
          !strcasecmp(calendar,"365")) {
    tsiy = 365*nsid;
  }
  else if(!strcasecmp(calendar,"allleap")  ||
          !strcasecmp(calendar,"all_leap") ||
          !strcasecmp(calendar,"366_day")  ||
          !strcasecmp(calendar,"366")) {
    tsiy = 366*nsid;
  }
  else if(!strcasecmp(calendar,"360_day") ||
          !strcasecmp(calendar,"360")) {
    tsiy = 360*nsid;
  }
  else {
    fprintf(stderr,"seconds_in_year: illegal calendar\n");
    tsiy = msg;
  }
  return(tsiy);
}

int isleapyear(int year,const char *calendar)
{
/*      
 * This function will return a value of 1 if year is a leap year.
 * If year is not a leap year then a value of 0 is returned.
 * 
 * A year is a leap year if it is divisible by 4, unless year is a century
 * year (e.g. 1800, 1900, 2000, etc.) in which case it must be divisable
 * by 400.
 * 
 * In NCL V6.1.0, this function was updated to recognize the
 * "calendar" attribute.
 */
  int y4, y100, y400;
  int lmsg = -9999;
 
  if (year < 0) {
    fprintf (stderr,"isleapyear: illegal argument, year = %d\n", year);
    return(lmsg);
  }

  if (calendar == NULL) {
    fprintf (stderr,"isleapyear: calendar is NULL\n");
    return(lmsg);
  }

  if(!strcasecmp(calendar,"standard")  || 
     !strcasecmp(calendar,"gregorian") ||
     !strcasecmp(calendar,"none")) {
    y4   = (year % 4) == 0;
    y100 = (year % 100) == 0;
    y400 = (year % 400) == 0;
    return((y4 && !y100) || y400);
  }
  else if(!strcasecmp(calendar,"allleap")  ||
          !strcasecmp(calendar,"all_leap") ||
          !strcasecmp(calendar,"366_day")  ||
          !strcasecmp(calendar,"366")) {
    return(1);
  }
  else if(!strcasecmp(calendar,"noleap")  ||
          !strcasecmp(calendar,"no_leap") ||
          !strcasecmp(calendar,"365_day") ||
          !strcasecmp(calendar,"365")     ||
          !strcasecmp(calendar,"360_day") ||
          !strcasecmp(calendar,"360")) {
    return(0);
  }
  else if(!strcasecmp(calendar,"julian")) {
    y4   = (year % 4) == 0;
    return(y4);
  }
  else  {
    fprintf (stderr,"isleapyear: illegal calendar = '%s'\n", calendar);
    return(lmsg);
  }
}


/*
 * These are the calendars that are currently recognized by most of
 * the date routines:
 *      "standard", "gregorian"
 *      "noleap",  "no_leap"
 *      "allleap", "all_leap"
 *      "365_day", "365"
 *      "366_day", "366"
 *      "360_day", "360"
 *      "proleptic_gregorian"
 *      "julian"
 *      "none"
 */

const char *get_calendar_attribute(int arg_num, int total_args) {
/* 
 * This function checks the given input argument for
 * a "calendar" attribute. If none is found, then NULL
 * is returned.
 */
/*
 * Variables for retrieving attributes from the first argument.
 */
  NclAttList  *attr_list;
  NclAtt  attr_obj;
  NclStackEntry   stack_entry;
  NrmQuark *scal;
  const char   *ccal = NULL;
  const char *default_cal = "standard";

  stack_entry = _NclGetArg(arg_num, total_args, DONT_CARE);
  switch (stack_entry.kind) {
  case NclStk_VAR:
    if (stack_entry.u.data_var->var.att_id != -1) {
      attr_obj = (NclAtt) _NclGetObj(stack_entry.u.data_var->var.att_id);
      if (attr_obj == NULL) {
        return default_cal;
        break;
      }
    }
    else {
/*
 * att_id == -1 ==> no attributes specified.
 */
      return default_cal;
      break;
    }
/* 
 * Check for attributes. If none are specified, then return the default.
 */
    if (attr_obj->att.n_atts == 0) {
      return default_cal;
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
          scal = (NrmQuark *) attr_list->attvalue->multidval.val;
          ccal = NrmQuarkToString(*scal);
          if(strcasecmp(ccal,"standard") && strcasecmp(ccal,"gregorian") &&
             strcasecmp(ccal,"proleptic_gregorian") &&
             strcasecmp(ccal,"noleap")  && strcasecmp(ccal,"no_leap") &&
             strcasecmp(ccal,"allleap") && strcasecmp(ccal,"all_leap") &&
             strcasecmp(ccal,"365_day") && strcasecmp(ccal,"365") &&
             strcasecmp(ccal,"366_day") && strcasecmp(ccal,"366") &&
             strcasecmp(ccal,"360_day") && strcasecmp(ccal,"360") &&
             strcasecmp(ccal,"julian")  && strcasecmp(ccal,"none")) {
            NhlPError(NhlWARNING,NhlEUNKNOWN,"get_calendar_attribute: the 'calendar' attribute (%s) is not equal to a recognized calendar.",ccal);
            return(NULL);
          }
          else {
            return(ccal);
          }
        }
        attr_list = attr_list->next;
      }
    }
  default:
    break;
  }
  return default_cal;
}

double fraction_of_year (int year, int month, int day, int hour, int minute,
			 double second, const char *calendar)
{
/*      
 * This function will calculate how far we are into the current year
 * in seconds, and return it as a fraction of the year.
 *
 * This is meant as an internal function for option 4 of cd_calendar and
 * ut_calendar.
 */
  int doy, nsid, total_seconds_in_year, seconds_in_doy, seconds_in_hour;
  int seconds_in_minute;
  double current_seconds_in_year;

  nsid = 86400;      /* num seconds in a day */
/* 
 * Calculate how far we are into current year and multiply
 * by number of seconds in a day.
 */
  if(calendar == NULL) {
    total_seconds_in_year = seconds_in_year(year,"standard");
    doy = day_of_year(year,month,day,"standard");
  }
  else {
    total_seconds_in_year = seconds_in_year(year,calendar);
    doy = day_of_year(year,month,day,calendar);
  }
  if(doy > 1) {
    seconds_in_doy = (doy-1) * nsid;
  }
  else {
    seconds_in_doy = 0;
  }
/* 
 * Now add in how many seconds we are into the current day, by 
 * checking the hours, minutes, and seconds and doing the appropriate 
 * calculation.
 */
  if(hour > 0)   seconds_in_hour   = hour * 3600;
  else           seconds_in_hour   = 0;
  if(minute > 0) seconds_in_minute = minute * 60;
  else           seconds_in_minute = 0;
  current_seconds_in_year = seconds_in_doy + seconds_in_hour + 
    seconds_in_minute + second;
  return((double)current_seconds_in_year/(double)total_seconds_in_year);
}

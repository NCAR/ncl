#include <stdio.h>
#include "wrapper.h"

extern int day_of_year (int, int, int);
extern int days_in_month (int, int);
extern int day_of_week (int, int, int);
extern int monthday (int, int);
extern int isleapyear(int);


NhlErrorTypes day_of_year_W( void )
{
  int i, j, total;
/*
 * Input variables
 */
  int *year, *month, *day;
  int ndims_year, dsizes_year[NCL_MAX_DIMENSIONS];
  int ndims_month, dsizes_month[NCL_MAX_DIMENSIONS];
  int ndims_day, dsizes_day[NCL_MAX_DIMENSIONS];
/*
 * Output variables
 */
  int *dayofyear;
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
          NULL,
          NULL,
          NULL,
          2);
/*
 * Retrieve argument #2
 */
  month = (int*)NclGetArgValue(
          1,
          3,
          &ndims_month, 
          dsizes_month,
          NULL,
          NULL,
          NULL,
          2);
/*
 * Retrieve argument #3
 */
  day = (int*)NclGetArgValue(
          2,
          3,
          &ndims_day, 
          dsizes_day,
          NULL,
          NULL,
          NULL,
          2);
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
 * Call function.
 */
  for( i = 0; i < total; i++ ) dayofyear[i] = day_of_year(year[i],month[i],day[i]);
/*
 * Return.
 */
  return(NclReturnValue((void*)dayofyear,ndims_year,dsizes_year,NULL,NCL_int,0));
}

NhlErrorTypes days_in_month_W( void )
{
  int i, j, total;
/*
 * Input variables
 */
  int *year, *month;
  int ndims_year, dsizes_year[NCL_MAX_DIMENSIONS];
  int ndims_month, dsizes_month[NCL_MAX_DIMENSIONS];
/*
 * Output variables
 */
  int *daysinmonth;
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
          NULL,
          NULL,
          NULL,
          2);
/*
 * Retrieve argument #2
 */
  month = (int*)NclGetArgValue(
          1,
          2,
          &ndims_month, 
          dsizes_month,
          NULL,
          NULL,
          NULL,
          2);
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
 * Call function.
 */
  for( i = 0; i < total; i++ ) daysinmonth[i] = days_in_month(year[i],month[i]);
/*
 * Return.
 */
  return(NclReturnValue((void*)daysinmonth,ndims_year,dsizes_year,NULL,NCL_int,0));
}

NhlErrorTypes day_of_week_W( void )
{
  int i, j, total;
/*
 * Input variables
 */
  int *year, *month, *day;
  int ndims_year, dsizes_year[NCL_MAX_DIMENSIONS];
  int ndims_month, dsizes_month[NCL_MAX_DIMENSIONS];
  int ndims_day, dsizes_day[NCL_MAX_DIMENSIONS];
/*
 * Output variables
 */
  int *dayofweek;
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
          NULL,
          NULL,
          NULL,
          2);

  month = (int*)NclGetArgValue(
          1,
          3,
          &ndims_month, 
          dsizes_month,
          NULL,
          NULL,
          NULL,
          2);

  day = (int*)NclGetArgValue(
          2,
          3,
          &ndims_day, 
          dsizes_day,
          NULL,
          NULL,
          NULL,
          2);
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
 * Call function.
 */
  for( i = 0; i < total; i++ ) dayofweek[i] = day_of_week(year[i],month[i],day[i]);
/*
 * Return.
 */
  return(NclReturnValue((void*)dayofweek,ndims_year,dsizes_year,NULL,NCL_int,0));
}


NhlErrorTypes isleapyear_W( void )
{
  int i, j, total;
/*
 * Input variables
 */
  int *year;
  int ndims_year, dsizes_year[NCL_MAX_DIMENSIONS];
/*
 * Output variables
 */
  logical *isleap;
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
          NULL,
          NULL,
          NULL,
          2);
/*
 * Calculate total size of array.
 */
  total = 1;
  for( i = 0; i < ndims_year; i++ )	total *= dsizes_year[i];
/*
 * Allocate space for output array.
 */
  isleap = (logical*)NclMalloc(total*sizeof(logical));
  if( isleap == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"isleapyear: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Call function.
 */
  for( i = 0; i < total; i++ ) isleap[i] = (logical)isleapyear(year[i]);
/*
 * Return.
 */
  return(NclReturnValue((void*)isleap,ndims_year,dsizes_year,NULL,NCL_logical,0));
}

NhlErrorTypes monthday_W( void )
{
  int i, j, total;
/*
 * Input variables
 */
  int *year, *day;
  int ndims_year, dsizes_year[NCL_MAX_DIMENSIONS];
  int ndims_day, dsizes_day[NCL_MAX_DIMENSIONS];
/*
 * Output variables
 */
  int *mnthdy;
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
          NULL,
          NULL,
          NULL,
          2);
/*
 * Retrieve argument #2
 */
  day = (int*)NclGetArgValue(
          1,
          2,
          &ndims_day, 
          dsizes_day,
          NULL,
          NULL,
          NULL,
          2);
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
 * Call function.
 */
  for( i = 0; i < total; i++ ) mnthdy[i] = monthday(year[i],day[i]);
/*
 * Return.
 */
  return(NclReturnValue((void*)mnthdy,ndims_year,dsizes_year,NULL,NCL_int,0));
}

int day_of_year (int year, int month, int day)
{ 
  int dofy;
/* 
 * This function will return the day of the year.
 *
 * An error will result in day_of_year being set to -9999
 */ 
  int yrday[12] = {1,32,60,91,121,152,182,213,244,274,305,335};
  int daysinmonth[12] = {31,28,31,30,31,30,31,31,30,31,30,31};

  if (month < 1 || month > 12) {
	fprintf(stderr,"day_of_year: illegal month, %d\n", month);
	dofy = -9999;
  }
  else if (year < 0) {
	fprintf(stderr,"day_of_year: illegal year, year = %d\n", year);
	dofy = -9999;
  }
  else if (day < 1 || (isleapyear(year) && month == 2 && day > 29) ||
       (!(isleapyear(year) && month == 2) && day > daysinmonth[month-1])) {
	fprintf(stderr,"day_of_year: illegal arguments, year = %d, month = %d, day = %d\n", year, month, day);
	dofy = -9999;
  }
  else {
	dofy = yrday[month-1] + day - 1;
	if (isleapyear(year) && month > 2) dofy++;
  }
  return(dofy);
}

int days_in_month (int year, int month)
{ 
  int dinm;
/* 
 * This function will return the number of days in a particular month.
 *
 * An error will result in days_in_month being set to -9999
 */ 
  int daysinmonth[12] = {31,28,31,30,31,30,31,31,30,31,30,31};
  
  if (month < 1 || month > 12 || year < 0) {
    fprintf(stderr,"days_in_month: illegal argument, year = %d, month = %d\n", year, month);
	dinm = -9999;
  }
  else {
	dinm = daysinmonth[month-1];
	if (isleapyear(year) & month == 2) dinm = 29;
  }
  return(dinm);
}

int day_of_week (int year, int month, int day)
{ 
  int dow;
  int daysinmonth[12] = {31,28,31,30,31,30,31,31,30,31,30,31};
/* 
 * This function will return the day of the week (0-6) given year, month,
 * and day of month.
 *
 * An error will result in day_of_week being set to -9999
 *
 *    Day_Of_Week: (0=Sunday,1=Monday...6=Saturday)
 *    cf J.D.Robertson, CACM 15(10):918
 */
  if (month < 1 || month > 12) {
	fprintf(stderr,"day_of_week: illegal month, %d\n", month);
	dow = -9999;
  }
  else if (year < 0) {
	fprintf(stderr,"day_of_week: illegal year, year = %d\n", year);
	dow = -9999;
  }
  else if (day < 1 || (isleapyear(year) && month == 2 && day > 29) ||
       (!(isleapyear(year) && month == 2) && day > daysinmonth[month-1])) {
	fprintf(stderr,"day_of_week: illegal arguments, year = %d, month = %d, day = %d\n", year, month, day);
	dow = -9999;
  }
  else {
    dow = ((13*(month+10-(month+10)/13*12)-1)/5+day+77           
	   +5*(year+(month-14)/12-(year+(month-14)/12)/100*100)/4   
	   +(year+(month-14)/12)/400-(year+(month-14)/12)/100*2) % 7;
  }
  return(dow);
}

int monthday (int year, int dayofyear)
{ 
/*
 * Given the year and day of year this function will return
 * the concatenated month and day (integer).
 * 
 * For example: if year = 1933 and dayofyear = 245
 * then monthday= 902 for September 2.
 *
 * An error will result in monthday being set to -9999
 */ 
  int i, mday, work[13];
  int yearday[13] = {1,32,60,91,121,152,182,213,244,274,305,335,367};

  if (dayofyear < 1 || (isleapyear(year) && dayofyear > 366) ||
	  (!isleapyear(year) && dayofyear > 365) || year < 0) {
	fprintf (stderr,"monthday: illegal argument, year = %d, dayofyear = %d\n", year, dayofyear);
	mday = -9999;
  }
  else {
/*
 * Easy way around leapyr problem.
 */
	for( i = 0; i < 13; i++ ) work[i] = yearday[i];
/*
 * Add one day to normal yearr vector.
 */
	if (isleapyear(year)) {
	  for( i=2; i < 12; i++ ) work[i] = yearday[i]+1;
	}
	for( i = 1; i <= 12; i++ ) {
      if (dayofyear >= work[i-1] && dayofyear < work[i]) {
		mday = i*100 + (dayofyear - work[i-1]+1);
	  }
	}
  }
  return(mday);
}

int isleapyear(int year)
{
/*      
 * This function will return a value of 1 if year is a leap year.
 * If year is not a leap year then a value of 0 is returned.
 * 
 * A year is a leap year if it is divisible by 4, unless year is a century
 * year (e.g. 1800, 1900, 2000, etc.) in which case it must be divisable
 * by 400.
 * 
 */
  int y4, y100, y400;
 
  if (year < 0) {
	fprintf (stderr,"isleapyear: illegal argument, year = %d\n", year);
	return(0);
  }
  y4 = (year % 4) == 0;
  y100 = (year % 100) == 0;
  y400 = (year % 400) == 0;

  return((y4 && !y100) || y400);
}

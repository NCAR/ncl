/*
    The CalCalcs routines, a set of C-language routines to perform
    calendar calculations with an emphasis on calendars used by 
    global climate models, including "noleap", "365_day", "360_day",
    "proleptic_gregorian", and "standard" calendars. Support is 
    also provided for what I call "year 0" calendars, which is to
    say calendars that have a valid year 0, unlike the real calendar.

    Version 1.2, released 8 June 2014

    Copyright (C) 2010-2014, David W. Pierce, dpierce@ucsd.edu

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <strings.h>

#include "calcalcs.h"

static int c_isleap_gregorian	( int year, int *leap );
static int c_isleap_gregorian_y0( int year, int *leap );
static int c_isleap_julian   	( int year, int *leap );
static int c_isleap_never    	( int year, int *leap );

static int c_date2jday_julian      ( int year, int month, int day, int *jday );
static int c_date2jday_gregorian   ( int year, int month, int day, int *jday );
static int c_date2jday_gregorian_y0( int year, int month, int day, int *jday );
static int c_date2jday_noleap      ( int year, int month, int day, int *jday );
static int c_date2jday_360_day     ( int year, int month, int day, int *jday );

static int c_jday2date_julian      ( int jday, int *year, int *month, int *day );
static int c_jday2date_gregorian   ( int jday, int *year, int *month, int *day );
static int c_jday2date_gregorian_y0( int jday, int *year, int *month, int *day );
static int c_jday2date_noleap      ( int jday, int *year, int *month, int *day );
static int c_jday2date_360_day     ( int jday, int *year, int *month, int *day );

static int c_dpm_julian      ( int year, int month, int *dpm );
static int c_dpm_gregorian   ( int year, int month, int *dpm );
static int c_dpm_gregorian_y0( int year, int month, int *dpm );
static int c_dpm_noleap      ( int year, int month, int *dpm );
static int c_dpm_360_day     ( int year, int month, int *dpm );

#define CCS_ERROR_MESSAGE_LEN	8192
static char error_message[CCS_ERROR_MESSAGE_LEN];

/* Following are number of Days Per Month (dpm).  They are called 'idx1' to
 * emphasize that they are intended to be index by a month starting at 1
 * rather than at 0.
 *                     na, jan feb mar apr may jun jul aug sep oct nov dec */
static int dpm_idx1[]      = {-99, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
static int dpm_leap_idx1[] = {-99, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

/* Same as above, but SUM of previous months.  Indexing starts at 1 for January */
static int spm_idx1[]      = {-99, 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365};
/* static int spm_leap_idx1[] = {-99, 0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366}; note: spm is only used for calendars with no leap years */

static int date_ge( int year, int month, int day, int y2, int m2, int d2 );
static int date_le( int year, int month, int day, int y2, int m2, int d2 );
static int date_lt( int year, int month, int day, int y2, int m2, int d2 );
static int date_gt( int year, int month, int day, int y2, int m2, int d2 );
static int set_xition_extra_info( calcalcs_cal *cal );
static void ccs_dump_xition_dates( void );
static void ccs_gxd_add_country( char *code, char *longname, int year, int month, int day );
static void ccs_init_country_database( void );

/* Some arbitrary number that is unlikely to be encounterd in a string of random digits */
#define CCS_VALID_SIG	89132412

/* These implement the database that associates a two-letter country code, such as "UK", 
 * with the transition date that the country switched from the Julian to Gregorian calendar
 */ 
#define CCS_MAX_N_COUNTRY_CODES	5000
static int ccs_n_country_codes = 0;
static ccs_country_code *ccs_xition_dates[ CCS_MAX_N_COUNTRY_CODES ];
static int have_initted_country_codes = 0;

/**********************************************************************************************
 * Initialize a calendar.  The passed argument is the name of the calendar, and may be
 * one of the following character strings:
 *	"standard"
 *	"proleptic_Julian"
 *	"proleptic_Gregorian"
 *	"noleap" (aka "365_day" and "no_leap") (Mary added support for "365")
 *	"360_day" (Mary added support for "360")
 *
 * As a special hack, a calendar can be named "standard_XX" where XX is a two-letter
 * date code recognized by ccs_get_xition_date, in which case a standard calendar with
 * the specified transition date will be used.
 *
 * Returns a pointer to the new calendar, or NULL on error.
 */
calcalcs_cal *ccs_init_calendar( const char *calname )
{
	calcalcs_cal	*retval;
	int		use_specified_xition_date, spec_year_x, spec_month_x, spec_day_x;

	error_message[0] = '\0';

	if( (strncasecmp( calname, "standard",     8) == 0) 
	 && (strncasecmp( calname, "standard_y0", 11) != 0) ) {	 /* NOT a year0 calendar! */

		/* This is a "regular" Standard calendar, which does not include "year 0".
		 * See also calendar standard_y0, which does include a year 0, below
		 */

		if( ! have_initted_country_codes )
			ccs_init_country_database();

		/* See if this is a name of the form "Standard_XX" */
		use_specified_xition_date = 0;
		if( (strlen(calname) >= 11) && (calname[8] == '_')) {
			if( ccs_get_xition_date( calname+9, &spec_year_x, &spec_month_x, &spec_day_x ) != 0 ) {
				fprintf( stderr, "Error, unknown calendar passed to ccs_init_calendar: \"%s\". Returning NULL\n",
					calname );
				return(NULL);
				}
			use_specified_xition_date = 1;
			}

		retval = (calcalcs_cal *)malloc( sizeof(calcalcs_cal) );
		if( retval == NULL ) {
			fprintf( stderr, "Error, cannot allocate space for the calcalcs calendar. Returning NULL\n" );
			return( NULL );
			}
		retval->sig  = CCS_VALID_SIG;
		retval->name = (char *)malloc( sizeof(char) * (strlen(calname)+1) );
		strcpy( retval->name, calname );

		retval->mixed = 1;
		retval->early_cal = ccs_init_calendar( "proleptic_julian" );
		if( retval->early_cal == NULL ) {
			fprintf( stderr, "Error, initialization of early calendar (proleptic_julian_y0) for standard_y0 calendar failed!\n" );
			exit(-1);
			}
		retval->late_cal  = ccs_init_calendar( "proleptic_gregorian" );
		if( retval->late_cal == NULL ) {
			fprintf( stderr, "Error, initialization of late calendar (proleptic_gregorian_y0) for standard_y0 calendar failed!\n" );
			exit(-1);
			}

		/* Following are FIRST DAY the "later" calendar should be used */
		if( use_specified_xition_date == 1 ) {
			retval->year_x    = spec_year_x;
			retval->month_x   = spec_month_x;
			retval->day_x     = spec_day_x;
			}
		else
			{
			retval->year_x    = 1582;
			retval->month_x   = 10;
			retval->day_x     = 15;
			}

		/* Set the last date the earlier cal was used, and the transition day's Julian date */
		if( set_xition_extra_info( retval ) != 0 ) {
			fprintf( stderr, "calcalcs_init_cal: Error trying to initialize calendar \"%s\": %s. Returning NULL\n",
				calname, error_message );
			return(NULL);
			}
		}

	else if( strncasecmp( calname, "standard_y0", 11) == 0) {	 

		retval = (calcalcs_cal *)malloc( sizeof(calcalcs_cal) );
		if( retval == NULL ) {
			fprintf( stderr, "Error, cannot allocate space for the calcalcs calendar. Returning NULL\n" );
			return( NULL );
			}
		retval->sig  = CCS_VALID_SIG;
		retval->name = (char *)malloc( sizeof(char) * (strlen(calname)+1) );
		strcpy( retval->name, calname );

		retval->mixed = 1;
		retval->early_cal = ccs_init_calendar( "proleptic_julian_y0" );
		if( retval->early_cal == NULL ) {
			fprintf( stderr, "Error, initialization of early calendar (proleptic_julian_y0) for standard_y0 calendar failed!\n" );
			exit(-1);
			}
		retval->late_cal  = ccs_init_calendar( "proleptic_gregorian_y0" );
		if( retval->late_cal == NULL ) {
			fprintf( stderr, "Error, initialization of late calendar (proleptic_gregorian_y0) for standard_y0 calendar failed!\n" );
			exit(-1);
			}

		/* Following are FIRST DAY the "later" calendar should be used */
		retval->year_x    = 1582;
		retval->month_x   = 10;
		retval->day_x     = 15;

		/* Set the last date the earlier cal was used, and the transition day's Julian date */
		if( set_xition_extra_info( retval ) != 0 ) {
			fprintf( stderr, "calcalcs_init_cal: Error trying to initialize calendar \"%s\": %s. Returning NULL\n",
				calname, error_message );
			return(NULL);
			}
		}

	else if( (strcasecmp( calname, "gregorian" ) == 0) || 
		 (strcasecmp( calname, "proleptic_gregorian" ) == 0)) {

		/* This is a "regular" Gregorian calendar, which does not include "year 0".
		 * See also calendar gregorian_y0, which does include a year 0, below
		 */
		retval = (calcalcs_cal *)malloc( sizeof(calcalcs_cal) );
		if( retval == NULL ) {
			fprintf( stderr, "Error, cannot allocate space for the calcalcs calendar\n" );
			return( NULL );
			}
		retval->sig  = CCS_VALID_SIG;
		retval->name = (char *)malloc( sizeof(char) * (strlen(calname)+1) );
		strcpy( retval->name, calname );
		retval->ndays_reg  = 365;
		retval->ndays_leap = 366;

		retval->mixed = 0;

		retval->c_isleap    = &c_isleap_gregorian;
		retval->c_date2jday = &c_date2jday_gregorian;
		retval->c_jday2date = &c_jday2date_gregorian;
		retval->c_dpm       = &c_dpm_gregorian;
		}

	else if( (strcasecmp( calname, "gregorian_y0" ) == 0) || 
		 (strcasecmp( calname, "proleptic_gregorian_y0" ) == 0)) {

		/* This is a Gregorian calendar that includes "year 0".
		 */
		retval = (calcalcs_cal *)malloc( sizeof(calcalcs_cal) );
		if( retval == NULL ) {
			fprintf( stderr, "Error, cannot allocate space for the calcalcs calendar\n" );
			return( NULL );
			}
		retval->sig  = CCS_VALID_SIG;
		retval->name = (char *)malloc( sizeof(char) * (strlen(calname)+1) );
		strcpy( retval->name, calname );
		retval->ndays_reg  = 365;
		retval->ndays_leap = 366;

		retval->mixed = 0;

		retval->c_isleap    = &c_isleap_gregorian_y0;
		retval->c_date2jday = &c_date2jday_gregorian_y0;
		retval->c_jday2date = &c_jday2date_gregorian_y0;
		retval->c_dpm       = &c_dpm_gregorian_y0;
		}

	else if( (strcasecmp( calname, "julian" ) == 0 ) ||
	   	 (strcasecmp( calname, "proleptic_julian" ) == 0 )) {
		retval = (calcalcs_cal *)malloc( sizeof(calcalcs_cal) );
		if( retval == NULL ) {
			fprintf( stderr, "Error, cannot allocate space for the calcalcs calendar\n" );
			return( NULL );
			}

		retval->sig  = CCS_VALID_SIG;
		retval->name = (char *)malloc( sizeof(char) * (strlen(calname)+1) );
		strcpy( retval->name, calname );
		retval->ndays_reg  = 365;
		retval->ndays_leap = 366;

		retval->mixed = 0;

		retval->c_isleap    = &c_isleap_julian;
		retval->c_date2jday = &c_date2jday_julian;
		retval->c_jday2date = &c_jday2date_julian;
		retval->c_dpm       = &c_dpm_julian;
		}

	else if( (strcasecmp(calname,"noleap")==0) ||
	         (strcasecmp(calname,"no_leap")==0) ||
                 (strcasecmp(calname,"365")==0) ||
		 (strcasecmp(calname,"365_day")==0)) {
		retval = (calcalcs_cal *)malloc( sizeof(calcalcs_cal) );
		if( retval == NULL ) {
			fprintf( stderr, "Error, cannot allocate space for the calcalcs calendar\n" );
			return( NULL );
			}
		retval->sig  = CCS_VALID_SIG;
		retval->name = (char *)malloc( sizeof(char) * (strlen("noleap")+1) );
		strcpy( retval->name, "noleap" );
		retval->ndays_reg  = 365;
		retval->ndays_leap = 365;

		retval->mixed  = 0;

		retval->c_isleap    = &c_isleap_never;
		retval->c_date2jday = &c_date2jday_noleap;
		retval->c_jday2date = &c_jday2date_noleap;
		retval->c_dpm       = &c_dpm_noleap;
		}

        else if( (strcasecmp(calname,"360_day")==0) ||
		 (strcasecmp(calname,"360")==0)) {
		retval = (calcalcs_cal *)malloc( sizeof(calcalcs_cal) );
		if( retval == NULL ) {
			fprintf( stderr, "Error, cannot allocate space for the calcalcs calendar\n" );
			return( NULL );
			}
		retval->sig  = CCS_VALID_SIG;
		retval->name = (char *)malloc( sizeof(char) * (strlen("360_day")+1) );
		strcpy( retval->name, "360_day" );
		retval->ndays_reg  = 360;
		retval->ndays_leap = 360;

		retval->mixed  = 0;

		retval->c_isleap    = &c_isleap_never;
		retval->c_date2jday = &c_date2jday_360_day;
		retval->c_jday2date = &c_jday2date_360_day;
		retval->c_dpm       = &c_dpm_360_day;
		}

	else
		{
		fprintf( stderr, "Error, ccs_init_calendar called with unknown calendar name: %s\n", calname );
		return( NULL );
		}

	return( retval );
}

/**********************************************************************************************
 *
 * Determine if the passed year is a leap year in the specified calendar.
 * The passed parameter leap is set to '1' if the year is a leap year, and '0' if it is not.
 *
 * Returns 0 on success, and a negative value on error.
 * Errors include the passed year being invalid (before 4713 B.C.) or not existing
 * in the specified calendar (i.e., there is no year 0 in either the Gregorian or
 * Julian calendars).
 * 
 */
int ccs_isleap( calcalcs_cal *calendar, int year, int *leap ) 
{
	int	ierr;
	calcalcs_cal *c2use;

	if( calendar == NULL ) return(CALCALCS_ERR_NULL_CALENDAR);
	if( calendar->sig != CCS_VALID_SIG ) return(CALCALCS_ERR_INVALID_CALENDAR);

	if( year < -4714 ) {
		sprintf( error_message, "ccs_isleap: year %d is out of range for the %s calendar; dates must not be before 4713 B.C.", year, calendar->name );
		return( CALCALCS_ERR_OUT_OF_RANGE );
		}

	if( calendar->mixed ) {
		if( year >= calendar->year_x )	/* Q: did any countries transition during a year that had different leap status before and after??? Let's hope not! */
			c2use = calendar->late_cal;
		else
			c2use = calendar->early_cal;
		}
	else
		c2use = calendar;

	ierr = c2use->c_isleap( year, leap );
	return( ierr );
}

/**********************************************************************************************
 * ccs_dpm: returns the number of days per month for the passed year/month/calendar combo
 *
 * Returns 0 on success, and a negative number on error (for example, an illegal month number)
 */
int ccs_dpm( calcalcs_cal *calendar, int year, int month, int *dpm )
{
	int		ndays_reg, ierr;
	int		overlap_px_month, overlap_x_month;
	calcalcs_cal	*c2use;

	if( calendar->mixed ) {
		/* A calendar transition potentially affects two months -- the month containing the
		 * last day of the old calendar, and the month containing the first day of the
		 * new calendar.  If we are in either of those months, things get much harder.
		 * (Note that these can easily be the same month)
		 */
		overlap_px_month = ((year == calendar->year_px) && (month == calendar->month_px));
		overlap_x_month  = ((year == calendar->year_x ) && (month == calendar->month_x ));
		if( overlap_px_month || overlap_x_month ) {
			if( overlap_px_month && (!overlap_x_month)) {
				/* Last day of the month must have been last day the early calendar was used */
				*dpm = calendar->day_px;
				return(0);
				}
			else if( overlap_x_month && (!overlap_px_month)) {
				if( (ierr = ccs_dpm( calendar->late_cal, year, month, &ndays_reg )) != 0 )
					return( ierr );
				*dpm = ndays_reg - calendar->day_x + 1;
				return(0);
				}

			else	/* overlap_px_month && overlap_x_month */
				{
				if( (ierr = ccs_dpm( calendar->late_cal, year, month, &ndays_reg )) != 0 )
					return( ierr );
				*dpm = calendar->day_px + (ndays_reg - calendar->day_x + 1);
				return(0);
				}
			}
		else if( date_ge( year, month, 1, calendar->year_x, calendar->month_x, calendar->day_x ))
			c2use = calendar->late_cal;
		else
			c2use = calendar->early_cal;
		}
	else
		c2use = calendar;

	return( c2use->c_dpm( year, month, dpm ));
}

/**********************************************************************************************
 * ccs_jday2date: give a Julian day number, return the corresponding date in the 
 * 	  	selected calendar
 *
 * Returns 0 on success, <0 on error and fills string error_message
 */
int ccs_jday2date( calcalcs_cal *calendar, int jday, int *year, int *month, int *day )
{
	calcalcs_cal *c2use;

	if( calendar == NULL ) return(CALCALCS_ERR_NULL_CALENDAR);
	if( calendar->sig != CCS_VALID_SIG ) return(CALCALCS_ERR_INVALID_CALENDAR);

	if( calendar->mixed ) {
		if( jday >= calendar->jday_x )
			c2use = calendar->late_cal;
		else
			c2use = calendar->early_cal;
		}
	else
		c2use = calendar;

	return( c2use->c_jday2date( jday, year, month, day ));
}

/**********************************************************************************************
 * ccs_date2jday: given a date, return the (true) Julian day number
 *
 * Note that "Julian day number" is not the day number of the year, but rather the
 * day number starting with zero on Jan 1st 4713 BC (in the proleptic Julian calendar) and 
 * counting consecutively.
 *
 * Returns 0 on success, <0 on error and fills string error_message
 */
int ccs_date2jday( calcalcs_cal *calendar, int year, int month, int day, int *jday )
{
	int		dpm, ierr;
	calcalcs_cal	*c2use;

	if( calendar == NULL ) return(CALCALCS_ERR_NULL_CALENDAR);
	if( calendar->sig != CCS_VALID_SIG ) return(CALCALCS_ERR_INVALID_CALENDAR);

	if( calendar->mixed ) {
		if( date_ge( year, month, day, calendar->year_x, calendar->month_x, calendar->day_x ))
			c2use = calendar->late_cal;
		else if( date_le( year, month, day, calendar->year_px, calendar->month_px, calendar->day_px ))
			c2use = calendar->early_cal;
		else
			{
			sprintf( error_message, "ccs_date2jday: date %04d-%02d-%02d is not a valid date in the %s calendar; it falls between the last date the %s calendar was used (%04d-%02d-%02d) and the first date the %s calendar was used (%04d-%02d-%02d)", 
				year, month, day, calendar->name, 
				calendar->early_cal->name,
				calendar->year_px, calendar->month_px, calendar->day_px,
				calendar->late_cal->name,
				calendar->year_x, calendar->month_x, calendar->day_x );
			return( CALCALCS_ERR_DATE_NOT_IN_CALENDAR );
			}
		}
	else
		c2use = calendar;

	if( (ierr = ccs_dpm( c2use, year, month, &dpm )) != 0 )
		return( ierr );

	if( (month < 1) || (month > 12) || (day < 1) || (day > dpm)) {
		sprintf( error_message, "date2jday passed an date that is invalid in the %s calendar: %04d-%02d-%02d", 
			c2use->name, year, month, day );
		return( CALCALCS_ERR_DATE_NOT_IN_CALENDAR );
		}

	return( c2use->c_date2jday( year, month, day, jday ));
}

/********************************************************************************************
 *
 * ccs_date2doy: given a Y/M/D date, calculates the day number of the year, starting at 1 for
 * January 1st.
 *
 * Returns 0 on success, and a negative value on error (for example, an illegal date [one
 * that does not exist in the specified calendar])
 */
int ccs_date2doy( calcalcs_cal *calendar, int year, int month, int day, int *doy )
{
	int	ierr, jd0, jd1, doy_px, jd_args, xition_date_first_day_of_year,
		ndays_elapsed;
	calcalcs_cal	*c2use;

	if( calendar == NULL ) return(CALCALCS_ERR_NULL_CALENDAR);
	if( calendar->sig != CCS_VALID_SIG ) return(CALCALCS_ERR_INVALID_CALENDAR);

	if( calendar->mixed ) {

		/* If we fall in the twilight zone after the old calendar was stopped but before
		 * the new calendar was used, it's an error 
		 */
		if( date_gt( year, month, day, calendar->year_px, calendar->month_px, calendar->day_px ) &&
		    date_lt( year, month, day, calendar->year_x,  calendar->month_x,  calendar->day_x )) {
			sprintf( error_message, "ccs_date2doy: date %04d-%02d-%02d is not a valid date in the %s calendar; it falls between the last date the %s calendar was used (%04d-%02d-%02d) and the first date the %s calendar was used (%04d-%02d-%02d)", 
				year, month, day, calendar->name, 
				calendar->early_cal->name,
				calendar->year_px, calendar->month_px, calendar->day_px,
				calendar->late_cal->name,
				calendar->year_x, calendar->month_x, calendar->day_x );
			return( CALCALCS_ERR_DATE_NOT_IN_CALENDAR );
			}

		xition_date_first_day_of_year = ((year == calendar->year_x) && (calendar->month_x == 1) && (calendar->day_x == 1));
		if( (year > calendar->year_x) || xition_date_first_day_of_year )
			c2use = calendar->late_cal;
		else if( date_le( year, month, day, calendar->year_px, calendar->month_px, calendar->day_px ))
			c2use = calendar->early_cal;
		else
			{
			/* Complicated if we are asking for the day of the year during
			 * the transition year and after the transition date.  I'm choosing 
			 * to define the day numbering during a transition year as 
			 * consecutive, which means that the doy for dates
			 * after the transition date equals the doy of the last
			 * day of the earlier calendar plus the number of days
			 * that have elapsed since the transition day.
			 */

			/* Get the doy of the day BEFORE the transition day 
			 * in the earlier calendar
			*/
			if( (ierr = ccs_date2doy( calendar->early_cal, calendar->year_px, calendar->month_px, calendar->day_px, &doy_px )) != 0 )
				return( ierr );

			/* Get number of days that have elapsed between the transition day
			 * and the requested date
			 */
			if( (ierr = ccs_date2jday( calendar->late_cal, year, month, day, &jd_args )) != 0 )
				return( ierr );
			ndays_elapsed = jd_args - calendar->jday_x + 1;		/* if this IS the transition day, ndays_elapsed==1 */

			/* Finally, the day of the year is the day number of the day BEFORE the
			 * transition day plus the number of elapsed days since the
			 * transition day.
			 */
			*doy = doy_px + ndays_elapsed;

			return(0);
			}
		}
	else
		c2use = calendar;

	/* Get Julian day number of Jan 1st of the specified year */
	if( (ierr = c2use->c_date2jday( year, 1, 1, &jd0 )) != 0 ) 
		return( ierr );

	/* Get Julian day number of the specified date */
	if( (ierr = c2use->c_date2jday( year, month, day, &jd1 )) != 0 ) 
		return( ierr );

	*doy = jd1 - jd0 + 1;	/* Add 1 because numbering starts at 1 */

	return(0);
}

/********************************************************************************************
 *
 * ccs_doy2date: given a year and a day number in that year (with counting starting at 1 for 
 * 	Jan 1st), this returns the month and day of the month that the doy refers to.
 *
 * Returns 0 on success, and a negative value on error (for example, a day of the year
 * that is less than 1 or greater than 366).
 */
int ccs_doy2date( calcalcs_cal *calendar, int year, int doy, int *month, int *day )
{
	int	ierr, leap, jd0, jd1, tyear, doy_px, jd_want,
		xition_date_first_day_of_year, ndays_max;
	calcalcs_cal	*c2use;

	if( calendar == NULL ) return(CALCALCS_ERR_NULL_CALENDAR);
	if( calendar->sig != CCS_VALID_SIG ) return(CALCALCS_ERR_INVALID_CALENDAR);

	if( calendar->mixed ) {
		xition_date_first_day_of_year = ((year == calendar->year_x) && (calendar->month_x == 1) && (calendar->day_x == 1));
		if( year < calendar->year_x )
			c2use = calendar->early_cal;
		else if( (year > calendar->year_x) || xition_date_first_day_of_year )
			c2use = calendar->late_cal;
		else
			{
			/* Get the doy of the day BEFORE the transition day 
			 * in the earlier calendar
			*/
			if( (ierr = ccs_date2doy( calendar->early_cal, calendar->year_px, calendar->month_px, calendar->day_px, &doy_px )) != 0 )
				return( ierr );

			/* If our requested doy is before the transition doy, we
			 * can just easily calculate it with the early calendar
			 */
			if( doy <= doy_px ) 
				return( ccs_doy2date( calendar->early_cal, year, doy, month, day ));

			/* Finally calculate the Julian day we want, and convert it to a date */
			jd_want = calendar->jday_x + (doy - doy_px - 1); 
			if( (ierr = ccs_jday2date( calendar->late_cal, jd_want, &tyear, month, day)) != 0 ) 
				return(ierr);

			/* If the year we got from that Julian day is different from the original
			 * year specified, it means we have gone off the end of the transition year,
			 * probably because that year has less days than regular years.  In that
			 * event, return an error.
			 */
			if( tyear != year ) {
				sprintf( error_message, "year %d in the %s calendar (with transition date %04d-%02d-%02d) has less than %d days, but that was the day-of-year number requested in a call to ccs_doy2date\n",
					year, calendar->name, calendar->year_x, calendar->month_x, calendar->day_x, doy );
				return( CALCALCS_ERR_INVALID_DAY_OF_YEAR );
				}

			return(0);
			}
		}
	else
		c2use = calendar;

	/* Check to make sure we are not asking for a doy that does not exist,
	 * esp. as regards to the number of days in leap vs. non-leap years
	 */
	if( (ierr = c2use->c_isleap( year, &leap )) != 0 )
		return( ierr );
	if( leap == 1 )
		ndays_max = c2use->ndays_leap;
	else
		ndays_max = c2use->ndays_reg;

	if( (doy < 1) || (doy > ndays_max)) {
		sprintf( error_message, "routine ccs_doy2date was passed a day-of-year=%d, but for year %d in the %s calendar, the value must be between 1 and %d",
			doy, year, c2use->name, ndays_max );
		return( CALCALCS_ERR_INVALID_DAY_OF_YEAR );
		}

	/* Get Julian day number of Jan 1st of the specified year */
	if( (ierr = c2use->c_date2jday( year, 1, 1, &jd0 )) != 0 ) 
		return( ierr );

	/* Calculate new Julian day */
	jd1 = jd0 + doy - 1;

	/* Get date for new Julian day */
	if( (ierr = c2use->c_jday2date( jd1, &tyear, month, day )) != 0 ) 
		return( ierr );

	return(0);
}

/********************************************************************************************
 * ccs_dayssince: Given a Y/M/D date in a specified calendar, and the number of days since 
 *	that date, this returns the new Y/M/D date in a (possibly different) calendar.
 *
 * Note that specifying "zero" days since, and giving different calendars as the original
 *	and new calendars, essentially converts dates between calendars.
 *
 * Returns 0 on success, and a negative value on error.
 */
int ccs_dayssince( calcalcs_cal *calendar_orig, int year_orig, int month_orig, int day_orig,
		int ndays_since, calcalcs_cal *calendar_new, int *year_new, int *month_new, int *day_new )
{
	int		ierr, jd0, jd1;
	calcalcs_cal	*c2use_orig, *c2use_new;

	if( calendar_orig == NULL ) return(CALCALCS_ERR_NULL_CALENDAR);
	if( calendar_orig->sig != CCS_VALID_SIG ) return(CALCALCS_ERR_INVALID_CALENDAR);

	if( calendar_new == NULL ) return(CALCALCS_ERR_NULL_CALENDAR);
	if( calendar_new->sig != CCS_VALID_SIG ) return(CALCALCS_ERR_INVALID_CALENDAR);

	/* Figure out which calendar of the ORIGINAL calendar to use if it's a mixed calendar
	 */
	if( calendar_orig->mixed ) {
		if( date_ge( year_orig, month_orig, day_orig, 
				calendar_orig->year_x, calendar_orig->month_x, calendar_orig->day_x ))
			c2use_orig = calendar_orig->late_cal;
		else if( date_le( year_orig, month_orig, day_orig, 
				calendar_orig->year_px, calendar_orig->month_px, calendar_orig->day_px ))
			c2use_orig = calendar_orig->early_cal;
		else
			{
			sprintf( error_message, "ccs_dayssince: date %04d-%02d-%02d is not a valid date in the %s calendar; it falls between the last date the %s calendar was used (%04d-%02d-%02d) and the first date the %s calendar was used (%04d-%02d-%02d)", 
				year_orig, month_orig, day_orig, calendar_orig->name, 
				calendar_orig->early_cal->name,
				calendar_orig->year_px, calendar_orig->month_px, calendar_orig->day_px,
				calendar_orig->late_cal->name,
				calendar_orig->year_x, calendar_orig->month_x, calendar_orig->day_x );
			return( CALCALCS_ERR_DATE_NOT_IN_CALENDAR );
			}
		}
	else	
		c2use_orig = calendar_orig;

	/* Get Julian day in the original calendar and date combo */
	if( (ierr = c2use_orig->c_date2jday( year_orig, month_orig, day_orig, &jd0 )) != 0 )
		return(ierr);

	/* Get new Julian day */
	jd1 = jd0 + ndays_since;

	if( calendar_new->mixed ) {
		/* Figure out which calendar of the NEW calendar to use if it's a mixed calendar.
		 */
		if( jd1 >= calendar_new->jday_x )
			c2use_new = calendar_new->late_cal;
		else
			c2use_new = calendar_new->early_cal;
		}
	else
		c2use_new = calendar_new;

	/* Convert the new Julian day to a date in the new calendar */
	if( (ierr = c2use_new->c_jday2date( jd1, year_new, month_new, day_new )) != 0 )
		return( ierr );
	
	return(0);
}

/********************************************************************************************/
static void ccs_gxd_add_country( char *code, char *longname, int year, int month, int day ) 
{
	if( ccs_n_country_codes >= CCS_MAX_N_COUNTRY_CODES ) {
		fprintf( stderr, "Error, the calcalcs library is attempting to store more country codes than is possible; max is %d\n",
			CCS_MAX_N_COUNTRY_CODES );
		fprintf( stderr, "To fix, recompile with a larger number for CCS_MAX_N_COUNTRY_CODES\n" );
		exit( -1 );
		}

	ccs_xition_dates[ccs_n_country_codes] = (ccs_country_code *)malloc( sizeof( ccs_country_code ));
	if( ccs_xition_dates[ccs_n_country_codes] == NULL ) {
		fprintf( stderr, "calcalcs routine ccs_gxd_add_country: Error trying to allocate space for country code %s\n",
			code );
		exit(-1);
		}

	ccs_xition_dates[ccs_n_country_codes]->code = (char *)malloc( sizeof(char) * (strlen(code)+1) );
	if( ccs_xition_dates[ccs_n_country_codes]->code == NULL ) {
		fprintf( stderr, "calcalcs routine ccs_gxd_add_country: Error trying to allocate space for country code named %s\n",
			code );
		exit(-1);
		}
	strcpy( ccs_xition_dates[ccs_n_country_codes]->code, code );

	ccs_xition_dates[ccs_n_country_codes]->longname = (char *)malloc( sizeof(char) * (strlen(longname)+1) );
	if( ccs_xition_dates[ccs_n_country_codes]->longname == NULL ) {
		fprintf( stderr, "calcalcs routine ccs_gxd_add_country: Error trying to allocate space for country code long name %s\n",
			longname );
		exit(-1);
		}
	strcpy( ccs_xition_dates[ccs_n_country_codes]->longname, longname );

 	ccs_xition_dates[ccs_n_country_codes]->year  = year;
 	ccs_xition_dates[ccs_n_country_codes]->month = month;
 	ccs_xition_dates[ccs_n_country_codes]->day   = day;

	ccs_n_country_codes++;
}

/********************************************************************************************/
static void ccs_init_country_database()
{
	ccs_gxd_add_country( "AK", "Alaska", 		1867, 10, 18 );
	ccs_gxd_add_country( "AL", "Albania", 		1912, 12,  1 );
	ccs_gxd_add_country( "AT", "Austria", 		1583, 10, 16 );
	ccs_gxd_add_country( "BE", "Belgium", 		1582, 12, 25 );
	ccs_gxd_add_country( "BG", "Bulgaria", 		1916,  4,  1 );
	ccs_gxd_add_country( "CN", "China",   		1929,  1,  1 );
	ccs_gxd_add_country( "CZ", "Czechoslovakia", 	1584,  1, 17 );
	ccs_gxd_add_country( "DK", "Denmark",        	1700,  3,  1 );
	ccs_gxd_add_country( "NO", "Norway", 		1700,  3,  1 );
	ccs_gxd_add_country( "EG", "Egypt",		1875,  1,  1 );
	ccs_gxd_add_country( "EE", "Estonia",		1918,  1,  1 );
	ccs_gxd_add_country( "FI", "Finland",		1753,  3,  1 );
	ccs_gxd_add_country( "FR", "France", 		1582, 12, 20 );
	ccs_gxd_add_country( "DE", "Germany",		1583, 11, 22 );
	ccs_gxd_add_country( "UK", "United Kingdom",	1752,  9, 14 );
	ccs_gxd_add_country( "GR", "Greece",		1924,  3, 23 );
	ccs_gxd_add_country( "HU", "Hungary",		1587, 11,  1 );
	ccs_gxd_add_country( "IT", "Italy",		1582, 10, 15 );
	ccs_gxd_add_country( "JP", "Japan",		1918,  1,  1 );
	ccs_gxd_add_country( "LV", "Latvia",		1915,  1,  1 );
	ccs_gxd_add_country( "LT", "Lithuania",		1915,  1,  1 );
	ccs_gxd_add_country( "LU", "Luxemburg",		1582, 12, 15 );
	ccs_gxd_add_country( "NL", "Netherlands",	1582, 10, 15 );
	ccs_gxd_add_country( "PL", "Poland",		1582, 10, 15 );
	ccs_gxd_add_country( "PT", "Portugal",		1582, 10, 15 );
	ccs_gxd_add_country( "RO", "Romania",		1919,  4, 14 );
	ccs_gxd_add_country( "ES", "Spain",		1582, 10, 15 );
	ccs_gxd_add_country( "SE", "Sweden",		1753,  3,  1 );
	ccs_gxd_add_country( "CH", "Switzerland",	1584,  1, 22 );
	ccs_gxd_add_country( "TR", "Turkey",		1927,  1,  1 );
	ccs_gxd_add_country( "YU", "Yugoslavia",	1919,  1,  1 );
	ccs_gxd_add_country( "US", "United States",	1752,  9, 14 );
	ccs_gxd_add_country( "SU", "Soviet Union",	1918,  2,  1 );
	ccs_gxd_add_country( "RU", "Russia",		1918,  2,  1 );

	have_initted_country_codes = 1;
}

/********************************************************************************************/
int ccs_get_xition_date( const char *country_code, int *year, int *month, int *day )
{
	int	i;

	if( ! have_initted_country_codes )
		ccs_init_country_database();

	if( strcmp( country_code, "??" ) == 0 ) {
		ccs_dump_xition_dates();
		*year  = 0;
		*month = 0;
		*day   = 0;
		return(0);
		}
		
	/* Find the passed country code in our list */
	for( i=0; i<ccs_n_country_codes; i++ ) {
		if( strcmp( country_code, ccs_xition_dates[i]->code ) == 0 ) {
			*year  =  ccs_xition_dates[i]->year;
			*month =  ccs_xition_dates[i]->month;
			*day   =  ccs_xition_dates[i]->day;
			return(0);
			}
		}

	/* Maybe they passed a longname? */
	for( i=0; i<ccs_n_country_codes; i++ ) {
		if( strcmp( country_code, ccs_xition_dates[i]->longname ) == 0 ) {
			*year  =  ccs_xition_dates[i]->year;
			*month =  ccs_xition_dates[i]->month;
			*day   =  ccs_xition_dates[i]->day;
			return(0);
			}
		}

	sprintf( error_message, "ccs_get_xition_date: unknown calendar country/region code: \"%s\". Known codes: ", country_code );
	for( i=0; i<ccs_n_country_codes; i++ ) {
		if( (strlen(error_message) + strlen(ccs_xition_dates[i]->code) + strlen(ccs_xition_dates[i]->longname) + 10) < CCS_ERROR_MESSAGE_LEN ) {
			strcat( error_message, ccs_xition_dates[i]->code );
			strcat( error_message, " (" );
			strcat( error_message, ccs_xition_dates[i]->longname );
			strcat( error_message, ") " );
			}
		}

	return(CALCALCS_ERR_UNKNOWN_COUNTRY_CODE);
}

/********************************************************************************************/
static void ccs_dump_xition_dates( void )
{
	int	i;

	printf( "Calcalcs library known country codes:\n" );
	for( i=0; i<ccs_n_country_codes; i++ ) {
		printf( "Code: %s     Transition date: %04d-%02d-%02d   Country/Region: %s\n",
			ccs_xition_dates[i]->code,
			ccs_xition_dates[i]->year,
			ccs_xition_dates[i]->month,
			ccs_xition_dates[i]->day,
			ccs_xition_dates[i]->longname );
		if( i%3 == 2 )
			printf( "\n" );
		}
}

/********************************************************************************************/
int ccs_set_xition_date( calcalcs_cal *calendar, int year, int month, int day )
{
	int	ierr, dpm;

	if( calendar == NULL ) return(CALCALCS_ERR_NULL_CALENDAR);
	if( calendar->sig != CCS_VALID_SIG ) return(CALCALCS_ERR_INVALID_CALENDAR);

	if( calendar->mixed == 0 ) 
		return( CALCALCS_ERR_NOT_A_MIXED_CALENDAR );

	/* Check to make sure the specified date is a valid date in the
	 * LATE calendar (since the transition date is the first date
	 * that the late calendar is used)
	 */
 	if( (ierr = ccs_dpm( calendar->late_cal, year, month, &dpm )) != 0 )
		return( ierr );

	if( (month < 1) || (month > 12) || (day < 1) || (day > dpm)) {
		fprintf( stderr, "Error in routine set_cal_xition_date: trying to set the calendar Julian/Gregorian transition date to an illegal date: %04d-%02d-%02d\n", year, month, day );
		return( CALCALCS_ERR_DATE_NOT_IN_CALENDAR );
		}

	calendar->year_x    = year;
	calendar->month_x   = month;
	calendar->day_x     = day;

	if( (ierr = set_xition_extra_info( calendar )) != 0 ) 
		return( ierr );

	return(0);
}

/********************************************************************************************/
char *ccs_err_str( int errno ) 
{
	if( errno == 0 ) 
		sprintf( error_message, "no error from calcalcs routines version %f", CALCALCS_VERSION_NUMBER );

	else if( errno == CALCALCS_ERR_NULL_CALENDAR ) 
		sprintf( error_message, "a NULL calendar was passed to the calcalcs routine" );

	else if( errno == CALCALCS_ERR_INVALID_CALENDAR )
		sprintf( error_message, "an invalid, malformed, previously-freed, or uninitialized calendar was passed to the calcalcs routine" );

	return( error_message );
}

/*==================================================================================================
 * Returns 0 on success, <0 on error.
 */
int c_isleap_julian( int year, int *leap )
{
	int	tyear;

	if( year == 0 ) {
		sprintf( error_message, "the Julian calendar has no year 0" );
		return( CALCALCS_ERR_DATE_NOT_IN_CALENDAR );
		}

	/* Because there is no year 0 in the Julian calendar, years -1, -5, -9, etc
	 * are leap years.
	 */
	if( year < 0 ) 
		tyear = year + 1;
	else
		tyear = year;

	*leap = ((tyear % 4) == 0);

	return(0);
}

/*==================================================================================================
 * Returns 0 on success, <0 on error.
 */
int c_isleap_gregorian( int year, int *leap )
{
	int	tyear;

	if( year == 0 ) {
		sprintf( error_message, "the Gregorian calendar has no year 0. Use the \"Gregorian_y0\" calendar if you want to include year 0." );
		return( CALCALCS_ERR_DATE_NOT_IN_CALENDAR );
		}

	/* Because there is no year 0 in the gregorian calendar, years -1, -5, -9, etc
	 * are leap years.
	 */
	if( year < 0 ) 
		tyear = year + 1;
	else
		tyear = year;

	*leap = (((tyear % 4) == 0) && ((tyear % 100) != 0)) || ((tyear % 400) == 0);

	return(0);
}

/*==================================================================================================
 * Returns 0 on success, <0 on error.
 */
int c_isleap_gregorian_y0( int year, int *leap )
{
	*leap = (((year % 4) == 0) && ((year % 100) != 0)) || ((year % 400) == 0);

	return(0);
}

/*==================================================================================================
 * Given a Y/M/D in the Gregorian calendar, this computes the (true) Julian day number of the 
 * specified date.  Julian days are counted starting at 0 on 1 Jan 4713 BC using a proleptic Julian 
 * calendar.  The algorithm is based on the "counting" algorithm in the C++ code I obtained from 
 * Edward M. Reingold's web site at http://emr.cs.uiuc.edu/~reingold/calendar.C. 
 * In that file, the work is declared to be in the public domain.  I modified it by 
 * extending it to negative years (years BC) in addition to positive years, and to use
 * actual Julian Days as the counter.  Otherwise, the spirit of the algorithm is similar.
 *
 * Returns 0 on success, <0 on error.
 */
int c_date2jday_gregorian( int year, int month, int day, int *jday )
{
	int	m, leap, *dpm2use, err;

	if( (month < 1) || (month > 12) || (day < 1) || (day > 31)) {
		sprintf( error_message, "date %04d-%02d-%02d does not exist in the Gregorian calendar", 
			year, month, day );
		return( CALCALCS_ERR_DATE_NOT_IN_CALENDAR );
		}

	if( year == 0 ) {
		sprintf( error_message, "year 0 does not exist in the Gregorian calendar.  Use the \"Gregorian_y0\" calendar if you want to include year 0" );
		return( CALCALCS_ERR_NO_YEAR_ZERO );
		}

	/* Limit ourselves to positive Julian Days */
	if( year < -4714 ) {	
		sprintf( error_message, "year %d is out of range of the Gregorian calendar routines; must have year >= -4714", year );
		return( CALCALCS_ERR_OUT_OF_RANGE );
		}

	/* Following is necessary because Gregorian calendar skips year 0, so the
	 * offst for negative years is different than offset for positive years
	 */
	if( year < 0 ) 
		year += 4801;
	else
		year += 4800;

	if( (err = c_isleap_gregorian( year, &leap )) != 0 )
		return( err );

	if( leap )
		dpm2use = dpm_leap_idx1;
	else
		dpm2use = dpm_idx1;

	*jday = day;
	for( m=month-1; m>0; m-- )
		*jday += dpm2use[m];

	*jday += 365*(year-1) + (year-1)/4 - (year-1)/100 + (year-1)/400;

	/* Ajust to "true" Julian days. This constant is how many days difference there is 
	 * between the Julian Day origin date of 4713 BC and our offset date of 4800 BC 
	 */
	*jday -= 31739;		

	return(0);
}

/*==================================================================================================
 * Given a Y/M/D in the Gregorian calendar, this computes the (true) Julian day number of the 
 * specified date.  Julian days are counted starting at 0 on 1 Jan 4713 BC using a proleptic Julian 
 * calendar.  The algorithm is based on the "counting" algorithm in the C++ code I obtained from 
 * Edward M. Reingold's web site at http://emr.cs.uiuc.edu/~reingold/calendar.C. 
 * In that file, the work is declared to be in the public domain.  I modified it by 
 * extending it to negative years (years BC) in addition to positive years, and to use
 * actual Julian Days as the counter.  Otherwise, the spirit of the algorithm is similar.
 *
 * Returns 0 on success, <0 on error.
 */
int c_date2jday_gregorian_y0( int year, int month, int day, int *jday )
{
	int	m, leap, *dpm2use, err;

	if( (month < 1) || (month > 12) || (day < 1) || (day > 31)) {
		sprintf( error_message, "date %04d-%02d-%02d does not exist in the Gregorian calendar", 
			year, month, day );
		return( CALCALCS_ERR_DATE_NOT_IN_CALENDAR );
		}

	/* Limit ourselves to positive Julian Days */
	if( year < -4714 ) {	
		sprintf( error_message, "year %d is out of range of the Gregorian calendar routines; must have year >= -4714", year );
		return( CALCALCS_ERR_OUT_OF_RANGE );
		}

	year += 4800;

	if( (err = c_isleap_gregorian_y0( year, &leap )) != 0 )
		return( err );

	if( leap )
		dpm2use = dpm_leap_idx1;
	else
		dpm2use = dpm_idx1;

	*jday = day;
	for( m=month-1; m>0; m-- )
		*jday += dpm2use[m];

	*jday += 365*(year-1) + (year-1)/4 - (year-1)/100 + (year-1)/400;

	/* Ajust to "true" Julian days. This constant is how many days difference there is 
	 * between the Julian Day origin date of 4713 BC and our offset date of 4800 BC 
	 */
	*jday -= 31739;		

	return(0);
}

/*==========================================================================================
 * Given a (true) Julian Day, this converts to a date in the Gregorian calendar.  
 * Technically, in the proleptic Gregorian calendar, since this works for dates
 * back to 4713 BC.  Again based on the same public domain code from Edward Reingold's
 * web site as the date2jday routine, extended by me to apply to negative years (years BC).
 *
 * Returns 0 on success, <0 on error.
 */
int c_jday2date_gregorian( int jday, int *year, int *month, int *day )
{
	int	tjday, leap, *dpm2use, ierr, yp1;

	/* Make first estimate for year. We subtract 4714 because Julian Day number
	 * 0 occurs in year 4714 BC in the Gregorian calendar (recall that it occurs
	 * in year 4713 BC in the JULIAN calendar 
	 */
	*year = jday/366 - 4714;

	/* Advance years until we find the right one */
	yp1 = *year + 1;
	if( yp1 == 0 ) yp1 = 1;	/* no year 0 in the Gregorian calendar */
	if( (ierr = c_date2jday_gregorian( yp1, 1, 1, &tjday )) != 0 ) 
		return( ierr );
	while( jday >= tjday ) {
		(*year)++;
		if( *year == 0 ) 
			*year = 1;	/* no year 0 in the Gregorian calendar */
		yp1 = *year + 1;
		if( yp1 == 0 ) yp1 = 1;	/* no year 0 in the Gregorian calendar */
		if( (ierr = c_date2jday_gregorian( yp1, 1, 1, &tjday )) != 0 ) 
			return( ierr );
		}

	if( (ierr = c_isleap_gregorian( *year, &leap )) != 0 ) 
		return( ierr );
	if( leap )
		dpm2use = dpm_leap_idx1;
	else
		dpm2use = dpm_idx1;

	*month = 1;
	if( (ierr = c_date2jday_gregorian( *year, *month, dpm2use[*month], &tjday)) != 0)
		return( ierr );
	while( jday > tjday ) {
		(*month)++;
		if( (ierr = c_date2jday_gregorian( *year, *month, dpm2use[*month], &tjday) != 0))
			return( ierr );
		}

	if( (ierr = c_date2jday_gregorian( *year, *month, 1, &tjday)) != 0 )
		return( ierr );
	*day = jday - tjday + 1;

	return(0);
}

/*==========================================================================================
 * Given a (true) Julian Day, this converts to a date in the Gregorian calendar.  
 * Technically, in the proleptic Gregorian calendar, since this works for dates
 * back to 4713 BC.  Again based on the same public domain code from Edward Reingold's
 * web site as the date2jday routine, extended by me to apply to negative years (years BC).
 *
 * Returns 0 on success, <0 on error.
 */
int c_jday2date_gregorian_y0( int jday, int *year, int *month, int *day )
{
	int	tjday, leap, *dpm2use, ierr, yp1;

	/* Make first estimate for year. We subtract 4714 because Julian Day number
	 * 0 occurs in year 4714 BC in the Gregorian calendar (recall that it occurs
	 * in year 4713 BC in the JULIAN calendar 
	 */
	*year = jday/366 - 4715;

	/* Advance years until we find the right one */
	yp1 = *year + 1;
	if( (ierr = c_date2jday_gregorian_y0( yp1, 1, 1, &tjday )) != 0 ) 
		return( ierr );
	while( jday >= tjday ) {
		(*year)++;
		yp1 = *year + 1;
		if( (ierr = c_date2jday_gregorian_y0( yp1, 1, 1, &tjday )) != 0 ) 
			return( ierr );
		}

	if( (ierr = c_isleap_gregorian_y0( *year, &leap )) != 0 ) 
		return( ierr );
	if( leap )
		dpm2use = dpm_leap_idx1;
	else
		dpm2use = dpm_idx1;

	*month = 1;
	if( (ierr = c_date2jday_gregorian_y0( *year, *month, dpm2use[*month], &tjday)) != 0)
		return( ierr );
	while( jday > tjday ) {
		(*month)++;
		if( (ierr = c_date2jday_gregorian_y0( *year, *month, dpm2use[*month], &tjday) != 0))
			return( ierr );
		}

	if( (ierr = c_date2jday_gregorian_y0( *year, *month, 1, &tjday)) != 0 )
		return( ierr );
	*day = jday - tjday + 1;

	return(0);
}

/*==================================================================================================
 * Given a Y/M/D in the Julian calendar, this computes the (true) Julian day number of the 
 * specified date.  Julian days are counted starting at 0 on 1 Jan 4713 BC using a proleptic Julian 
 * calendar.  The algorithm is based on the "counting" algorithm in the C++ code I obtained from 
 * Edward M. Reingold's web site at http://emr.cs.uiuc.edu/~reingold/calendar.C. 
 * In that file, the work is declared to be in the public domain.  I modified it by 
 * extending it to negative years (years BC) in addition to positive years, and to use
 * actual Julian Days as the counter.  Otherwise, the spirit of the algorithm is similar.
 *
 * Returns 0 on success, <0 on error.
 */
int c_date2jday_julian( int year, int month, int day, int *jday )
{
	int	m, leap, *dpm2use, err;

	if( (month < 1) || (month > 12) || (day < 1) || (day > 31)) {
		sprintf( error_message, "date %04d-%02d-%02d does not exist in the Julian calendar", 
			year, month, day );
		return( CALCALCS_ERR_DATE_NOT_IN_CALENDAR );
		}

	if( year == 0 ) {
		sprintf( error_message, "year 0 does not exist in the Julian calendar" );
		return( CALCALCS_ERR_NO_YEAR_ZERO );
		}

	/* Limit ourselves to positive Julian Days */
	if( year < -4713 ) {	
		sprintf( error_message, "year %d is out of range of the Julian calendar routines; must have year >= -4713", year );
		return( CALCALCS_ERR_OUT_OF_RANGE );
		}

	/* Following is necessary because Julian calendar skips year 0, so the
	 * offst for negative years is different than offset for positive years
	 */
	if( year < 0 ) 
		year += 4801;
	else
		year += 4800;

	if( (err = c_isleap_julian( year, &leap )) != 0 )
		return( err );

	if( leap )
		dpm2use = dpm_leap_idx1;
	else
		dpm2use = dpm_idx1;

	*jday = day;
	for( m=month-1; m>0; m-- )
		*jday += dpm2use[m];

	*jday += 365*(year-1) + (year-1)/4;

	/* Ajust to "true" Julian days. This constant is how many days difference there is 
	 * between the Julian Day origin date of 4713 BC and our offset date of 4800 BC 
	 */
	*jday -= 31777;		

	return(0);
}

/*==========================================================================================
 * Given a (true) Julian Day, this converts to a date in the Julian calendar.  
 * Technically, in the proleptic Julian calendar, since this works for dates
 * back to 4713 BC.  Again based on the same public domain code from Edward Reingold's
 * web site as the date2jday routine, extended by me to apply to negative years (years BC).
 *
 * Returns 0 on success, <0 on error.
 */
int c_jday2date_julian( int jday, int *year, int *month, int *day )
{
	int	tjday, leap, *dpm2use, ierr, yp1;

	/* Make first estimate for year. We subtract 4713 because Julian Day number
	 * 0 occurs in year 4713 BC in the Julian calendar
	 */
	*year = jday/366 - 4713;

	/* Advance years until we find the right one */
	yp1 = *year + 1;
	if( yp1 == 0 ) yp1 = 1;	/* no year 0 in the Julian calendar */
	if( (ierr = c_date2jday_julian( yp1, 1, 1, &tjday )) != 0 ) 
		return( ierr );
	while( jday >= tjday ) {
		(*year)++;
		if( *year == 0 ) 
			*year = 1;	/* no year 0 in the Julian calendar */
		yp1 = *year + 1;
		if( yp1 == 0 ) yp1 = 1;	/* no year 0 in the Julian calendar */
		if( (ierr = c_date2jday_julian( yp1, 1, 1, &tjday )) != 0 ) 
			return( ierr );
		}

	if( (ierr = c_isleap_julian( *year, &leap )) != 0 ) 
		return( ierr );
	if( leap )
		dpm2use = dpm_leap_idx1;
	else
		dpm2use = dpm_idx1;

	*month = 1;
	if( (ierr = c_date2jday_julian( *year, *month, dpm2use[*month], &tjday)) != 0)
		return( ierr );
	while( jday > tjday ) {
		(*month)++;
		if( (ierr = c_date2jday_julian( *year, *month, dpm2use[*month], &tjday) != 0))
			return( ierr );
		}

	if( (ierr = c_date2jday_julian( *year, *month, 1, &tjday)) != 0 )
		return( ierr );
	*day = jday - tjday + 1;

	return(0);
}

/*==================================================================================================
 * Free the storage associated with a calendar
 */
void ccs_free_calendar( calcalcs_cal *cc )
{
	if( cc == NULL )
		return;

	if( cc->mixed == 1 ) {
		ccs_free_calendar( cc->early_cal );
		ccs_free_calendar( cc->late_cal );
		}

	if( cc->sig != CCS_VALID_SIG ) {
		fprintf( stderr, "Warning: invalid calendar passed to routine ccs_free_calendar!\n" );
		return;
		}

	cc->sig = 0;

	if( cc->name != NULL )
		free( cc->name );

	free( cc );
}

/**********************************************************************************************/
static int date_gt( int year, int month, int day, int y2, int m2, int d2 ) 
{
	return( ! date_le( year, month, day, y2, m2, d2 ));
}

/**********************************************************************************************/
static int date_lt( int year, int month, int day, int y2, int m2, int d2 ) 
{
	return( ! date_ge( year, month, day, y2, m2, d2 ));
}

/**********************************************************************************************/
static int date_le( int year, int month, int day, int y2, int m2, int d2 ) {

	if( year > y2 )
		return(0);

	if( year < y2 )
		return(1);

	/* If get here, must be same year */
	if( month > m2 )
		return(0);
	if( month < m2)
		return(1);

	/* If get here, must be same month */
	if( day > d2 )
		return(0);

	return(1);
}

/**********************************************************************************************/
static int date_ge( int year, int month, int day, int y2, int m2, int d2 ) {

	if( year < y2 )
		return(0);

	if( year > y2 )
		return(1);

	/* If get here, must be same year */
	if( month < m2 )
		return(0);
	if( month > m2)
		return(1);

	/* If get here, must be same month */
	if( day < d2 )
		return(0);

	return(1);
}

/**********************************************************************************************/
int c_isleap_never( int year, int *leap )
{
	*leap = 0;
	return( 0 );
}

/**********************************************************************************************/
int c_date2jday_360_day( int year, int month, int day, int *jday )
{
	int spm;

	if( day > 30) {
		sprintf( error_message, "date %04d-%02d-%02d does not exist in the 360_day calendar",
			year, month, day );
		return( CALCALCS_ERR_DATE_NOT_IN_CALENDAR );
		}
		
	spm = (month-1)*30;	/* sum of days in the previous months */

	*jday = year*360 + spm + (day-1);

	return( 0 );
}

/**********************************************************************************************/
int c_date2jday_noleap( int year, int month, int day, int *jday )
{
	if( (month == 2) && (day == 29)) {
		sprintf( error_message, "date %04d-%02d-%02d does not exist in the noleap calendar",
			year, month, day );
		return( CALCALCS_ERR_DATE_NOT_IN_CALENDAR );
		}

	*jday = year*365 + spm_idx1[month] + (day-1);

	return( 0 );
}

/**********************************************************************************************/
int c_jday2date_360_day( int jday, int *year, int *month, int *day )
{
	int	nextra, yr_offset, doy;

	yr_offset = 0;
	if( jday < 0 ) {
		yr_offset = (-jday)/360+1;                   
		jday += 360*yr_offset;
		}

	*year = jday/360;

	nextra = jday - *year*360;
	doy    = nextra + 1;	/* Julday numbering starts at 0, doy starts at 1 */
	*month = nextra/30 + 1;
	*day   = doy - (*month-1)*30;

	*year -= yr_offset;

	return(0);
}

/**********************************************************************************************/
int c_jday2date_noleap( int jday, int *year, int *month, int *day )
{
	int	nextra, yr_offset, doy;

	yr_offset = 0;
	if( jday < 0 ) {
		yr_offset = (-jday)/365+1;                   
		jday += 365*yr_offset;
		}

	*year = jday/365;

	nextra = jday - *year*365;
	doy    = nextra + 1;	/* Julday numbering starts at 0, doy starts at 1 */
	*month = 1;
	while( doy > spm_idx1[*month + 1] )
		*month += 1;

	*day = doy - spm_idx1[*month];

	*year -= yr_offset;

	return(0);
}

/**********************************************************************************************/
int c_dpm_gregorian( int year, int month, int *dpm )
{
	int	ierr, leap;

	if( (month<1) || (month>12)) {
		sprintf( error_message, "month %d does not exist in the Gregorian calendar", month );
		return( CALCALCS_ERR_DATE_NOT_IN_CALENDAR );
		}

	if( (ierr = c_isleap_gregorian( year, &leap )) != 0 )
		return( ierr );

	if( leap ) 
		*dpm = dpm_leap_idx1[month];
	else
		*dpm = dpm_idx1[month];

	return(0);
}


/**********************************************************************************************/
int c_dpm_gregorian_y0( int year, int month, int *dpm )
{
	int	ierr, leap;

	if( (month<1) || (month>12)) {
		sprintf( error_message, "month %d does not exist in the Gregorian calendar", month );
		return( CALCALCS_ERR_DATE_NOT_IN_CALENDAR );
		}

	if( (ierr = c_isleap_gregorian_y0( year, &leap )) != 0 )
		return( ierr );

	if( leap ) 
		*dpm = dpm_leap_idx1[month];
	else
		*dpm = dpm_idx1[month];

	return(0);
}

/**********************************************************************************************/
int c_dpm_julian( int year, int month, int *dpm )
{
	int	ierr, leap;

	if( (month<1) || (month>12)) {
		sprintf( error_message, "month %d does not exist in the Julian calendar", month );
		return( CALCALCS_ERR_DATE_NOT_IN_CALENDAR );
		}

	if( (ierr = c_isleap_julian( year, &leap )) != 0 )
		return( ierr );

	if( leap ) 
		*dpm = dpm_leap_idx1[month];
	else
		*dpm = dpm_idx1[month];

	return(0);
}

/**********************************************************************************************/
int c_dpm_360_day( int year, int month, int *dpm )
{
	if( (month<1) || (month>12)) {
		sprintf( error_message, "month %d does not exist in the 360_day calendar", month );
		return( CALCALCS_ERR_DATE_NOT_IN_CALENDAR );
		}

	*dpm = 30;

	return(0);
}

/**********************************************************************************************/
int c_dpm_noleap( int year, int month, int *dpm )
{
	if( (month<1) || (month>12)) {
		sprintf( error_message, "month %d does not exist in the noleap calendar", month );
		return( CALCALCS_ERR_DATE_NOT_IN_CALENDAR );
		}

	*dpm = dpm_idx1[month];

	return(0);
}

/*******************************************************************************************/
static int set_xition_extra_info( calcalcs_cal *cal )
{
	int	ierr;

	/* This is the Julian Day of the transition date */
	ierr = ccs_date2jday( cal->late_cal, cal->year_x, cal->month_x, cal->day_x, &(cal->jday_x) );
	if( ierr != 0 ) {
		sprintf( error_message, "Failed to turn the mixed calendar transition day %04d-%02d-%02d in the %s calendar into a Julian day!\n",
			cal->year_x, cal->month_x, cal->day_x, cal->name );
		return(ierr);
		}

	/* This is the date of the day BEFORE the transition day,
	 * i.e., the last day that the early calendar was used
	 */ 
	ierr = ccs_jday2date( cal->early_cal, (cal->jday_x-1), &(cal->year_px), &(cal->month_px), &(cal->day_px));
	if( ierr != 0 ) {
		sprintf( error_message, "Failed to turn the day BEFORE the mixed calendar transition day of %04d-%02d-%02d into a date while using calendar %s! %s\n",
			cal->year_x, cal->month_x, cal->day_x, cal->early_cal->name, ccs_err_str(ierr) );
		return(ierr);
		}

	return(0);
}


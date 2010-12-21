/*
This routines provide the equivalent functionality of 'utCalendar' and 'utInvCalendar'
for the udunits-2 library, which does not directly have equivalent calls.  

Furthermore, they extend these to add two more new routines, utCalendar_cal and
utInvCalendar_cal, which take a "calendar" argument indicating which calendar to use
for the calculations.  The calendar string can be "standard", "noleap", "365_day", 
"360", or "360_day" at the moment.  If the calendar string is set to NULL, a standard 
calendar is used.

Note that the change to the udunits-2 library requires that the argument list of
utCalendar and utInvCalendar be different from the argument list that these routines
had in the udunits-1 library.

Note that these routines call the udunits-2 routines, so they have to be installed to use this!

Version 3.01	(versions < 2 are for the udunits-1 library, >=3 are for udunits-2 library)
David W. Pierce
dpierce@ucsd.edu
2010-01-13

Thanks to Christian Page' of CERFACS, France, for bug fixes!

PLEASE READ: IMPORTANT DISCUSSION OF THE "YEAR 0" PROBLEM
---------------------------------------------------------

In our actual calendars there is no year 0, because the calendar
goes from year 1 BC to year 1 AD, with no "year 0" in between.  Nevertheless,
people often do mistakenly start their calendars from "year 0".  In particular,
the NCAR CAM atmospheric model (and consequently, the CCSM3 coupled earth
system model) bases dates on "year 0".

The standard udunits library treats a units spec of the form "... since 0000-01-01"
as if it instead specified ".... since 0001-01-01".  Ie., it treats a reference
to a origin of year 0 with a reference to an origin of year 1.  This can be confusing 
if the user expects, for example, that the dates "0 days since 0001-01-01" and 
"0 days since 0000-01-01" are different.  To the udunits library, these are the same date.

Particular headaches are caused when trying to understand CAM/CCSM3 model output
based on ".... since 0000-01-01".  For the *model*, "23 days since 0000-01-01"
is intended to be the date 0000-01-24.  However, for the *udunits library*, 
"23 days since 0000-01-01" is the date 0001-01-24.  So, in essence, the udunits
library adds 1 to the year of a CAM/CCSM3 file that uses ".... since 0000-01-01"
as its date.

This code is intended to be similar to a "drop in" replacement of the standard
udunits-1 calls that works in udunits-2, and so DOES NOT MODIFY THE YEAR 0 
BEHAVIOR in any way.  All origins that start at "year 0000" (which does not 
exist) are treated as if they had specified "year 0001".  Again, this is exactly 
what the standard udunits library does.  NOTE: the udunits-2 library treats
the "year 0" problem exactly the same as does the udunits-1 library.
*/

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <udunits2.h>
#include <string.h>
#include <strings.h>
#include "utCalendar2_cal.h"

/* define DEBUG */

/*                                         J   F   M   A   M   J   J   A   S   O   N   D    */
static long days_per_month_reg_year[] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
static long days_per_month_360[]      = { 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30 };

static ut_unit *udu_origin_zero=NULL;

static void make_udu_origin_zero( ut_unit *units );
static int udu_sec_since_ref_date( double val, ut_unit *dataunits, int *yr0, int *mon0, 
		int *day0, int *hr0, int *min0, double *sec0, double *ssrd );	/* ssrd = "seconds since reference date */
static int utCalendar2_noleap_inner( double val, ut_unit *dataunits, int *year, int *month, int *day, int *hour, 
				int *minute, double *second, int days_per_year, long *days_per_month );
static int utCalendar2_360( double val, ut_unit *dataunits, int *year, int *month, int *day, int *hour, 
				int *minute, double *second );
static int utCalendar2_noleap( double val, ut_unit *dataunits, int *year, int *month, int *day, int *hour, 
				int *minute, double *second );
static int utInvCalendar2_noleap_inner( int year, int month, int day, int hour, int minute,
                double second, ut_unit *unit, double *value, long *days_per_month );
static void err_mess_not_t_convertible( ut_unit *unit );

static int shown_proleptic_warning = 0;
static int shown_julian_warning    = 0;

/*
 * Mary removed the have_initted flag, because we want to initialize
 * every time. 
 * static int have_initted		   = 0;
 */

#ifdef DEBUG
static void dateify( int year, int month, int day, int hour, int minute, double second, char *buf );
/******************************************************************************/
static void dateify( int year, int month, int day, int hour, int minute, 
		double second, char *buf )
{
	sprintf( buf, "%04d-%02d-%02d %02d:%02d:%.2f", year, month, day, hour, minute, second );
}
#endif

/******************************************************************************
 * Creates a time unit that has zero offset relative to the time unit that 
 * the udunits-2 library uses.
 */
static void make_udu_origin_zero( ut_unit *units )
{
	double	zero;
	int	y0, m0, d0, h0, min0;
	double	s0, rez;
	char	u_str[1024];
	ut_unit	*tmpu;
	ut_system *unitSystem;

	unitSystem = ut_get_system( units );

	/* Work around a bug in the udunits-2 library that resets the 
	 * library's internal time origin value upon the first
	 * ut_parse( timestamp ) call, but not upon the first
	 * ut_decode_time call
	 */
	if( (tmpu = ut_parse( unitSystem, "days since 1901-01-01", UT_ASCII )) == NULL ) {
		fprintf( stderr, "Internal error in utCalendar2_cal routine make_udu_origin_zero: could not decode test timestamp string\n" );
		exit(-1);
		}
	ut_free( tmpu );	/* We just do the parse for its side effect */

	zero = 0.0;
	ut_decode_time( zero, &y0, &m0, &d0, &h0, &min0, &s0, &rez );

	sprintf( u_str, "seconds since %04d-%02d-%02d %02d:%02d:%.12lf",
		y0, m0, d0, h0, min0, s0 );

	if (udu_origin_zero) 
		ut_free(udu_origin_zero);

	udu_origin_zero = ut_parse( unitSystem, u_str, UT_ASCII );	/* NOTE: sets a global (udu_origin_zero) */
	if( udu_origin_zero == NULL ) {
		fprintf( stderr, "Internal error in utCalendar2_cal routine make_udu_origin_zero: could not decode string \"%s\"\n",
			u_str );
		exit(-1);
		}
}

/******************************************************************************/
/* This extends the standard utCalendar call by recognizing CF-1.0 compliant
 * calendar names.
 * Here are some check values:
 *
 *   Units string		calendar 	input val	 Output date  	note
 * ---------------		--------	----------	 -----------	----
 * days since 0001-01-01	standard	146000 (400x365) 0400-09-23	400*365 days leaves us 100 leap days short of 1 Jan
 * days since 1001-01-01	standard	146000 (400x365) 1400-09-23	Advance by 1000 yrs leaves same deficit as above
 * days since 1601-01-01	standard	146000 (400x365) 2000-09-26	NOW have 3 less leap days than prev, since udunits 
 *										switches to Gregorian calendar after 1582
 * days since 1001-01-01	standard	-146000		 0601-04-11	works with neg vals too; 400*365 leaves us in day #101
 * days since 2001-01-01	standard	-146000		 1601-04-08	Gregorian calendar, vs. Julian in prev line
 *
 * days since 1001-07-15	standard	146000		 1401-04-06	Offset can be other than first day of year; fall 100 
 *										days short of going exactly 400 yrs, due to lack of leap days
 *
 * days since 0001-01-01	noleap		146000		 0401-01-01	No leap days, 400*365 = 400 yrs exactly
 * days since 1601-01-01	noleap		146000		 2001-01-01	"noleap" calendar doesn't change behavior around 1582
 * days since 2001-01-01	noleap		-146000		 1601-01-01	works with neg values too
 * days since 1001-01-01	noleap		-146000		 0601-01-01	neg values don't care before/after 1582 either
 *
 * Returns 0 on success, UT_ENOINIT if the package hasn't been initialized yet, and UT_EINVALID if the
 * passed unit structure is not a temporal one.
 */
/************************************************************************************************************************/
int utCalendar2_cal( double val, ut_unit *dataunits, int *year, int *month, int *day, int *hour, 
				int *minute, double *second, const char *calendar ) 
{
	static 	int have_shown_warning = 0;

#ifdef DEBUG
	printf( "entering utCalendar2_cal\n" );
	printf( "Input value: %lf  Input calendar: %s\n", val, calendar );
#endif

/*
	if( have_initted == 0 ) {
		make_udu_origin_zero( dataunits );
		have_initted = 1;
		}
 */
	  make_udu_origin_zero( dataunits );

	if( (calendar == NULL) || (strlen(calendar)==0) || (strncasecmp(calendar,"standard",8)==0) || (strncasecmp(calendar,"gregorian",9)==0) ) {
#ifdef DEBUG
		printf( "utCalendar_cal: using standard calendar\n" );
#endif
		return( utCalendar2( val, dataunits, year, month, day, hour, minute, second ));
		}

	else if( (strcmp(calendar,"365")==0) || (strncasecmp(calendar,"365_day",7)==0) || (strncasecmp(calendar,"noleap",6)==0) ) {
#ifdef DEBUG
		printf( "utCalendar_cal: using 365-day calendar\n" );
#endif
		return( utCalendar2_noleap( val, dataunits, year, month, day, hour, minute, second ));
		}

	else if( (strcmp(calendar,"360")==0) || (strncasecmp(calendar,"360_day",7)==0) ) {
#ifdef DEBUG
		printf( "utCalendar_cal: using 360-day calendar\n" );
#endif
		return( utCalendar2_360( val, dataunits, year, month, day, hour, minute, second ));
		}

	else if( strncasecmp(calendar,"proleptic_gregorian",19)==0) {
		if( shown_proleptic_warning == 0 ) {
			fprintf( stderr, "********************************************************************************\n" );
			fprintf( stderr, "Sorry, proleptic_gregorian calendar not implemented yet; using standard calendar\n" );
			fprintf( stderr, "********************************************************************************\n" );
			shown_proleptic_warning = 1;
			}
		return( utCalendar2( val, dataunits, year, month, day, hour, minute, second ));
		}

	else if( strncasecmp(calendar,"julian",6)==0) {
		if( shown_julian_warning == 0 ) {
			fprintf( stderr, "Sorry, julian calendar not implemented yet; using standard calendar\n" );
			shown_julian_warning = 1;
			}
		return( utCalendar2( val, dataunits, year, month, day, hour, minute, second ));
		}

	else
		{
		if( ! have_shown_warning ) {
			fprintf( stderr, "WARNING: unknown calendar: \"%s\". Using standard calendar instead!\n", calendar );
			have_shown_warning = 1;
			}
		return( utCalendar2( val, dataunits, year, month, day, hour, minute, second ));
		}
}

/*******************************************************************************************************************
 * Print an error message that the passed unit is not convertible to a timestamp unit 
 * (i.e., is temporal with an origin)
 */
static void err_mess_not_t_convertible( ut_unit *unit )
{
	static	ut_unit	*last_erroneous_unit=NULL;
	char	tunit[1024];

	if( (last_erroneous_unit == NULL) || (ut_compare(unit, last_erroneous_unit) != 0)) {
		if( ut_format( unit, tunit, 1020, UT_ASCII|UT_NAMES) != -1 )
			fprintf( stderr, "utCalendar2: error, can't convert units \"%s\" to a date; did you pass a correctly formatted timestamp unit (for example, \"days since 1901-01-01 00:00\"?\n",
				tunit );
		}

	if( (last_erroneous_unit != NULL) && (ut_compare(unit, last_erroneous_unit) != 0))
		/* We have a previously erroneous unit, but it's different from current one */
		ut_free( last_erroneous_unit );

	last_erroneous_unit = ut_clone( unit );
}

/*******************************************************************************************************************
 * For some reason udunits-2 doesn't seem to directly include this function, so here it is. Note that
 * the API differs from the udunits-1 version in that the "second" argument is double instead of float.
 */
int utCalendar2( double value, ut_unit *unit, int *year, int *month, int *day, int *hour, int *minute, double *second )
{
	double	resolution, value_conv;

	/* Following values are saved between invocations */
	static ut_unit	*last_unit=NULL;
	static cv_converter *conv_user_to_lib=NULL;

	if( unit == NULL )
		return( UT_ENOINIT );

/*
	if( ! have_initted ) {
		make_udu_origin_zero( unit );
		have_initted = 1;
		}
 */
	make_udu_origin_zero( unit );

	if( (last_unit == NULL) || (ut_compare(unit, last_unit) != 0)) {

		if( last_unit != NULL )
			ut_free( last_unit );

		if( conv_user_to_lib != NULL )
			cv_free( conv_user_to_lib );

		/* Make a converter FROM given units TO library units */
		conv_user_to_lib = ut_get_converter( unit, udu_origin_zero );
		if( conv_user_to_lib == NULL ) {
			err_mess_not_t_convertible( unit );
			return( UT_EINVALID );
			}

		last_unit = ut_clone( unit );
		}

	/* Do the conversion */
	value_conv = cv_convert_double( conv_user_to_lib, value );

	/* Change into a date */
	ut_decode_time( value_conv, year, month, day, hour, minute, second, &resolution );

	return(0);
}

/*******************************************************************************************************************
 * For some reason udunits-2 doesn't seem to directly include this function, so here it is. Note that
 * the API differs from the udunits-1 version in that the "second" argument is double instead of float.
 */
int utInvCalendar2( int year, int month, int day, int hour, int minute, double second, ut_unit *unit, double *value )
{
	double	lib_tval;

	static ut_unit	*last_unit=NULL;
	static cv_converter *conv_lib_to_user=NULL;

	if( unit == NULL )
		return( UT_ENOINIT );

/*
	if( have_initted == 0 ) {
		make_udu_origin_zero( unit );
		have_initted = 1;
		}
 */
	make_udu_origin_zero( unit );

	/* First turn the date into a time w.r.t. the udunits library's internal 
	 * reference date and units (which as of this writing is seconds since
	 * 2001-01-01, but it is irrelevant what they are, the code doesn't care)
	 */
	lib_tval = ut_encode_time( year, month, day, hour, minute, second );

	if( (last_unit == NULL) || (ut_compare(unit, last_unit) != 0)) {

		if( last_unit != NULL )
			ut_free( last_unit );

		if( conv_lib_to_user != NULL )
			cv_free( conv_lib_to_user );

		/* Make a converter FROM library units TO user units */
		conv_lib_to_user = ut_get_converter( udu_origin_zero, unit );
		if( conv_lib_to_user == NULL ) {
			err_mess_not_t_convertible( unit );
			return( UT_EINVALID );
			}

		last_unit = ut_clone( unit );
		}

	/* Do the conversion */
	*value = cv_convert_double( conv_lib_to_user, lib_tval );

	return(0);
}

/*************************************************************************************************
 * This returns 2 things.  1) the "since" time encoded in the "dataunits" object
 * (yr0, mon0, day0, etc) and 2) the number of seconds since that since time.  I.e.,
 * if the units string is "days since 1979-01-01", then the "since time" is 1 Jan 1979, which
 * is returned in yr0, mon0, day0, etc.  The returned value of this routine is 
 * the number of seconds that the "val, dataunits" combination indicates since that reference date.
 * So if val=2 and dataunits=days, then the returned value will be 2*86400.
 *
 * Returns 0 on success, <0 on error.
 */
static int udu_sec_since_ref_date( double val, ut_unit *dataunits, int *yr0, int *mon0, 
		int *day0, int *hr0, int *min0, double *sec0, double *ssrd )	/* ssrd = "seconds since reference date */
{
	double		tval0, tval_conv, rez;
	char		tunits[1024];
#ifdef DEBUG
	char		buf[1024];
#endif
	ut_unit		*users_units_in_seconds;
	int		same_units_as_before;
	cv_converter 	*conv_user_date_to_ref_date;

	/* Following are saved between invocations */
	static ut_unit	*last_dataunits=NULL;
	static cv_converter *conv_user_units_to_sec=NULL;
	static int p_yr0, p_mon0, p_day0, p_hr0, p_min0;	/* "previous" values from prior call */
	static double p_sec0;					/* "previous" value from prior call */

	if( udu_origin_zero == NULL )	/* Should never happen, but be safe anyway */
		make_udu_origin_zero( dataunits );

	/* If we are doing the same units as previously, use cached values */
	same_units_as_before = ((last_dataunits != NULL) && (ut_compare(dataunits, last_dataunits) == 0));

	/* Happily, udunits-2 makes it much easier to figure out the "since date".  All
	 * you need to do is convert a quantiy of zero units since the since date
	 * into the library's time system, then decode that value.  Example: if the
	 * original string was "days since 2005-01-01", make a converter that turns
	 * "days since 2005-01-01" into the library's reference time unit.  Then,
	 * convert the value of "0 days since 2005-01-01" and the result will be the number
	 * of seconds (since that is the time unit the ref library uses) between the
	 * reference library's since date and the user's since date.  Decode that
	 * time, and viola! You have the originally specified since date.
	 */
	if( same_units_as_before ) {
		*yr0=p_yr0; *mon0=p_mon0; *day0=p_day0; *hr0=p_hr0; *min0=p_min0; *sec0=p_sec0; 
		}
	else
		{
		conv_user_date_to_ref_date = ut_get_converter( dataunits, udu_origin_zero );
		if( conv_user_date_to_ref_date == NULL ) {
			err_mess_not_t_convertible( dataunits );
			return( UT_EINVALID );
			}
		tval0 = 0.0;
		tval_conv = cv_convert_double( conv_user_date_to_ref_date, tval0 );

		ut_decode_time( tval_conv, yr0, mon0, day0, hr0, min0, sec0, &rez );
		p_yr0 = *yr0; p_mon0 = *mon0; p_day0 = *day0; p_hr0 = *hr0; p_min0 = *min0; p_sec0 = *sec0;

#ifdef DEBUG
		dateify( *yr0, *mon0, *day0, *hr0, *min0, *sec0, buf );
		printf( "udu_sec_since_ref_date: here is the <<since date>> that the user specified: %s\n", buf );
#endif

		/* Now convert the value from whatever units it might be in to seconds.
		 * For example, if the user specified "days since 1901-01-01", and the
		 * passed argument "val" is 1, then we want to return 86400.
		 */
		sprintf( tunits, "seconds since %04d-%02d-%02d %02d:%02d:%.12lf", 
			*yr0, *mon0, *day0, *hr0, *min0, *sec0 );

		users_units_in_seconds = ut_parse( ut_get_system(dataunits), tunits, UT_ASCII );
		if( users_units_in_seconds == NULL ) {
			fprintf( stderr, "Internal error in routine udu_sec_since_ref_date: cannot parse string \"%s\"\n",
				tunits );
			return( UT_EINVALID );
			}

		/* Get the new converter */
		if( conv_user_units_to_sec != NULL )
			cv_free( conv_user_units_to_sec );
		conv_user_units_to_sec = ut_get_converter( dataunits, users_units_in_seconds );
		if( conv_user_units_to_sec == NULL ) {
			err_mess_not_t_convertible( dataunits );
			return( UT_EINVALID );
			}

		ut_free( users_units_in_seconds );
		
		if( last_dataunits != NULL )
			ut_free( last_dataunits );
		last_dataunits = ut_clone( dataunits );
		}
		
	*ssrd = cv_convert_double( conv_user_units_to_sec, val );

#ifdef DEBUG
	printf( "udu_sec_since_ref_date: passed VAL in user units was %lf, converted that to %lf seconds\n", 
		val, *ssrd );
#endif
	return( 0 );
}

/*************************************************************************************/
/* A calendar with no leap days; days per month and year are passed in, not assumed!  
 * Can pass in days_per_year=365 and days_per_month={30,28,31,30, etc} for a "noleap" calendar,
 * or days_per_year=360 and days_per_month={30,30,30,...} for a "360 day" calendar
 */
static int utCalendar2_noleap_inner( double val, ut_unit *dataunits, int *year, int *month, int *day, int *hour, 
				int *minute, double *second, int days_per_year, long *days_per_month )
{
	int yr0, mon0, day0, hr0, min0, ierr;
	double sec0;
	double ss, ss_extra;
	long dy, ds, sec_per_day, sec_per_hour, sec_per_min, nny;
	long nhrs, nmin;

	sec_per_day   = 86400;
	sec_per_hour  = 3600;
	sec_per_min   = 60;

	/* -------------------------------------------------------------------------------------
	 * Get both the REFERENCE TIME that the netCDF file specifies for the units string
	 * (yr0, mon0, day0, etc) and the number of seconds since that reference date.  I.e.,
	 * if the units string is "days since 1979-01-01", then the reference date is 1 Jan 1979
	 * and 'ss' is the number of seconds since that date that we want to turn into a calendar
	 * date, given the specified calendar.
	 *--------------------------------------------------------------------------------------*/
	if( (ierr = udu_sec_since_ref_date( val, dataunits, &yr0, &mon0, &day0, &hr0, &min0, &sec0, &ss )) != 0 )
		return( ierr );
#ifdef DEBUG
 	printf( "utCalendar2_noleap_inner: converting time %lf seconds since %04d-%02d-%02d %02d:%02d:%06.3lf\n", ss, yr0, mon0, day0, hr0, min0, sec0 ); 
#endif

	/*--------------------------------------------------------------------------
	 * If we have a date before our reference date (indicated by a negative ss),
	 * then wind back the reference date to to be before the target
	 * date.  This avoids having to muck around with negative offsets.
	 *------------------------------------------------------------------------*/
	if( ss < 0 ) {
		nny = -ss / (sec_per_day*days_per_year) + 1;
		yr0 -= nny;
		ss  += nny * sec_per_day*days_per_year;
		}

	/*-------------------------------------------------------------------
	 * We now have seconds since (ss) yr0, mon0, day0, hr0, min0, sec0.
	 * Try to turn this into integer days since reference date and seconds
	 * extra, avoiding problems with roundoff and limited precision.  Not
	 * an exact science.  We use days since (ds) instead of sticking 
	 * strictly with seconds since (ss) becuase we can overflow longs in
	 * fairly routine circumstances, if tring to put a century or so of
	 * seconds into a long.
	 *-------------------------------------------------------------------*/
	ds = (long)((ss + .01)/(double)sec_per_day);
	ss_extra = ss - ((double)ds)*((double)sec_per_day);  /* need to be careful of overflow here */
	if( ss_extra < 0. )
		ss_extra = 0;

#ifdef DEBUG
	printf( "utCalendar2_noleap_inner: # of days since ref date: %ld   # of extra seconds after days taken out: %lf\n", ds, ss_extra );
#endif

	/*------------------------------------------------
	 * Easier to do things relative to 1 Jan  00:00:00
	 *-----------------------------------------------*/
	if( (sec0 != 0) || (min0 != 0) || (hr0 != 0) || (day0 != 1) || (mon0 != 1)) {

		ss_extra += sec0;
		sec0 = 0;

		ss_extra += min0 * sec_per_min;
		min0 = 0;

		ss_extra += hr0 * sec_per_hour;
		hr0 = 0;

		ds += (day0-1);
		day0 = 1;

		while( mon0 > 1 ) {
			ds += days_per_month[ mon0-2 ];	/*  -2 cuz -1 for prev month, -1 for 0 offset */
			mon0--;
			}
		}

	/* After adjustments immediately above, could have more seconds
	 * than there are in a day
	 */
	while( ss_extra > (double)sec_per_day ) {
		ds++;
		ss_extra -= (double)sec_per_day;
		}

	dy = ds / days_per_year;
	*year = yr0 + dy;
	ds = ds - dy*days_per_year;

	*month = 1;
	while( ds > days_per_month[(*month) - 1] - 1 ) {
		ds -= days_per_month[(*month) - 1];
		(*month)++;
		}

	*day = ds + 1;

	nhrs = ss_extra / sec_per_hour;
	*hour = nhrs;
	ss_extra -= nhrs * sec_per_hour;

	nmin = ss_extra / sec_per_min;
	*minute = nmin;
	ss_extra -= nmin * sec_per_min;

	*second = ss_extra;

	return(0);
}

/******************************************************************************/
static int utCalendar2_360( double val, ut_unit *dataunits, int *year, int *month, int *day, int *hour, 
				int *minute, double *second )
{
	long days_per_year;

	days_per_year = 360L;

	return( utCalendar2_noleap_inner( val, dataunits, year, month, day, hour, minute, second,
		days_per_year, days_per_month_360 ));
}

/******************************************************************************/
static int utCalendar2_noleap( double val, ut_unit *dataunits, int *year, int *month, int *day, int *hour, 
				int *minute, double *second )
{
	long days_per_year;

	days_per_year = 365L;

	return( utCalendar2_noleap_inner( val, dataunits, year, month, day, hour, minute, second,
		days_per_year, days_per_month_reg_year ));
}


/*****************************************************************************************************
 *****************************************************************************************************
 *****************************************************************************************************
 *****************************************************************************************************
 *
 *  Inverse Calendar routines 
 *
 *****************************************************************************************************
 *****************************************************************************************************
 *****************************************************************************************************
 *
Similar to utInvCalendar, but takes an extra 'cal' argument that can be one of the
following:
	'noleap':	A calendar with no leap years, so just 365 days every year
	'365_day': 	A synonym for noleap
	'360_day':	A day with 360 days per year, arranged in 12 months of 30 days each
	'standard':	An ordinary, Gregorian calendar

Check values:

Units string		calendar	Input: yr,mo,dy,hr,min,sec	Output val	Notes
------------		--------	--------------------------	----------	-----
days since 2000-02-25	standard	2000, 3, 1, 12, 0, 0.0		5.5		2000 was a leap year
days since 2000-02-25	noleap 		2000, 3, 1, 12, 0, 0.0		4.5		2000 was a leap year
*/


/********************************************************************************************************/
int utInvCalendar2_cal( int year, int month, int day, int hour, int minute, 
		double second, ut_unit *unit, double *value, const char *calendar )
{
#ifdef DEBUG
	char		buf[1024];
#endif

#ifdef DEBUG
	dateify( year, month, day, hour, minute, second, buf );
	printf( "called utInvCalendar_cal with date to convert=%s\n", buf );
#endif

/*
	if( have_initted == 0 ) {
		make_udu_origin_zero( unit );
		have_initted = 1;
		}
 */
	make_udu_origin_zero( unit );

	if( (calendar == NULL) || (strlen(calendar) == 0) || (strncasecmp(calendar,"standard",8)==0) || (strncasecmp(calendar,"gregorian",9)==0) ) {
#ifdef DEBUG
		printf( "called utInvCalendar_cal with a standard calendar\n" );
#endif
		return( utInvCalendar2( year, month, day, hour, minute, second, unit, value ));
		}

	else if( (strncasecmp(calendar,"365_day",7)==0) || (strncasecmp(calendar,"noleap",6)==0) 
		|| (strncasecmp(calendar,"365",3) == 0) || (strncasecmp(calendar,"no_leap",7)==0)) {
#ifdef DEBUG
		printf( "called utInvCalendar_cal with a noleap calendar\n" );
#endif
		return( utInvCalendar2_noleap_inner( year, month, day, hour, minute, second, unit, value,
			days_per_month_reg_year ));
		}

	else if((strncasecmp(calendar,"360_day",7)==0) || (strncasecmp(calendar,"360",3)==0)) {
#ifdef DEBUG
		printf( "called utInvCalendar_cal with a 360_day calendar\n" );
#endif
		return( utInvCalendar2_noleap_inner( year, month, day, hour, minute, second, unit, value,
			days_per_month_360 ));
		}
	else
		{
		printf( "Sorry, %s calendar not implemented yet; using standard calendar\n", calendar );
		return( utInvCalendar2( year, month, day, hour, minute, second, unit, value ));
		}
}

/********************************************************************************************************/
static int utInvCalendar2_noleap_inner( int year, int month, int day, int hour, int minute,
                double second, ut_unit *user_unit, double *value, long *days_per_month )
{ 
	int	u_year, u_month, u_day, u_hour, u_minute, i, err,
		yr0, yr1, mo0, mo1, dy0, dy1, hr0, hr1, mn0, mn1;
	double	u_second, tmp_tval, sec0, sec1;
	long	units_earlier, sep_seconds_i, ss_extra_i, sep_days, days_per_year, dd_extra;
	double	sep_seconds_f, ss_extra_f, sec_per_min, sec_per_hour, sec_per_day, unused;
	char	tmp_unit_str[1024];
	ut_unit	*tmp_unit;
#ifdef DEBUG
	char		buf[1024];
#endif

	static ut_unit 		*last_user_unit;
	static cv_converter 	*conv_seconds_to_user_units=NULL;
	static cv_converter 	*conv_days_to_user_units=NULL;

	sec_per_min = 60;
	sec_per_hour = sec_per_min * 60;
	sec_per_day  = sec_per_hour * 24;

	/* Get, into u_year, u_month, etc., the date passed in the units string */
	if( (err = udu_sec_since_ref_date( 0.0, user_unit, &u_year, &u_month, &u_day, &u_hour, &u_minute, &u_second, &unused )) != 0 )
		return( err );
#ifdef DEBUG
	dateify( u_year, u_month, u_day, u_hour, u_minute, u_second, buf );
	printf( "utInvCalendar_cal, date in passed units string: %s\n", buf );
#endif

 	/*--------------------------------------------------------------------
	 * Find out which is earlier, the passed date or the unit's base date.
	 * If they are the same, just return zero.
	 *-------------------------------------------------------------------*/
	if( (u_year==year) && (u_month==month) && (u_day==day) && (u_hour==hour) && 
			(u_minute==minute) && (u_second==second)) {
		*value = 0.0;
		return(0);
		}
		
	units_earlier = 1;
	if( u_year > year ) 
		units_earlier = 0;
	else if( u_year == year ) {
		if( u_month > month )
			units_earlier = 0;
		else if( u_month == month ) {
			if( u_day > day )
				units_earlier = 0;
			else if( u_day == day ) {
				if( u_hour > hour )
					units_earlier = 0;
				else if( u_hour == hour ) {
					if( u_minute > minute ) 
						units_earlier = 0;
					else if( u_minute == minute ) {
						if( u_second > second )
							units_earlier = 0;
						}
					}
				}
			}
		}
		
	/* Put things in the early and late dates for ease, apply 
	 * proper sign at the end */
	if( units_earlier ) {
		yr0 = u_year;
		yr1 = year;
		mo0 = u_month;
		mo1 = month;
		dy0 = u_day;
		dy1 = day;
		hr0 = u_hour;
		hr1 = hour;
		mn0 = u_minute;
		mn1 = minute;
		sec0 = u_second;
		sec1 = second;
		}
	else
		{
		yr1 = u_year;
		yr0 = year;
		mo1 = u_month;
		mo0 = month;
		dy1 = u_day;
		dy0 = day;
		hr1 = u_hour;
		hr0 = hour;
		mn1 = u_minute;
		mn0 = minute;
		sec1 = u_second;
		sec0 = second;
		}

	/* This is the total separation between the early and
	 * late dates, in seconds plus days.  We split out
	 * integer seconds and fracitonal seconds to better 
	 * address rounding issues.
	 */
	sep_seconds_i = 0;
	sep_seconds_f = 0.0;
	sep_days      = 0;

	/* Wind the early date back to Jan 1 of that year for ease of computation */
	ss_extra_i = 0;
	ss_extra_f = 0.0;
	dd_extra   = 0;
	if( (mo0 != 1) || (dy0 != 1) || (hr0 != 0) || (mn0 != 0) || (sec0 != 0.0)) {
		
		ss_extra_i = floor( sec0 );
		ss_extra_f = sec0 - floor( sec0 );
		sec0 = 0.0;

		ss_extra_i += mn0 * sec_per_min;
		mn0 = 0;

		ss_extra_i += hr0 * sec_per_hour;
		hr0 = 0;

		dd_extra += dy0 - 1;
		dy0 = 1;

		while( mo0 > 1 ) {
			dd_extra += days_per_month[ mo0-2 ];  /* -2 cuz -1 for prev month, -1 for 0 offset */
			mo0--;
			}
		}

	days_per_year = 0;
	for( i=0; i<12; i++ )
		days_per_year += days_per_month[i];

	/* Add up all the years that separate our early and late dates.  We
	 * use days rather than seconds because we can overflow seconds in
	 * fairly routine calculations involving century timescales. 
	 */
	sep_days += (yr1-yr0) * days_per_year;

	/* Now just add up days from beginning of the year to our
	 * later target date...
	 */
	sep_days += dy1 - 1;
	while( mo1 > 1 ) {
		sep_days += days_per_month[ mo1-2 ];	/* -2 cuz -1 for prev month, -1 for 0 offset */
		mo1--;
		}

	sep_seconds_i += floor( sec1 );
	sep_seconds_f =  sec1 - floor(sec1);
	sep_seconds_i += mn1*sec_per_min + hr1*sec_per_hour;	/* note: days taken care of just above! */

	/* Correct for the unwinding of the base ref date to Jan 1st */
	sep_seconds_f -= ss_extra_f;
	sep_seconds_i -= ss_extra_i;
	sep_days      -= dd_extra;

	/* We now have the correct number of days and seconds separating
	 * our early and late dates: sep_days and (sep_seconds_i + sep_seconds_f)
	 */
#ifdef DEBUG
	printf( "utInvCalendar2_noleap_inner: dates differ by %ld days, and (%ld + %lf) seconds\n", sep_days, sep_seconds_i, sep_seconds_f );
#endif

	/* Now convert our days/seconds since the user's reference date to the units required 
	 * by the user. Note that we go through this whole days/seconds rigormarole so
	 * that we don't overflow seconds when doing century-scale conversions.
	 */
	if( (last_user_unit == NULL) || (ut_compare(last_user_unit, user_unit) != 0)) {

		/* Get converter, seconds to user units */
		sprintf( tmp_unit_str, "seconds since %04d-%02d-%02d %02d:%02d:%.12lf", 
			u_year, u_month, u_day, u_hour, u_minute, u_second );
		if( (tmp_unit = ut_parse( ut_get_system(user_unit), tmp_unit_str, UT_ASCII )) == NULL ) {
			fprintf( stderr, "Internal error in routine utInvCalendar2_noleap_inner: could not parse string \"%s\"\n",
				tmp_unit_str );
			exit(-1);
			}
		if( conv_seconds_to_user_units != NULL )
			cv_free( conv_seconds_to_user_units );
		if( (conv_seconds_to_user_units = ut_get_converter( tmp_unit, user_unit )) == NULL ) {
			err_mess_not_t_convertible( user_unit );
			return( UT_EINVALID );
			}
		ut_free( tmp_unit );

		/* Get converter, days to user units */
		sprintf( tmp_unit_str, "days since %04d-%02d-%02d %02d:%02d:%.12lf", 
			u_year, u_month, u_day, u_hour, u_minute, u_second );
		if( (tmp_unit = ut_parse( ut_get_system(user_unit), tmp_unit_str, UT_ASCII )) == NULL ) {
			fprintf( stderr, "Internal error in routine utInvCalendar2_noleap_inner: could not parse string \"%s\"\n",
				tmp_unit_str );
			exit(-1);
			}
		if( conv_days_to_user_units != NULL )
			cv_free( conv_days_to_user_units );
		if( (conv_days_to_user_units = ut_get_converter( tmp_unit, user_unit )) == NULL ) {
			err_mess_not_t_convertible( user_unit );
			return( UT_EINVALID );
			}
		ut_free( tmp_unit );

		if( last_user_unit != NULL )
			ut_free( last_user_unit );
		last_user_unit = ut_clone( user_unit );
		}

	tmp_tval = (double)sep_seconds_i + sep_seconds_f;
	*value = cv_convert_double( conv_seconds_to_user_units, tmp_tval );

	tmp_tval = sep_days;
	*value += cv_convert_double( conv_days_to_user_units, tmp_tval );

	/* Apply sign */
	if( ! units_earlier )
		*value = -(*value);

#ifdef DEBUG
	printf( "utInvCalendar2_noleap_inner: that separation in users units is %lf\n", *value );
#endif
	return(0);
}


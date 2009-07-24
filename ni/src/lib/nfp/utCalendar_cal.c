/*
These are drop-in replacements for utCalendar and utInvCalendar that take an additional
character argument at the end that indicates the calendar to use.  The calendar string
can be "standard", "noleap", "365_day", "360", or "360_day" at the moment.  If the calendar
string is set to NULL, a standard calendar is used.

Note that these routines call the udunits routines, so they have to be installed to use this!

Version 2.15
David W. Pierce
dpierce@ucsd.edu
2009-01-14

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

This code is intended to be a simple "drop in" replacement of the standard
udunits calls, and so DOES NOT MODIFY THE YEAR 0 BEHAVIOR in any way.  All
origins that start at "year 0000" (which does not exist) are treated as if
they had specified "year 0001".  Again, this is exactly what the standard
udunits library does.
*/

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include "udunits.h"
#include "utCalendar_cal.h"

/* #define DEBUG */

/*                                         J   F   M   A   M   J   J   A   S   O   N   D    */
static long days_per_month_reg_year[] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
static long days_per_month_360[]      = { 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30 };

static utUnit udu_origin_zero, udu_days;
static  int   udu_library_version( void );
static double udu_sec_since_ref_date( double val, utUnit *dataunits, int *yr0, int *mon0, 
		int *day0, int *hr0, int *min0, float *sec0 );
static int utCalendar_noleap_inner( double val, utUnit *dataunits, int *year, int *month, int *day, int *hour, 
				int *minute, float *second, int days_per_year, long *days_per_month );
static int utCalendar_360( double val, utUnit *dataunits, int *year, int *month, int *day, int *hour, 
				int *minute, float *second );
static int utCalendar_noleap( double val, utUnit *dataunits, int *year, int *month, int *day, int *hour, 
				int *minute, float *second );
static int utInvCalendar_noleap_inner( int year, int month, int day, int hour, int minute,
                double second, utUnit *unit, double *value, long *days_per_month );

static int shown_proleptic_warning = 0;
static int shown_julian_warning    = 0;

/******************************************************************************/
static void dateify( int year, int month, int day, int hour, int minute, 
		float second, char *buf )
{
	sprintf( buf, "%04d-%02d-%02d %02d:%02d:%.2f", year, month, day, hour, minute, second );
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
 */

/************************************************************************************************************************/
/* Returns 1, 2, or halts with an error
 */
int udu_library_version()
{
	static int udu_lib_version = -1;	/* '1' is pre 1.12.9, '2' is >= 1.12.9 or so, don't know exact rev this changed */
	char	ss[2048];
	int	err;
	double 	zero = 0.0;
	int 	ref_year, ref_month, ref_day, ref_hour, ref_minute;
	float 	ref_second;
	utUnit 	udu_ref_date_plus_1;

	if( udu_lib_version != -1 )
		return( udu_lib_version );

	/*----------------------------------------------------------------------------------------
	 * Somewhere between version 1.12.4 and 1.12.9, the udunits library changed its behavior
	 * w.r.t. how it records a user-specified time origin.  In the earlier method, a time origin
	 * would be convereted to seconds and stored that way.  The "factor" (i.e., multiplicative
	 * factor needed to convert from the ORIGINAL user-specified units to seconds) would still
	 * be set on the basis of the original units however.  Perhaps the library maintainers 
	 * found this to be slighly weird, because they later changed it so that the time origin
	 * is stored in user-specified units instead of being converted to seconds.  The upshot
	 * of all this is that we have to determine which behavior we are seeing, and adjust
	 * what we do accordingly.
	 *---------------------------------------------------------------------------------------*/
	/* First step: get reference date being used by udunits library */
	err = utCalendar( zero, &udu_origin_zero, &ref_year, &ref_month, &ref_day, &ref_hour, &ref_minute, &ref_second );
#ifdef DEBUG
	printf( "udunits libray is using the following internal reference date: %d-%02d-%02d %02d:%02d\n", 
			ref_year, ref_month, ref_day, ref_hour, ref_minute );
#endif
	/* Now print out a string that has ONE DAY LATER than the reference date;
	 * note that our user-specified units is "hours"
	 */
	sprintf( ss, "hours since %d-%02d-%02d %02d:%02d", ref_year, ref_month, ref_day+1, 
		ref_hour, ref_minute );
	/* convert this string to a udunits... */
	err = utScan( ss, &udu_ref_date_plus_1 );
	if( err != 0 ) {
		fprintf( stderr, "Internal error scanning ref date plus one day string \"%s\"\n", ss );
		exit(-1);
		}
	if( fabs(udu_ref_date_plus_1.origin - 86400.0 ) < 0.1 ) 
		udu_lib_version = 1;	/* orig version converts the one day to SECONDS */
	else if( fabs(udu_ref_date_plus_1.origin - 24.0 ) < 0.1 ) 
		udu_lib_version = 2;	/* new version leaves the one day in user-specified units (here, hours) */
	else
		{
		fprintf( stderr, "Error, cannot determine behavior of the udunits library!\n" );
		fprintf( stderr, "plus one origin (should be 24 hours or 86400 seconds) is: %lf\n",
			udu_ref_date_plus_1.origin );
		fprintf( stderr, "I will assume new version of udunits library, but this will likely give wrong dates\n" );
		udu_lib_version = 2;
		}
#ifdef DEBUG
	printf( "***** Version of udunits library: %d (1=old, before about 1.12.5; 2=new, >= 1.12.9 or so)\n", 
		udu_lib_version );
#endif
	return( udu_lib_version );
}

/************************************************************************************************************************/

int utCalendar_cal( double val, utUnit *dataunits, int *year, int *month, int *day, int *hour, 
				int *minute, float *second, char *calendar ) 
{
	int 	ii, err;
	static 	int have_shown_warning = 0;
	static 	int have_initted = 0;

#ifdef DEBUG
	printf( "entering utCalendar_cal\n" );
	printf( "Input value: %lf  Input calendar: %s\n", val, calendar );
#endif

	if( have_initted == 0 ) {
#ifdef DEBUG
		printf( "utCalendar_cal: initting\n" );
#endif
		/*-------------------------------------------------------------------------------------------
		 * The idea of this snippet is to "trick" the udunits library into telling us the year, month,
		 * and date that the user specified in the units string.  This prevents us from having to 
		 * reinvent the wheel by parsing the units string ourselves.  See further comments
		 * in routine udu_sec_since_ref_date
		 *------------------------------------------------------------------------------------------*/
		err = utScan( "seconds since 1234-05-06 00:00", &udu_origin_zero );  /* YYYY-MM-DD used here is irrelevant */
		if( err == 0 ) {
			udu_origin_zero.origin = 0.0;   /* override specified YYYY-MM-DD to set to same date as lib uses internally */
			}
		else
			{
			fprintf( stderr, "Error, could not decode internal date string for reference date!\n" );
			return(-1);
			}

		have_initted = 1;
		}

	if( (calendar == NULL) || (strncasecmp(calendar,"standard",8)==0) ||
	    (strncasecmp(calendar,"gregorian",9)==0) ) {
#ifdef DEBUG
		printf( "utCalendar_cal: using standard calendar\n" );
#endif
		return( utCalendar( val, dataunits, year, month, day, hour, minute, second ));
		}
	else if( (strcmp(calendar,"365")==0) ||
		 (strncasecmp(calendar,"365_day",7)==0) || 
		 (strncasecmp(calendar,"noleap",6)==0) ) {
#ifdef DEBUG
		printf( "utCalendar_cal: using 365-day calendar\n" );
#endif
		return( utCalendar_noleap( val, dataunits, year, month, day, hour, minute, second ));
		}
	else if( (strcmp(calendar,"360")==0) || 
		 (strncasecmp(calendar,"360_day",7)==0) ) {
#ifdef DEBUG
		printf( "utCalendar_cal: using 360-day calendar\n" );
#endif
		return( utCalendar_360( val, dataunits, year, month, day, hour, minute, second ));
		}
	else if( strncasecmp(calendar,"proleptic_gregorian",19)==0) {
		if( shown_proleptic_warning == 0 ) {
			fprintf( stderr, "********************************************************************************\n" );
			fprintf( stderr, "Sorry, proleptic_gregorian calendar not implemented yet; using standard calendar\n" );
			fprintf( stderr, "********************************************************************************\n" );
			shown_proleptic_warning = 1;
			}
		return( utCalendar( val, dataunits, year, month, day, hour, minute, second ));
		}
	else if( strncasecmp(calendar,"julian",6)==0) {
		if( shown_julian_warning == 0 ) {
			fprintf( stderr, "Sorry, julian calendar not implemented yet; using standard calendar\n" );
			shown_julian_warning = 1;
			}
		return( utCalendar( val, dataunits, year, month, day, hour, minute, second ));
		}
	else
		{
		if( ! have_shown_warning ) {
			fprintf( stderr, "WARNING: unknown calendar: \"%s\". Using standard calendar instead!\n", calendar );
			have_shown_warning = 1;
			}
		return( utCalendar( val, dataunits, year, month, day, hour, minute, second ));
		}
}

/******************************************************************************/
double udu_sec_since_ref_date( double val, utUnit *dataunits, int *yr0, int *mon0, 
		int *day0, int *hr0, int *min0, float *sec0 )
{
	double retval, origin2use;
	int err;
	char	buf[1024];

        /*---------------------------------------------------------------------
         * Use a bit of a trick to get the year, month, and day that the
         * original user specified in the units string and was subsequently
         * parsed by the udunits library.  We make use of the fact that the
         * udunits offset is set to some specific date -- it doesn't matter
         * which.  But everything is referenced to this date.  So we make
         * a time units, and SET ITS ORIGIN TO ZERO, so that it is "as if"
         * we specified the date relative to udunit's internal reference
         * date (currently, 2001-01-01).  We then convert the origin of the
         * user-specified units, which is the number of seconds since the
         * udunits reference date, into a calendar date.  Voila!  We then
         * have the year, month, day, etc. that the user specified.
         *--------------------------------------------------------------------*/
	if( udu_library_version() == 1 )
		origin2use = dataunits->origin;
	else if( udu_library_version() == 2 )
		origin2use = dataunits->origin * dataunits->factor;
	else
		{
		fprintf( stderr, "Error, udu_lib_version not set to valid value: %d\n", udu_library_version() );
		exit( -1 );
		}
	err = utCalendar( origin2use, &udu_origin_zero, yr0,
		mon0, day0, hr0, min0, sec0 );  /* note: all these yr0 etc values are RETURNED */
#ifdef DEBUG
	dateify( *yr0, *mon0, *day0, *hr0, *min0, *sec0, buf );
	printf( "udu_sec_since_ref_date: here is the <<since date>> that the user specified: %s\n", buf );
#endif


	retval = val * dataunits->factor;

	return( retval );
}

/*************************************************************************************/
/* A calendar with no leap days; days per month and year are passed in, not assumed!  
 * Can pass in days_per_year=365 and days_per_month={30,28,31,30, etc} for a "noleap" calendar,
 * or days_per_year=360 and days_per_month={30,30,30,...} for a "360 day" calendar
 */
int utCalendar_noleap_inner( double val, utUnit *dataunits, int *year, int *month, int *day, int *hour, 
				int *minute, float *second, int days_per_year, long *days_per_month )
{
	int yr0, mon0, day0, hr0, min0;
	float sec0;
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
	ss = udu_sec_since_ref_date( val, dataunits, &yr0, &mon0, &day0, &hr0, &min0, &sec0 );
#ifdef DEBUG
 	printf( "converting time %lf seconds since %04d-%02d-%02d %02d:%02d\n", ss, yr0, mon0, day0, hr0, min0 ); 
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
	printf( "# of days since ref date: %ld   # of extra seconds after days taken out: %lf\n", ds, ss_extra );
#endif

	/*--------------------------------------
	 * Easier to do things relative to 1 Jan 
	 *-------------------------------------*/
	if( (min0 != 0) || (hr0 != 0) || (day0 != 1) || (mon0 != 1)) {

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
int utCalendar_360( double val, utUnit *dataunits, int *year, int *month, int *day, int *hour, 
				int *minute, float *second )
{
	long days_per_year;

	days_per_year = 360L;

	return( utCalendar_noleap_inner( val, dataunits, year, month, day, hour, minute, second,
		days_per_year, days_per_month_360 ));
}

/******************************************************************************/
int utCalendar_noleap( double val, utUnit *dataunits, int *year, int *month, int *day, int *hour, 
				int *minute, float *second )
{
	long days_per_year;

	days_per_year = 365L;

	return( utCalendar_noleap_inner( val, dataunits, year, month, day, hour, minute, second,
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
int utInvCalendar_cal( int year, int month, int day, int hour, int minute, 
		double second, utUnit *unit, double *value, const char *calendar )
{
	char	buf[1024];
	static	int have_initted = 0;
	int	err;

	if( have_initted == 0 ) {
#ifdef DEBUG
		printf( "utInvCalendar_cal: initting\n" );
#endif
		/*-------------------------------------------------------------------------------------------
		 * The idea of this snippet is to "trick" the udunits library into telling us the year, month,
		 * and date that the user specified in the units string.  This prevents us from having to 
		 * reinvent the wheel by parsing the units string ourselves.  See further comments
		 * in routine udu_sec_since_ref_date
		 *------------------------------------------------------------------------------------------*/
		err = utScan( "seconds since 1900-01-01 00:00", &udu_origin_zero );  /* YYYY-MM-DD used here is irrelevant but DO NOT CHAGNE FROM SECONDS!! */
		if( err == 0 ) {
			udu_origin_zero.origin = 0.0;   /* override specified YYYY-MM-DD to set to same date as lib uses internally */
			}
		else
			{
			fprintf( stderr, "Error, could not decode internal seconds/date string for reference date!\n" );
			return(-1);
			}

		err = utScan( "days since 1900-01-01 00:00", &udu_days );
		if( err != 0 ) {
			fprintf( stderr, "Error, could not decode internal days/date string for reference date!\n" );
			return(-1);
			}

		have_initted = 1;
		}

#ifdef DEBUG
		dateify( year, month, day, hour, minute, second, buf );
		printf( "called utInvCalendar_cal with date to convert=%s\n", buf );
#endif

	if( (calendar == NULL) || (strncasecmp(calendar,"standard",8)==0) ||
	    (strncasecmp(calendar,"gregorian",9)==0) ) {
#ifdef DEBUG
		printf( "called utInvCalendar_cal with a standard calendar\n" );
#endif
		return( utInvCalendar( year, month, day, hour, minute, second, unit, value ));
		}

	else if( (strncasecmp(calendar,"365",3)==0) || 
		 (strncasecmp(calendar,"365_day",7)==0) || 
		 (strncasecmp(calendar,"noleap",6)==0) ) {
#ifdef DEBUG
		printf( "called utInvCalendar_cal with a noleap calendar\n" );
#endif
		return( utInvCalendar_noleap_inner( year, month, day, hour, minute, second, unit, value,
			days_per_month_reg_year ));
		}

	else if( (strncasecmp(calendar,"360_day",7)==0) ||
		 (strncasecmp(calendar,"360",3)    ==0) )  {
#ifdef DEBUG
		printf( "called utInvCalendar_cal with a 360_day calendar\n" );
#endif
		return( utInvCalendar_noleap_inner( year, month, day, hour, minute, second, unit, value,
			days_per_month_360 ));
		}
	else
		{
		printf( "Sorry, %s calendar not implemented yet; using standard calendar\n", calendar );
		return( utInvCalendar( year, month, day, hour, minute, second, unit, value ));
		}
}

/********************************************************************************************************/
int utInvCalendar_noleap_inner( int year, int month, int day, int hour, int minute,
                double second, utUnit *unit, double *value, long *days_per_month )
{
	int	u_year, u_month, u_day, u_hour, u_minute, i, err,
		yr0, yr1, mo0, mo1, dy0, dy1, hr0, hr1, mn0, mn1, sec0, sec1;
	float	u_second;
	long	units_earlier, sep_seconds_i, ss_extra_i, sep_days, days_per_year, dd_extra;
	double	sep_seconds_f, ss_extra_f, sec_per_min, sec_per_hour, sec_per_day, unused,
		slope, intercept;
	char	buf[100];

	sec_per_min = 60;
	sec_per_hour = sec_per_min * 60;
	sec_per_day  = sec_per_hour * 24;

	/* Get, into u_year, u_month, etc., the date passed in the units string */
	unused = udu_sec_since_ref_date( 0.0, unit, &u_year, &u_month, &u_day, &u_hour, &u_minute, &u_second );
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
	printf( "utInvCalendar_noleap_inner: dates differ by %ld days, and (%ld + %lf) seconds\n", sep_days, sep_seconds_i, sep_seconds_f );
#endif

	/* Now convert our days/seconds to the units required by the user.
	 * Note that we go through this whole days/seconds rigormarole so
	 * that we don't overflow seconds when doing century-scale conversions.
	 */
	err = utConvert( &udu_origin_zero, unit, &slope, &intercept );	/* udu_origin_zero has units of 'seconds' */
	*value = (sep_seconds_i * slope) + (sep_seconds_f * slope);

	err = utConvert( &udu_days, unit, &slope, &intercept );	/* udu_days has units of 'days' */
	*value += sep_days * slope;

	/* Apply sign */
	if( ! units_earlier )
		*value = -(*value);

#ifdef DEBUG
	printf( "utInvCalendar_noleap_inner: that separation in users units is %lf\n", *value );
#endif
	return(0);
}

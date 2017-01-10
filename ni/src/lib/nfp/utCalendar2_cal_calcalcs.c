/*
 * This code was added in NCL V6.4.0 to address problems found in the
 * cd_calendar, cd_inv_calendar, ut_calendar, ut_inv_calendar routines,
 * especially having to do with minutes and seconds not always being
 * handled properly.  I'm grateful to David Pierce, developer of 
 * ncview and Calcalcs (this code here) for giving us permission to
 * use this code.
 *
 * I'm not ready to make this code the default version yet. There are 
 * some issues with this new code:
 *
 *    1) It doesn't "months since" type of units very well. I had to add
 *       a hack to udunitsW_calcalcs.c (which is the wrapper that calls
 *       these routines) to convert "months since" to a different units 
 *       like "hours since", do the calculation, then convert back to 
 *       "months since".
 * 
 *    2) It doesn't like the use of year 0 with a Julian calendar. This
 *       causes an error and a seg fault because of an invalid memory
 *       free. I believe the memory free is happening when 
 *       "prev_user_unit_str" is freed. Search on 
 *       "new units, initting units structures".
 * 
 *    3) Finally, and this is not an issue with the code itself, but 
 *       I'm not sure whether I should replace the deprecated 
 *       ut_calendar/ut_inv_calendar routines with this routine, or
 *       replace cd_calendar/cd_inv_calendar, or create a new set of 
 *       routines entirely, like cc_calendar/cc_inv_calendar (I don't 
 *       like the "cc" solution because we already have too many of 
 *       these things).
 * 
 * For now, I decided to create new wrappers "ut_calendar_fix" and 
 * "ut_inv_calendar_fix", and I also created ut_convert_fix,
 * time_to_newtime_fix, and ut_string_fix to hold us over until we
 * make a decision about these routines.
 */

/*
    The CalCalcs routines, a set of C-language routines to perform
    calendar calculations.

    Version 1.2, released 8 June 2014

    Copyright (C) 2010-2014 David W. Pierce, dpierce@ucsd.edu

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

/* #define DEBUG  */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "udunits2.h"
#include "calcalcs.h"

#include "utCalendar2_cal_calcalcs.h"


char *ccs_err_str( int errno );
char *ccs_err_str( int errno );
char *ccs_err_str( int errno );
char *ccs_err_str( int errno );
char *ccs_err_str( int errno );

static int have_initted=0;
static calcalcs_cal *cal_std=NULL;
static ut_unit *udu_ref_date;

/* Following controls the rounding precision of the routines. I.e., if we end up with a value
   such as 59.99999999 seconds, we are going to round it to 60 seconds. The value is given
   in seconds, so, for example, 1.e-3 means round up to 1 second if the value is 0.999 seconds or greater,
   and 1.e-6 means round up to 1 second if the value is 0.999999 seconds or greater.
*/
static double sec_rounding_value = 1.e-6;

/* Internal to this file only */
static void 	initialize( const char *ut_read_xml_arg );
static void 	get_origin( const char *dataunits_str, ut_unit *dataunits, int *y0, int *mon0, int *d0, int *h0, int *min0, double *s0 );
static cv_converter *get_day_to_user_converter( ut_unit *uu, int y0, int mon0, int d0, int h0, int min0, double s0 );
static cv_converter *get_user_to_day_converter( ut_unit *uu, int y0, int mon0, int d0, int h0, int min0, double s0 );
static calcalcs_cal *getcal( const char *name );
static void unknown_cal_emit_warning( const char *calendar_name );
static int inferred_origin_year( const char *s );

/* Stores previuosly initialized calendars and their names */
static int		maxcals_known=100;
static int		ncals_known=0;
static calcalcs_cal	**known_cal;		/* ptr to array of calcals_cal ptrs */
static char		**known_cal_name;
static ut_system	*units_system=NULL;

/* Stores previously emitted "unknown calendar" warnings */
#define UTC2_MAX_UNKCAL_WARNS 	1000
static	char *unknown_cal_emitted_warning_for[ UTC2_MAX_UNKCAL_WARNS ];
static int n_unkcal=0;

/* Note on version 1.2: It is now required that the second argument to these routines
 * (utCalendar2_cal and utInvCalendar2_cal) be the actual units string rether then
 * the decoded ut_unit, since the user may have specified "year 0" as the origin date.
 * If that was done, then the udunits2 library internally changes that specified year
 * to "year 1" and there is no way of determining this from the returned ut_unit
 * structure, so that information is lost. Therefore, to keep that bit of information
 * it is necessary for these routines to recieve the actual units string that the
 * user specified rather than the pre-decoded ut_unit structure.
 */

/* 01/08/16 Mary Haley note: I renamed utCalendar2_cal to utCalendar2_cal_calcalcs to avoid
 * conflict with the original utCalendar2_cal routine that we are still using for
 * ut_calendar and ut_inv_calendar. */

/*========================================================================================
 * Turns the passed value into a Y/M/D date 
 */
int utCalendar2_cal_calcalcs( double val, const char *dataunits_str, int *year, int *month, int *day, int *hour,
                                int *minute, double *second, const char *calendar_name )
{

	int	jdnew, ndinc, ierr, iorig, iround;
	double	fdays, extra_seconds, tot_extra_seconds;
	int	ndays;
	ut_unit	*dataunits;
	calcalcs_cal	*cal2use;
char *fuckyou;

	/* Following vars are saved between invocations and reused if the
	 * passed units are the same as last time.
	 */
	static	ut_unit *prev_units=NULL;
	static	char *prev_dataunits_str=NULL;
	static	cv_converter *conv_user_units_to_days=NULL;
	static	int	y0, mon0, d0, h0, min0, jday0;
	static	double  s0, extra_seconds0;
	static	char *prev_calendar=NULL;

#ifdef DEBUG
	fprintf( stderr, "utCalendar2_cal_calcalcs: entering with val=%lf calendar_name=%s\n", val, calendar_name );
#endif

	if( (dataunits_str == NULL) || (strlen(dataunits_str) == 0)) {
		fprintf( stderr, "Error, utCalendar2 passed a NULL units string\n" );
		return( UT_ENOINIT );
		}

	if( have_initted == 0 ) {
#ifdef DEBUG
		fprintf( stderr, "utCalendar2_cal_calcalcs: initializing\n" );
#endif
		initialize( NULL ); /* this sets file-wide static var units_system */
		}

	/* Get the calendar we will be using, based on the passed name
	 */
#ifdef DEBUG
	fprintf( stderr, "utCalendar2_cal_calcalcs: getting calendar based on name\n" );
#endif
	cal2use = getcal( calendar_name );
	if( cal2use == NULL ) {
		unknown_cal_emit_warning( calendar_name );
		cal2use = getcal( "Standard" );
		}

	/* See if we are being passed the same units and calendar as last time.  If so,
	 * we can optimize by not recomputing all this junk 
	 */
	if( (prev_dataunits_str != NULL) && (prev_calendar != NULL) 
			&& (strcmp(prev_calendar, cal2use->name)==0) 
			&& (strcmp(prev_dataunits_str, dataunits_str)==0)) {
#ifdef DEBUG
		fprintf( stderr, "utCalendar2_cal_calcalcs: using calendar and units from saved last call\n" );
		fprintf( stderr, "utCalendar2_cal_calcalcs: saved origin day=%d/%d/%d %d:%d:%f\n", 
			y0, mon0, d0, h0, min0, s0 );
		fprintf( stderr, "utCalendar2_cal_calcalcs: saved extra_seconds0=%lf\n", extra_seconds0 );
		fprintf( stderr, "utCalendar2_cal_calcalcs: in saved calendar, saved origin date %d/%d/%d is saved julian day %d\n", 
			y0, mon0, d0, jday0 );
#endif
		/* Units and calendar are same as used last call */
		}
	else
		{
		/* Units/calendar combo are different from previously saved units, must redo calcs */
#ifdef DEBUG
		fprintf( stderr, "utCalendar2_cal_calcalcs: calendar and units are different from last call\n" );
#endif
		if( prev_dataunits_str != NULL ) {
			free( prev_dataunits_str );
			ut_free( prev_units );
			}

		if( prev_calendar != NULL )
			free( prev_calendar );

		/* We have a new units string, so must udunits-library parse it */
		if( (dataunits = ut_parse( units_system, dataunits_str, UT_ASCII )) == NULL ) {
			fprintf( stderr, "Error parsing units string \"%s\" -- are you sure it is a udunits2 library compatible time units specification?\n", dataunits_str );
			exit(-1);
			}

		/* Get origin day of the data units */
#ifdef DEBUG
		fprintf( stderr, "utCalendar2_cal_calcalcs: getting origin day of data units\n" );
#endif
		get_origin( dataunits_str, dataunits, &y0, &mon0, &d0, &h0, &min0, &s0 );	/* Note: static vars */
#ifdef DEBUG
		fprintf( stderr, "utCalendar2_cal_calcalcs: origin day for string >%s< is %d/%d/%d %d:%d:%f\n", 
			dataunits_str, y0, mon0, d0, h0, min0, s0 );
#endif

		/* Number of seconds into the specified origin day */
		extra_seconds0 = h0*3600.0 + min0*60.0 + s0;			/* Note: static vars */
#ifdef DEBUG
		fprintf( stderr, "utCalendar2_cal_calcalcs: extra_seconds0=%lf\n", extra_seconds0 );
#endif

		/* Convert the origin day to Julian Day number in the specified calendar */
#ifdef DEBUG
		fprintf( stderr, "utCalendar2_cal_calcalcs: convering origin day to julian day num in specified calendar\n" );
#endif
		if( (ierr = ccs_date2jday( cal2use, y0, mon0, d0, &jday0 )) != 0 ) {
fuckyou = (char *)ccs_err_str((int)ierr);
			/* fprintf( stderr, "Error in utCalendar2: %s\n", ccs_err_str(ierr) ); */
			fprintf( stderr, "Error in utCalendar2: %s\n", fuckyou );
			return( UT_EINVALID );
			}
#ifdef DEBUG
		fprintf( stderr, "utCalendar2_cal_calcalcs: in calendar, date %d/%d/%d is julian day %d\n", 
			y0, mon0, d0, jday0 );
#endif

		/* Get converter from user-specified units to "days" */
		if( conv_user_units_to_days != NULL ) 
			cv_free( conv_user_units_to_days );
		conv_user_units_to_days = get_user_to_day_converter( dataunits, y0, mon0, d0, h0, min0, s0 );

		/* Save these units so we can reuse our time-consuming
		 * calculations next time if they are the same units
		 */
#ifdef DEBUG
		fprintf( stderr, "utCalendar2_cal_calcalcs: saving units for future use\n" );
#endif
		prev_units = ut_clone( dataunits );
		if( ut_compare( prev_units, dataunits ) != 0 ) {
			fprintf( stderr, "error, internal error in udunits2 library found in routine utCalendar2: a clone of the user's units does not equal the original units!\n" );
			exit(-1);
			}

		prev_calendar = (char *)malloc( sizeof(char) * (strlen(cal2use->name)+1 ));
		strcpy( prev_calendar, cal2use->name );

		prev_dataunits_str = (char *)malloc( sizeof(char) * (strlen(dataunits_str)+1 ));
		strcpy( prev_dataunits_str, dataunits_str );
		}

	/* Convert user value of offset to floating point days */
#ifdef DEBUG
	fprintf( stderr, "utCalendar2_cal_calcalcs: converting user val=%lf to floating point days\n", val );
#endif
	fdays = cv_convert_double( conv_user_units_to_days, val );

	/* Get integer number of days and seconds past that */
	ndays = fdays;	
	extra_seconds = (fdays - ndays)*86400.0;
#ifdef DEBUG
	fprintf( stderr, "utCalendar2_cal_calcalcs: integer number of days & seconds past that; days=%d extra_seconds=%lf\n", ndays, extra_seconds );
#endif

	/* Get new Julian day */
	jdnew = jday0 + ndays;
#ifdef DEBUG
	fprintf( stderr, "utCalendar2_cal_calcalcs: new julian day=%d\n", jdnew );
#endif

	/* Handle the sub-day part */
#ifdef DEBUG
	fprintf( stderr, "utCalendar2_cal_calcalcs: handling sub-day part\n" );
#endif
	tot_extra_seconds = extra_seconds0 + extra_seconds;
	ndinc = tot_extra_seconds / 86400.0;
	jdnew += ndinc;
	tot_extra_seconds -= ndinc*86400.0;
	if( tot_extra_seconds < 0.0 ) {
		tot_extra_seconds += 86400.0;
		jdnew--;
		}

	/* Convert to a date */
#ifdef DEBUG
	fprintf( stderr, "utCalendar2_cal_calcalcs: converting to a date\n" );
#endif
	if( (ierr = ccs_jday2date( cal2use, jdnew, year, month, day )) != 0 ) {
		fprintf( stderr, "Error in utCalendar2: %s\n", ccs_err_str(ierr) );
		return( UT_EINVALID );
		}

	*hour = tot_extra_seconds / 3600.0;
	tot_extra_seconds -= *hour * 3600.0;
	*minute = tot_extra_seconds / 60.0;
	tot_extra_seconds -= *minute * 60.0;
	*second = tot_extra_seconds;

	/* Handle the rouding issues */
	iorig  = *second;			/* Integer conversion */
	iround = *second + sec_rounding_value;	
#ifdef DEBUG
	fprintf( stderr, "utCalendar2_cal_calcalcs: handling rounding issues; orig second=%lf  orig+rounding value:%lf\n", *second, *second + sec_rounding_value );
#endif
	if( iround > iorig ) {
#ifdef DEBUG
		printf( "rounding alg invoked, orig date: %04d-%02d-%02d %02d:%02d:%.20lf\n", *year, *month, *day, *hour, *minute, *second );
#endif
		*second = (double)iround;
		if( *second >= 60.0 ) {
			*second -= 60.0;
			*minute += 1.0;
			if( *minute >= 60.0 ) {
				*minute -= 60.0;
				*hour += 1.0;
				if( *hour >= 24.0 ) {
					*hour -= 24.0;
					if( (ierr = ccs_jday2date( cal2use, jdnew+1, year, month, day )) != 0 ) {
						fprintf( stderr, "Error in utCalendar2: %s\n", ccs_err_str(ierr) );
						return( UT_EINVALID );
						}
					}
				}
			}
#ifdef DEBUG
		printf( "after rounding alg here is new date: %04d-%02d-%02d %02d:%02d:%02.20lf\n", *year, *month, *day, *hour, *minute, *second );
#endif
		}

#ifdef DEBUG
	fprintf( stderr, "utCalendar2_cal_calcalcs: returning date %d/%d/%d %d:%d:%f\n", *year, *month, *day, *hour, *minute, *second  );
#endif
	return(0);
}

/*========================================================================================
 * Turn the passed Y/M/D date into a value in the user's units
 */
int utInvCalendar2_cal_calcalcs( int year, int month, int day, int hour, int minute, double second,
	const char *user_unit_str, double *value, const char *calendar_name )
{
	int	jday, ierr, diff_in_days;
	double	fdiff_in_days, val_days, val_partdays, fdiff_in_partdays, fpartday;
	calcalcs_cal *cal2use;
	ut_unit	*user_unit;

	/* Following vars are static and retained between invocations for efficiency */
	static	ut_unit *prev_units=NULL;
	static char 	*prev_user_unit_str=NULL;
	static int	y0, mon0, d0, h0, min0, jday0;
	static double	s0, fpartday0;
	static	cv_converter *conv_days_to_user_units=NULL;
	static char 	*prev_calendar=NULL;

#ifdef DEBUG
	fprintf( stderr, "utInvCalendar2_cal_calcalcs: entering with date=%d/%d/%d  calendar_name=%s\n", 
			year, month, day, calendar_name );
#endif

	if( have_initted == 0 ) {
#ifdef DEBUG
		fprintf( stderr, "utInvCalendar2_cal_calcalcs: calling initialize\n" );
#endif
		initialize( NULL );
		}

	/* Get the calendar we will be using, based on the passed name
	 */
#ifdef DEBUG
	fprintf( stderr, "utInvCalendar2_cal_calcalcs: getting calendar to use\n" );
#endif
	cal2use = getcal( calendar_name );
	if( cal2use == NULL ) {
		unknown_cal_emit_warning( calendar_name );
		cal2use = getcal( "Standard" );
		}

	if( (prev_user_unit_str != NULL) && (prev_calendar != NULL) 
			&& (strcmp(prev_calendar,cal2use->name)==0) 
			&& (strcmp( prev_user_unit_str, user_unit_str ) == 0)) {
		/* Units are same as used last call */
#ifdef DEBUG
		fprintf( stderr, "utInvCalendar2_cal_calcalcs: using previously stored units from last call\n" );
#endif
		}
	else
		{
#ifdef DEBUG
		fprintf( stderr, "utInvCalendar2_cal_calcalcs: new units, initting units structures\n" );
#endif
		if( prev_user_unit_str != NULL ) {
			free( prev_user_unit_str );
			ut_free( prev_units );
			}

		if( prev_calendar != NULL )
			free( prev_calendar );
		if( conv_days_to_user_units != NULL )
			cv_free( conv_days_to_user_units );

		/* We have a new units string, so must udunits-library parse it */
		if( (user_unit = ut_parse( units_system, user_unit_str, UT_ASCII )) == NULL ) {
			fprintf( stderr, "Error parsing units string \"%s\" -- are you sure it is a udunits2 library compatible time units specification?\n", user_unit_str );
			exit(-1);
			}

		/* Get origin day of the data units */
		get_origin( user_unit_str, user_unit, &y0, &mon0, &d0, &h0, &min0, &s0 );	/* Note: static vars */

		/* Convert the origin day to Julian Day number in the specified calendar */
#ifdef DEBUG
		fprintf( stderr, "utInvCalendar2_cal_calcalcs: convering origin day to Julian day in specified calendar\n" );
#endif
		if( (ierr = ccs_date2jday( cal2use, y0, mon0, d0, &jday0 )) != 0 ) {
			fprintf( stderr, "Error in utCalendar2: %s\n", ccs_err_str(ierr) );
			return( UT_EINVALID );
			}

		/* Get the origin's HMS in fractional (floating point) part of a Julian day */
		fpartday0 = (double)h0/24.0 + (double)min0/1440.0 + s0/86400.0;

		/* Get converter for turning days into user's units */
		conv_days_to_user_units = get_day_to_user_converter( user_unit, y0, mon0, d0, h0, min0, s0 );

		/* Save these units so we can reuse our time-consuming
		 * calculations next time if they are the same units
		 */
#ifdef DEBUG
		fprintf( stderr, "utInvCalendar2_cal_calcalcs: saving units\n" );
#endif
		prev_units = ut_clone( user_unit );
		if( ut_compare( prev_units, user_unit ) != 0 ) {
			fprintf( stderr, "error, internal error in udunits2 library found in routine utInvCalendar2: a clone of the user's units does not equal the original units!\n" );
			exit(-1);
			}

		prev_calendar = (char *)malloc( sizeof(char) * (strlen(cal2use->name)+1 ));
		strcpy( prev_calendar, cal2use->name );

		prev_user_unit_str = (char *)malloc( sizeof(char) * (strlen(user_unit_str)+1 ));
		strcpy( prev_user_unit_str, user_unit_str );
		}

	/* Turn passed date into a Julian day */
#ifdef DEBUG
	fprintf( stderr, "utInvCalendar2_cal_calcalcs: turning passed date into a julian day\n" );
#endif
	if( (ierr = ccs_date2jday( cal2use, year, month, day, &jday )) != 0 ) {
		fprintf( stderr, "Error in utInvCalendar2: %s\n", ccs_err_str(ierr) );
		return( UT_EINVALID );
		}

	/* jday and jday0 can be very large and nearly equal, so we difference
	 * them first to keep the precision high
	 */
	diff_in_days = jday - jday0;
	fdiff_in_days = (double)diff_in_days;

	/* Get the fractional (floating point) part of a Julian day difference
	 */
	fpartday = (double)hour/24.0 + (double)minute/1440.0 + second/86400.0;
	fdiff_in_partdays = fpartday - fpartday0;

	/* Convert days and partial days to user's units */
#ifdef DEBUG
	fprintf( stderr, "utInvCalendar2_cal_calcalcs: convering days and partial days to users units\n" );
#endif
	val_days     = cv_convert_double( conv_days_to_user_units, fdiff_in_days     );
	val_partdays = cv_convert_double( conv_days_to_user_units, fdiff_in_partdays );

	/* Hopefully this will minimize the roundoff errors */
	*value = val_days + val_partdays;

#ifdef DEBUG
	fprintf( stderr, "utInvCalendar2_cal_calcalcs: returning\n" );
#endif
	return(0);
}

/*==============================================================================================
 * Get a converter that turns the user's units into days 
 */
static cv_converter *get_user_to_day_converter( ut_unit *uu, int y0, int mon0, int d0, int h0, int min0, double s0 ) 
{
	char		daystr[1024];
	ut_unit 	*udu_days;
	cv_converter	*conv_user_units_to_days;

	snprintf( daystr, 1024, "days since %04d-%02d-%02d %02d:%02d:%f",
		y0, mon0, d0, h0, min0, s0 );
	daystr[1023] = '\0';
		
	udu_days = ut_parse( ut_get_system(uu), daystr, UT_ASCII );
	if( udu_days == NULL ) {
		fprintf( stderr, "internal error in utCalendar2/conv_to_days: failed to parse following string: \"%s\"\n",
			daystr );
		exit(-1);
		}
	conv_user_units_to_days = ut_get_converter( uu, udu_days );
	if( conv_user_units_to_days == NULL ) {
		fprintf( stderr, "internal error in utCalendar2/conv_to_days: cannot convert from \"%s\" to user units\n",
		 	daystr );
		exit(-1);
		}

	ut_free( udu_days );
	return( conv_user_units_to_days );
}

/*==============================================================================================
 * Get a converter that turns days into the user's units 
 */
static cv_converter *get_day_to_user_converter( ut_unit *uu, int y0, int mon0, int d0, int h0, int min0, double s0 ) 
{
	char		daystr[1024];
	ut_unit 	*udu_days;
	cv_converter	*conv_days_to_user_units;

	snprintf( daystr, 1024, "days since %04d-%02d-%02d %02d:%02d:%f",
		y0, mon0, d0, h0, min0, s0 );
	daystr[1023] = '\0';
		
	udu_days = ut_parse( ut_get_system(uu), daystr, UT_ASCII );
	if( udu_days == NULL ) {
		fprintf( stderr, "internal error in utCalendar2/conv_to_user_units: failed to parse following string: \"%s\"\n",
			daystr );
		exit(-1);
		}
	conv_days_to_user_units = ut_get_converter( udu_days, uu );
	if( conv_days_to_user_units == NULL ) {
		fprintf( stderr, "internal error in utCalendar2/conv_to_user_units: cannot convert from user units to \"%s\"\n",
		 	daystr );
		exit(-1);
		}

	free( udu_days );
	return( conv_days_to_user_units );
}

/*==========================================================================================
 * The user specified some origin to the time units. For example, if the units string
 * were "days since 2005-10-15", then the origin date is 2005-10-15.  This routine
 * deduces the specified origin date from the passed units structure 
 */
static void get_origin( const char *dataunits_str, ut_unit *dataunits, int *y0, int *mon0, int *d0, int *h0, int *min0, double *s0 )
{
	double	tval, tval_conv, resolution;
	cv_converter	*conv_user_date_to_ref_date;

	/* Get converter from these units to the library time units */
	conv_user_date_to_ref_date = ut_get_converter( dataunits, udu_ref_date );

	/* convert tval=0 to ref date */
	tval = 0.0;
	tval_conv = cv_convert_double( conv_user_date_to_ref_date, tval );

	/* Now decode the converted value */
	ut_decode_time( tval_conv, y0, mon0, d0, h0, min0, s0, &resolution );

	/* We MIGHT have a problem if the user-specified date is returned as "1". It might
	 * actually have been specified as "0", in which case the udunits library chagnes it
	 * to 1. Try to check for this situation.
	 */
	if( *y0 == 1 ) {
#ifdef DEBUG 
		printf( "Note: found year 1 as the origin of the user's unit string, but that might be a udunits library hack; actual string is >%s<\n",
			dataunits_str );
#endif
		if( inferred_origin_year( dataunits_str ) == 0 ) 
			*y0 = *y0 - 1;
		}

	cv_free( conv_user_date_to_ref_date );
}

/*========================================================================================*/
/* If in doubt, pass a NULL to this routine. That is what should almost always be done anyway 
 */
static void initialize( const char *ut_read_xml_arg )
{
	double	tval, rez, s0lib; 
	int	i, y0lib, mon0lib, d0lib, h0lib, min0lib;
	char	ustr[1024];

	/* Initialize the udunits library if needed */
	if( units_system == NULL ) {

		/* Turn annoying "override" errors off */
		ut_set_error_message_handler( ut_ignore );

		if( (units_system = ut_read_xml( ut_read_xml_arg )) == NULL ) {
			fprintf( stderr, "Error initializing udunits-2 unit system\n" );
			exit(-1);
			}

		/* Turn errors back on */
		ut_set_error_message_handler( ut_write_to_stderr );
		}

	/* Make space for our saved calendars */
	known_cal = (calcalcs_cal **)malloc( sizeof( calcalcs_cal * ) * maxcals_known );
	if( known_cal == NULL ) {
		fprintf( stderr, "Error in utCalendar2 routines, could not allocate internal storage\n" );
		exit(-1);
		}
	for( i=0; i<maxcals_known; i++ )
		known_cal[i] = NULL;
	known_cal_name = (char **)malloc( sizeof( char * ) * maxcals_known );
	if( known_cal_name == NULL ) {
		fprintf( stderr, "Error in utCalendar2 routines, could not allocate internal storage for calendar names\n" );
		exit(-1);
		}
	for( i=0; i<maxcals_known; i++ )
		known_cal_name[i] = NULL;

	/* Make a standard calendar */
	cal_std = ccs_init_calendar( "Standard" );

	/* Make a timestamp units that refers to the udunits2 library's intrinsic
	 * time origin
	 */
	tval = 0.0;
	ut_decode_time( tval, &y0lib, &mon0lib, &d0lib, &h0lib, &min0lib, &s0lib, &rez );
	snprintf( ustr, 1024, "seconds since %04d-%02d-%02d %02d:%02d:%f",
		y0lib, mon0lib, d0lib, h0lib, min0lib, s0lib );
	ustr[1023] = '\0';
	udu_ref_date = ut_parse( units_system, ustr, UT_ASCII );
	if( udu_ref_date == NULL ) {	
		fprintf( stderr, "internal error in routine utCalendar2/initialize: could not parse origin string \"%s\"\n",
			ustr );
		exit(-1);
		}

	have_initted = 1;	/* a global */
}

/*========================================================================================
 * We already have routine ccs_date2doy, so why do we need this one? It can be convenient
 * to supply this functionality to Fortran, which does not have the ability to easily
 * initialize or use calendars, and only identify them by a character-string name. So,
 * provide an interface that gives the ccs_date2doy functionality given only a character
 * string calendar name.
 */
int ccs_date2doy_calname( char *calendar_name, int year, int month, int day, int *doy )
{
	calcalcs_cal	*cal2use;

	cal2use = getcal( calendar_name );
	if( cal2use == NULL ) {
		unknown_cal_emit_warning( calendar_name );
		cal2use = getcal( "Standard" );
		}

	return( ccs_date2doy( cal2use, year, month, day, doy ));
}

/*========================================================================================
 * Returns NULL if the passed calendar name is both not found and not creatable
 */
static calcalcs_cal *getcal( const char *name )
{
	int	i, new_index;
	calcalcs_cal *new_cal;

	if( cal_std == NULL ) {
		fprintf( stderr, "Coding error in utCalendar2_cal_calcalcs routines, cal_std is null!\n" );
		exit(-1);
		}

	if( (name == NULL) || (strlen(name) == 0))
		return( cal_std );

	/* See if it is one of the previously known calendars */
	for( i=0; i<ncals_known; i++ ) {
		if( strcmp( known_cal_name[i], name ) == 0 ) 
			return( known_cal[i] );
		}

	/* If we get here, the cal is not known, so create it if possible */
	new_cal = ccs_init_calendar( name );
	if( new_cal == NULL ) 
		return( NULL );		/* unknown name */

	/* We now know a new calendar */
	new_index = ncals_known;
	ncals_known++;

	/* new_index is where we will be storing the new calendar */
	if( ncals_known > maxcals_known ) {
		ncals_known = maxcals_known;
		new_index = strlen(name);	/* some arbitrary value */
		if( new_index >= maxcals_known )
			new_index = 10;
		}

	/* If there was already a calendar stored in this slot 
	 * (because we might be reusing slots) then clear it out
	 */
	if( known_cal[new_index] != NULL ) 
		ccs_free_calendar( known_cal[new_index] );
	
	if( known_cal_name[new_index] != NULL ) 
		free( known_cal_name[new_index] );

	known_cal[new_index] = new_cal;
	known_cal_name[new_index] = (char *)malloc( sizeof(char) * (strlen(name)+1 ));
	strcpy( known_cal_name[new_index], name );

	return( new_cal );
}

/*=============================================================================================
 * Given a string such as "days since xxxx-xx-xx", this tries to return the year in the
 * specified date string
 */
static int inferred_origin_year( const char *s ) 
{
	int		sl, i, loc_since_start, ifnbss, idash, nyd, retval;
	char		*year_digits;

	if( (s == NULL) || (strlen(s) < 6)) 	/* have to have at least room for since and a year */
		return( -9999 );

	/* Find the location of the first non-blank after "since" */
	sl = strlen(s);
/* printf( "string: >%s< sl:%d\n", s, sl ); */
	loc_since_start = -1;
	for( i=0; i<(sl-4); i++ ) {
		if( strncasecmp( s+i, "since", 5 ) == 0 ) 
			loc_since_start = i;
		}
	if( loc_since_start == -1 )
		return( -9999 );
/* printf( "loc that string 'since' starts: %d\n", loc_since_start ); */

	ifnbss = loc_since_start + 5;	/* ifnbss = "i first non blank since since" */
	while( isblank( *(s+ifnbss) ) && (*(s+ifnbss) != '\0') && (ifnbss < sl))
		ifnbss++;

/* printf( "i first non blank since since:%d\n", ifnbss ); */

	/* Ran out of string before finding a non-blank char */
	if( (ifnbss >= sl) || (*(s+ifnbss) == '\0') )
		return( -9999 );

	/* Now advance to first dash sign "-" */
	idash = ifnbss + 1;
	while( (idash <= sl) && (*(s+idash) != '-') && (*(s+idash) != '\0'))
		idash++;

/* printf( "i first dash sign: %d\n", idash ); */

	if( *(s+idash) != '-' )
		return( -9999 );

	/* Make a copy of just the year digits */
	nyd = idash - ifnbss;		/* number of digits in the year string */
	year_digits = (char *)malloc( sizeof(char) * nyd );
	strncpy( year_digits, s+ifnbss, nyd );

/* printf( "YEAR DIGITS: >%s<\n", year_digits ); */

	if( sscanf( year_digits, "%d", &retval ) != 1 ) 
		return( -9999 );

	return( retval );
}

/*=============================================================================================
 */
static void unknown_cal_emit_warning( const char *calendar_name )
{
	int	i;

	for( i=0; i<n_unkcal; i++ ) {
		if( strcmp( calendar_name, unknown_cal_emitted_warning_for[i] ) == 0 ) 
			/* Already emitted a warning for this calendar */
			return;
		}

	if( n_unkcal == UTC2_MAX_UNKCAL_WARNS )
		/* Too many warnings already given, give up */
		return;

	/* OK, emit the warning now */
	fprintf( stderr, "Error in utCalendar2_cal_calcalcs/utInvCalendar2_cal_calcalcs: unknown calendar \"%s\".  Using Standard calendar instead\n",
		calendar_name );

	/* Save the fact that we have warned for this calendar */
	unknown_cal_emitted_warning_for[ n_unkcal ] = (char *)malloc( sizeof(char) * (strlen(calendar_name)+1 ));
	if( unknown_cal_emitted_warning_for[ n_unkcal ] == NULL ) {
		fprintf( stderr, "Error in utCalendar2_cal_calcalcs: could not allocate internal memory to store string \"%s\"\n",
			calendar_name );
		return;
		}

	strcpy( unknown_cal_emitted_warning_for[ n_unkcal ], calendar_name );
	n_unkcal++;
}


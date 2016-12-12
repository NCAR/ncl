/*
    The CalCalcs routines, a set of C-language routines to perform
    calendar calculations.

    Version 1.2, released 16 June 2014

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

#ifdef __cplusplus
extern "C" {
#endif

#define CALCALCS_VERSION_NUMBER	1.0

struct cccalendar {
	int	sig;
	char	*name;
	int	ndays_reg, ndays_leap;

	int	(*c_isleap)	( int, int * );
	int	(*c_date2jday) 	( int, int, int, int * );
	int	(*c_jday2date) 	( int, int *, int *, int * );
	int	(*c_dpm)   	( int, int, int * );

	/* The following implement "mixed" calendars, for example, our standard
	 * civil calendar, which converts from Julian to Gregorian, with the
	 * last day of the Julian calendar being 4 Oct 1582 and the first day
	 * of the Gregorian calendar being 15 Oct 1582
	 */
	int	mixed;
	struct cccalendar *early_cal, *late_cal;
	int	year_x, month_x, day_x;		/* These are the transition Y,M,D, i.e., the FIRST DATE the LATER CAL is used */
	int	year_px, month_px, day_px;	/* These are the DAY BEFORE the transition Y,M,D, i.e., the last date the earlier cal is used */
	int	jday_x;				/* Julian day of the transition date */
};

/* A "country code", which holds the 2-letter code (abbreviation) for the country or region,
 * its long name, and the Y/M/D date that it transitioned from the Julian to Gregorian calendar
 */
struct ccs_ccode {
	char	*code, *longname;
	int	year, month, day;
};

typedef struct cccalendar calcalcs_cal;
typedef struct ccs_ccode  ccs_country_code;

/* =====================================================================================
 * Here are all the services this library supplies 
 *
 * -------------------------------------------------------------------------
 * ccs_init_calendar : initialize a calendar.  Valid passed arguments are
 * 		one of the following strings:
 *	Standard, proleptic_Gregorian, proleptic_Julian, noleap (aka 365_day and no_leap), 360_day		
 *
 * The "Standard" calendar transitions from the Julian to the Gregorian calendar, 
 * with the last day of the Julian calender being 1582-10-04 and the next day being
 * the first day of the Gregorian calendar, with the date 1582-10-15.  This "transition date"
 * can be set to be the value used by various countries, or set to any arbitrary
 * date, using routine "set_cal_xition_date", below.
 */
calcalcs_cal *ccs_init_calendar( const char *calname );

/*------------------------------------------------------------------------------
 * Frees the storage previously allocated by ccs_init_calendar()
 */
void ccs_free_calendar( calcalcs_cal *calendar );

/*--------------------------------------------------------------------------
 * ccs_date2jday: turn a Y/M/D date into a (true) Julian day number.  Note that 
 * 	      a Julian day is not the day number of the year, but rather
 *	      the integer day number starting from 1 on the day that would
 *            have been 1 Jan 4713 BC if the Julian calendar went back
 *	      to that time.
 */
int ccs_date2jday( calcalcs_cal *calendar, int year, int month, int day, int *jday );

/*--------------------------------------------------------------------------
 * ccs_jday2date: turn a (true) Julian day number into a calendar date.
 */
int ccs_jday2date( calcalcs_cal *calendar, int jday, int *year, int *month, int *day );

/*--------------------------------------------------------------------------
 * ccs_isleap: determine if the specified year is a leap year in 
 * 	the specified calendar
 */
int ccs_isleap( calcalcs_cal *calendar, int year, int *leap );

/*--------------------------------------------------------------------------
 * ccs_dpm: returns the days per month for the given year/month.
 * Note that during the month that transitions from a Julian to a 
 * Gregorian calendar, this might be a strange number of days.
 */
int ccs_dpm( calcalcs_cal *calendar, int year, int month, int *dpm );

/*--------------------------------------------------------------------------
 * ccs_date2doy: given a Y/M/D date, calculates the day number of the year, starting at 1 for
 *	January 1st.
 */
int ccs_date2doy( calcalcs_cal *calendar, int year, int month, int day, int *doy );

/*--------------------------------------------------------------------------
 *  ccs_doy2date: given a year and a day number in that year (with counting starting at 1 for
 *	Jan 1st), this returns the month and day of the month that the doy refers to.
 */
int ccs_doy2date( calcalcs_cal *calendar, int year, int doy, int *month, int *day );

/*--------------------------------------------------------------------------
 *  ccs_dayssince: Given a Y/M/D date in a specified calendar, and the number of days since
 *      that date, this returns the new Y/M/D date in a (possibly different) calendar.
 *
 * Note that specifying "zero" days since, and giving different calendars as the original
 *      and new calendars, essentially converts dates between calendars.
 */
int ccs_dayssince( calcalcs_cal *calendar_orig, int year_orig, int month_orig, int day_orig,
                int ndays_since, calcalcs_cal *calendar_new, int *year_new, int *month_new, int *day_new );

/*--------------------------------------------------------------------------
 * get/set_cal_xition_date: these routines set the transition date for a Standard
 * calendar, which is the date that the Gregorian calendar was first used.
 * Before that, it is assumed that the Julian calendar was used.
 *
 * Historically, this transition date varies by country and region.  The
 * variation can be extreme, and over the centuries country boundaries have
 * changed, so this should be considered only the grossest approximation. For 
 * that matter, for several countries, different districts/regions converted
 * at different times anyway, so actually doing a good job at this task is
 * far beyond this library's capability.  Nevertheless, this does give some
 * basic simplified capabilities in this regard.  And you can always set
 * the routines to use an arbitrary transition date of your own calculation.
 *
 * How to use these routines:
 *   
 * If you know the transition date you want to impose, simply call
 * set_cal_xition_date with the specified transition date.  The date
 * specified is the FIRST date that the new (Gregorian) calendar was
 * used.  For exaple, in Italy, the Gregorian calendar was first used
 * on 15 October 1582.
 *
 * If you don't know what transition date to use, there is a brief
 * database with some APPROXIMATE dates of transition that can be accessed
 * by calling get_cal_xition_date with a two-letter country code, corresponding
 * to the internet suffix for the country.  (As a special case, "US" is used
 * for the United States of America.)  If the routine returns 0, then the
 * country code is recognized and the approximate transition date is returned
 * in the passed parameters year, month, day.  These should then be given to
 * routine set_cal_xition_date to make that calendar use the specified 
 * transition date.  If the get_cal_xition_date routine does not return 0,
 * then there is no information on that country in the database.
 * 
 * routine set_cal_xition_date returns 0 on success, and something other than
 * 0 on an error.  Errors include trying to set the transition date to an
 * invalid date, or trying to set the transition date for any calendar
 * other than the "Standard" calendar.
 *
 * The following country/region codes are recognized: AK (Alaska) 1867/10/18;  
 * AL (Albania) 1912/12/1; AT (Austria) 1583/10/16; BE (Belgium) 1582/12/25;
 * BG (Bulgaria) 1916/4/1; CN (China) 1929/1/1; CZ (Czechoslovakia) 1584/1/17;
 * DK (Denmark) 1700/3/1; NO (Norway) 1700/3/1; EG (Egypt) 1875/1/1; 
 * EE (Estonia) 1918/1/1; FI (Finland) 1753/3/1; FR (France) 1582/12/20;
 * DE (Germany, note different states actually used different dates between
 * 1583 and 1700!) 1583/11/22; UK (Great Britain and Dominions) 1752/9/14;
 * GR (Greece) 1924/3/23; HU (Hungary) 1587/11/1; IT (Italy) 1582/10/15;
 * JP (Japan) 1918/1/1; LV (Latvia) 1915/1/1; LT (Lithuania) 1915/1/1;
 * LU (Luxemburg) 1582/12/15; NL (Netherlands, note Catholic regions
 * transitioned in various dates of 1582/83, while Protestant regions
 * transitioned in various dates of 1700/01) 1582/10/15; NO (Norway) 1700/3/1;
 * PL (Poland) 1582/10/15; PT (Portugal) 1582/10/15; RO (Romania) 1919/4/14;
 * ES (Spain) 1582/10/15; SE (Sweden) 1753/3/1; CH (Switzerland, note,
 * varied bewteen 1584 and 1701 by Canton) 1584/1/22; TR (Turkey)
 * 1927/1/1; YU (Yugoslavia) 1919/1/1; UK (Great Britain and Dominions) 1752/9/14;
 * US (United States) 1752/9/14; SU (former Soviet Union) 1918/2/1; 
 * RU (Russia) 1918/2/1.
 */
int ccs_set_xition_date( calcalcs_cal *calendar, int year, int month, int day );
int ccs_get_xition_date( const char *country_code, int *year, int *month, int *day );

/*--------------------------------------------------------------------------
 * calcalcs_err_str: return a static char * to an error string, given the error nmmber
 */
char *ccs_err_str( int errno );

#define CALCALCS_ERR_NO_YEAR_ZERO		-10
#define CALCALCS_ERR_DATE_NOT_IN_CALENDAR	-11
#define CALCALCS_ERR_INVALID_DAY_OF_YEAR	-12
#define CALCALCS_ERR_NOT_A_MIXED_CALENDAR	-13
#define CALCALCS_ERR_UNKNOWN_COUNTRY_CODE	-14
#define CALCALCS_ERR_OUT_OF_RANGE		-15
#define CALCALCS_ERR_NULL_CALENDAR		-16
#define CALCALCS_ERR_INVALID_CALENDAR		-17

#ifdef __cplusplus
}
#endif

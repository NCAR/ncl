/*
    The CalCalcs routines, a set of C-language routines to perform
    calendar calculations.

    Version 1.0, released 7 January 2010

    Copyright (C) 2010 David W. Pierce, dpierce@ucsd.edu

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

#define UT_ENOINIT -10
#define UT_EINVALID -11

#ifdef __cplusplus
extern "C" {
#endif

int utCalendar2_cal_calcalcs( double val, const char *dataunits_str, int *year, int *month, int *day, int *hour, 
	int *minute, double *second, const char *calendar_name );
int utInvCalendar2_cal_calcalcs( int year, int month, int day, int hour, int minute, double second,
        const char *user_unit_str, double *value, const char *calendar_name );

#ifdef __cplusplus
}
#endif


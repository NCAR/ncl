int utCalendar2( double val, ut_unit *dataunits, int *year, int *month, int *day, int *hour, 
				int *minute, double *second );

int utCalendar2_cal( double val, ut_unit *dataunits, int *year, int *month, int *day, int *hour, 
				int *minute, double *second, const char *calendar );

int utInvCalendar2( int year, int month, int day, int hour, int minute,
                double second, ut_unit *unit, double *value );

int utInvCalendar2_cal( int year, int month, int day, int hour, int minute,
                double second, ut_unit *unit, double *value, const char *calendar );


#define UT_EINVALID     -5      /* invalid unit-structure */
#define UT_ENOINIT      -6      /* package not initialized */


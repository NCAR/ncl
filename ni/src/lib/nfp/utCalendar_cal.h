/* 
This version of the utCalendar_cal routines is based on code from ncview version 1.93f
http://meteora.ucsd.edu/~pierce/ncview
*/

int utCalendar_cal( double val, utUnit *dataunits, int *year, int *month,
		    int *day, int *hour, int *minute, float *second, 
		    char *calendar );

int utInvCalendar_cal( int year, int month, int day, int hour, int minute, 
		       double second, utUnit *unit, double *value, 
		       const char *calendar );

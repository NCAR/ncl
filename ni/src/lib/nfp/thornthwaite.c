/* Computation of the reference potential evapotranspiration
// following the method of Thornthwaite
*/
/* Function prototypes */
#include "spei.h"

/* thornthwaite(tempSeries)
// Calculates the potential evapotranspiration from a series of monthly
// temperature data, following the method of Thornthwaite.
*/
void thornthwaite(double *tempSeries, int n, double tmsg, double lat, double *etpSeries){

	double T, J, J2, J3, c, N, omega, K[13], tanLatMonth;
	int month, i, k;

	double pi = 3.14159265358979;
	/* Tangens of the average solar declination angle for each month of the year */
	double tanLat = tan(0.0174532925*lat);
	double tanDelta[13] = {-0.37012566,-0.23853358,-0.04679872,0.16321764,
		0.32930908,0.40677729,0.3747741,0.239063,
		0.04044485,-0.16905776,-0.33306377,-0.40743608};
	double days[13] = {31,28,31,30,31,30,31,31,30,31,30,31};
	/*double julian[13] = {16,45.5,75,105.5,136,166.5,197,228,258.5,289,319.5,350}; */

        /* Initialize to missing */
	for (i=0; i<n; i++) etpSeries[i] = tmsg;

	/* Compute J: annual temperature efficiency index */
	J = 0;
	for (month=1; month<13; month++) {
		T = k = 0;
		for (i=month-1; i<n; i+=12) {
		  if (tempSeries[i] != tmsg) {
		    T = T + tempSeries[i];
		    k++;
		  }
		}
		/* T: monthly average temperature */
		T = T/k;
		if (T>0) J = J + pow(T/5,1.514);
		/*printf("\nTemp. efficiency index (J) = %.4f", J); */
	}
	/* if J=0 ... nothing else matters */
	if (J == 0.0)  {
	  for (i=0; i<n; i++) {
	    if(tempSeries[i] != tmsg) etpSeries[i] = 0.0;
	  }
	  return;
	}

	/*printf("\nAnnual temp. efficiency index (J) = %.4f", J); */
	/* Compute c exponent */
	J2 = J*J;
	J3 = J2*J;
	c = 0.000000675*J3 - 0.0000771*J2 + 0.01792*J + 0.49239;
	/*printf("\nEmpirical exponent (c) = %.4f", c); */
	/* Compute K: monthly correction factor, depending on latitude */
	/*printf("\nMonthly correction factor (K) = "); */
	for (month=1; month<13; month++) {
		tanLatMonth = tanLat*tanDelta[month-1];
		if (tanLatMonth>-1 && tanLatMonth<1) omega = acos(-tanLatMonth);
		/* high latitudes: acos not defined for <-1 or >1 */
		if (tanLatMonth<=-1) omega = 0;
		if (tanLatMonth>=1) omega = pi;
		N = 24*omega/pi; /* monthly average number of sun hours */
		/*printf("\nMonthly average sun hours (N) = %.4f", N); */
		K[month] = N/12 * days[month-1]/30;
	}
	/* Compute potential evapotranspiration series */
	month = 1;
	for (i=0; i<n; i++) {
	  if(tempSeries[i] != tmsg) {
		if (tempSeries[i]>0 && J>0) etpSeries[i] = K[month] * 16 * pow((10*tempSeries[i]/J),c);
		else etpSeries[i] = 0;
	  }
	  if (month<12) month++;
	  else month = 1;
	}
}

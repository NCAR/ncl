#include <stdio.h>
#include <math.h>
#include <stdlib.h>

// Max size of raw rainfall and events matrices
#define NUMDATOSMAX 5000
#define NUMRESULTMAX 5000
#define NUMSEASONSMAX 12

// Function prototypes:  'n' is length of 'dataSeries' 
void spei(float dataSeries[], int n, int seasons, float speiSeries[]);
}

// spei()
// Calculates the Standardized Precipitation-Evapotransporation Index
// from a series of climatic balance (precipitation minus etp). The
// SPEI is the standardized value of the climatic balance (P-ETP),
// computed following a Log Logistic probability distribution.
void spei(float dataSeries[], int n, int seasons, float speiSeries[]) {

	int i, j, k, nSeason;
	float seasonSeries[NUMDATOSMAX], beta[3], logLogisticParams[NUMSEASONSMAX][3];

	// Loop through all seasons defined by seasons
	for (j=1; j<=seasons; j++) {
		// Extract and sort the seasonal series
		k = 0;
		for (i=j-1; i<n; i+=seasons) {
			seasonSeries[k] = dataSeries[i];
			k++;
		}
		nSeason = k;
		upward(seasonSeries, nSeason);
		// Compute probability weighted moments
		//pwm(seasonSeries, nSeason, beta, -0.35, 0, 0);
		pwm(seasonSeries, nSeason, beta, 0, 0, 0);
		// Fit a Log Logistic probability function
		logLogisticFit(beta, logLogisticParams[j]);
		// Calculate the standardized values
		for (i=j-1; i<n; i+=seasons) {
			speiSeries[i] = logLogisticCDF(dataSeries[i], logLogisticParams[j]);
			speiSeries[i] = -standardGaussianInvCDF(speiSeries[i]);
		}
	}
}

// A bunch of auxiliary functions for computing L-moments,
// probability distribution functions, etc.

// upward()
// Sorts a given data series from the lowest to the highest value
void upward(float series[], int n) {

	int i, j;
	double temp;

	for (i=0; i<n-1; i++) {
		for (j=0; j<n-1-i; j++) {
			if (series[j+1] < series[j]) {
				temp = series[j];
				series[j] = series[j+1];
				series[j+1] = temp;
			}
		}
	}

// logLogisticFit()
// Estimates the parameters of a Gamma distribution functions
void logLogisticFit(float beta[], float logLogisticParams[]) {

        float g1, g2;

        // estimate gamma parameter
        logLogisticParams[2] = (2*beta[1]-beta[0]) / (6*beta[1]-beta[0]-6*beta[2]);
        g1 = exp(gammaLn(1+1/logLogisticParams[2]));
        g2 = exp(gammaLn(1-1/logLogisticParams[2]));
        // estimate alpha parameter
        logLogisticParams[1] = (beta[0]-2*beta[1])*logLogisticParams[2] / (g1*g2);
        // estimate beta parameter
        logLogisticParams[0] = beta[0] - logLogisticParams[1]*g1*g2;
}

// factorial()
// compute the factorial of an integer
long int factorial(int anInteger) {
        if (anInteger<=1)
                return(1);
        else
                anInteger = anInteger * factorial(anInteger-1);
        return(anInteger);
}

// gammaLn()
// Returns the natural logarithm of the gamma function, ln[gamma(xx)] for xx > 0
float gammaLn(float xx) {

        double x, y, tmp, ser;
        static double cof[6]={76.18009172947146,-86.50532032941677,
                24.01409824083091,-1.231739572450155,0.1208650973866179e-2,
                -0.5395239384953e-5};
        int j;

        y = x = xx;
        tmp = x + 5.5;
        tmp -= (x+0.5) * log(tmp);
        ser = 1.000000000190015;
        for (j=0; j<=5; j++) ser += cof[j]/++y;
        return -tmp+log(2.5066282746310005*ser/x);
}

/*
// A bunch of auxiliary functions for computing L-moments,
// probability distribution functions, etc.
*/
/* Function prototypes */
#include "spei.h"

/*
// factorial()
// compute the factorial of an integer
*/
long int factorial(int anInteger) {
	if (anInteger<=1)
		return(1);
	else
		anInteger = anInteger * factorial(anInteger-1);
	return(anInteger);
}
/*
// gammaLn()
// Returns the natural logarithm of the gamma function, ln[gamma(xx)] for xx > 0
*/
double gammaLn(double xx) {

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

/*
// upward()
// Sorts a given data series from the lowest to the highest value
*/
void upward(double *series, int n) {

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
}


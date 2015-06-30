/*
// Functions for computing probability distribution
// functions commonly used in extreme value analysis
*/
/* Function prototypes */
#include "spei.h"

/*
// gammaFit() function:
// Gives the parameters of a Gamma distribution function
// fitted to a data series through its L-moments.
*/
void gammaFit(double *L, double *gammaParams) {

	double lMomentRatio, pi, t, t2, t3;

	pi = 3.14159265358979;
	lMomentRatio = L[2] / L[1]; /* second l-Moment ratio */
	/*	printf("\nL-moment ratio = %.4f", lMomentRatio); */
	if (lMomentRatio > 0.0 && lMomentRatio < 0.5) {
			t = pi * lMomentRatio*lMomentRatio;
			t2 = t*t;
			t3 = t2*t;
			gammaParams[1] = (1 - 0.308*t) / (t - 0.05812*t2 + 0.01765*t3);
			gammaParams[0] = L[1] / gammaParams[1];
	}
	else if (lMomentRatio >= 0.5 && lMomentRatio < 1) {
			t = 1 - lMomentRatio;
			t2 = t*t;
			gammaParams[1] = (0.7213*t - 0.5947*t2) / (1 - 2.1817*t + 1.2113*t2);
			gammaParams[0] = L[2] / gammaParams[1];
	}
	/*
	printf("\nGamma distribution alfa param.: %.4f\n", gammaParams[0]);
	printf("Gamma distribution beta param.: %.4f\n", gammaParams[1]);
	*/
}
/*
// gammaStandardize() function:
// standardize a single value following a Gamma distribution
*/
double gammaStandardize(double value, double *params) {

	double alfa, beta, y, z;

	alfa = params[0];
	beta = params[1];
	y = value / alfa / beta;
	z = (pow(y,0.333333333333333) + 1/(9*beta) - 1) * pow(9*beta, 0.5);

	return(z);
}

/*
// pearsonIIIFit() function:
// Gives the parameters of a Pearson III distribution function
// fitted to a data series through its L-moments.
// pearsonIIIParams[0] = origin
// pearsonIIIParams[1] = scale (alpha)
// pearsonIIIParams[2] = shape (beta)
*/
void pearsonIIIFit(double L[], double pearsonIIIParams[]) {

	double lMomentRatio, pi, sqrtPi, t, t2, t3;

	pi = 3.14159265358979;
	sqrtPi = sqrt(pi);
	lMomentRatio = L[3] / L[2]; /* third l-Moment ratio */
	/*	printf("L-moment ratio = %.4f\n", lMomentRatio); */
	if (lMomentRatio > 0.0 && lMomentRatio < 0.333333) {
			t = 3*pi * lMomentRatio*lMomentRatio;
			t2 = t*t;
			t3 = t2*t;
			pearsonIIIParams[2] = (1 + 0.2906*t) / (t + 0.1882*t2 + 0.0442*t3);
	}
	else if (lMomentRatio >= 0.333333 && lMomentRatio < 1) {
			t = 1 - lMomentRatio;
			t2 = t*t;
			t3 = t2*t;
			pearsonIIIParams[2] = (0.36067*t - 0.5967*t2 + 0.25361*t3) /
				(1 - 2.78861*t + 2.56096*t2 - 0.77045*t3);
	}
    pearsonIIIParams[1] = sqrtPi * L[2] *
		exp(gammaLn(pearsonIIIParams[2]) - gammaLn(pearsonIIIParams[2]+0.5));
    pearsonIIIParams[0] = L[1] - pearsonIIIParams[1] * pearsonIIIParams[2];
}

/*
// pearsonIIIStandardize() function:
// standardize a single value following a Pearson III distribution
*/
double pearsonIIIStandardize(double value, double params[]) {
	double alfa, beta, origin, y, z;

	origin = params[0]; /* location */
	alfa = params[1];   /* shape */
	beta = params[2];   /* scale */

	if (value>=origin) {
        y = (value-origin) / alfa;
        z = (pow(y/beta,0.333333333333333) + 1/(9*beta) - 1) * pow(9*beta,0.5);
	}
    else z = -3.09023230616779;

	return(z);
}

/*
// logLogisticFit()
// Estimates the parameters of a Gamma distribution functions
*/
void logLogisticFit(double beta[], double logLogisticParams[]) {

	double g1, g2;

	/* estimate gamma parameter */
	logLogisticParams[2] = (2*beta[1]-beta[0]) / (6*beta[1]-beta[0]-6*beta[2]);
	g1 = exp(gammaLn(1+1/logLogisticParams[2]));
	g2 = exp(gammaLn(1-1/logLogisticParams[2]));
	/* estimate alpha parameter */
	logLogisticParams[1] = (beta[0]-2*beta[1])*logLogisticParams[2] / (g1*g2);
	/* estimate beta parameter */
	logLogisticParams[0] = beta[0] - logLogisticParams[1]*g1*g2;
}

/*
// logLogisticCDF()
// Gives the cumulative distribution function of 'value', following a LogLogistic distribution
*/
double logLogisticCDF(double value, double params[]) {
	return 1 / (1+pow((params[1]/(value-params[0])),params[2]));
}

/*
// gaussianInvCDF()
// Gives the inverse cumulative distribution function following a standard Gaussian
// distribution. I.e., the function gives the standardized value (Z) corresponding to
// a given probability (0<prob<1) following a standard Gaussian distribution (mean=0
// and sd=1).
*/
double standardGaussianInvCDF(double prob) {

	static double C[3] = {2.515517,0.802853,0.010328};
	static double d[4] = {0,1.432788,0.189269,0.001308};
	double W, WW, WWW, resul;

	if (prob<=0.5) W = sqrt(-2*log(prob));
	else W =sqrt(-2*log(1-prob));
	WW = W*W;
	WWW = WW*W;
	resul = W - (C[0] + C[1]*W + C[2]*WW) / (1 + d[1]*W + d[2]*WW + d[3]*WWW);
	if (prob>0.5) resul = -resul;
	return resul;
}

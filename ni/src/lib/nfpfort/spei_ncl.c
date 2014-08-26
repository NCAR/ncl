#include <stdio.h>
#include <math.h>
#include <stdlib.h>

/* Max size of raw rainfall and events matrices */
#define NUMDATOSMAX 5000
#define NUMRESULTMAX 5000
#define NUMSEASONSMAX 12

/* Function prototypes:  'n' is length of 'dataSeries'  */
extern void spei(float*,int,int, float*);
extern void upward(float*,int);
extern float gammaLn(float);


/* From pdfs.c */
extern void gammaFit(float L[], float gammaParams[]);
extern float gammaStandardize(float value, float params[]);
extern void pearsonIIIFit(float L[], float pearsonIIIParams[]);
extern float pearsonIIIStandardize(float value, float params[]);
extern void logLogisticFit(float pwm[], float logLogisticParams[]);
extern float logLogisticCDF(float value, float params[]);
extern float standardGaussianInvCDF(float prob);

/* From lmoments.c */
extern void pwm(float series[], int n, float beta[], float A, float B, int isBeta);
extern void lMoments(float series[], int n, float lMoment[], float A, float B);

/*
 * spei()
 * Calculates the Standardized Precipitation-Evapotransporation Index
 * from a series of climatic balance (precipitation minus etp). The
 * SPEI is the standardized value of the climatic balance (P-ETP),
 * computed following a Log Logistic probability distribution.
 */
void spei(float dataSeries[], int n, int seasons, float speiSeries[]) {

	int i, j, k, nSeason;
	float seasonSeries[NUMDATOSMAX], beta[3], logLogisticParams[NUMSEASONSMAX][3];

	/* Loop through all seasons defined by seasons */
	for (j=1; j<=seasons; j++) {
  	  /* Extract and sort the seasonal series */
		k = 0;
		for (i=j-1; i<n; i+=seasons) {
			seasonSeries[k] = dataSeries[i];
			k++;
		}
		nSeason = k;
		upward(seasonSeries, nSeason);
		/* Compute probability weighted moments */
		/*pwm(seasonSeries, nSeason, beta, -0.35, 0, 0); */
		pwm(seasonSeries, nSeason, beta, 0, 0, 0);
		/* Fit a Log Logistic probability function */
		logLogisticFit(beta, logLogisticParams[j]);
		/* Calculate the standardized values */
		for (i=j-1; i<n; i+=seasons) {
			speiSeries[i] = logLogisticCDF(dataSeries[i], logLogisticParams[j]);
			speiSeries[i] = -standardGaussianInvCDF(speiSeries[i]);
		}
	}
}

/*
 * A bunch of auxiliary functions for computing L-moments,
 * probability distribution functions, etc.
 *
 * upward()
 * Sorts a given data series from the lowest to the highest value
 */
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
}

/*
 * factorial()
 * compute the factorial of an integer
 */
long int factorial(int anInteger) {
        if (anInteger<=1)
                return(1);
        else
                anInteger = anInteger * factorial(anInteger-1);
        return(anInteger);
}

/*
 * gammaLn()
 * Returns the natural logarithm of the gamma function, ln[gamma(xx)] for xx > 0
 */
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


/* Functions for computing probability distribution */
/* functions commonly used in extreme value analysis */

/*
 * Functions from pdfs.c for computing probability distribution
 * functions commonly used in extreme value analysis
 *
 * Function prototypes
 */

/*
 * gammaFit() function:
 * Gives the parameters of a Gamma distribution function
 * fitted to a data series through its L-moments.
 */
void gammaFit(float L[], float gammaParams[]) {

  float lMomentRatio, pi, t, t2, t3;

  pi = 3.14159265358979;
  lMomentRatio = L[2] / L[1]; /* second l-Moment ratio */
  /*printf("\nL-moment ratio = %.4f", lMomentRatio); */
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
  /*printf("\nGamma distribution alfa param.: %.4f\n", gammaParams[0]); */
  /*printf("Gamma distribution beta param.: %.4f\n", gammaParams[1]); */
}

/*
 * gammaStandardize() function:
 * standardize a single value following a Gamma distribution
 */
float gammaStandardize(float value, float params[]) {

  float alfa, beta, y, z;

  alfa = params[0];
  beta = params[1];
  y = value / alfa / beta;
  z = (pow(y,0.333333333333333) + 1/(9*beta) - 1) * pow(9*beta, 0.5);

  return(z);
}

/*
 * pearsonIIIFit() function:
 * Gives the parameters of a Pearson III distribution function
 * fitted to a data series through its L-moments.
 * pearsonIIIParams[0] = origin
 * pearsonIIIParams[1] = scale (alpha)
 * pearsonIIIParams[2] = shape (beta)
 */
void pearsonIIIFit(float L[], float pearsonIIIParams[]) {

  float lMomentRatio, pi, sqrtPi, t, t2, t3;

  pi = 3.14159265358979;
  sqrtPi = sqrt(pi);
  lMomentRatio = L[3] / L[2]; /* third l-Moment ratio */
  /*printf("L-moment ratio = %.4f\n", lMomentRatio); */
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
 * pearsonIIIStandardize() function:
 * standardize a single value following a Pearson III distribution
 */
float pearsonIIIStandardize(float value, float params[]) {

  float alfa, beta, origin, y, z;

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
 * logLogisticFit()
 * Estimates the parameters of a Gamma distribution functions
 */
void logLogisticFit(float beta[], float logLogisticParams[]) {

  float g1, g2;

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
 * logLogisticCDF()
 * Gives the cumulative distribution function of 'value', following a LogLogistic distribution
 */
float logLogisticCDF(float value, float params[]) {
  return 1 / (1+pow((params[1]/(value-params[0])),params[2]));
}

/*
 * gaussianInvCDF()
 * Gives the inverse cumulative distribution function following a standard Gaussian
 * distribution. I.e., the function gives the standardized value (Z) corresponding to
 * a given probability (0<prob<1) following a standard Gaussian distribution (mean=0
 * and sd=1).
 */
float standardGaussianInvCDF(float prob) {

  static float C[3] = {2.515517,0.802853,0.010328};
  static float d[4] = {0,1.432788,0.189269,0.001308};
  double W, WW, WWW, resul;

  if (prob<=0.5) W = sqrt(-2*log(prob));
  else W =sqrt(-2*log(1-prob));
  WW = W*W;
  WWW = WW*W;
  resul = W - (C[0] + C[1]*W + C[2]*WW) / (1 + d[1]*W + d[2]*WW + d[3]*WWW);
  if (prob>0.5) resul = -resul;
  return resul;
}

/*
 * Functions from lmoments.c for computing the L-moments of a sample,
 * plus a bunch of other auxiliar functions
 */

/*
 * pwm()
 * Calculates the first three probability weighted moments of a sample,
 * using either the unbiased estimator (when A=B=0)
 * or a plotting position formula (when A<=B<=0).
 * This are alpha PWMs, following Rao & Hamed 2000, eqs. 3.1.4 and 3.1.6
 */
void pwm(float series[], int n, float pwms[], float A, float B, int isBeta) {

	int i;
	float acum[3], F;

	acum[0] = acum[1] = acum[2] = 0;
	if (A==0 && B==0) { /* use unbiased estimator */
		for (i=1; i<=n; i++) {
			acum[0] += series[i-1];
			if (isBeta==0) { /* compute alpha PWMs */
				acum[1] += series[i-1] * (n-i);
				acum[2] += series[i-1] * (n-i) * (n-i-1);
			}
			if (isBeta==1) { /* compute beta PWMs */
				acum[1] += series[i-1] * (i-1);
				acum[2] += series[i-1] * (i-1) * (i-2);
			}
		}
	}
	else { /* use plotting-position (biased) estimator */
		for (i=1; i<=n; i++) {
			acum[0] += series[i-1];
			F = (i+A) / (n+B);
			if (isBeta==0) { /* compute alpha PWMs */
				acum[1] += series[i-1]*(1-F);
				acum[2] += series[i-1]*(1-F)*(1-F);
			}
			if (isBeta==1) { /* compute beta PWMs */
				acum[1] += series[i-1]*F;
				acum[2] += series[i-1]*F*F;
			}
		}
	}
	pwms[0] = acum[0] / n;
	pwms[1] = acum[1] / n / (n-1);
	pwms[2] = acum[2] / n / ((n-1)*(n-2));

	/*printf("pwm0: %.4f\n", pwms[0]); */
	/*printf("pwm1: %.4f\n", pwms[1]); */
	/*printf("pwm2: %.4f\n\n", pwms[2]); */
}

/*
 * lMoments()
 * Estimates the first two L-moments of the sample
 */
void lMoments(float series[], int n, float lMoment[], float A, float B) {

	int i, j, ordenLMom;
	float C, D, E, acum[3], alpha[3];

	/* Calculate the first three PWMs */
	pwm(series, n, alpha, A, B, 0);

	/* Obtain the first two L-moments */
/*
 *	ordenLMom = 1;
 *	lMoment[ordenLMom] = beta[0];
 *	for (j=2; j<3; j++) {
 *		acum[0] = 0;
 *		ordenLMom = j;
 *		for (i=0; i<ordenLMom; i++) {
 *			C = pow(-1, ordenLMom-1-i);
 *			D = 1;
 *			E = 1;
 *			if (i>0) {
 *				if (i==ordenLMom-1) {
 *					E = factorial(ordenLMom-1+i) / factorial(i) / factorial(ordenLMom-1);
 *				}
 *				else {
 *					D = factorial(ordenLMom-1) / factorial(i) / factorial(ordenLMom-1-i);
 *					E = factorial(ordenLMom-1+i) / factorial(i) / factorial(ordenLMom-1);
 *				}
 *			}
 *			acum[0] = acum[0] + (C * D * E * beta[i]);
 *			lMoment[ordenLMom] = acum[0];
 *		}
 *	}
 *	if (A!=0 || B!=0) lMoment[2]=-lMoment[2];
 */
	lMoment[1] = alpha[0];
	lMoment[2] = alpha[0] - 2*alpha[1];
	lMoment[3] = alpha[0] - 6*alpha[1] + 6*alpha[2];
/*
 *	lMoment[4] = alpha[0] - 12*alpha[1] + 30*alpha[2] - 20*alpha[3];
 *	printf("\nL-moment order 1 = %.4f\n", lMoment[1]);
 *	printf("L-moment order 2 = %.4f\n", lMoment[2]);
 *	printf("L-moment order 3 = %.4f\n", lMoment[3]);
 */
}


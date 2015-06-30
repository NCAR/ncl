/*
// Functions for computing the L-moments of a sample,
// plus a bunch of other auxiliar functions
*/
/* Function prototypes */
#include "spei.h"

/*
// pwm()
// Calculates the first three probability weighted moments of a sample,
// using either the unbiased estimator (when A=B=0)
// or a plotting position formula (when A<=B<=0).
// This are alpha PWMs, following Rao & Hamed 2000, eqs. 3.1.4 and 3.1.6
*/
void pwm(double *series, int n, double *pwms, double A, double B, int isBeta) {

	int i;
	double acum[3], F;

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
// lMoments()
// Estimates the first two L-moments of the sample
*/
void lMoments(double *series, int n, double *lMoment, double A, double B) {

        /*      int i, j, ordenLMom; */
	/*	double C, D, E, acum[3];*/
	double alpha[3];
	/* Calculate the first three PWMs */
	pwm(series, n, alpha, A, B, 0);

	/* Obtain the first two L-moments */
	/*
//	ordenLMom = 1;
//	lMoment[ordenLMom] = beta[0];
//	for (j=2; j<3; j++) {
//		acum[0] = 0;
//		ordenLMom = j;
//		for (i=0; i<ordenLMom; i++) {
//			C = pow(-1, ordenLMom-1-i);
//			D = 1;
//			E = 1;
//			if (i>0) {
//				if (i==ordenLMom-1) {
//					E = factorial(ordenLMom-1+i) / factorial(i) / factorial(ordenLMom-1);
//				}
//				else {
//					D = factorial(ordenLMom-1) / factorial(i) / factorial(ordenLMom-1-i);
//					E = factorial(ordenLMom-1+i) / factorial(i) / factorial(ordenLMom-1);
//				}
//			}
//			acum[0] = acum[0] + (C * D * E * beta[i]);
//			lMoment[ordenLMom] = acum[0];
//		}
//	}
//	if (A!=0 || B!=0) lMoment[2]=-lMoment[2];
*/
	lMoment[1] = alpha[0];
	lMoment[2] = alpha[0] - 2*alpha[1];
	lMoment[3] = alpha[0] - 6*alpha[1] + 6*alpha[2];
	/*lMoment[4] = alpha[0] - 12*alpha[1] + 30*alpha[2] - 20*alpha[3]; */
	/*printf("\nL-moment order 1 = %.4f\n", lMoment[1]); */
	/*printf("L-moment order 2 = %.4f\n", lMoment[2]); */
	/*printf("L-moment order 3 = %.4f\n", lMoment[3]); */
}


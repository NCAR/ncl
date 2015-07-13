/* Function prototypes for various spei-based functions
   These are used in: 
     auxiliary.c 
     lmoments.c
     pdfs.c 
     spei_func.c 
     thornthwaite.c
*/

#include <stdio.h>
#include <math.h>

extern void spei_driver(double *rainSeries,double *tempSeries,double tmsg,
			int npts,double lat,
			int acumulated,int seasonality, double *etpSeries, 
			double *balanceSeries, double *acumSeries,
			double *seasonSeries,double *speiSeries);

extern void spei_func(double *dataSeries, int n, double tmsg, int seasons, 
		      double *speiSeries, double *seasonSeries);

extern long int factorial(int anInteger);
extern void     gammaFit(double *L, double *gammaParams);
extern double   gammaLn(double xx);
extern double   gammaStandardize(double value, double *params);
extern void     lMoments(double *series, int n, double *lMoment, double A, double B);
extern double   logLogisticCDF(double value, double *params);
extern void     logLogisticFit(double *pwm, double *logLogisticParams);
extern void     pearsonIIIFit(double *L, double *pearsonIIIParams);
extern double   pearsonIIIStandardize(double value, double *params);
extern void     pwm(double *series, int n, double *beta, double A, double B, int isBeta);
extern double   standardGaussianInvCDF(double prob);
extern void     thornthwaite(double *tempSeries, int n, double tmsg, double lat, double *etpSeries);
extern void     upward(double *series, int n);


#include <stdio.h>
#include <math.h>
#include <time.h>
#include <stdlib.h>


/*
 * Original code used #include to include other *.c files.
 * I decided to put function defintions in a *.h file
 * and include that instead.
 */
/*
#include "auxiliary.c"
#include "lmoments.c"
#include "pdfs.c"
#include "thornthwaite.c"
*/
#include "spei.h"
#include "wrapper.h"

/* Max size of raw rainfall and events matrices */
#define NUMRESULTMAX 5000
#define NUMSEASONSMAX 12

/* Define max() and min() functions */
#ifndef max
	#define max( a, b ) ( ((a) > (b)) ? (a) : (b) )
#endif

#ifndef min
	#define min( a, b ) ( ((a) < (b)) ? (a) : (b) )
#endif

/* Calculate the Standardized Precipitation Index */
void spei_driver(double *rainSeries,double *tempSeries,double tmsg,
		 int npts,double lat,
		 int acumulated,int seasonality,double *etpSeries,
		 double *balanceSeries, double *acumSeries,
		 double *seasonSeries,double *speiSeries)
{
  int   numRegistros,acumRegistros,indice,jndice;

  /* Initialize variables */
  acumRegistros = indice = jndice = 0;
  for (indice=0; indice<npts; indice++) {
    etpSeries[indice] = balanceSeries[indice] = 
      acumSeries[indice] = speiSeries[indice] = 0.0;
  }
  numRegistros = npts;
  if(tempSeries[1]==0) numRegistros-=1;
  /* Print metadata (just to check) */
  /*
  printf("latitude: %.3f\n", lat);
  printf("initial date: %d/%d\n", mes, anio);
  printf("seasonality: %d\n", seasonality);
  printf("%d registers\n", numRegistros);
  printf("calculating SPEI at %d month", acumulated);
  if (acumulated>1) printf("s");
  */

/* 
 * Compute the climatic balance: precipitation minus 
 * potential evapotranspiration 
 */
  if (tempSeries[1]!=0 && tempSeries[2]!=0) {
    thornthwaite(tempSeries, numRegistros, tmsg, lat, etpSeries);
    for (indice=0; indice<numRegistros; indice++) {
      if(etpSeries[indice] != tmsg) {
	balanceSeries[indice] = rainSeries[indice]-etpSeries[indice];
      }
      else {
	balanceSeries[indice] = tmsg;
      }
    }
  }
  else {
    for (indice=0; indice<numRegistros; indice++) {
      if(etpSeries[indice] != tmsg) {
	balanceSeries[indice] = rainSeries[indice];
      }
      else {
	balanceSeries[indice] = tmsg;
      }
    }
  }
  /* Compute the cumulative series */
/* 
  Commented the anio/mes (year/month) code, b/c it is not used
  in the calculation. We may decide to add it later.
  anio += (acumulated-1)/12;
  mes += acumulated-1;
  while (mes>12) mes-=12;
*/
  acumRegistros = numRegistros-acumulated+1;
  for (indice=acumulated-1; indice<numRegistros; indice++) {
    for (jndice=0; jndice<acumulated; jndice++) {
      if(balanceSeries[indice-jndice] != tmsg) {
	acumSeries[indice-acumulated+1] += balanceSeries[indice-jndice];
      }
      else {
	acumSeries[indice-acumulated+1] = tmsg;
      }
    }
  }
  /* Compute the SPEI series*/
  spei_func(acumSeries, acumRegistros, tmsg, seasonality, speiSeries, seasonSeries);
}

/*
// spei_func()
// Calculates the Standardized Precipitation-Evapotransporation Index
// from a series of climatic balance (precipitation minus etp). The
// SPEI is the standardized value of the climatic balance (P-ETP),
// computed following a Log Logistic probability distribution.
*/
void spei_func(double *dataSeries, int n, double tmsg, int seasons,
	       double *speiSeries,  double *seasonSeries) {

	int i, j, k, nSeason;
	double beta[3], logLogisticParams[NUMSEASONSMAX][3];

	/*
	for (i=0; i<n; i++) {
	  printf("spei acum[%d] = %g\n", i,dataSeries[i]);
	}
	*/
	/* Loop through all seasons defined by seasons */
	for (j=1; j<=seasons; j++) {
	  /* Extract and sort the seasonal series */
		k = 0;
		for (i=j-1; i<n; i+=seasons) {
			seasonSeries[k] = dataSeries[i];
			k++;
		}
		nSeason = k;
		/* "upward" is a simple ascending order qsort. */
		   upward(seasonSeries, nSeason);
		/*
		 * May not be able to use qsort because seasonSeries may
 		  actually be larger than nSeason. Need to check into this.
		qsort((void*)seasonSeries,nSeason,sizeof(double),cmpdouble);
		*/
		/* Compute probability weighted moments */
		/*pwm(seasonSeries, nSeason, beta, -0.35, 0, 0); */
		pwm(seasonSeries, nSeason, beta, 0, 0, 0);
		/* Fit a Log Logistic probability function */
		logLogisticFit(beta, logLogisticParams[j]);
		/*
		printf("\nSeason %u", j);
		printf("\nLogLogistic beta param.: %.4f", logLogisticParams[j][0]);
		printf("\nLogLogistic alpha param.: %.4f", logLogisticParams[j][1]);
		printf("\nLogLogistic gamma param.: %.4f\n", logLogisticParams[j][2]);
		*/
		  /* Calculate the standardized values */
		for (i=j-1; i<n; i+=seasons) {
			speiSeries[i] = logLogisticCDF(dataSeries[i], logLogisticParams[j]);
			speiSeries[i] = -standardGaussianInvCDF(speiSeries[i]);
		}
	}
}

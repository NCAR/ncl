/*
 *      $Id: linaprox.c,v
 */
#include <stdio.h>
#include <ncarg/hlu/hluutil.h>





void linaprox
#if	NhlNeedProto
(int n,float *x,float *y,float *c)
#else
(n,x,y,c)
	int n;
	float *x;
	float *y;
	float *c;
#endif
{
	int i;

	for(i = 0; i < n-1; i++) {
		c[i] =  (y[i+1] - y[i])/(x[i+1] - x[i]);
	}
	return;
}

void evallinaprox
#if	NhlNeedProto
(int n, float *x, float* y,float *c,float xval, float *yval)
#else
(n,x,y,c,xval,yval)
	int n;
	float *x;
	float *y;
	float *c;
	float xval;
	float *yval;
#endif
{
	int i = 0;

	i = searchb(x,n,xval);
/*
* linear extrapolation if value outside of boundaries
*/
	if( (i != -1) && (i != n)) {
		*yval = c[i] * (xval - x[i]) + y[i];
	} else if (i==n) {
		*yval = c[n-2] * (xval - x[n-1]) + y[n-1];
	} else {
		*yval = -c[0] * (x[0] - xval) + y[0];
	}
	return;
}

/*
 *      $Id: bsearch.c,v
 */
#include <stdio.h>
#include <ncarg/hlu/hluutil.h>



int searchb
#if	NhlNeedProto
(float *a,int n,float value)
#else
(a,n,value)
	float	*a;
	int	n;
	float	value;
#endif
{
	int   lower,upper,mid,direction;

	upper = n;
	lower = -1;

	direction = ( a[n-1] > a[0] ) ? 1 /* asscending */: 0; /* descending */
	while(upper-lower > 1) {
		mid = ((upper+lower)>>1);
		if(a[mid] > value) {
			if(direction) 
				upper = mid ;
			else 
				lower = mid;
		} else {
			if(direction) 
				lower = mid ; 
			else 
				upper = mid;
		}
	}
	return(lower);
}


/*
 *      $Id: nicevals.c,v 1.3 2001-06-13 23:53:57 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		nicevals.c
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Aug 21 16:25:48 MDT 2000	
 *
 *	Description:	This file contains a utility function for determining
 *			"nice" values for endpoints and spacing for a set
 *                      of steps through the values of a dataset.
 */

#include <float.h>
#include <math.h>
#include <ncarg/hlu/hluutil.h>

/*
 * Function:	_NhlGetEndpointsAndStepSize
 *
 * Description: Given min and max values of a data domain and the maximum
 *              number of steps desired, determines "nice" values of 
 *              for endpoints and spacing to create a series of steps 
 *              through the data domain. A flag controls whether the max 
 *              and min are inside or outside the data range.
 *
 * In Args: double min 		the minimum value of the domain
 *          double max   	the maximum value of the domain
 *          int   max_steps	the maximum number of steps desired
 *          NhlBoolean outside  controls whether return min/max fall just
 *                              outside or just inside the data domain.
 *				if outside: 
 * 				    *min_out <= min < *min_out + *step_size
 *                                  *max_out >= max > *max_out - *step_size
 *                              if inside:
 * 				    *min_out >= min > *min_out - *step_size
 *                                  *max_out <= max < *max_out + *step_size
 *
 *
 * Out Args: double *min_out     a "nice" minimum value
 *           double *max_out     a "nice" maximum value
 *           double *step_size   a step value such that 
 *                              (where n is an integer < max_steps):
 *                              *min_out + n * *step_size == *max_out 
 *                               with no remainder 
 *
 * Return Values: NhlErrorTypes:
 *   	                NhlFATAL if min is larger than max or if they are
 *                      equal within 7 digits of precision. Values less than
 *                      _NhlMIN_NONZERO (defined in hluutil.h) are considered
 *                      equal to 0.0.
 *
 * Side Effects: NONE
 */
NhlErrorTypes _NhlGetEndpointsAndStepSize
#if	NhlNeedProto
(
	double		min,
	double   	max,
	int		max_steps,
	NhlBoolean	outside,
	double		*min_out,
	double		*max_out,
	double		*step_size
)
#else
(min,max,max_steps,outside,min_out,max_out,step_size)
	double		min;
	double   	max;
	int		max_steps;
	NhlBoolean	outside;
	double		*min_out;
	double		*max_out;
	double		*step_size;
#endif
{
	double	table[] = 
	{ 1.0,2.0,2.5,4.0,5.0,
		  10.0,20.0,25.0,40.0,50.0,
		  100.0,200.0,250.0,400.0,500.0 };
	double	d,u,t,am1,ax1;
	double	am2=0.0,ax2=0.0;
	int	npts = 15;
	int	i;
	char	*e_text;
	char	func[] = "_NhlGetEndpointsAndStepSize";

	if(_NhlCmpFAny2((float)max,(float)min,7,_NhlMIN_NONZERO)<=0.0) {
		e_text = "%s: Max value less than or equal to min value";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,func);
		return(NhlFATAL);
	}
		
	d = pow(10.0,floor(log10(max - min)) - 2.0);
	u = *step_size = FLT_MAX;
	if (outside) {
		for(i=0;i<npts; i++) {
			t = table[i] * d;
			am1 = floor(min/t) *t;
			ax1 = ceil(max/t) * t;
			if(((i>=npts-1)&&(*step_size == u))||
			   ((t <= *step_size)&&
			    (_NhlCmpFAny2((float)((ax1-am1)/t),
					  (float)(max_steps - 1),7,
					  _NhlMIN_NONZERO) <= 0.0))){
				*step_size = t;
				ax2 = ax1;
				am2 = am1;
			}
		}
	}
	else {
		for(i=0;i<npts; i++) {
			t = table[i] * d;
			am1 = ceil(min/t) *t;
			ax1 = floor(max/t) * t;
			if(((i>=npts-1)&&(*step_size == u))||
			   ((t <= *step_size)&&
			    (_NhlCmpFAny2((float)((ax1-am1)/t),
					  (float)(max_steps - 1),7,
					  _NhlMIN_NONZERO) <= 0.0))){
				*step_size = t;
				ax2 = ax1;
				am2 = am1;
			}
		}
	}
	*min_out = am2;
	*max_out = ax2;
	return(NhlNOERROR);
}

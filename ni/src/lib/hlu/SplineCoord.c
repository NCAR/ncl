/*
 *      $Id: SplineCoord.c,v 1.3 1993-11-02 15:57:53 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Spline.c
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Oct 7 16:30:29 MDT 1992
 *
 *	Description:	Contains functions for manipulating and managing
 *			spline approximations of 
 */

#include <stdio.h>
#include <math.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/CoordApprox.h>
static Ordering GetOrdering(
#ifdef NhlNeedProto
float * /*v*/,
int   /*nv*/,
float * /* min */,
float * /* max */
#endif
);
static void reverse(
#ifdef NhlNeedProto
float	* /*a*/,
int	  /*n*/
#endif
);

/*
 * Function:	_NhlCreateSplineCoordApprox
 *
 * Description: Builds a data sctructure needed to evaluate forward and 
 *		reverse transformations for rectilinear coordinate systems.
 *		Input values must be either monotonically increasing or 
 *		descreasing to be able to build both forward and inverse
 *		transformations.
 *
 * In Args:	float *x	contains x coordinates
 *		float *x_int	optional: contains intermediate values
 *		int   nx	contains number of x coordinate points;
 *		float *y	contains y coordinates
 *		float *y_int	optional: conatins intermediate values
 *		int   ny	contains number of y coordinate points;
		float	xsigma;
		float	ysigma;
		int   xsample;
		int   ysample;
		Status *xstatus 
		Status*ystatus
 *
 * Out Args:	NONE
 *
 * Return Values: Returns a pointer to a struture  that contains all of the
 *		information needed to evaluate the approximation.
 *
 * Side Effects:
 */

/*
* In general x_int and y_int are pretty wel behaved sequences of numbers or
* integers when set to null and x and y can be almost any function. For this
* reason the functions x_int ==> x and y_int ==> y are calculated first and
* then intermediate points between x_int[i] an x_int[i+1] are evaluated and
* then the functions x ==>x_int and y ==> y_int are computed. This should
* reduce the error involved in performing the following mapping 
* x ==> x_int ==> x and mapping x_int ==> x ==> x_int.
*/

/*
* Forward is defined to mean mapping from x ==> x_int
* Inverse is defined to mean mapping from x_int ==> x
*/

NhlErrorTypes _NhlCreateSplineCoordApprox
#if __STDC__
(NhlCoordDat *thedat,
int	x_use_log,
float *x, 
float *x_int, 
int nx,
int	y_use_log,
float *y, 
float *y_int, 
int ny,
float xsigma, 
float ysigma,
int xsample,
int ysample, 
Status *xstatus,
Status *ystatus)
#else
(thedat,x_use_log,x,x_int,nx,y_use_log,y,y_int,ny,xsigma,ysigma,xsample,ysample,xstatus,ystatus)
	NhlCoordDat *thedat;
	int x_use_log;
	float *x;
	float *x_int;
	int nx;
	int y_use_log;
	float *y;
	float *y_int;
	int ny;
 	float xsigma;
 	float ysigma;
	int   xsample;
	int   ysample;
	Status	*xstatus;  /* 0 ERROR 	NONE
			 1 x==>x_int	FORWARD	
			 2 x<==x_int	INVERSE
			 3 x<==>x_int	BOTH
		      */
	Status	*ystatus;  /* 0 ERROR	NONE
			 1 y==>y_int	FORWARD
			 2 y<==y_int	INVERSE
			 3 y<==>y_int	BOTH
		      */
#endif
{
	int		i,j;
	Ordering	xdirection;
	Ordering	xdirection_int;
	Ordering	ydirection;
	Ordering	ydirection_int;
	int		iop = 3;
	float dir0 = 0.0;
	float dirn = 0.0;
	int ierr = 0;
	float		*work_array;
	float		interval;
	int sample;
	NhlErrorTypes	xret = NOERROR;
	NhlErrorTypes	yret = NOERROR;

	thedat->xstatus = *xstatus = NONE;
	thedat->ystatus = *ystatus = NONE;

	work_array =(float*)NhlMalloc(
			(unsigned)((MAX(xsample,ysample))*sizeof(float)*(MAX(nx,ny))+1));
	thedat->y_use_log = 0;
	thedat->x_use_log = 0;
	if(nx > 2)  {
		sample = xsample;
		thedat->x_orig_inverse = (float*)NhlMalloc((unsigned)
					sizeof(float)*nx);
		thedat->fx_orig_inverse = (float*)NhlMalloc((unsigned)
					sizeof(float)*nx);
		thedat->x_coefs_inverse = (float*)NhlMalloc((unsigned)
					sizeof(float)*nx);
		thedat->nx_inverse = nx;
		thedat->xsigma = xsigma;

		if(x_int == NULL) {	
			for(i = 0 ; i < nx; i++) 
				thedat->x_orig_inverse[i] = (float) i;
			xdirection_int = INCREASING;
			memcpy((char*)thedat->fx_orig_inverse,(char*)x,
							nx*sizeof(float));
		} else {
			xdirection_int = GetOrdering(x_int,nx,&(thedat->x_int_min),&(thedat->x_int_max));
			if(xdirection_int == INCREASING) {
				memcpy((char*)thedat->x_orig_inverse,
						(char*)x_int,nx*sizeof(float));
				memcpy((char*)thedat->fx_orig_inverse,(char*)x,
							nx*sizeof(float));
			} else if(xdirection_int == DECREASING) {
				reverse(thedat->fx_orig_inverse,thedat->nx_inverse);
				reverse(thedat->x_orig_inverse,thedat->nx_inverse);
			} 
		}

		if(xdirection_int == NONMONOTONIC) {
			thedat->nx_inverse = 0;
			NhlFree(thedat->x_orig_inverse);
			thedat->x_orig_inverse = NULL;
			NhlFree(thedat->fx_orig_inverse);
			thedat->fx_orig_inverse = NULL;
			NhlFree(thedat->x_coefs_inverse);
			thedat->x_coefs_inverse = NULL;
			NhlPError(WARNING,E_UNKNOWN,"Spline: A non-monotonic coordinate vector was passed to CreateSplineApprox");
			xret = WARNING;
		} else
		if((xdirection_int == INCREASING)
			||(xdirection_int == DECREASING)){
			if(x_use_log) {
				thedat->x_use_log = 1;
				for(i=0; i< nx; i++ ) {
					thedat->fx_orig_inverse[i]=(float)
						log10(thedat->fx_orig_inverse[i]);
				}
			} 
/* FORTRAN */		_NHLCALLF(curv1,CURV1)(&(thedat->nx_inverse),
				thedat->x_orig_inverse,
				thedat->fx_orig_inverse,
				&dir0,&dirn,&iop,
				thedat->x_coefs_inverse,
				work_array,&(thedat->xsigma),&ierr);
			*xstatus = thedat->xstatus = INVERSE;
		} 
		if(*xstatus==INVERSE) {
			thedat->x_orig_forward = (float*)NhlMalloc((unsigned)
						sample*sizeof(float)*nx);
			thedat->fx_orig_forward = (float*)NhlMalloc((unsigned)
						sample*sizeof(float)*nx);
			thedat->x_coefs_forward = (float*)NhlMalloc((unsigned)
						sample*sizeof(float)*nx);
	
			for(i = 0 ; i< nx -1  ; i++) {	
				thedat->x_orig_forward[sample*i] = thedat->fx_orig_inverse[i];
				thedat->fx_orig_forward[sample*i] = thedat->x_orig_inverse[i];
				j = 1;
				interval = (thedat->x_orig_inverse[i+1] - 
					thedat->x_orig_inverse[i])/sample;
				while( j % sample != 0 ) {
					thedat->fx_orig_forward[sample * i + j]
					= thedat->x_orig_inverse[i] + ((float)j * 
						interval);
/* FORTRAN */				_NHLCALLF(curv2,CURV2)(&(thedat->fx_orig_forward[sample 
							* i+j]),
						&(thedat->x_orig_forward[sample 
							* i+j]),
						&(thedat->nx_inverse),
						thedat->x_orig_inverse,
						thedat->fx_orig_inverse,
						thedat->x_coefs_inverse,
						&(thedat->xsigma));
					j++;
				}
			}
			thedat->x_orig_forward[sample * (nx-1)] = thedat->fx_orig_inverse[i];
			thedat->fx_orig_forward[sample * (nx-1)] = thedat->x_orig_inverse[i];
			thedat->nx_forward = sample * (nx-1) + 1;
	
	
			xdirection = GetOrdering(thedat->x_orig_forward,thedat->nx_forward,&(thedat->x_min),&(thedat->x_max));
				
			if(xdirection == DECREASING) {
				reverse(thedat->fx_orig_forward,thedat->nx_forward);
				reverse(thedat->x_orig_forward,thedat->nx_forward);
			} else if(xdirection == NONMONOTONIC){
				thedat->nx_forward= 0;
				NhlFree(thedat->x_orig_forward);
				thedat->x_orig_forward = NULL;
				NhlFree(thedat->fx_orig_forward);
				thedat->fx_orig_forward= NULL;
				NhlFree(thedat->x_coefs_forward);
				thedat->x_coefs_inverse = NULL;
				xret = WARNING;
				NhlPError(WARNING,E_UNKNOWN,"Spline: A non-monotonic coordinate vector was passed to CreateSplineApprox");
			}
			if((xdirection == DECREASING)||
					(xdirection == INCREASING)) {
/* FORTRAN */			_NHLCALLF(curv1,CURV1)(&(thedat->nx_forward),
					thedat->x_orig_forward,
					thedat->fx_orig_forward,
					&dir0,&dirn,&iop,
					thedat->x_coefs_forward,
					work_array,&(thedat->xsigma),&ierr);
				*xstatus = thedat->xstatus = BOTH;
			} 
		} else {
/*
* CONINUE TO INTERPOLATE
*/
			NhlPError(WARNING,E_UNKNOWN,"Accuracy of spline coordinate approximation in question");
			xret = WARNING;	
			*xstatus =  thedat->xstatus = NONE;
		}
	} else {
/*
* ERROR: NOT ENOUGH X COORDINATE POINTS
*/
	
		NhlPError(FATAL,E_UNKNOWN,"A Miniumum of 3 coordinates points must be provided to CreateSplineApprox");	
		xret = FATAL;
		*xstatus = NONE;
	}
	if(ny > 2) {
		sample = ysample;
		thedat->y_orig_inverse = (float*)NhlMalloc((unsigned)
					sizeof(float)*ny);
		thedat->fy_orig_inverse = (float*)NhlMalloc((unsigned)
					sizeof(float)*ny);
		thedat->y_coefs_inverse = (float*)NhlMalloc((unsigned)
					sizeof(float)*ny);
		thedat->ny_inverse = ny;
		thedat->ysigma = ysigma;
		if(y_int == NULL) {	
			for(i = 0 ; i < ny; i++) 
				thedat->y_orig_inverse[i] = (float) i;
			ydirection_int = INCREASING;
			memcpy((char*)thedat->fy_orig_inverse,(char*)y,
							ny*sizeof(float));
		} else {
			ydirection_int = GetOrdering(y_int,ny,&(thedat->y_int_min),&(thedat->y_int_max));
			if(ydirection_int == INCREASING) {
				memcpy((char*)thedat->y_orig_inverse,
						(char*)y_int,ny*sizeof(float));
				memcpy((char*)thedat->fy_orig_inverse,(char*)y,
							ny*sizeof(float));
			} else if(ydirection_int == DECREASING) {
				reverse(thedat->fy_orig_inverse,ny);
				reverse(thedat->y_orig_inverse,ny);
			} 
		}

		if(ydirection_int == NONMONOTONIC) {
			NhlPError(WARNING,E_UNKNOWN,"Spline: A non-monotonic coordinate vector was passed to CreateSplineApprox for the y axis");
			thedat->ny_inverse = 0;
			NhlFree(thedat->y_orig_inverse);
			thedat->y_orig_inverse = NULL;
			NhlFree(thedat->fy_orig_inverse);
			thedat->fy_orig_inverse = NULL;
			NhlFree(thedat->y_coefs_inverse);
			thedat->y_coefs_inverse = NULL;
			yret = WARNING;
			thedat->ystatus = *ystatus = NONE;
		} else

		if((ydirection_int == INCREASING)
			||(ydirection_int == DECREASING)){
			if(y_use_log) {
				thedat->y_use_log = 1;
				for(i=0; i< ny; i++ ) {
					thedat->fy_orig_inverse[i]=(float)
						log10(thedat->fy_orig_inverse[i]);
				}
			} 
/* FORTRAN */		_NHLCALLF(curv1,CURV1)(&(thedat->ny_inverse),
				thedat->y_orig_inverse,
				thedat->fy_orig_inverse,
				&dir0,&dirn,&iop,
				thedat->y_coefs_inverse,
				work_array,&(thedat->ysigma),&ierr);
			thedat->ystatus = *ystatus = INVERSE;
		}
		if(*ystatus == INVERSE) { 
			thedat->y_orig_forward = (float*)NhlMalloc((unsigned)
						sample*sizeof(float)*ny);
			thedat->fy_orig_forward = (float*)NhlMalloc((unsigned)
						sample*sizeof(float)*ny);
			thedat->y_coefs_forward = (float*)NhlMalloc((unsigned)
						sample*sizeof(float)*ny);
			for(i = 0 ; i< ny -1  ; i++) {	
				thedat->y_orig_forward[sample*i] = thedat->fy_orig_inverse[i];
				thedat->fy_orig_forward[sample*i] = thedat->y_orig_inverse[i];
				j = 1;
				interval = (thedat->y_orig_inverse[i+1] - 
					thedat->y_orig_inverse[i])/sample;
				while( j % sample != 0 ) {
					thedat->fy_orig_forward[sample * i + j]
					= thedat->y_orig_inverse[i] + ((float)j * 
						interval);
/* FORTRAN */				_NHLCALLF(curv2,CURV2)(&(thedat->fy_orig_forward[sample 
							* i+j]),
						&(thedat->y_orig_forward[sample 
							* i+j]),
						&(thedat->ny_inverse),
						thedat->y_orig_inverse,
						thedat->fy_orig_inverse,
						thedat->y_coefs_inverse,
						&(thedat->ysigma));
					j++;
				}
			}
			thedat->y_orig_forward[sample * (ny-1)] = thedat->fy_orig_inverse[i];
			thedat->fy_orig_forward[sample * (ny-1)] = thedat->y_orig_inverse[i];
			thedat->ny_forward = sample * (ny-1) + 1;
	
	
			ydirection = GetOrdering(thedat->y_orig_forward,thedat->ny_forward,&(thedat->y_min),&(thedat->y_max));
				
	
				
			if(ydirection == DECREASING) {
				reverse(thedat->fy_orig_forward,thedat->ny_forward);
				reverse(thedat->y_orig_forward,thedat->ny_forward);
			} else if(ydirection == NONMONOTONIC){
/*
ERROR
*/
				thedat->ny_forward= 0;
				NhlFree(thedat->y_orig_forward);
				thedat->y_orig_forward = NULL;
				NhlFree(thedat->fy_orig_forward);
				thedat->fy_orig_forward= NULL;
				NhlFree(thedat->y_coefs_forward);
				thedat->y_coefs_inverse = NULL;
				thedat->ystatus = *ystatus = INVERSE;
				NhlPError(WARNING,E_UNKNOWN,"Spline: A non-monotonic coordinate vector was passed to CreateSplineApprox");
			}
			if((ydirection == DECREASING)||
					(ydirection == INCREASING)) {
/* FORTRAN */			_NHLCALLF(curv1,CURV1)(&(thedat->ny_forward),
					thedat->y_orig_forward,
					thedat->fy_orig_forward,
					&dir0,&dirn,&iop,
					thedat->y_coefs_forward,
					work_array,&(thedat->ysigma),&ierr);
				thedat->ystatus = *ystatus = BOTH;
			}
		} else {
/*
* INTERPOLATE ANYWAYS
NOTYET
*/
			NhlPError(WARNING,E_UNKNOWN,"Accuracy of spline coordinate approximation for y axis in question");
			yret = WARNING;
		}
	}
	 else {
/*
* ERROR: NOT ENOUGH Y COORDINATE POINTS
*/
		NhlPError(FATAL,E_UNKNOWN,"A Miniumum of 3 coordinates points must be provided to CreateSplineApprox");	
		yret = FATAL;
		thedat->ystatus = *ystatus = NONE;
	}
	(void)NhlFree(work_array);
	return(MIN(yret,xret));
}


/*
 * Function:	_NhlDestroySplineCoordApprox
 *
 * Description:
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
NhlErrorTypes _NhlDestroySplineCoordApprox
#if __STDC__
( NhlCoordDat * thedat )
#else 
(thedat)
	NhlCoordDat *thedat;
#endif
{
	NhlFree(thedat->x_orig_forward);
	NhlFree(thedat->fx_orig_forward);
	NhlFree(thedat->x_coefs_forward);
	NhlFree(thedat->x_orig_inverse);
	NhlFree(thedat->fx_orig_inverse);
	NhlFree(thedat->x_coefs_inverse);
	NhlFree(thedat->y_orig_forward);
	NhlFree(thedat->fy_orig_forward);
	NhlFree(thedat->y_coefs_forward);
	NhlFree(thedat->y_orig_inverse);
	NhlFree(thedat->fy_orig_inverse);
	NhlFree(thedat->y_coefs_inverse);
	return(NOERROR);
}


/*
 * Function:	_NhlEvalSplineCoordForward
 *
 * Description:	Given a CoordDat structure and a
 *
 * Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */

NhlErrorTypes _NhlEvalSplineCoordForward
#if	__STDC__
(NhlCoordDat *thedat,float x,float y,float *xout,float *yout,float *xmissing,
	float *ymissing)
#else
(thedat,x,y,xout,yout,xmissing,ymissing)
	NhlCoordDat *thedat;
	float	x;
	float 	y;
	float	*xout;
	float	*yout;
	float 	*xmissing;
	float 	*ymissing;
#endif
{
	int itab[3];
	float x_prime = x;
	float y_prime = y;
	NhlErrorTypes ret=NOERROR;

	itab[0] = 1;
	itab[1] = 0;
	itab[2] = 0;

	if((thedat->xstatus == FORWARD) || (thedat->xstatus == BOTH)) {
		itab[0] = 1;
		itab[1] = 0;
		itab[2] = 0;
		if(thedat->x_use_log) {
			x_prime = (float)log10(x_prime);
		}
		if((xmissing == NULL)||(x_prime != *xmissing)) {
/*
			if(!((x_prime >= thedat->x_min)&&(x_prime<= thedat->x_max))) {
				NhlPError(WARNING,E_UNKNOWN,"_NhlEvalSplineCoordForward: out of range value detected converting to closest in bounds value");
				if(x_prime < thedat->x_min)
					x_prime = thedat->x_min;
				else 
					x_prime = thedat->x_max; 
				ret = WARNING;
			}
*/
/* FORTRAN */		_NHLCALLF(curv2,CURV2)(&x_prime,xout,&(thedat->nx_forward),
					thedat->x_orig_forward,
					thedat->fx_orig_forward,
					thedat->x_coefs_forward,
					&(thedat->xsigma));
		} else {
			*xout = *xmissing;
		}
	} else {
		NhlPError(FATAL,E_UNKNOWN,"The coordinate approximation has no forward X transformation");
		return(FATAL);
	}

	if((thedat->ystatus == FORWARD) || (thedat->ystatus == BOTH)) {
		itab[0] = 1;
		itab[1] = 0;
		itab[2] = 0;
		if(thedat->y_use_log) {
			y_prime = (float)log10(y_prime);
		}
		if((ymissing == NULL)||(y_prime != *ymissing)) {
/*
			if(!((y_prime >= thedat->y_min)&&(y_prime<= thedat->y_max))) {
				NhlPError(WARNING,E_UNKNOWN,"_NhlEvalSplineCoordForward: out of range value detected converting to closest in bounds value");
				if(y_prime < thedat->y_min)
					y_prime = thedat->y_min;
				else 
					y_prime = thedat->y_max; 
				ret = WARNING;
			}
*/
/* FORTRAN */		_NHLCALLF(curv2,CURV2)(&y_prime,yout,&(thedat->ny_forward),
					thedat->y_orig_forward,
					thedat->fy_orig_forward,
					thedat->y_coefs_forward,
					&(thedat->ysigma));
		} else {
			*yout = *ymissing;
		}
	} else {
		NhlPError(FATAL,E_UNKNOWN,"The coordinate approximation has no forward Y transformation");
		return(FATAL);
	}
	return(ret);
}


/*
 * Function:	_NhlEvalSplineCoordInverse
 *
 * Description:	Evaluates the inverse transformation of the coordinate system
 *		described in thedat. 
 *
 * In Args:	thedat	dat for spline functions
 *		x	x coordinate point to transform
 *		y	y coordinate point to transform
 *
 * Out Args:	xout	transformed x coordinate
 * 		yout	transformed y coordinate
 *
 * Return Values:	Errors if any
 *
 * Side Effects:	NONE
 */
NhlErrorTypes _NhlEvalSplineCoordInverse
#if	__STDC__
(NhlCoordDat *thedat,float x,float y,float *xout,float *yout,float *xmissing,float *ymissing)
#else
(thedat,x,y,xout,yout,xmissing,ymissing)
	NhlCoordDat *thedat;
	float	x;
	float 	y;
	float	*xout;
	float	*yout;
	float   *xmissing;
	float   *ymissing;
#endif
{
	int itab[3];
	float x_prime = x;
	float y_prime = y;
	NhlErrorTypes ret=NOERROR;

	itab[0] = 1;
	itab[1] = 0;
	itab[2] = 0;

	if((thedat->xstatus == INVERSE) || (thedat->xstatus == BOTH)) {
		itab[0] = 1;
		itab[1] = 0;
		itab[2] = 0;
		if((xmissing == NULL)||(x_prime != *xmissing)) {
/*
			if(!((x_prime >= thedat->x_int_min)&&(x_prime<= thedat->x_int_max))) {
				NhlPError(WARNING,E_UNKNOWN,"_NhlEvalSplineCoordInverse: out of range value detected converting to closest in bounds value");
				if(x_prime < thedat->x_min)
					x_prime = thedat->x_int_min;
				else 
					x_prime = thedat->x_int_max; 
				ret = WARNING;
			}
*/
/* FORTRAN */		_NHLCALLF(curv2,CURV2)(&x_prime,xout,&(thedat->nx_inverse),
					thedat->x_orig_inverse,
					thedat->fx_orig_inverse,
					thedat->x_coefs_inverse,
					&(thedat->xsigma));
			if(thedat->x_use_log)
				*xout = (float)pow(10.0,(double)*xout);
		} else {
			*xout = *xmissing;
		}
		
	} else {
		NhlPError(FATAL,E_UNKNOWN,"The coordinate approximation has no inverse X transformation");
		return(FATAL);
	}

	if((thedat->ystatus == INVERSE) || (thedat->ystatus == BOTH)) {
		itab[0] = 1;
		itab[1] = 0;
		itab[2] = 0;
		if((ymissing == NULL) ||(y_prime != *ymissing)){
/*
			if(!((y_prime >= thedat->y_int_min)&&(y_prime<= thedat->y_int_max))) {
				NhlPError(WARNING,E_UNKNOWN,"_NhlEvalSplineCoordInverse: out of range value detected converting to closest in bounds value");
				if(y_prime < thedat->y_min)
					y_prime = thedat->y_int_min;
				else 
					y_prime = thedat->y_int_max; 
				ret = WARNING;
			}
*/
/* FORTRAN */		_NHLCALLF(curv2,CURV2)(&y_prime,yout,&(thedat->ny_inverse),
					thedat->y_orig_inverse,
					thedat->fy_orig_inverse,
					thedat->y_coefs_inverse,
					&(thedat->ysigma));
			if(thedat->y_use_log)
				*yout = (float)pow(10.0,(double)*yout);
		} else {
			*yout = *ymissing;
		}
	} else {
		NhlPError(FATAL,E_UNKNOWN,"The coordinate approximation has no inverse Y transformation");
		return(FATAL);
	}
	return(ret);
}

/*
 * Function:	_NhlMultiEvalSplineCoordForward
 *
 * Description:	Compute forward tranformation for several points
 *
 * In Args:	thedat		transformation data
 *		x		array of x points
 *		y		array of y points
 *		npts		total number of points
 *
 * Out Args:
 *		xout		array of transformed points
 *		yout		array of transformed points
 *
 * Return Values:
 *
 * Side Effects:
 */
NhlErrorTypes _NhlMultiEvalSplineCoordForward
#if __STDC__
(
	NhlCoordDat	*thedat,
	float		*x,
	float		*y,
	float		*xout,
	float		*yout,
	int		xnpts,
	int		ynpts,
	float		*xmissing,
	float		*ymissing
)
#else
(thedat,x,y,xout,yout,xnpts,ynpts,xmissing,ymissing)
	NhlCoordDat	*thedat;
	float		*x;
	float		*y;
	float		*xout;
	float		*yout;
	int		xnpts;
	int		ynpts;
	float		*xmissing;
	float		*ymissing;
#endif
{
	int itab[3];
	NhlErrorTypes ret=NOERROR;
	int i,xok,yok;
	float tmp;


	if((thedat->ystatus == FORWARD) || (thedat->ystatus == BOTH)) {
		yok = 1;
	} else {
		yok = 0;
		NhlPError(WARNING,E_UNKNOWN,"Unable to evaluate a forward transformation for Y axis");
		ret = WARNING;
	}	

	if((thedat->xstatus == FORWARD) || (thedat->xstatus == BOTH)) {
		xok = 1;
	} else {
		xok = 0;
		if(ret == WARNING) {
			NhlPError(FATAL,E_UNKNOWN,"Unable to evaluate a forward transformation for either axis");
			return(FATAL);
		} else {
			NhlPError(FATAL,E_UNKNOWN,"Unable to evaluate a forward transformation for X axis");
			ret = WARNING;
		}
	}
	if(xok) {
		for(i = 0 ; i<xnpts; i++) {
			itab[0] = 1;
			itab[1] = 0;
			itab[2] = 0;
			if((xmissing == NULL)||(x[i] != *xmissing)) {
			if(thedat->x_use_log) 
				tmp = (float)log10(x[i]);
			else 
				tmp = x[i];
/* FORTRAN */		_NHLCALLF(curv2,CURV2)(&tmp,&(xout[i]),&(thedat->nx_forward),
					thedat->x_orig_forward,
					thedat->fx_orig_forward,
					thedat->x_coefs_forward,
					&(thedat->xsigma));
			} else {
				xout[i] = *xmissing;
			}
		}
	}
	if(yok) {
		for(i = 0 ; i<ynpts; i++) {
			itab[0] = 1;
			itab[1] = 0;
			itab[2] = 0;
			if((ymissing == NULL)||(y[i] != *ymissing)) {
			if(thedat->y_use_log) 
				tmp = (float)log10(y[i]);
			else 
				tmp = y[i];
/* FORTRAN */		_NHLCALLF(curv2,CURV2)(&tmp,&(yout[i]),&(thedat->ny_forward),
					thedat->y_orig_forward,
					thedat->fy_orig_forward,
					thedat->y_coefs_forward,
					&(thedat->ysigma));
			} else {
				yout[i] = *ymissing;
			}
		}
	}
	return(ret);
}


/*
 * Function:	
 *
 * Description:
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
NhlErrorTypes _NhlMultiEvalSplineCoordInverse
#if __STDC__
(NhlCoordDat *thedat,float *x, float *y,float *xout, float *yout, int xnpts, int ynpts, float *xmissing, float *ymissing)
#else
(thedat,x,y,xout,yout,xnpts,ynpts,xmissing,ymissing)
	NhlCoordDat	*thedat;
	float		*x;
	float		*y;
	float		*xout;
	float		*yout;
	int		xnpts;
	int		ynpts;
	float 		*xmissing;
	float		*ymissing;
#endif
{
	int itab[3];
	NhlErrorTypes ret=NOERROR;
	int i,xok,yok;

	itab[0] = 1;
	itab[1] = 0;
	itab[2] = 0;

	if((thedat->ystatus == BOTH) || (thedat->ystatus == INVERSE)) {
		yok = 1;
	} else {
		yok = 0;
		NhlPError(WARNING,E_UNKNOWN,"Unable to evaluate an inverse transformation for Y axis");
		ret = WARNING;
	}	

	if((thedat->xstatus == BOTH) || (thedat->xstatus == INVERSE)) {
		xok = 1;
	} else {
		xok = 0;
		if(ret == WARNING) {
			NhlPError(FATAL,E_UNKNOWN,"Unable to evaluate an inverse transformation for either axis");
			return(FATAL);
		} else {
			NhlPError(FATAL,E_UNKNOWN,"Unable to evaluate a forward transformation for X axis");
			ret = WARNING;
		}
	}
		

	if(xok){
		for(i = 0 ; i<xnpts; i++) {
			itab[0] = 1;
			itab[1] = 0;
			itab[2] = 0;
			if((xmissing == NULL)||(x[i] != *xmissing)){
/* FORTRAN */		_NHLCALLF(curv2,CURV2)(&(x[i]),&(xout[i]),&(thedat->nx_inverse),
					thedat->x_orig_inverse,
					thedat->fx_orig_inverse,
					thedat->x_coefs_inverse,
					&(thedat->xsigma));	
				if(thedat->x_use_log)
					xout[i] = (float)pow(10.0,(double)xout[i]);
			} else {
				xout[i] = *xmissing;
			}
		}
	}
	if(yok){
		for(i = 0 ; i<ynpts; i++) {
			itab[0] = 1;
			itab[1] = 0;
			itab[2] = 0;
			if((ymissing == NULL)||(y[i] != *ymissing)){
/* FORTRAN */		_NHLCALLF(curv2,CURV2)(&(y[i]),&(yout[i]),&(thedat->ny_inverse),
					thedat->y_orig_inverse,
					thedat->fy_orig_inverse,
					thedat->y_coefs_inverse,
					&(thedat->ysigma));
				if(thedat->y_use_log)
					yout[i] = (float)pow(10.0,(double)yout[i]);
			} else {
				yout[i] = *ymissing;
			}
		}
	}
	return(ret);
}


static Ordering GetOrdering
#if __STDC__
(float *v,int nv,float* min, float*max) 
#else
(v,nv,min,max)
	float *v;
	int   nv;
	float	*min;
	float	*max;
#endif
{
	int i;

	i = 1;
	while((i<nv)&&(v[i-1] <= v[i])) {
		i++;
	}
	if(i == nv) {
		*min = v[0];
		*max = v[nv-1];
		return(INCREASING);
	}
	i = 1;
	while((i<nv)&&(v[i-1] >= v[i])) {
		i++;
	}
	if(i==nv){
		*max = v[0];
		*min = v[nv-1];
		return(DECREASING);
	} else {
		return(NONMONOTONIC);
	}

	
}

static void reverse
#if __STDC__
(float *a,int n)
#else
(a,n)
	float 	*a;
	int	n;
#endif
{
	float tmp;
	int i;
	for(i = 0; i < n/2; i++) {
		tmp = a[i];
		a[i] = a[n-i-1];
		a[n-i-1] = tmp;
	}
}

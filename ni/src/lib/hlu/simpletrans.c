
/*
 *      $Id: simpletrans.c,v 1.4 1995-02-19 08:19:43 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Oct 20 11:05:23 MDT 1992
 *
 *	Description:	
 */

#include <stdio.h>
#include <ncarg/hlu/hluutil.h>

void strans
#if	NhlNeedProto
(
	float	froml,
	float	fromr,
	float	fromb,
	float	fromt,
	float	tol,
	float	tor,
	float	tob,
	float	tot,
	float	x,
	float	y,
	float	*xout,
	float	*yout
)
#else
(froml,fromr,fromb,fromt,tol,tor,tob,tot,x,y,xout,yout) 
	float	froml;
	float	fromr;
	float	fromb;
	float	fromt;
	float	tol;
	float	tor;
	float	tob;
	float	tot;
	float	x;
	float	y;
	float	*xout;
	float	*yout;
#endif
{
	*xout = (x - froml)/(fromr - froml)*(tor - tol) + tol;
	if(tol < tor){
		if(*xout > tor)
			*xout = tor;
		else if(*xout < tol)
			*xout = tol;
	}
	else{
		if(*xout > tol)
			*xout = tol;
		else if(*xout < tor)
			*xout = tor;
	}

	*yout = (y - fromb)/(fromt - fromb)*(tot - tob) + tob;
	if(tob < tot){
		if(*yout > tot)
			*yout = tot;
		else if(*yout < tob)
			*yout = tob;
	}
	else{
		if(*yout > tob)
			*yout = tob;
		else if(*yout < tot)
			*yout = tot;
	}

	return;
}

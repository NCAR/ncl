
/*
 *      $Id: simpletrans.c,v 1.1 1994-08-11 23:17:34 boote Exp $
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
#if __STDC__
(float froml, float fromr, float fromb, float fromt, float tol, float tor,float tob, float tot, float x, float y, float *xout, float *yout)
#else
(froml,fromr,fromb,fromt,tol,tor,tob,tot,x,y,xout,yout) 
float froml,fromr,fromb,fromt,tol,tor,tob,tot,x,y;
float* xout;
float* yout;
#endif
{
	*xout = (x - froml)/(fromr - froml)*(tor - tol) + tol;
	*yout = (y - fromb)/(fromt - fromb)*(tot - tob) + tob;
	return;
}

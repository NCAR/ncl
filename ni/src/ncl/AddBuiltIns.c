
/*
 *      $Id: AddBuiltIns.c,v 1.7 1995-01-31 22:25:41 ethan Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		AddBuiltIns.c
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Oct 15 10:43:07 MDT 1993
 *
 *	Description:	
 */
#ifdef __cpluplus
extern "C" {
#endif
#include <stdio.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include "defs.h"
#include "NclBuiltIns.h"
#include "MathFuncs.h"

void _NclAddBuiltIns
#if	NhlNeedProto
(void)
#else 
()
#endif
{
	void *args;
	int dimsizes[NCL_MAX_DIMENSIONS];
	int nargs = 0;

	args = NewArgs(1);
	SetArgTemplate(args,nargs,"float",NclANY,NclANY);nargs++;
	NclRegisterFunc(_Nclsinh,args,"sinh",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,0,"float",NclANY,NclANY);nargs++;
	NclRegisterFunc(_Nclcosh,args,"cosh",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,0,"float",NclANY,NclANY);nargs++;
	NclRegisterFunc(_Ncltanh,args,"tanh",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,0,"float",NclANY,NclANY);nargs++;
	NclRegisterFunc(_Nclsin,args,"sin",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,0,"float",NclANY,NclANY);nargs++;
	NclRegisterFunc(_Nclcos,args,"cos",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,0,"float",NclANY,NclANY);nargs++;
	NclRegisterFunc(_Ncltan,args,"tan",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,0,"float",NclANY,NclANY);nargs++;
	NclRegisterFunc(_Nclasin,args,"asin",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,0,"float",NclANY,NclANY);nargs++;
	NclRegisterFunc(_Nclacos,args,"acos",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,0,"float",NclANY,NclANY);nargs++;
	NclRegisterFunc(_Nclatan,args,"atan",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,0,"float",NclANY,NclANY);nargs++;
	NclRegisterFunc(_Nclceil,args,"ceil",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,0,"float",NclANY,NclANY);nargs++;
	NclRegisterFunc(_Nclfloor,args,"floor",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,0,"float",NclANY,NclANY);nargs++;
	NclRegisterFunc(_Nclfabs,args,"fabs",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,0,"float",NclANY,NclANY);nargs++;
	NclRegisterFunc(_Ncllog,args,"log",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,0,"float",NclANY,NclANY);nargs++;
	NclRegisterFunc(_Ncllog10,args,"log10",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,0,"float",NclANY,NclANY);nargs++;
	NclRegisterFunc(_Nclsqrt,args,"sqrt",nargs);
/*
	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,0,"float",NclANY,NclANY);nargs++;
	NclRegisterFunc(_Nclrint,args,"rint",nargs);
	
	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,0,"float",NclANY,NclANY);nargs++;
	NclRegisterFunc(_Nclasinh,args,"asinh",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,0,"float",NclANY,NclANY);nargs++;
	NclRegisterFunc(_Nclacosh,args,"acosh",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,0,"float",NclANY,NclANY);nargs++;
	NclRegisterFunc(_Nclatanh,args,"atanh",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,0,"float",NclANY,NclANY);nargs++;
	NclRegisterFunc(_Ncltrunc,args,"trunc",nargs);

*/
	
	return;
}


#ifdef __cpluplus
}
#endif

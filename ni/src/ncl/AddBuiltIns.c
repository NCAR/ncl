
/*
 *      $Id: AddBuiltIns.c,v 1.11 1995-04-01 00:54:13 ethan Exp $
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
#include "HLUFunctions.h"

extern NhlErrorTypes _Nclsystem(
#if NhlNeedProto
void
#endif
);

extern NhlErrorTypes _Nclidsfft(
#if NhlNeedProto
void
#endif
);

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

	nargs = 0;
	args = NewArgs(1);
	dimsizes[0] = 1;
	SetArgTemplate(args,0,"string",1,dimsizes);nargs++;
	NclRegisterProc(_Nclsystem,args,"system",nargs);

	nargs = 0;
	args = NewArgs(4);
	SetArgTemplate(args,0,"float",1,NclANY);nargs++;
	SetArgTemplate(args,1,"float",1,NclANY);nargs++;
	SetArgTemplate(args,2,"float",1,NclANY);nargs++;
	dimsizes[0] = 2;
	SetArgTemplate(args,3,"integer",1,dimsizes);nargs++;
	NclRegisterFunc(_Nclidsfft,args,"idsfft",nargs);

	nargs = 0;
	args = NewArgs(2);
	SetArgTemplate(args,0,"graphic",NclANY,NclANY);nargs++;
	dimsizes[0] = 1;
	SetArgTemplate(args,1,"graphic",1,dimsizes);nargs++;
	NclRegisterProc(_NclIChangeWorkstation,args,"NhlChangeWorkstation",nargs);
	nargs = 0;
	args = NewArgs(5);
	SetArgTemplate(args,0,"graphic",1,NclANY);nargs++;
	SetArgTemplate(args,1,"integer",1,NclANY);nargs++;
	SetArgTemplate(args,2,"float",1,NclANY);nargs++;
	SetArgTemplate(args,3,"float",1,NclANY);nargs++;
	SetArgTemplate(args,4,"float",1,NclANY);nargs++;
	NclRegisterProc(_NclISetColor,args,"NhlSetColor",nargs);
	
	nargs = 0;
	args = NewArgs(4);
	SetArgTemplate(args,0,"graphic",1,NclANY);nargs++;
	SetArgTemplate(args,1,"float",1,NclANY);nargs++;
	SetArgTemplate(args,2,"float",1,NclANY);nargs++;
	SetArgTemplate(args,3,"float",1,NclANY);nargs++;
	NclRegisterFunc(_NclINewColor,args,"NhlNewColor",nargs);

	nargs = 0;
	args = NewArgs(2);
	SetArgTemplate(args,0,"graphic",1,NclANY);nargs++;
	SetArgTemplate(args,1,"integer",1,NclANY);nargs++;
	NclRegisterProc(_NclIFreeColor,args,"NhlFreeColor",nargs);

	nargs = 0;
	args = NewArgs(2);
	SetArgTemplate(args,0,"graphic",1,NclANY);nargs++;
	SetArgTemplate(args,1,"integer",1,NclANY);nargs++;
	NclRegisterFunc(_NclIIsAllocatedColor,args,"NhlIsAllocatedColor",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,0,"graphic",1,NclANY);nargs++;
	NclRegisterFunc(_NclIGetBB,args,"NhlGetBB",nargs);

	nargs = 0;
	args = NewArgs(3);
	dimsizes[0] = 1;
	SetArgTemplate(args,0,"graphic",1,NclANY);nargs++;
	SetArgTemplate(args,1,"string",1,dimsizes);nargs++;
	SetArgTemplate(args,2,"graphic",1,NclANY);nargs++;
	NclRegisterFunc(_NclIAddData,args,"NhlAddData",nargs);

	nargs = 0;
	args = NewArgs(3);
	dimsizes[0] = 1;
	SetArgTemplate(args,0,"graphic",1,NclANY);nargs++;
	SetArgTemplate(args,1,"string",1,dimsizes);nargs++;
	SetArgTemplate(args,2,"graphic",1,NclANY);nargs++;
	NclRegisterProc(_NclIRemoveData,args,"NhlRemoveData",nargs);

	nargs = 0;
	args = NewArgs(3);
	dimsizes[0] = 1;
	SetArgTemplate(args,0,"graphic",1,dimsizes);nargs++;
	SetArgTemplate(args,1,"graphic",1,NclANY);nargs++;
	SetArgTemplate(args,2,"logical",1,dimsizes);nargs++;
	NclRegisterProc(_NclIRemoveOverlay,args,"NhlRemoveOverlay",nargs);

	nargs = 0;
	args = NewArgs(2);
	dimsizes[0] = 1;
	SetArgTemplate(args,0,"graphic",1,dimsizes);nargs++;
	SetArgTemplate(args,1,"graphic",1,NclANY);nargs++;
	NclRegisterFunc(_NclIAddAnnotation,args,"NhlAddAnnotation",nargs);

	nargs = 0;
	args = NewArgs(2);
	dimsizes[0] = 1;
	SetArgTemplate(args,0,"graphic",1,dimsizes);nargs++;
	SetArgTemplate(args,1,"graphic",1,NclANY);nargs++;
	NclRegisterProc(_NclIRemoveAnnotation,args,"NhlRemoveAnnotation",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,0,"graphic",1,dimsizes);nargs++;
	NclRegisterProc(_NclIUpdateData,args,"NhlUpdateData",nargs);

	nargs = 0;
	args = NewArgs(3);
	SetArgTemplate(args,0,"graphic",1,NclANY);nargs++;
	SetArgTemplate(args,1,"float",1,NclANY);nargs++;
	SetArgTemplate(args,2,"float",1,NclANY);nargs++;
	NclRegisterProc(_NclIDataPolyline,args,"NhlDataPolyline",nargs);

	nargs = 0;
	args = NewArgs(3);
	SetArgTemplate(args,0,"graphic",1,NclANY);nargs++;
	SetArgTemplate(args,1,"float",1,NclANY);nargs++;
	SetArgTemplate(args,2,"float",1,NclANY);nargs++;
	NclRegisterProc(_NclINDCPolyline,args,"NhlNDCPolyline",nargs);
	

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,0,"graphic",1,NclANY);nargs++;
	NclRegisterFunc(_NclIClassName,args,"NhlClassName",nargs);
	
	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,0,"graphic",1,NclANY);nargs++;
	NclRegisterFunc(_NclIName,args,"NhlName",nargs);


	
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

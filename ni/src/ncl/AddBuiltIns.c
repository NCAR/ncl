
/*
 *      $Id: AddBuiltIns.c,v 1.21 1996-11-19 22:46:07 ethan Exp $
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

extern NhlErrorTypes _Nclind(
#if NhlNeedProto
void
#endif
);
extern NhlErrorTypes _Nclnum(
#if NhlNeedProto
void
#endif
);
extern NhlErrorTypes _Ncl1dtond(
#if NhlNeedProto
void
#endif
);
extern NhlErrorTypes _Nclndto1d(
#if NhlNeedProto
void
#endif
);

extern NhlErrorTypes _Ncldim_product(
#if NhlNeedProto
void
#endif
);
extern NhlErrorTypes _Nclproduct(
#if NhlNeedProto
void
#endif
);
extern NhlErrorTypes _Ncldim_sum(
#if NhlNeedProto
void
#endif
);
extern NhlErrorTypes _Nclsum(
#if NhlNeedProto
void
#endif
);

extern NhlErrorTypes _Ncldim_avg(
#if NhlNeedProto
void
#endif
);

extern NhlErrorTypes _Nclavg(
#if NhlNeedProto
void
#endif
);

extern NhlErrorTypes _NclIinttobyte(
#if NhlNeedProto
void
#endif
);
extern NhlErrorTypes _NclIinttochar(
#if NhlNeedProto
void
#endif
);
extern NhlErrorTypes _NclIinttoshort(
#if NhlNeedProto
void
#endif
);

extern NhlErrorTypes _NclIshorttobyte(
#if NhlNeedProto
void
#endif
);
extern NhlErrorTypes _NclIshorttochar(
#if NhlNeedProto
void
#endif
);

extern NhlErrorTypes _NclIlongtobyte(
#if NhlNeedProto
void
#endif
);
extern NhlErrorTypes _NclIlongtochar(
#if NhlNeedProto
void
#endif
);
extern NhlErrorTypes _NclIlongtoshort(
#if NhlNeedProto
void
#endif
);
extern NhlErrorTypes _NclIlongtoint(
#if NhlNeedProto
void
#endif
);

extern NhlErrorTypes _NclIfloattobyte(
#if NhlNeedProto
void
#endif
);
extern NhlErrorTypes _NclIfloattochar(
#if NhlNeedProto
void
#endif
);
extern NhlErrorTypes _NclIfloattoshort(
#if NhlNeedProto
void
#endif
);
extern NhlErrorTypes _NclIfloattoint(
#if NhlNeedProto
void
#endif
);
extern NhlErrorTypes _NclIfloattolong(
#if NhlNeedProto
void
#endif
);

extern NhlErrorTypes _NclIdoubletobyte(
#if NhlNeedProto
void
#endif
);
extern NhlErrorTypes _NclIdoubletochar(
#if NhlNeedProto
void
#endif
);
extern NhlErrorTypes _NclIdoubletoshort(
#if NhlNeedProto
void
#endif
);
extern NhlErrorTypes _NclIdoubletoint(
#if NhlNeedProto
void
#endif
);
extern NhlErrorTypes _NclIdoubletolong(
#if NhlNeedProto
void
#endif
);
extern NhlErrorTypes _NclIdoubletofloat(
#if NhlNeedProto
void
#endif
);

extern NhlErrorTypes _NclIchartodouble(
#if NhlNeedProto
void
#endif
);
extern NhlErrorTypes _NclIchartofloat(
#if NhlNeedProto
void
#endif
);
extern NhlErrorTypes _NclIchartolong(
#if NhlNeedProto
void
#endif
);
extern NhlErrorTypes _NclIchartoshort(
#if NhlNeedProto
void
#endif
);
extern NhlErrorTypes _NclIchartoint(
#if NhlNeedProto
void
#endif
);
extern NhlErrorTypes _NclIstringtochar(
#if NhlNeedProto
void
#endif
);
extern NhlErrorTypes _NclIchartostring(
#if NhlNeedProto
void
#endif
);
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

NhlErrorTypes _NclIIsVar(
#if     NhlNeedProto
void
#endif
);

NhlErrorTypes _NclIIsFileVar(
#if     NhlNeedProto
void
#endif
);

NhlErrorTypes _NclIIsAtt(
#if     NhlNeedProto
void
#endif
);

NhlErrorTypes _NclIIsFileVarAtt(
#if     NhlNeedProto
void
#endif
);

NhlErrorTypes _NclIIsDim(
#if     NhlNeedProto
void
#endif
);

NhlErrorTypes _NclIIsFileVarDim(
#if     NhlNeedProto
void
#endif
);


void _NclAddBuiltIns
#if     NhlNeedProto
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
	dimsizes[0] = 1;
	SetArgTemplate(args,0,"graphic",NclANY,NclANY);nargs++;
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
	args = NewArgs(4);
	SetArgTemplate(args,0,"graphic",1,NclANY);nargs++;
	SetArgTemplate(args,1,"graphic",1,NclANY);nargs++;
	SetArgTemplate(args,2,"float",1,NclANY);nargs++;
	SetArgTemplate(args,3,"float",1,NclANY);nargs++;
	NclRegisterProc(_NclIDataPolyline,args,"NhlDataPolyline",nargs);

	nargs = 0;
	args = NewArgs(4);
	SetArgTemplate(args,0,"graphic",1,NclANY);nargs++;
	SetArgTemplate(args,1,"graphic",1,NclANY);nargs++;
	SetArgTemplate(args,2,"float",1,NclANY);nargs++;
	SetArgTemplate(args,3,"float",1,NclANY);nargs++;
	NclRegisterProc(_NclIDataPolygon,args,"NhlDataPolygon",nargs);

	nargs = 0;
	args = NewArgs(4);
	SetArgTemplate(args,0,"graphic",1,NclANY);nargs++;
	SetArgTemplate(args,1,"graphic",1,NclANY);nargs++;
	SetArgTemplate(args,2,"float",1,NclANY);nargs++;
	SetArgTemplate(args,3,"float",1,NclANY);nargs++;
	NclRegisterProc(_NclIDataPolymarker,args,"NhlDataPolymarker",nargs);

	nargs = 0;
	args = NewArgs(4);
	SetArgTemplate(args,0,"graphic",1,NclANY);nargs++;
	SetArgTemplate(args,1,"graphic",1,NclANY);nargs++;
	SetArgTemplate(args,2,"float",1,NclANY);nargs++;
	SetArgTemplate(args,3,"float",1,NclANY);nargs++;
	NclRegisterProc(_NclINDCPolyline,args,"NhlNDCPolyline",nargs);

	nargs = 0;
	args = NewArgs(4);
	SetArgTemplate(args,0,"graphic",1,NclANY);nargs++;
	SetArgTemplate(args,1,"graphic",1,NclANY);nargs++;
	SetArgTemplate(args,2,"float",1,NclANY);nargs++;
	SetArgTemplate(args,3,"float",1,NclANY);nargs++;
	NclRegisterProc(_NclINDCPolygon,args,"NhlNDCPolygon",nargs);
	
	nargs = 0;
	args = NewArgs(4);
	SetArgTemplate(args,0,"graphic",1,NclANY);nargs++;
	SetArgTemplate(args,1,"graphic",1,NclANY);nargs++;
	SetArgTemplate(args,2,"float",1,NclANY);nargs++;
	SetArgTemplate(args,3,"float",1,NclANY);nargs++;
	NclRegisterProc(_NclINDCPolymarker,args,"NhlNDCPolymarker",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,0,"graphic",1,NclANY);nargs++;
	NclRegisterFunc(_NclIClassName,args,"NhlClassName",nargs);
	
	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,0,"graphic",1,NclANY);nargs++;
	NclRegisterFunc(_NclIName,args,"NhlName",nargs);


	

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,nargs,"integer",NclANY,NclANY);nargs++;
	NclRegisterFunc( _NclIinttobyte,args,"inttobyte",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,nargs,"integer",NclANY,NclANY);nargs++;
	NclRegisterFunc( _NclIinttochar,args,"inttochar",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,nargs,"integer",NclANY,NclANY);nargs++;
	NclRegisterFunc( _NclIinttoshort,args,"inttoshort",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,nargs,"short",NclANY,NclANY);nargs++;
	NclRegisterFunc( _NclIshorttobyte,args,"shorttobyte",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,nargs,"short",NclANY,NclANY);nargs++;
	NclRegisterFunc( _NclIshorttochar,args,"shorttochar",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,nargs,"long",NclANY,NclANY);nargs++;
	NclRegisterFunc( _NclIlongtobyte,args,"longtobyte",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,nargs,"long",NclANY,NclANY);nargs++;
	NclRegisterFunc( _NclIlongtochar,args,"longtochar",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,nargs,"long",NclANY,NclANY);nargs++;
	NclRegisterFunc(_NclIlongtoshort,args,"longtoshort",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,nargs,"long",NclANY,NclANY);nargs++;
	NclRegisterFunc(_NclIlongtoint,args,"longtoint",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,nargs,"float",NclANY,NclANY);nargs++;
	NclRegisterFunc( _NclIfloattobyte,args,"floattobyte",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,nargs,"float",NclANY,NclANY);nargs++;
	NclRegisterFunc( _NclIfloattochar,args,"floattochar",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,nargs,"float",NclANY,NclANY);nargs++;
	NclRegisterFunc( _NclIfloattoshort,args,"floattoshort",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,nargs,"float",NclANY,NclANY);nargs++;
	NclRegisterFunc( _NclIfloattoint,args,"floattoint",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,nargs,"float",NclANY,NclANY);nargs++;
	NclRegisterFunc( _NclIfloattolong,args,"floattolong",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,nargs,"double",NclANY,NclANY);nargs++;
	NclRegisterFunc( _NclIdoubletobyte,args,"doubletobyte",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,nargs,"double",NclANY,NclANY);nargs++;
	NclRegisterFunc( _NclIdoubletochar,args,"doubletochar",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,nargs,"double",NclANY,NclANY);nargs++;
	NclRegisterFunc( _NclIdoubletoshort,args,"doubletoshort",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,nargs,"double",NclANY,NclANY);nargs++;
	NclRegisterFunc( _NclIdoubletoint,args,"doubletoint",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,nargs,"double",NclANY,NclANY);nargs++;
	NclRegisterFunc( _NclIdoubletolong,args,"doubletolong",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,nargs,"double",NclANY,NclANY);nargs++;
	NclRegisterFunc( _NclIdoubletofloat,args,"doubletofloat",nargs);


	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,nargs,"character",NclANY,NclANY);nargs++;
	NclRegisterFunc( _NclIchartodouble,args,"chartodouble",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,nargs,"character",NclANY,NclANY);nargs++;
	NclRegisterFunc( _NclIchartofloat,args,"chartofloat",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,nargs,"character",NclANY,NclANY);nargs++;
	NclRegisterFunc( _NclIchartolong,args,"chartolong",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,nargs,"character",NclANY,NclANY);nargs++;
	NclRegisterFunc( _NclIchartoshort,args,"chartoshort",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,nargs,"character",NclANY,NclANY);nargs++;
	NclRegisterFunc( _NclIchartoint,args,"chartoint",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,nargs,"character",NclANY,NclANY);nargs++;
	NclRegisterFunc( _NclIchartostring,args,"chartostring",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,nargs,"string",NclANY,NclANY);nargs++;
	NclRegisterFunc( _NclIstringtochar,args,"stringtochar",nargs);
	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,nargs,"string",NclANY,NclANY); nargs++;
	NclRegisterFunc( _NclIIsVar,args,"isvar",nargs);

	nargs = 0;
	args = NewArgs(2);
	dimsizes[0] = 1;
	SetArgTemplate(args,nargs,"file",1,dimsizes); nargs++;
	SetArgTemplate(args,nargs,"string",NclANY,NclANY); nargs++;
	NclRegisterFunc( _NclIIsFileVar,args,"isfilevar",nargs);

	nargs = 0;
	args = NewArgs(2);
	SetArgTemplate(args,nargs,NclANY,NclANY,NclANY); nargs++;
	SetArgTemplate(args,nargs,"string",NclANY,NclANY); nargs++;
	NclRegisterFunc( _NclIIsAtt,args,"isatt",nargs);

	nargs = 0;
	args = NewArgs(3);
	dimsizes[0] = 1;
	SetArgTemplate(args,nargs,"file",1,dimsizes); nargs++;
	SetArgTemplate(args,nargs,"string",1,dimsizes); nargs++;
	SetArgTemplate(args,nargs,"string",NclANY,NclANY); nargs++;
	NclRegisterFunc( _NclIIsFileVarDim,args,"isfilevardim",nargs);

	nargs = 0;
	args = NewArgs(3);
	dimsizes[0] = 1;
	SetArgTemplate(args,nargs,"file",1,dimsizes); nargs++;
	SetArgTemplate(args,nargs,"string",1,dimsizes); nargs++;
	SetArgTemplate(args,nargs,"string",NclANY,NclANY); nargs++;
	NclRegisterFunc( _NclIIsFileVarAtt,args,"isfilevaratt",nargs);
	
	nargs = 0;
	args = NewArgs(2);
	SetArgTemplate(args,nargs,NclANY,NclANY,NclANY); nargs++;
	SetArgTemplate(args,nargs,"string",NclANY,NclANY); nargs++;
	NclRegisterFunc( _NclIIsDim,args,"isdim",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,nargs,"numeric",NclANY,NclANY); nargs++;
	NclRegisterFunc( _Nclproduct,args,"product",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,nargs,"numeric",NclANY,NclANY); nargs++;
	NclRegisterFunc( _Ncldim_product,args,"dim_product",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,nargs,"numeric",NclANY,NclANY); nargs++;
	NclRegisterFunc( _Ncldim_sum,args,"dim_sum",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,nargs,"numeric",NclANY,NclANY); nargs++;
	NclRegisterFunc( _Nclsum,args,"sum",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,nargs,"numeric",NclANY,NclANY); nargs++;
	NclRegisterFunc( _Nclavg,args,"avg",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,nargs,"numeric",NclANY,NclANY); nargs++;
	NclRegisterFunc( _Ncldim_avg,args,"dim_avg",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,nargs,NclANY,NclANY,NclANY); nargs++;
	NclRegisterFunc( _Nclndto1d,args,"ndtooned",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,nargs,"logical",1,NclANY); nargs++;
	NclRegisterFunc( _Nclnum,args,"num",nargs);

	nargs = 0;
	args = NewArgs(1);
	SetArgTemplate(args,nargs,"logical",1,NclANY); nargs++;
	NclRegisterFunc( _Nclind,args,"ind",nargs);

	nargs = 0;
	args = NewArgs(2);
	SetArgTemplate(args,nargs,NclANY,NclANY,NclANY); nargs++;
	SetArgTemplate(args,nargs,"integer",1,NclANY); nargs++;
	NclRegisterFunc( _Ncl1dtond,args,"onedtond",nargs);
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

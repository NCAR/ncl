


/*
 *      $Id: AddHLUObjs.c,v 1.10 1995-03-25 21:55:25 ethan Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1994			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		AddHLUObjs.c
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Feb 7 11:07:41 MST 1994
 *
 *	Description:	
 */
#ifdef __cpluplus
extern "C" {
#endif
#include <stdio.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/BaseP.h>

extern void _NclAddSingleObj(
#if	NhlNeedProto
char * /*name*/,
struct _NhlLayerClassRec * /* the_ptr */
#endif
);


extern NhlLayerClass NhltickMarkLayerClass;
extern NhlLayerClass NhltitleLayerClass;
extern NhlLayerClass NhlxWorkstationLayerClass;
extern NhlLayerClass NhlncgmWorkstationLayerClass;
extern NhlLayerClass NhlcontourLayerClass;
extern NhlLayerClass NhltextItemLayerClass;
extern NhlLayerClass NhlxyPlotLayerClass;
extern NhlLayerClass NhllabelBarLayerClass;
extern NhlLayerClass NhllegendLayerClass;
extern NhlLayerClass NhlcoordArraysLayerClass;
extern NhlLayerClass NhlscalarFieldLayerClass;
extern NhlLayerClass NhlmapPlotLayerClass;
extern NhlLayerClass NhlappLayerClass;
extern NhlLayerClass NhlannotationLayerClass;
extern NhlLayerClass NhlpsWorkstationLayerClass;




void _NclAddHLUObjs
#if	NhlNeedProto
(void)
#else
()
#endif
{
	_NclAddSingleObj(NhltickMarkLayerClass->base_class.class_name,NhltickMarkLayerClass);
	_NclAddSingleObj(NhltitleLayerClass->base_class.class_name,NhltitleLayerClass);
	_NclAddSingleObj(NhlxWorkstationLayerClass->base_class.class_name,NhlxWorkstationLayerClass);
	_NclAddSingleObj(NhlncgmWorkstationLayerClass->base_class.class_name,NhlncgmWorkstationLayerClass);
	_NclAddSingleObj(NhlcontourLayerClass->base_class.class_name,NhlcontourLayerClass);
	_NclAddSingleObj(NhltextItemLayerClass->base_class.class_name,NhltextItemLayerClass);
	_NclAddSingleObj(NhlxyPlotLayerClass->base_class.class_name,NhlxyPlotLayerClass);
	_NclAddSingleObj(NhllabelBarLayerClass->base_class.class_name,NhllabelBarLayerClass);
	_NclAddSingleObj(NhllegendLayerClass->base_class.class_name,NhllegendLayerClass);
	_NclAddSingleObj(NhlcoordArraysLayerClass->base_class.class_name,NhlcoordArraysLayerClass);
	_NclAddSingleObj(NhlscalarFieldLayerClass->base_class.class_name,NhlscalarFieldLayerClass);
	_NclAddSingleObj(NhlmapPlotLayerClass->base_class.class_name,NhlmapPlotLayerClass);
	_NclAddSingleObj(NhlappLayerClass->base_class.class_name,NhlappLayerClass);
	_NclAddSingleObj(NhlannotationLayerClass->base_class.class_name,NhlannotationLayerClass);
	_NclAddSingleObj(NhlpsWorkstationLayerClass->base_class.class_name,NhlpsWorkstationLayerClass);
	return;
	
}

#ifdef __cpluplus
}
#endif

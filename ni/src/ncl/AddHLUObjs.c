


/*
 *      $Id: AddHLUObjs.c,v 1.14 1995-11-29 00:26:32 dbrown Exp $
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
struct _NhlClassRec * /* the_ptr */
#endif
);


extern NhlClass NhltickMarkClass;
extern NhlClass NhltitleClass;
extern NhlClass NhlxWorkstationClass;
extern NhlClass NhlncgmWorkstationClass;
extern NhlClass NhlcontourPlotClass;
extern NhlClass NhltextItemClass;
extern NhlClass NhlxyPlotClass;
extern NhlClass NhllabelBarClass;
extern NhlClass NhllegendClass;
extern NhlClass NhlcoordArraysClass;
extern NhlClass NhlscalarFieldClass;
extern NhlClass NhlmapPlotClass;
extern NhlClass NhllogLinPlotClass;
extern NhlClass NhlirregularPlotClass;
extern NhlClass NhlmapPlotClass;
extern NhlClass NhlappClass;
extern NhlClass NhlannoManagerClass;
extern NhlClass NhlpsWorkstationClass;
extern NhlClass NhlvectorPlotClass;
extern NhlClass NhlvectorFieldClass;




void _NclAddHLUObjs
#if	NhlNeedProto
(void)
#else
()
#endif
{
	_NclAddSingleObj(NhltickMarkClass->base_class.class_name,NhltickMarkClass);
	_NclAddSingleObj(NhltitleClass->base_class.class_name,NhltitleClass);
	_NclAddSingleObj(NhlxWorkstationClass->base_class.class_name,NhlxWorkstationClass);
	_NclAddSingleObj(NhlncgmWorkstationClass->base_class.class_name,NhlncgmWorkstationClass);
	_NclAddSingleObj(NhlcontourPlotClass->base_class.class_name,NhlcontourPlotClass);
	_NclAddSingleObj(NhltextItemClass->base_class.class_name,NhltextItemClass);
	_NclAddSingleObj(NhlxyPlotClass->base_class.class_name,NhlxyPlotClass);
	_NclAddSingleObj(NhllabelBarClass->base_class.class_name,NhllabelBarClass);
	_NclAddSingleObj(NhllegendClass->base_class.class_name,NhllegendClass);
	_NclAddSingleObj(NhlcoordArraysClass->base_class.class_name,NhlcoordArraysClass);
	_NclAddSingleObj(NhlscalarFieldClass->base_class.class_name,NhlscalarFieldClass);
	_NclAddSingleObj(NhlmapPlotClass->base_class.class_name,NhlmapPlotClass);
	_NclAddSingleObj(NhlappClass->base_class.class_name,NhlappClass);
	_NclAddSingleObj(NhlannoManagerClass->base_class.class_name,NhlannoManagerClass);
	_NclAddSingleObj(NhlpsWorkstationClass->base_class.class_name,NhlpsWorkstationClass);
	_NclAddSingleObj(NhllogLinPlotClass->base_class.class_name,NhllogLinPlotClass);
	_NclAddSingleObj(NhlirregularPlotClass->base_class.class_name,NhlirregularPlotClass);
	_NclAddSingleObj(NhlvectorPlotClass->base_class.class_name,NhlvectorPlotClass);
	_NclAddSingleObj(NhlvectorFieldClass->base_class.class_name,NhlvectorFieldClass);
	return;
	
}

#ifdef __cpluplus
}
#endif

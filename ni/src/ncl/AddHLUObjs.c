


/*
 *      $Id: AddHLUObjs.c,v 1.5 1994-08-08 22:34:48 ethan Exp $
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

extern void _NclAddSingleObj(
#ifdef NhlNeedProto
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
extern NhlLayerClass NhlxyDataDepLayerClass;
extern NhlLayerClass NhllabelBarLayerClass;
extern NhlLayerClass NhllegendLayerClass;
extern NhlLayerClass NhlcoordArraysLayerClass;
extern NhlLayerClass NhlscalarFieldLayerClass;
extern NhlLayerClass NhlmapPlotLayerClass;




void _NclAddHLUObjs
#if __STDC__
(void)
#else
()
#endif
{
	_NclAddSingleObj("tickMarkLayerClass",NhltickMarkLayerClass);
	_NclAddSingleObj("titleLayerClass",NhltitleLayerClass);
	_NclAddSingleObj("xWorkstationLayerClass",NhlxWorkstationLayerClass);
	_NclAddSingleObj("ncgmWorkstationLayerClass",NhlncgmWorkstationLayerClass);
	_NclAddSingleObj("contourLayerClass",NhlcontourLayerClass);
	_NclAddSingleObj("textItemLayerClass",NhltextItemLayerClass);
	_NclAddSingleObj("xyPlotLayerClass",NhlxyPlotLayerClass);
	_NclAddSingleObj("xyDataDepLayerClass",NhlxyDataDepLayerClass);
	_NclAddSingleObj("labelBarLayerClass",NhllabelBarLayerClass);
	_NclAddSingleObj("legendLayerClass",NhllegendLayerClass);
	_NclAddSingleObj("coordArraysLayerClass",NhlcoordArraysLayerClass);
	_NclAddSingleObj("scalarFieldLayerClass",NhlscalarFieldLayerClass);
	_NclAddSingleObj("mapPlotLayerClass",NhlmapPlotLayerClass);
	return;
	
}

#ifdef __cpluplus
}
#endif

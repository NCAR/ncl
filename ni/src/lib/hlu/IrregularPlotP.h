/*
 *      $Id: IrregularPlotP.h,v 1.1 1993-11-20 01:06:03 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		IrregularPlotP.h
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Oct 2 15:01:59 MDT 1992
 *
 *	Description:	Provides generic hooks for plot classes to assign
 *			functions that compute the forward and backward 
 *			data transformations to support point-n-click 
 *			features.
 */

#ifndef _NIrregularPlotP_h
#define _NIrregularPlotP_h

#include <ncarg/hlu/TransformP.h>
#include <ncarg/hlu/IrregularType2TransObj.h>
#include <ncarg/hlu/IrregularTransObj.h>
#include <ncarg/hlu/IrregularPlot.h>

typedef struct IrregularPlotLayerPart {
	/* User settable resource fields */
	int foo;
	/* Private Fields */
} IrregularPlotLayerPart;

typedef struct _IrregularPlotLayerRec {
	BaseLayerPart		base;
	ViewLayerPart		view;
	TransformLayerPart	trans;
	IrregularPlotLayerPart	irrplot;
} IrregularPlotLayerRec;

typedef struct IrregularPlotLayerClassPart{
	void *foo;
} IrregularPlotLayerClassPart;

typedef struct _IrregularPlotLayerClassRec{
	BaseLayerClassPart		base_class;
	ViewLayerClassPart		view_class;
	TransformLayerClassPart		trans_class;
	IrregularPlotLayerClassPart	irrplot_class;
} IrregularPlotLayerClassRec;

extern IrregularPlotLayerClassRec irregularPlotLayerClassRec;

#endif  /* _NIrregularPlotP_h */

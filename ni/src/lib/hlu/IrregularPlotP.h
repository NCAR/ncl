/*
 *      $Id: IrregularPlotP.h,v 1.5 1995-04-01 00:04:02 dbrown Exp $
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

typedef struct _NhlIrregularPlotLayerPart {

	/* Public resources */

	/* Private resources */

	NhlBoolean		update_req;

	/* Private Fields */

	NhlLayer		overlay_object;

} NhlIrregularPlotLayerPart;

typedef struct _NhlIrregularPlotLayerRec {
	NhlBaseLayerPart		base;
	NhlViewLayerPart		view;
	NhlTransformLayerPart		trans;
	NhlIrregularPlotLayerPart	irrplot;
} NhlIrregularPlotLayerRec;

typedef struct NhlIrregularPlotLayerClassPart{
	void *foo;
} NhlIrregularPlotLayerClassPart;

typedef struct _NhlIrregularPlotLayerClassRec{
	NhlBaseLayerClassPart		base_class;
	NhlViewLayerClassPart		view_class;
	NhlTransformLayerClassPart	trans_class;
	NhlIrregularPlotLayerClassPart	irrplot_class;
} NhlIrregularPlotLayerClassRec;

typedef struct _NhlIrregularPlotLayerClassRec *NhlIrregularPlotLayerClass;
typedef struct _NhlIrregularPlotLayerRec *NhlIrregularPlotLayer;

extern NhlIrregularPlotLayerClassRec NhlirregularPlotLayerClassRec;

#endif  /* _NIrregularPlotP_h */

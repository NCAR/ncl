/*
 *      $Id: MapPlotP.h,v 1.4 1994-01-27 21:24:43 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		MapPlotP.h
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Oct 2 15:01:59 MDT 1992
 *
 *	Description:	Generic Map Plotting object
 */

#ifndef _NMapPlotP_h
#define _NMapPlotP_h

#include <ncarg/hlu/TransformP.h>
#include <ncarg/hlu/MapTransObj.h>
#include <ncarg/hlu/MapPlot.h>

typedef struct NhlMapPlotLayerPart {

	/* Public resources */

	/* Private Fields */

	NhlLayer		overlay_object;

} NhlMapPlotLayerPart;

typedef struct _NhlMapPlotLayerRec {
	NhlBaseLayerPart	base;
	NhlViewLayerPart	view;
	NhlTransformLayerPart	trans;
	NhlMapPlotLayerPart	mapplot;
} NhlMapPlotLayerRec;

typedef struct NhlMapPlotLayerClassPart{
	void *foo;
} NhlMapPlotLayerClassPart;

typedef struct _NhlMapPlotLayerClassRec{
	NhlBaseLayerClassPart		base_class;
	NhlViewLayerClassPart		view_class;
	NhlTransformLayerClassPart	trans_class;
	NhlMapPlotLayerClassPart	mapplot_class;
} NhlMapPlotLayerClassRec;

typedef struct _NhlMapPlotLayerClassRec *NhlMapPlotLayerClass;
typedef struct _NhlMapPlotLayerRec *NhlMapPlotLayer;

extern NhlMapPlotLayerClassRec NhlmapPlotLayerClassRec;

#endif  /* _NMapPlotP_h */

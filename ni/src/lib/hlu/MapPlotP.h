/*
 *      $Id: MapPlotP.h,v 1.3 1994-01-12 00:34:46 dbrown Exp $
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

typedef struct MapPlotLayerPart {

	/* Public resources */

	/* Private Fields */

	Layer		overlay_object;

} MapPlotLayerPart;

typedef struct _MapPlotLayerRec {
	BaseLayerPart		base;
	ViewLayerPart		view;
	TransformLayerPart	trans;
	MapPlotLayerPart	mapplot;
} MapPlotLayerRec;

typedef struct MapPlotLayerClassPart{
	void *foo;
} MapPlotLayerClassPart;

typedef struct _MapPlotLayerClassRec{
	BaseLayerClassPart	base_class;
	ViewLayerClassPart	view_class;
	TransformLayerClassPart	trans_class;
	MapPlotLayerClassPart	mapplot_class;
} MapPlotLayerClassRec;

extern MapPlotLayerClassRec mapPlotLayerClassRec;

#endif  /* _NMapPlotP_h */

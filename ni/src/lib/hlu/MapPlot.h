/*
 *      $Id: MapPlot.h,v 1.1 1993-11-20 01:06:14 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		MapPlot.h
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Nov 16 15:18:58 MST 1993
 *
 *	Description:	Public header for MapPlot class.
 */

#ifndef _NMapPlot_h
#define _NMapPlot_h

#include <ncarg/hlu/Overlay.h>

/*
 * MapPlot class resources
 */

typedef struct _MapPlotLayerClassRec *MapPlotLayerClass;
typedef struct _MapPlotLayerRec *MapPlotLayer;

extern LayerClass mapPlotLayerClass;

#endif /*_NMapPlot_h */

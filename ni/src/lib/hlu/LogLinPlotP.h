/*
 *      $Id: LogLinPlotP.h,v 1.1 1993-11-20 01:06:10 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		LogLinPlotP.h
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Oct 2 15:01:59 MDT 1992
 *
 *	Description:	Generic Log-Linear plot object
 */

#ifndef _NLogLinPlotP_h
#define _NLogLinPlotP_h

#include <ncarg/hlu/TransformP.h>
#include <ncarg/hlu/LogLinTransObj.h>
#include <ncarg/hlu/LogLinPlot.h>

typedef struct LogLinPlotLayerPart {

	/* Public resources */

	NhlBoolean	overlay_plot_base;
	float 		x_min;
	float		x_max;
	int		x_log;
	int 		x_reverse;
	float 		y_min;
	float		y_max;
	int		y_log;
	int 		y_reverse;

	/* Private Fields */

	Layer		trans;
	Layer		overlay;

} LogLinPlotLayerPart;

typedef struct _LogLinPlotLayerRec {
	BaseLayerPart		base;
	ViewLayerPart		view;
	TransformLayerPart	trans;
	LogLinPlotLayerPart	llplot;
} LogLinPlotLayerRec;

typedef struct LogLinPlotLayerClassPart{
	void *foo;
} LogLinPlotLayerClassPart;

typedef struct _LogLinPlotLayerClassRec{
	BaseLayerClassPart		base_class;
	ViewLayerClassPart		view_class;
	TransformLayerClassPart		trans_class;
	LogLinPlotLayerClassPart	llplot_class;
} LogLinPlotLayerClassRec;

extern LogLinPlotLayerClassRec logLinPlotLayerClassRec;

#endif  /* _NLogLinPlotP_h */

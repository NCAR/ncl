/*
 *      $Id: LogLinPlotP.h,v 1.3 1994-01-27 21:24:21 boote Exp $
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
#include <ncarg/hlu/LogLinTransObjP.h>
#include <ncarg/hlu/LogLinPlot.h>

typedef struct NhlLogLinPlotLayerPart {

	/* Public resources */

	/* Private Fields */

	NhlLayer		overlay_object;

} NhlLogLinPlotLayerPart;

typedef struct _NhlLogLinPlotLayerRec {
	NhlBaseLayerPart		base;
	NhlViewLayerPart		view;
	NhlTransformLayerPart		trans;
	NhlLogLinPlotLayerPart		llplot;
} NhlLogLinPlotLayerRec;

typedef struct NhlLogLinPlotLayerClassPart{
	void *foo;
} NhlLogLinPlotLayerClassPart;

typedef struct _NhlLogLinPlotLayerClassRec{
	NhlBaseLayerClassPart		base_class;
	NhlViewLayerClassPart		view_class;
	NhlTransformLayerClassPart	trans_class;
	NhlLogLinPlotLayerClassPart	llplot_class;
} NhlLogLinPlotLayerClassRec;

typedef struct _NhlLogLinPlotLayerClassRec	*NhlLogLinPlotLayerClass;
typedef struct _NhlLogLinPlotLayerRec	*NhlLogLinPlotLayer;

extern NhlLogLinPlotLayerClassRec NhllogLinPlotLayerClassRec;

#endif  /* _NLogLinPlotP_h */

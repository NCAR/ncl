/*
 *      $Id: IrregularPlotP.h,v 1.8 1996-10-31 23:06:20 dbrown Exp $
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
#include <ncarg/hlu/IrregularTransObj.h>
#include <ncarg/hlu/IrregularPlot.h>

typedef struct _NhlIrregularPlotLayerPart {

	/* Public resources */

	/* Private resources */

	NhlBoolean		update_req;

	/* Private Fields */

	int			trans_change_count;
	NhlLayer		overlay_object;

} NhlIrregularPlotLayerPart;

typedef struct _NhlIrregularPlotLayerRec {
	NhlBaseLayerPart		base;
	NhlViewLayerPart		view;
	NhlTransformLayerPart		trans;
	NhlIrregularPlotLayerPart	irrplot;
} NhlIrregularPlotLayerRec;

typedef struct NhlIrregularPlotClassPart{
	void *foo;
} NhlIrregularPlotClassPart;

typedef struct _NhlIrregularPlotClassRec{
	NhlBaseClassPart		base_class;
	NhlViewClassPart		view_class;
	NhlTransformClassPart	trans_class;
	NhlIrregularPlotClassPart	irrplot_class;
} NhlIrregularPlotClassRec;

typedef struct _NhlIrregularPlotClassRec *NhlIrregularPlotClass;
typedef struct _NhlIrregularPlotLayerRec *NhlIrregularPlotLayer;

extern NhlIrregularPlotClassRec NhlirregularPlotClassRec;

#endif  /* _NIrregularPlotP_h */

/*
 *      $Id: LogLinPlotP.h,v 1.6 1996-10-31 23:06:24 dbrown Exp $
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

	/* Private resources */

	NhlBoolean		update_req;

	/* Private Fields */

	int			trans_change_count;
	NhlLayer		overlay_object;

} NhlLogLinPlotLayerPart;

typedef struct _NhlLogLinPlotLayerRec {
	NhlBaseLayerPart		base;
	NhlViewLayerPart		view;
	NhlTransformLayerPart		trans;
	NhlLogLinPlotLayerPart		llplot;
} NhlLogLinPlotLayerRec;

typedef struct NhlLogLinPlotClassPart{
	void *foo;
} NhlLogLinPlotClassPart;

typedef struct _NhlLogLinPlotClassRec{
	NhlBaseClassPart		base_class;
	NhlViewClassPart		view_class;
	NhlTransformClassPart	trans_class;
	NhlLogLinPlotClassPart	llplot_class;
} NhlLogLinPlotClassRec;

typedef struct _NhlLogLinPlotClassRec	*NhlLogLinPlotClass;
typedef struct _NhlLogLinPlotLayerRec	*NhlLogLinPlotLayer;

extern NhlLogLinPlotClassRec NhllogLinPlotClassRec;

#endif  /* _NLogLinPlotP_h */

/*
 *      $Id: ContourP.h,v 1.3 1994-01-27 21:21:36 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		ContourP.h
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Oct 2 15:01:59 MDT 1992
 *
 *	Description:	Contour plot object private header file
 */

#ifndef _NContourP_h
#define _NContourP_h

#include <ncarg/hlu/TransformP.h>
#include <ncarg/hlu/LogLinTransObjP.h>
#include <ncarg/hlu/OverlayI.h>
#include <ncarg/hlu/Contour.h>

typedef struct _NhlContourLayerPart {

	/* Public resources */

	float		out_of_range_val;

	float 		x_min;
	float		x_max;
	NhlBoolean	x_log;
	NhlBoolean	x_reverse;
	float 		y_min;
	float		y_max;
	NhlBoolean	y_log;
	NhlBoolean	y_reverse;

	/* Private Fields */

	NhlLayer	overlay_object;

} NhlContourLayerPart;

typedef struct _NhlContourLayerRec {
	NhlBaseLayerPart	base;
	NhlViewLayerPart	view;
	NhlTransformLayerPart	trans;
	NhlContourLayerPart	contour;
} NhlContourLayerRec;

typedef struct NhlContourLayerClassPart{
	void *foo;
} NhlContourLayerClassPart;

typedef struct _NhlContourLayerClassRec{
	NhlBaseLayerClassPart		base_class;
	NhlViewLayerClassPart		view_class;
	NhlTransformLayerClassPart	trans_class;
	NhlContourLayerClassPart	contour_class;
} NhlContourLayerClassRec;

typedef struct _NhlContourLayerClassRec	*NhlContourLayerClass;
typedef struct _NhlContourLayerRec	*NhlContourLayer;

extern NhlContourLayerClassRec		NhlcontourLayerClassRec;

#endif  /* _NContourP_h */

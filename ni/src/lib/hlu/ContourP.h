/*
 *      $Id: ContourP.h,v 1.2 1993-12-22 00:55:45 dbrown Exp $
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
#include <ncarg/hlu/Contour.h>

typedef struct ContourLayerPart {

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

	Layer		overlay_object;

} ContourLayerPart;

typedef struct _ContourLayerRec {
	BaseLayerPart		base;
	ViewLayerPart		view;
	TransformLayerPart	trans;
	ContourLayerPart	contour;
} ContourLayerRec;

typedef struct ContourLayerClassPart{
	void *foo;
} ContourLayerClassPart;

typedef struct _ContourLayerClassRec{
	BaseLayerClassPart		base_class;
	ViewLayerClassPart		view_class;
	TransformLayerClassPart		trans_class;
	ContourLayerClassPart		contour_class;
} ContourLayerClassRec;

extern ContourLayerClassRec		contourLayerClassRec;

#endif  /* _NContourP_h */

/*
 *      $Id: XyPlotP.h,v 1.4 1993-11-10 01:19:48 ethan Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		XyPlotP.h
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Dec 30 13:17:21 MST 1992
 *
 *	Description:	
 */
#ifndef _NXyPlotP_h
#define _NXyPlotP_h

#include <ncarg/hlu/DataCommP.h>
#include <ncarg/hlu/XyPlot.h>
#include <ncarg/hlu/TickMark.h>

typedef struct _XyDataDepLayerPart{
	/* Public resources	*/
	NhlGenArray	colors;
	int		color;

	NhlGenArray	dash_patterns;
	int		dash;

	LineLabelModes	label_mode;
	NhlGenArray	labels;

	/* Private fields	*/
} XyDataDepLayerPart;

typedef struct _XyPlotLayerPart {
	/* Publically setable resources */

	/* DataResources should use the NhlPointer type */
	NhlGenArray		curve_data;

	float			curve_thickness;

	TickMarkStyles		x_style;
	TickMarkStyles 		y_style;

	NhlGenArray		x_irregular_points;
	NhlGenArray		y_irregular_points;

	NhlBoolean		x_reverse;
	NhlBoolean		y_reverse;

	NhlBoolean		compute_x_min;
	NhlBoolean		compute_x_max;
	NhlBoolean		compute_y_max;
	NhlBoolean		compute_y_min;
	float			x_min;
	float			x_max;
	float			y_max;
	float			y_min;

	AlternatePlace		x_alternate;
	AlternatePlace		y_alternate;

	NhlGenArray		x_alternate_coords;
	NhlGenArray		x_original_coords;
	NhlGenArray		y_alternate_coords;
	NhlGenArray		y_original_coords;

	NhlBoolean		titles;


	float			line_label_font_height;
	float			dash_segment_length;
	
	float 			ti_main_offset_x;
	float			ti_x_axis_offset_x;
	float			ti_y_axis_offset_y;
	TitlePositions 		ti_main_position;
	TitlePositions		ti_x_axis_position;
	TitlePositions		ti_y_axis_position;

	float			x_tension;
	float			y_tension;

	/* Private fields */
	Layer		ticks;
	Layer		ttitles;
	Layer		thetrans;
	NhlBoolean	have_irreg_trans;
	NhlBoolean	fake_x;
	float		fake_x_max;
	float		fake_x_min;
	NhlBoolean	fake_y;
	float		fake_y_max;
	float		fake_y_min;

	NhlBoolean	x_min_set;
	NhlBoolean	x_max_set;
	NhlBoolean	y_max_set;
	NhlBoolean	y_min_set;
	NhlBoolean	comp_x_min_set;
	NhlBoolean	comp_x_max_set;
	NhlBoolean	comp_y_max_set;
	NhlBoolean	comp_y_min_set;

	NhlBoolean	data_ranges_set;
	NhlBoolean	check_ranges;

	float		x_data_min;
	float		x_data_max;

	float		y_data_min;
	float		y_data_max;

	float		x_irreg_min;
	float		x_irreg_max;
	float		y_irreg_min;
	float		y_irreg_max;

	float		real_main_offset_x;
	float		real_x_axis_offset_x;
	float		real_y_axis_offset_y;
}XyPlotLayerPart;

typedef struct _XyDataDepLayerRec{
	ObjLayerPart		base;
	DataSpecLayerPart	dataspec;
	XyDataDepLayerPart	xydata;
} XyDataDepLayerRec;

typedef struct _XyPlotLayerRec {
	BaseLayerPart		base;
	ViewLayerPart		view;
	TransformLayerPart	trans;
	DataCommLayerPart	datacomm;
	XyPlotLayerPart		xyplot;
}XyPlotLayerRec;

typedef struct _XyDataDepLayerClassPart{
	int	foo;
} XyDataDepLayerClassPart;

typedef struct _XyPlotLayerClassPart {
	char *foo;
} XyPlotLayerClassPart;

typedef struct _XyDataDepLayerClassRec{
	ObjLayerClassPart	base_class;
	DataSpecLayerClassPart	dataspec_class;
	XyDataDepLayerClassPart	xydata_class;
} XyDataDepLayerClassRec;

typedef struct _XyPlotLayerClassRec {
	BaseLayerClassPart	base_class;
	ViewLayerClassPart	view_class;
	TransformLayerClassPart	trans_class;
	DataCommLayerClassPart	datacomm_class;
	XyPlotLayerClassPart	xyplot_class;
}XyPlotLayerClassRec;

extern XyDataDepLayerClassRec xyDataDepLayerClassRec;
extern XyPlotLayerClassRec xyPlotLayerClassRec;

extern LayerClass xyDataDepLayerClass;

typedef struct _XyDataDepLayerClassRec *XyDataDepLayerClass;
typedef struct _XyDataDepLayerRec *XyDataDepLayer;

#endif /* _NXyPlotP_h */

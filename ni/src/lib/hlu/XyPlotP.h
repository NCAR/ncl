/*
 *      $Id: XyPlotP.h,v 1.6 1994-08-19 20:37:01 ethan Exp $
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

typedef struct _NhlXyDataDepLayerPart{
	/* Public resources	*/
	NhlGenArray		colors;
	int			color;

	NhlGenArray		dash_patterns;
	int			dash;

	NhlGenArray		marker_colors;
	int			marker_color;

	NhlGenArray		marker_sizes;
	float			marker_size;
	
	NhlGenArray		marker_modes;
	NhlMarkerModes		marker_mode;

	NhlGenArray		markers;
	int			marker;
	
	NhlLineLabelModes	label_mode;
	NhlGenArray		labels;

	/* Private fields	*/
} NhlXyDataDepLayerPart;

typedef struct _NhlXyPlotLayerPart {
	/* Publically setable resources */

	/* DataResources should use the NhlPointer type */
	NhlGenArray		curve_data;

	float			curve_thickness;

	NhlTickMarkStyles	x_style;
	NhlTickMarkStyles 	y_style;

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

	NhlAlternatePlace	x_alternate;
	NhlAlternatePlace	y_alternate;

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
	NhlTitlePositions 	ti_main_position;
	NhlTitlePositions	ti_x_axis_position;
	NhlTitlePositions	ti_y_axis_position;

	float			x_tension;
	float			y_tension;

	/* Private fields */
	NhlLayer	ticks;
	NhlLayer	ttitles;
	NhlLayer	thetrans;
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
}NhlXyPlotLayerPart;

typedef struct _NhlXyDataDepLayerRec{
	NhlObjLayerPart		base;
	NhlDataSpecLayerPart	dataspec;
	NhlXyDataDepLayerPart	xydata;
} NhlXyDataDepLayerRec;

typedef struct _NhlXyPlotLayerRec {
	NhlBaseLayerPart		base;
	NhlViewLayerPart		view;
	NhlTransformLayerPart		trans;
	NhlDataCommLayerPart		datacomm;
	NhlXyPlotLayerPart		xyplot;
}NhlXyPlotLayerRec;

typedef struct _NhlXyDataDepLayerClassPart{
	int	foo;
} NhlXyDataDepLayerClassPart;

typedef struct _NhlXyPlotLayerClassPart {
	char *foo;
} NhlXyPlotLayerClassPart;

typedef struct _NhlXyDataDepLayerClassRec{
	NhlObjLayerClassPart		base_class;
	NhlDataSpecLayerClassPart	dataspec_class;
	NhlXyDataDepLayerClassPart	xydata_class;
} NhlXyDataDepLayerClassRec;

typedef struct _NhlXyPlotLayerClassRec {
	NhlBaseLayerClassPart		base_class;
	NhlViewLayerClassPart		view_class;
	NhlTransformLayerClassPart	trans_class;
	NhlDataCommLayerClassPart	datacomm_class;
	NhlXyPlotLayerClassPart		xyplot_class;
}NhlXyPlotLayerClassRec;

extern NhlXyDataDepLayerClassRec NhlxyDataDepLayerClassRec;
extern NhlXyPlotLayerClassRec NhlxyPlotLayerClassRec;

extern NhlLayerClass NhlxyDataDepLayerClass;

typedef struct _NhlXyDataDepLayerClassRec *NhlXyDataDepLayerClass;
typedef struct _NhlXyDataDepLayerRec *NhlXyDataDepLayer;

typedef struct _NhlXyPlotLayerClassRec *NhlXyPlotLayerClass;
typedef struct _NhlXyPlotLayerRec *NhlXyPlotLayer;

#endif /* _NXyPlotP_h */

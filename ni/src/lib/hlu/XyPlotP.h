/*
 *      $Id: XyPlotP.h,v 1.11 1995-04-01 00:04:26 dbrown Exp $
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
#ifndef _NXYPLOTP_h
#define _NXYPLOTP_h

#include <ncarg/hlu/DataCommP.h>
#include <ncarg/hlu/PlotManagerI.h>
#include <ncarg/hlu/XyPlot.h>
#include <ncarg/hlu/TickMark.h>

#define	_NhlNxyDSpecChanged	"xyDspec.Changed"
#define	_NhlCxyDSpecChanged	"XyDspec.Changed"

typedef struct _NhlXyDataSpecLayerPart{
	/* Public resources	*/
	NhlDashIndex		dash;
	NhlGenArray		dashes;
	NhlBoolean		mono_dash;

	NhlMarkLineMode		marker_mode;
	NhlGenArray		marker_modes;
	NhlBoolean		mono_marker_mode;

	NhlGenArray		lg_label_strings;

	NhlColorIndex		color;
	NhlGenArray		colors;
	NhlBoolean		mono_color;

	NhlColorIndex		label_color;
	NhlGenArray		label_colors;
	NhlBoolean		mono_label_color;

	NhlLineLabelMode	label_mode;
	NhlGenArray		labels;

	float			line_thickness;
	NhlGenArray		line_thicknesses;
	NhlBoolean		mono_line_thickness;

	NhlColorIndex		marker_color;
	NhlGenArray		marker_colors;
	NhlBoolean		mono_marker_color;

	NhlMarkerIndex		marker;
	NhlGenArray		markers;
	NhlBoolean		mono_marker;
	
	NhlBoolean		marker_size_set;
	float			marker_size;
	NhlGenArray		marker_sizes;
	NhlBoolean		mono_marker_size;
	
	float			marker_thickness;
	NhlGenArray		marker_thicknesses;
	NhlBoolean		mono_marker_thickness;
	
	NhlBoolean		dash_seg_len_set;
	float			dash_seg_len;

	NhlBoolean		llabel_fheight_set;
	float			llabel_fheight;
	
	/* Private fields	*/
} NhlXyDataSpecLayerPart;

typedef struct _NhlXyPlotLayerPart {
	/* Publically setable resources */

	NhlGenArray		curve_data;
	NhlGenArray		dspeclist;

	NhlTickMarkStyle	x_style;
	NhlTickMarkStyle 	y_style;

	float			x_tension;
	float			y_tension;

	NhlGenArray		x_irregular_points;
	NhlGenArray		y_irregular_points;

	NhlBoolean		x_reverse;
	NhlBoolean		y_reverse;

	NhlBoolean		comp_x_min_set;
	NhlBoolean		compute_x_min;
	NhlBoolean		comp_x_max_set;
	NhlBoolean		compute_x_max;
	NhlBoolean		comp_y_max_set;
	NhlBoolean		compute_y_max;
	NhlBoolean		comp_y_min_set;
	NhlBoolean		compute_y_min;

	NhlBoolean		x_min_set;
	float			x_min;
	NhlBoolean		x_max_set;
	float			x_max;
	NhlBoolean		y_max_set;
	float			y_max;
	NhlBoolean		y_min_set;
	float			y_min;

/*
 * These resources have not been implimented yet...
 */
#ifdef	NOT
	NhlAlternatePlace	x_alternate;
	NhlAlternatePlace	y_alternate;

	NhlGenArray		x_alternate_coords;
	NhlGenArray		x_original_coords;
	NhlGenArray		y_alternate_coords;
	NhlGenArray		y_original_coords;
#endif

	NhlAnnotationDisplayMode	display_legend;
	NhlAnnotationDisplayMode	display_titles;
	NhlAnnotationDisplayMode	display_tickmarks;

	/* Private fields */
	NhlLayer	thetrans;
	NhlBoolean	have_irreg_trans;
	NhlBoolean	fake_x;
	float		fake_x_max;
	float		fake_x_min;
	NhlBoolean	fake_y;
	float		fake_y_max;
	float		fake_y_min;

	NhlLayer	overlay;

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

	float		vp_average;

	int		num_cpairs;
	int		size_cpair_arrays;

	NhlGenArray	dash_indexes;
	NhlGenArray	item_types;
	NhlGenArray	lg_label_strings;
	NhlGenArray	line_colors;
	NhlGenArray	dash_seg_lens;
	NhlGenArray	llabel_colors;
	NhlGenArray	llabel_strings;
	NhlGenArray	llabel_fheights;
	NhlGenArray	line_thicknesses;
	NhlGenArray	marker_colors;
	NhlGenArray	marker_indexes;
	NhlGenArray	marker_sizes;
	NhlGenArray	marker_thicknesses;
	NhlGenArray	xvectors;
	NhlGenArray	yvectors;
	NhlGenArray	len_vectors;
	NhlGenArray	missing_set;
	NhlGenArray	xmissing;
	NhlGenArray	ymissing;

	NhlBoolean	dspec_changed;
}NhlXyPlotLayerPart;

typedef struct _NhlXyDataSpecLayerRec{
	NhlBaseLayerPart	base;
	NhlDataSpecLayerPart	dataspec;
	NhlXyDataSpecLayerPart	xydata;
} NhlXyDataSpecLayerRec;

typedef struct _NhlXyPlotLayerRec {
	NhlBaseLayerPart		base;
	NhlViewLayerPart		view;
	NhlTransformLayerPart		trans;
	NhlDataCommLayerPart		datacomm;
	NhlXyPlotLayerPart		xyplot;
}NhlXyPlotLayerRec;

typedef struct _NhlXyDataSpecLayerClassPart{
	int	foo;
} NhlXyDataSpecLayerClassPart;

typedef struct _NhlXyPlotLayerClassPart {
	char *foo;
} NhlXyPlotLayerClassPart;

typedef struct _NhlXyDataSpecLayerClassRec{
	NhlBaseLayerClassPart		base_class;
	NhlDataSpecLayerClassPart	dataspec_class;
	NhlXyDataSpecLayerClassPart	xydata_class;
} NhlXyDataSpecLayerClassRec;

typedef struct _NhlXyPlotLayerClassRec {
	NhlBaseLayerClassPart		base_class;
	NhlViewLayerClassPart		view_class;
	NhlTransformLayerClassPart	trans_class;
	NhlDataCommLayerClassPart	datacomm_class;
	NhlXyPlotLayerClassPart		xyplot_class;
}NhlXyPlotLayerClassRec;

extern NhlXyDataSpecLayerClassRec NhlxyDataSpecLayerClassRec;
extern NhlXyPlotLayerClassRec NhlxyPlotLayerClassRec;

extern NhlLayerClass NhlxyDataSpecLayerClass;

typedef struct _NhlXyDataSpecLayerClassRec *NhlXyDataSpecLayerClass;
typedef struct _NhlXyDataSpecLayerRec *NhlXyDataSpecLayer;

typedef struct _NhlXyPlotLayerClassRec *NhlXyPlotLayerClass;
typedef struct _NhlXyPlotLayerRec *NhlXyPlotLayer;

#endif /* _NXYPLOTP_h */

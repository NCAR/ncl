
/*
 *      $Id: XyPlotP.h,v 1.2 1993-06-03 15:12:25 ethan Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		
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

#include <ncarg/hlu/TransformP.h>
#include <ncarg/hlu/XyPlot.h>
#include <ncarg/hlu/TickMark.h>

typedef struct _XyPlotLayerPart {
	/* Publically setable resources */
	LineLabelModes curve_line_label_mode;
	int	num_curves;
	float	**x_values;
	float	**y_values;
	int	*curve_colors;
	int	*curve_lengths;
	int	*curve_dash_patterns;
	char	**curve_line_labels;
	float	curve_thickness;
	float	x_missing;
	float	y_missing;
	TickMarkStyles	x_style;
	TickMarkStyles 	y_style;
	int	clip;
	int 	x_num_irregular_points;
	float	*x_irregular_points;
	int 	y_num_irregular_points;
	float	*y_irregular_points;
	int	x_reverse;
	int	y_reverse;
	float	x_left;
	float	x_right;
	float	y_top;
	float	y_bottom;
	int	titles;
	AlternatePlace	x_alternate;
	AlternatePlace	y_alternate;
	int		x_num_alternate_coords;
	float		*x_alternate_coords;
	float		*x_original_coords;
	int		y_num_alternate_coords;
	float		*y_alternate_coords;
	float		*y_original_coords;
	float		line_label_font_height;
	float		dash_segment_length;
	
	float 		ti_main_offset_x;
	float		ti_x_axis_offset_x;
	float		ti_y_axis_offset_y;
	TitlePositions	ti_x_axis_position;
	TitlePositions	ti_y_axis_position;
	TitlePositions 	ti_main_position;
	int		ti_main_on;
	int		ti_x_axis_on;
	int		ti_y_axis_on;
	char*		ti_main_string;
	char*		ti_x_axis_string;
	char*		ti_y_axis_string;

	TickMarkModes 	tm_x_b_mode;
	TickMarkModes	tm_x_t_mode;
	TickMarkModes	tm_y_r_mode;
	TickMarkModes	tm_y_l_mode;
	float		tm_x_b_tick_start;
	float		tm_x_b_tick_end;
	float 		tm_x_b_tick_spacing;
	int		tm_x_b_spacing_type;
	float		tm_x_t_tick_start;
	float		tm_x_t_tick_end;
	float 		tm_x_t_tick_spacing;
	int		tm_x_t_spacing_type;
	float		tm_y_r_tick_start;
	float		tm_y_r_tick_end;
	float 		tm_y_r_tick_spacing;
	int		tm_y_r_spacing_type;
	float		tm_y_l_tick_start;
	float		tm_y_l_tick_end;
	float 		tm_y_l_tick_spacing;
	int		tm_y_l_spacing_type;

	/* Private fields */
	Layer	ticks;
	Layer	ttitles;
	Layer	thetrans;
	float	x_data_min;
	float	x_data_max;
	float	y_data_min;
	float	y_data_max;
	float   x_irr_min;
	float   x_irr_max;
	float	y_irr_min;
	float	y_irr_max;
	float   **x_final_values;
	float   **y_final_values;
	int	noxvalues;
	int	noyvalues;
	float	real_main_offset_x;
	float	real_x_axis_offset_x;
	float	real_y_axis_offset_y;
	float   *thexmissing;
	float   *theymissing;
	float   *dummy_array;
	float   *dummy_array_final;
	int	dummy_array_length;
	int	dash_dollar_size;
	int	char_size;
}XyPlotLayerPart;

typedef struct _XyPlotLayerRec {
	BaseLayerPart	base;
	ViewLayerPart	view;
	TransformLayerPart	trans;
	XyPlotLayerPart	xyplot;
}XyPlotLayerRec;

typedef struct _XyPlotLayerClassPart {
	char *foo;
} XyPlotLayerClassPart;

typedef struct _XyPlotLayerClassRec {
	BaseLayerClassPart	base_class;
	ViewLayerClassPart	view_class;
	TransformLayerClassPart	trans_class;
	XyPlotLayerClassPart	xyplot_class;
}XyPlotLayerClassRec;

extern XyPlotLayerClassRec xyPlotLayerClassRec;

#endif /* _NXyPlotP_h */



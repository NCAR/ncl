/*
 *      $Id: TickMarkP.h,v 1.3 1993-11-10 01:19:36 ethan Exp $
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
 *	Date:		Wed Dec 2 13:57:39 MST 1992
 *
 *	Description:	
 */
#ifndef  _NTickMarkP_h
#define _NTickMarkP_h 

#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/ViewP.h>
#include <ncarg/hlu/TickMark.h>
#include <ncarg/hlu/TextItem.h>

#define DEFAULTOFFSET 1.0

typedef struct _TickMarkLayerPart {
	/* Publically setable resources */
	int		sci_note_cutoff;
	int		x_use_bottom;
	int		x_b_on;
	int		x_t_on;
	int		x_b_labels_on;
	int		x_t_labels_on;
	int		x_b_border_on;
	int		x_t_border_on;
	TickMarkModes	x_b_mode;
	TickMarkModes	x_t_mode;
	TickMarkStyles	x_b_style;
	TickMarkStyles	x_t_style;
	float		x_t_tension;
	float		x_b_tension;
	int		x_b_precision;
	int		x_t_precision;
	float		border_thickness;
	int		border_line_color;
	int		x_major_grid;
	int		x_minor_grid;
	float		x_major_grid_thickness;
	int		x_major_grid_line_color;
	int		x_major_grid_line_dash_pattern;
	float		x_minor_grid_thickness;
	int		x_minor_grid_line_color;
	int		x_minor_grid_line_dash_pattern;
	int		x_b_minor_per_major;
	int		x_t_minor_per_major;
	int		x_b_no_minor;
	int		x_t_no_minor;
	int		x_b_label_stride;
	int		x_t_label_stride;
	float		x_b_data_left;
	float		x_b_data_right;
	float		x_b_tick_start;
	float		x_b_tick_end;
	int		x_b_max_ticks;
	float		x_b_tick_spacing;
	int		x_b_spacing_type;
	float		*x_b_irregular_points;
	int		x_b_num_irregular_points;
	float		*x_b_values;
	int		x_b_num_values;
	char		**x_b_labels;
	float		x_b_major_thickness;
	int		x_b_major_line_color;
	float 		x_b_major_length;
	float		x_b_major_outward_length;
	float		x_b_minor_thickness;
	int		x_b_minor_line_color;
	float		x_b_minor_length;
	float		x_b_minor_outward_length;
	int		x_b_label_font;	
	float		x_b_label_font_height;
	int		x_b_label_font_color;
	float		x_b_label_font_aspect;
	int		x_b_label_just;
	float		x_b_label_angle;
	TextDirection   x_b_label_direction;
	float		x_b_label_delta;
	int		x_b_auto_precision;
	float		x_t_data_left;
	float		x_t_data_right;
	float		x_t_tick_start;
	float		x_t_tick_end;
	int		x_t_max_ticks;
	float		x_t_tick_spacing;
	int		x_t_spacing_type;
	float		*x_t_irregular_points;
	int		x_t_num_irregular_points;
	float		*x_t_values;
	int		x_t_num_values;
	char		**x_t_labels;
	float		x_t_major_thickness;
	int		x_t_major_line_color;
	float 		x_t_major_length;
	float		x_t_major_outward_length;
	float		x_t_minor_thickness;
	int		x_t_minor_line_color;
	float		x_t_minor_length;
	float		x_t_minor_outward_length;
	int		x_t_label_font;	
	float		x_t_label_font_height;
	int		x_t_label_font_color;
	float		x_t_label_font_aspect;
	int		x_t_label_just;
	float		x_t_label_angle;
	TextDirection   x_t_label_direction;
	float		x_t_label_delta;
	int		x_t_auto_precision;
	int		y_use_left;
	int		y_r_on;
	int		y_l_on;
	int		y_r_labels_on;
	int		y_l_labels_on;
	int		y_r_border_on;
	int		y_l_border_on;
	TickMarkModes	y_r_mode;
	TickMarkModes	y_l_mode;
	TickMarkStyles	y_l_style;
	TickMarkStyles	y_r_style;
	float		y_l_tension;
	float		y_r_tension;
	int		y_l_precision;
	int		y_r_precision;
	int		y_major_grid;
	int		y_minor_grid;
	float		y_major_grid_thickness;
	int		y_major_grid_line_color;
	int		y_major_grid_line_dash_pattern;
	float		y_minor_grid_thickness;
	int		y_minor_grid_line_color;
	int		y_minor_grid_line_dash_pattern;
	int		y_r_minor_per_major;
	int		y_l_minor_per_major;
	int		y_r_no_minor;
	int		y_l_no_minor;
	int		y_r_label_stride;
	int		y_l_label_stride;
	float		y_l_data_top;
	float		y_l_data_bottom;
	float		y_l_tick_start;
	float		y_l_tick_end;
	int		y_l_max_ticks;
	float		y_l_tick_spacing;
	int		y_l_spacing_type;
	float		*y_l_irregular_points;
	int		y_l_num_irregular_points;
	float		*y_l_values;
	int		y_l_num_values;
	char		**y_l_labels;
	float		y_l_major_thickness;
	int		y_l_major_line_color;
	float 		y_l_major_length;
	float		y_l_major_outward_length;
	float		y_l_minor_thickness;
	int		y_l_minor_line_color;
	float		y_l_minor_length;
	float		y_l_minor_outward_length;
	int		y_l_label_font;	
	float		y_l_label_font_height;
	int		y_l_label_font_color;
	float		y_l_label_font_aspect;
	int		y_l_label_just;
	float		y_l_label_angle;
	TextDirection   y_l_label_direction;
	float		y_l_label_delta;
	int		y_l_auto_precision;
	float		y_r_data_top;
	float		y_r_data_bottom;
	float		y_r_tick_start;
	float		y_r_tick_end;
	int		y_r_max_ticks;
	float		y_r_tick_spacing;
	int		y_r_spacing_type;
	float		*y_r_irregular_points;
	int		y_r_num_irregular_points;
	float		*y_r_values;
	int		y_r_num_values;
	char		**y_r_labels;
	float		y_r_major_thickness;
	int		y_r_major_line_color;
	float 		y_r_major_length;
	float		y_r_major_outward_length;
	float		y_r_minor_thickness;
	int		y_r_minor_line_color;
	float		y_r_minor_length;
	float		y_r_minor_outward_length;
	int		y_r_label_font;	
	float		y_r_label_font_height;
	int		y_r_label_font_color;
	float		y_r_label_font_aspect;
	int		y_r_label_just;
	float		y_r_label_angle;
	TextDirection   y_r_label_direction;
	float		y_r_label_delta;
	int		y_r_auto_precision;
/* Private fields */
	Layer		xb_yl_trans_obj;  /* used to tranform tick mark data locations
					to tickmark NDC locations */
	Layer		xt_yr_trans_obj;
	Layer		xb_multi;
	Layer		xt_multi;
	Layer		yl_multi;
	Layer		yr_multi;

/* different than data_left/right/top/bottom these are used to know what
   the data ranges actually are */


	float 		x_b_data_min;
	float 		x_b_data_max;
	float 		x_t_data_min;
	float 		x_t_data_max;
	float 		y_l_data_min;
	float 		y_l_data_max;
	float 		y_r_data_min;
	float 		y_r_data_max;

	float 		ir_xbmin;
	float 		ir_xtmin;
	float 		ir_ylmin;
	float 		ir_yrmin;
	float 		ir_xbmax;
	float 		ir_xtmax;
	float 		ir_ylmax;
	float 		ir_yrmax;
	int		new_ir_xb;
	int		new_ir_xt;
	int		new_ir_yl;
	int		new_ir_yr;

	float		*x_b_major_ndc_locs;
	float		*x_b_major_data_locs;
	char		**x_b_major_labels;
	int		x_b_nmajor;
	float		*x_b_minor_ndc_locs;
	float		*x_b_minor_data_locs;
	int		x_b_nminor;
	float		x_b_ndc_label_y;

	float		*x_t_major_ndc_locs;
	float		*x_t_major_data_locs;
	char		**x_t_major_labels;
	int		x_t_nmajor;
	float		*x_t_minor_ndc_locs;
	float		*x_t_minor_data_locs;
	int		x_t_nminor;
	float		x_t_ndc_label_y;

	float		*y_l_major_ndc_locs;
	float		*y_l_major_data_locs;
	char		**y_l_major_labels;
	int		y_l_nmajor;
	float		*y_l_minor_ndc_locs;
	float		*y_l_minor_data_locs;
	int		y_l_nminor;
	float		y_l_ndc_label_x;

	float		*y_r_major_ndc_locs;
	float		*y_r_major_data_locs;
	char		**y_r_major_labels;
	int		y_r_nmajor;
	float		*y_r_minor_ndc_locs;
	float		*y_r_minor_data_locs;
	int		y_r_nminor;
	float		y_r_ndc_label_x;
}TickMarkLayerPart;

typedef struct _TickMarkLayerRec {
	BaseLayerPart	base;
	ViewLayerPart	view;
	TickMarkLayerPart	tick;
}TickMarkLayerRec;

typedef struct _TickMarkLayerClassPart {
	void *foo;
}TickMarkLayerClassPart;

typedef struct _TickMarkLayerClassRec {
	BaseLayerClassPart	base_class;
	ViewLayerClassPart	view_class;
	TickMarkLayerClassPart	tick_class;
}TickMarkLayerClassRec;

extern TickMarkLayerClassRec	tickMarkLayerClassRec;

#endif /* _NTickMarkP_h */

/*
 *      $Id: TickMarkP.h,v 1.19 2001-12-05 00:19:05 dbrown Exp $
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
#include <ncarg/hlu/FormatI.h>

#define	DEFAULTOFFSET	1.0
#define MAXTICKS	256
#define MAXMINORTICKS	1024
#define NhltmDEF_FORMAT "0@*+^sg"

typedef struct _NhlTickMarkLayerPart {
	/* Publically setable resources */
	int		sci_note_cutoff;
	NhlBoolean	equalize_xy_sizes;
	NhlBoolean	label_auto_stride;
	NhlBoolean	x_use_bottom;
	NhlBoolean	x_b_on;
	NhlBoolean	x_t_on;
	NhlBoolean	x_b_labels_on;
	NhlBoolean	x_t_labels_on;
	NhlBoolean	x_b_border_on;
	NhlBoolean	x_t_border_on;
	NhlTickMarkMode	x_b_mode;
	NhlTickMarkMode	x_t_mode;
	NhlTickMarkStyle	x_b_style;
	NhlTickMarkStyle	x_t_style;
	float		x_t_tension;
	float		x_b_tension;
	NhlBoolean	x_b_precision_set;
	int		x_b_precision;
	NhlBoolean	x_t_precision_set;
	int		x_t_precision;
	NhlFormatRec	x_b_format;
	NhlFormatRec	x_t_format;
	float		border_thickness;
	int		border_line_color;
	NhlBoolean	x_major_grid;
	NhlBoolean	x_minor_grid;
	float		x_major_grid_thickness;
	int		x_major_grid_line_color;
	int		x_major_grid_line_dash_pattern;
	float		x_minor_grid_thickness;
	int		x_minor_grid_line_color;
	int		x_minor_grid_line_dash_pattern;
	NhlBoolean	x_b_minor_per_major_set;
	int		x_b_minor_per_major;
	NhlBoolean	x_t_minor_per_major_set;
	int		x_t_minor_per_major;
	NhlBoolean	x_b_minor_on;
	NhlBoolean	x_t_minor_on;
	int		x_b_label_stride;
	int		x_t_label_stride;
	float		x_b_data_left;
	float		x_b_data_right;
	NhlBoolean	x_b_tick_start_set;
	float		x_b_tick_start;
	NhlBoolean	x_b_tick_end_set;
	float		x_b_tick_end;
	int		x_b_max_ticks;
	float		x_b_tick_spacing;
	int		x_b_spacing_type;
	NhlGenArray	x_b_irregular_points;
	NhlGenArray	x_b_values;
	NhlGenArray	x_b_minor_values;
	NhlGenArray	x_b_labels;
	float		x_b_major_thickness;
	int		x_b_major_line_color;
	NhlBoolean	x_b_major_length_set;
	float 		x_b_major_length;
	NhlBoolean	x_b_major_outward_length_set;
	float		x_b_major_outward_length;
	float		x_b_minor_thickness;
	int		x_b_minor_line_color;
	NhlBoolean	x_b_minor_length_set;
	float		x_b_minor_length;
	NhlBoolean	x_b_minor_outward_length_set;
	float		x_b_minor_outward_length;
	NhlFont		x_b_label_font;	
	NhlBoolean	x_b_label_font_height_set;
	float		x_b_label_font_height;
	int		x_b_label_font_color;
	float		x_b_label_font_aspect;
	int		x_b_label_just;
	float		x_b_label_angle;
	NhlTextDirection   x_b_label_direction;
	char		x_b_label_fcode;
	float		x_b_label_font_thickness;
	NhlFontQuality	x_b_label_font_quality;
	float		x_b_label_constant_spacing;
	float		x_b_label_delta;
	NhlBoolean	x_b_auto_precision;
	float		x_b_max_label_len;
	float		x_b_label_spacing;
	float		x_t_data_left;
	float		x_t_data_right;
	NhlBoolean	x_t_tick_start_set;
	float		x_t_tick_start;
	NhlBoolean	x_t_tick_end_set;
	float		x_t_tick_end;
	int		x_t_max_ticks;
	float		x_t_tick_spacing;
	int		x_t_spacing_type;
	NhlGenArray	x_t_irregular_points;
	NhlGenArray	x_t_values;
	NhlGenArray	x_t_minor_values;
	NhlGenArray	x_t_labels;
	float		x_t_major_thickness;
	int		x_t_major_line_color;
	NhlBoolean	x_t_major_length_set;
	float 		x_t_major_length;
	NhlBoolean	x_t_major_outward_length_set;
	float		x_t_major_outward_length;
	float		x_t_minor_thickness;
	int		x_t_minor_line_color;
	NhlBoolean	x_t_minor_length_set;
	float		x_t_minor_length;
	NhlBoolean	x_t_minor_outward_length_set;
	float		x_t_minor_outward_length;
	NhlFont		x_t_label_font;	
	NhlBoolean	x_t_label_font_height_set;
	float		x_t_label_font_height;
	int		x_t_label_font_color;
	float		x_t_label_font_aspect;
	int		x_t_label_just;
	float		x_t_label_angle;
	NhlTextDirection   x_t_label_direction;
	char		x_t_label_fcode;
	float		x_t_label_font_thickness;
	NhlFontQuality	x_t_label_font_quality;
	float		x_t_label_constant_spacing;
	float		x_t_label_delta;
	NhlBoolean	x_t_auto_precision;
	float		x_t_max_label_len;
	float		x_t_label_spacing;
	NhlBoolean	y_use_left;
	NhlBoolean	y_r_on;
	NhlBoolean	y_l_on;
	NhlBoolean	y_r_labels_on;
	NhlBoolean	y_l_labels_on;
	NhlBoolean	y_r_border_on;
	NhlBoolean	y_l_border_on;
	NhlTickMarkMode	y_r_mode;
	NhlTickMarkMode	y_l_mode;
	NhlTickMarkStyle	y_l_style;
	NhlTickMarkStyle	y_r_style;
	float		y_l_tension;
	float		y_r_tension;
	NhlBoolean	y_l_precision_set;
	int		y_l_precision;
	NhlBoolean	y_r_precision_set;
	int		y_r_precision;
	NhlFormatRec	y_l_format;
	NhlFormatRec	y_r_format;
	NhlBoolean	y_major_grid;
	NhlBoolean	y_minor_grid;
	float		y_major_grid_thickness;
	int		y_major_grid_line_color;
	int		y_major_grid_line_dash_pattern;
	float		y_minor_grid_thickness;
	int		y_minor_grid_line_color;
	int		y_minor_grid_line_dash_pattern;
	NhlBoolean	y_r_minor_per_major_set;
	int		y_r_minor_per_major;
	NhlBoolean	y_l_minor_per_major_set;
	int		y_l_minor_per_major;
	NhlBoolean	y_r_minor_on;
	NhlBoolean	y_l_minor_on;
	int		y_r_label_stride;
	int		y_l_label_stride;
	float		y_l_data_top;
	float		y_l_data_bottom;
	NhlBoolean	y_l_tick_start_set;
	float		y_l_tick_start;
	NhlBoolean	y_l_tick_end_set;
	float		y_l_tick_end;
	int		y_l_max_ticks;
	float		y_l_tick_spacing;
	int		y_l_spacing_type;
	NhlGenArray	y_l_irregular_points;
	NhlGenArray	y_l_values;
	NhlGenArray	y_l_minor_values;
	NhlGenArray	y_l_labels;
	float		y_l_major_thickness;
	int		y_l_major_line_color;
	NhlBoolean	y_l_major_length_set;
	float 		y_l_major_length;
	NhlBoolean	y_l_major_outward_length_set;
	float		y_l_major_outward_length;
	float		y_l_minor_thickness;
	int		y_l_minor_line_color;
	NhlBoolean	y_l_minor_length_set;
	float		y_l_minor_length;
	NhlBoolean	y_l_minor_outward_length_set;
	float		y_l_minor_outward_length;
	NhlFont		y_l_label_font;	
	NhlBoolean	y_l_label_font_height_set;
	float		y_l_label_font_height;
	int		y_l_label_font_color;
	float		y_l_label_font_aspect;
	int		y_l_label_just;
	float		y_l_label_angle;
	NhlTextDirection   y_l_label_direction;
	char		y_l_label_fcode;
	float		y_l_label_font_thickness;
	NhlFontQuality	y_l_label_font_quality;
	float		y_l_label_constant_spacing;
	float		y_l_label_delta;
	NhlBoolean	y_l_auto_precision;
	float		y_l_max_label_len;
	float		y_l_label_spacing;
	float		y_r_data_top;
	float		y_r_data_bottom;
	NhlBoolean	y_r_tick_start_set;
	float		y_r_tick_start;
	NhlBoolean	y_r_tick_end_set;
	float		y_r_tick_end;
	int		y_r_max_ticks;
	float		y_r_tick_spacing;
	int		y_r_spacing_type;
	NhlGenArray	y_r_irregular_points;
	NhlGenArray	y_r_values;
	NhlGenArray	y_r_minor_values;
	NhlGenArray	y_r_labels;
	float		y_r_major_thickness;
	int		y_r_major_line_color;
	NhlBoolean	y_r_major_length_set;
	float 		y_r_major_length;
	NhlBoolean	y_r_major_outward_length_set;
	float		y_r_major_outward_length;
	float		y_r_minor_thickness;
	int		y_r_minor_line_color;
	NhlBoolean	y_r_minor_length_set;
	float		y_r_minor_length;
	NhlBoolean	y_r_minor_outward_length_set;
	float		y_r_minor_outward_length;
	NhlFont		y_r_label_font;	
	NhlBoolean	y_r_label_font_height_set;
	float		y_r_label_font_height;
	int		y_r_label_font_color;
	float		y_r_label_font_aspect;
	int		y_r_label_just;
	float		y_r_label_angle;
	NhlTextDirection   y_r_label_direction;
	char		y_r_label_fcode;
	float		y_r_label_font_thickness;
	NhlFontQuality	y_r_label_font_quality;
	float		y_r_label_constant_spacing;
	float		y_r_label_delta;
	NhlBoolean	y_r_auto_precision;
	float		y_r_max_label_len;
	float		y_r_label_spacing;
/* Private fields */
	NhlLayer		xb_yl_trans_obj;  /* used to tranform tick mark data locations
					to tickmark NDC locations */
	NhlLayer		xt_yr_trans_obj;
	NhlLayer		xb_multi;
	NhlLayer		xt_multi;
	NhlLayer		yl_multi;
	NhlLayer		yr_multi;

/* different than data_left/right/top/bottom these are used to know what
   the data ranges actually are */


	float 		x_b_data_min;
	float 		x_b_data_max;
	NhlBoolean	x_b_data_valid;
	float		x_b_min_nonzero;
	float 		x_t_data_min;
	float 		x_t_data_max;
	NhlBoolean	x_t_data_valid;
	float		x_t_min_nonzero;
	float 		y_l_data_min;
	float 		y_l_data_max;
	NhlBoolean	y_l_data_valid;
	float		y_l_min_nonzero;
	float 		y_r_data_min;
	float 		y_r_data_max;
	NhlBoolean	y_r_data_valid;
	float		y_r_min_nonzero;

	float 		ir_xbmin;
	float 		ir_xtmin;
	float 		ir_ylmin;
	float 		ir_yrmin;
	float 		ir_xbmax;
	float 		ir_xtmax;
	float 		ir_ylmax;
	float 		ir_yrmax;
	NhlBoolean	new_ir_xb;
	NhlBoolean	new_ir_xt;
	NhlBoolean	new_ir_yl;
	NhlBoolean	new_ir_yr;

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

	NhlBoolean	new_draw_req;	
        NhlTransDat	*trans_dat;	/* segment transform data */
}NhlTickMarkLayerPart;

typedef struct _NhlTickMarkLayerRec {
	NhlBaseLayerPart	base;
	NhlViewLayerPart	view;
	NhlTickMarkLayerPart	tick;
}NhlTickMarkLayerRec;

typedef struct _NhlTickMarkClassPart {
	void *foo;
}NhlTickMarkClassPart;

typedef struct _NhlTickMarkClassRec {
	NhlBaseClassPart	base_class;
	NhlViewClassPart	view_class;
	NhlTickMarkClassPart	tick_class;
}NhlTickMarkClassRec;

typedef struct _NhlTickMarkClassRec *NhlTickMarkClass;
typedef struct _NhlTickMarkLayerRec	*NhlTickMarkLayer;

extern NhlTickMarkClassRec	NhltickMarkClassRec;

#endif /* _NTickMarkP_h */

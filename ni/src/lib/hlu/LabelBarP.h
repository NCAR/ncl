
/*
 *      $Id: LabelBarP.h,v 1.1 1993-07-27 18:03:01 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		LabelBarP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Jun 11 15:17:49 MDT 1993
 *
 *	Description:	Private header file for LabelBar class
 */
#ifndef _NLabelBarP_h
#define _NLabelBarP_h

#include <ncarg/hlu/ViewP.h>
#include <ncarg/hlu/LabelBar.h>

typedef struct _LabelBarLayerPart {

	/* public resource fields */

	int	labelbar_on;
	NhlOrientation	orient;
	NhlJustification	just;
	float	lb_x;
	float	lb_y;
	float	lb_width;
	float	lb_height;
	float	box_major_ext;
	float	box_minor_ext;
	int	alignment;
	int	box_count;
	int	box_mode;
	int     box_sizing;

	int	*fill_patterns;
	float	*fill_scale_facs;
	int	*colors;
	float	*values;
	char	**label_strings;
	float	*box_fractions;

	int	labels_on;
	NhlPosition	label_pos;
	NhlJustification label_just;
	int     label_alignment;    /* 0 - Box Centers, 1 - Interior_Edges,
				       2 - External_Edges */
	int	label_dir;
	float	label_angle;
	int	label_font;
	int	label_color;
	float	label_height;
	float	label_aspect;
	float	label_thickness;
	FontQuality label_quality;
	float	label_const_spacing;
	char	label_func_code;
	TextDirection label_direction;
	int     label_stride;
	
	float	title_ext;
	char	*title_string;
	int	title_on;
	NhlPosition title_pos;
	NhlJustification  title_just;
	int     title_direction;
	float	title_angle;
	int	title_font;
	int	title_color;
	float	title_height;
	float	title_aspect;
	float	title_thickness;
	FontQuality title_quality;
	float	title_const_spacing;
	char	title_func_code;

	int	box_line_on;
	int	box_line_color;
	float	box_line_thickness;
	int	box_line_dash_pattern;
	float	box_line_dash_length;
	
	int	perim_on;
	int	perim_color;
	float	perim_thickness;
	int	perim_dash_pattern;
	float	perim_dash_length;

	int	fill_line_color;
	int	fill_line_thickness;

	NhlBoundingBox	margin;

	/* private fields */

	int 		*gks_colors;
	int		last_box_count; /* reset at end of init and update */
	NhlBoundingBox	expand_perim;     /* geometry adjustment fraction */
	NhlBoundingBox	perim;
	NhlBoundingBox  adj_perim;
	NhlBoundingBox  ndc_margin;
	NhlBoundingBox	bar;	         /* preliminary bar boundary */
	NhlBoundingBox	adj_bar;        /* after external label, label angle */
	NhlCoord	box_size;        /* size of box assuming uniform */
	NhlCoord        adj_box_size;    /* size of box after adjustments */
	float		*box_locs;       /* x or y depending on orientation */
	NhlBoundingBox	labels;          /* overall boundary of label area */
	int		labels_id;       /* multitext id */
	float		*label_locs;     /* locations for multitext */
	char		**stride_labels; /* subset of label_strings */
	NhlBoundingBox	title;
	int		title_id;
	float		title_x;
	float		title_y;


}LabelBarLayerPart;

typedef struct _LabelBarLayerRec{
	BaseLayerPart	base;
	ViewLayerPart	view;
	LabelBarLayerPart labelbar;
}LabelBarLayerRec;

typedef struct _LabelBarLayerClassPart {
	char *foo;
}LabelBarLayerClassPart;

typedef struct _LabelBarLayerClassRec{
	BaseLayerClassPart base_class;
	ViewLayerClassPart view_class;
	LabelBarLayerClassPart labelbar_class;
}LabelBarLayerClassRec;

extern LabelBarLayerClassRec labelBarLayerClassRec;


#endif  /*_NLabelBarP_h*/

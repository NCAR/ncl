
/*
 *      $Id: LegendP.h,v 1.3 1993-11-02 20:00:47 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		LegendP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Jun 11 15:17:49 MDT 1993
 *
 *	Description:	Private header file for Legend class
 */
#ifndef _NLegendP_h
#define _NLegendP_h

#include <ncarg/hlu/ViewP.h>
#include <ncarg/hlu/Legend.h>

typedef struct _LegendLayerPart {

	/* public resource fields */

	int	legend_on;
	NhlOrientation	orient;
	NhlJustification just;
	float	box_major_ext;
	float	box_minor_ext;
	int	item_count;
	int     item_placement;

	NhlBoolean	auto_manage;
	float	label_angle_add;
	float	label_off;
	float	title_off;
	NhlBoundingBox	margin;

	NhlGenArray	item_indexes;
	NhlGenArray	item_strings;
	NhlBoolean	mono_item_type;
	NhlGenArray	item_types;
	NhlBoolean 	mono_item_color;
	NhlGenArray	item_colors;
	NhlBoolean	mono_item_thickness;
	NhlGenArray	item_thicknesses;
	NhlBoolean 	mono_item_text_height;
	NhlGenArray	item_text_heights;
	NhlGenArray	label_strings;
	NhlGenArray	item_positions;

	int	labels_on;
	NhlPosition	label_pos;
	NhlJustification label_just;
	int     label_alignment;    /* 0 - Item Centers, 1 - Above Items,
				       2 - Below Items */
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
	
	float	max_title_ext;
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

	int	box_background;
	int	box_line_on;
	int	box_line_color;
	float	box_line_thickness;
	int	box_line_dash_pattern;
	float	box_line_dash_length;
	
	int	perim_on;
	int	perim_color;
	int     perim_fill;
	int	perim_fill_color;
	float	perim_thickness;
	int	perim_dash_pattern;
	float	perim_dash_length;

	int	fill_background;
	float	fill_line_thickness;

	/* private fields */

	float		lg_x;		/* base position and size */
	float  		lg_y;
	float		lg_width;
	float		lg_height;
	NhlBoundingBox	perim;		/* base perimeter */
	NhlBoundingBox  adj_perim;	/* perimeter minus margins */
	NhlBoundingBox	real_perim;	/* perimeter after accounting for
					   excess label and title extent */

	int 		*gks_colors;
	int		label_draw_count;
	int		max_label_draw_count;
	int		max_label_stride_count;

	NhlBoundingBox	bar;	         /* preliminary bar boundary */
	NhlBoundingBox	adj_bar;        /* after external label, label angle */
	NhlCoord	box_size;        /* size of box assuming uniform */
	NhlCoord        adj_box_size;    /* size of box after adjustments */
	float		*item_locs;       /* x or y depending on orientation */
	NhlBoundingBox	labels;          /* overall boundary of label area */
	int		labels_id;       /* multitext id */
	float		const_pos;       /* constant position for labels */
	float		*label_locs;     /* locations for multitext */
	char		**stride_labels; /* subset of label_strings */
	NhlBoundingBox	title;
	int		title_id;
	float		title_x;
	float		title_y;

}LegendLayerPart;

typedef struct _LegendLayerRec{
	BaseLayerPart	base;
	ViewLayerPart	view;
	LegendLayerPart legend;
}LegendLayerRec;

typedef struct _LegendLayerClassPart {
	char *foo;
}LegendLayerClassPart;

typedef struct _LegendLayerClassRec{
	BaseLayerClassPart base_class;
	ViewLayerClassPart view_class;
	LegendLayerClassPart legend_class;
}LegendLayerClassRec;

extern LegendLayerClassRec legendLayerClassRec;


#endif  /*_NLegendP_h*/

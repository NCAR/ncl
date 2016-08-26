/*
 *      $Id: LabelBarP.h,v 1.17 2005-08-24 21:12:13 dbrown Exp $
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

/* defines */

#define NhlLB_MAX_BOXES   	256
#define NhlLB_MAX_LBL_STRINGS  	(NhlLB_MAX_BOXES + 1)
#define NhlLB_DEF_BOX_COUNT   	16
#define NhlLB_DEF_COLOR   	NhlFOREGROUND
#define NhlLB_DEF_PATTERN 	1
#define NhlLB_DEF_VALUE   	0.0
#define NhlLB_DEF_STRING  	"Label_"
#define NhlLB_DEF_BAR_MAJOR	1.0
#define NhlLB_DEF_BAR_MINOR	0.33
#define NhlLb_DEF_LABEL_MINOR	0.33
#define NhlLB_DEF_CHAR_HEIGHT	0.04
#define NhlLB_DEF_TITLE_EXT	0.15
#define NhlLB_DEF_TITLE_OFF	0.03

typedef struct _lbLocInfo {
      float l,r,b,t;
      float lxtr,rxtr,bxtr,txtr;
} lbLocInfo;

typedef struct _NhlLabelBarLayerPart {

	/* public resource fields */

	NhlBoolean	labelbar_on;
	NhlOrientation	orient;
	NhlJustification just;
	float	box_major_ext;
	float	box_minor_ext;
	int	box_count;
	NhllbBoxSizingMode	box_sizing;

	NhlBoolean	auto_manage;
	float	label_angle_add;
	float	label_off;
	float	title_off;
	NhlBoundingBox	margin;
	int	margin_mode;

	NhlBoolean	mono_fill_color;
	NhlColorIndex	fill_color;
	NhlGenArray	fill_colors;
        NhlBoolean      override_fill_opacity;
	NhlBoolean	mono_fill_pattern;
	NhlFillIndex	fill_pattern;
	NhlGenArray	fill_patterns;
	NhlBoolean	mono_fill_scale;
	float		fill_scale;
	NhlGenArray	fill_scales;
	float		fill_dot_size;
	NhlGenArray	label_strings;
	NhlGenArray	box_fractions;

	NhlBoolean label_auto_stride;
	int	labels_on;
	NhlPosition	label_pos;
	NhlJustification label_just;
	NhllbLabelAlignmentMode	label_alignment;    
	int	label_dir;
	float	label_angle;
	NhlFont	label_font;
	int	label_color;
	float	label_height;
	float	label_aspect;
	float	label_thickness;
	NhlFontQuality label_quality;
	float	label_const_spacing;
	char	label_func_code;
	NhlTextDirection label_direction;
	int     label_stride;
	float   max_label_len;
	float   min_label_spacing;
	
	float	title_ext;
	char	*title_string;
	NhlBoolean title_on_set;
	int	title_on;
	NhlPosition title_pos;
	NhlJustification  title_just;
	NhlBoolean title_direction_set;
	int     title_direction;
	float	title_angle;
	NhlFont	title_font;
	int	title_color;
	float	title_height;
	float	title_aspect;
	float	title_thickness;
	NhlFontQuality title_quality;
	float	title_const_spacing;
	char	title_func_code;

	int	box_lines_on;
	int	box_separator_lines_on;
	int	box_line_color;
	float	box_line_thickness;
	int	box_line_dash_pattern;
	float	box_line_dash_seglen;
        NhllbBoxEndCapStyle box_end_cap_style;
	
	int	perim_on;
	int	perim_color;
	int     perim_fill;
	int	perim_fill_color;
	float	perim_thickness;
	int	perim_dash_pattern;
	float	perim_dash_seglen;

	int	fill_background;
	float	fill_line_thickness;
	NhlBoolean raster_fill_on;

	/* private fields */

	int             orient_set;     /* orientation set? */
	float		lb_x;		/* base position and size */
	float  		lb_y;
	float		lb_width;
	float		lb_height;
	lbLocInfo	perim;
	NhlBoundingBox  adj_perim;	/* perimeter minus margins */
	float		adj_width;	/* width minus margins  */
	float		adj_height;	/* height minus margins */
	lbLocInfo	title;
	lbLocInfo	labels;
	NhlBoundingBox	bar;	         /* preliminary bar boundary */
	NhlBoundingBox	adj_bar;       /* after external label, label angle */
	NhlCoord	box_size;        /* size of box assuming uniform */
	NhlCoord        adj_box_size;    /* size of box after adjustments */
	float		title_off_ndc;
	float		label_off_ndc;
	float		small_axis;

	int		current_label_count;
	int		label_draw_count;
	int		max_label_draw_count;
	int		max_label_stride_count;

	float		*box_locs;       /* x or y depending on orientation */
	int		labels_id;       /* multitext id */
	float		const_pos;       /* constant position for labels */
	float		*label_locs;     /* locations for multitext */
	char		**stride_labels; /* subset of label_strings */
	int		title_id;
	float		title_x;
	float		title_y;
	NhlBoolean	new_draw_req;	
        NhlTransDat	*trans_dat;	/* segment transform data */
	float		actual_label_height;
        float           fill_opacity;


}NhlLabelBarLayerPart;

typedef struct _NhlLabelBarLayerRec{
	NhlBaseLayerPart	base;
	NhlViewLayerPart	view;
	NhlLabelBarLayerPart	labelbar;
}NhlLabelBarLayerRec;

typedef struct _NhlLabelBarClassPart {
	char *foo;
}NhlLabelBarClassPart;

typedef struct _NhlLabelBarClassRec{
	NhlBaseClassPart		base_class;
	NhlViewClassPart		view_class;
	NhlLabelBarClassPart	labelbar_class;
}NhlLabelBarClassRec;

typedef struct _NhlLabelBarClassRec *NhlLabelBarClass;
typedef struct _NhlLabelBarLayerRec	*NhlLabelBarLayer;

extern NhlLabelBarClassRec NhllabelBarClassRec;

#endif  /*_NLabelBarP_h*/

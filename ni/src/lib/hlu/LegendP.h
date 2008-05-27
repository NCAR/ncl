/*
 *      $Id: LegendP.h,v 1.21 2008-05-27 20:55:30 dbrown Exp $
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
#include <ncarg/hlu/Workstation.h>
#include <ncarg/hlu/Legend.h>

/* defines */

#define NhlLG_MAX_ITEMS   	256
#define NhlLG_MAX_LBL_STRINGS  	(NhlLG_MAX_ITEMS)
#define NhlLG_DEF_ITEM_COUNT   	16
#define NhlLG_DEF_COLOR   	NhlFOREGROUND
#define NhlLG_DEF_LINE_INDEX	1
#define NhlLG_MIN_LINE_INDEX	0
#define NhlLG_DEF_MARKER_INDEX  1
#define NhlLG_MIN_MARKER_INDEX  0
#define NhlLG_DEF_STRING  	"Label_"
#define NhlLG_DEF_ITEM_STRING  	"L"
#define NhlLG_DEF_BAR_MAJOR	1.0
#define NhlLG_DEF_BAR_MINOR	0.33
#define NhlLg_DEF_LABEL_MINOR	0.33
#define NhlLG_DEF_CHAR_HEIGHT	0.04
#define NhlLG_DEF_TITLE_EXT	0.15
#define NhlLG_DEF_TITLE_OFF	0.03

typedef struct _lgLocInfo {
	float l,r,b,t;
	float lxtr,rxtr,bxtr,txtr;
} lgLocInfo;

typedef struct _NhlLegendLayerPart {

	/* public resource fields */

	int		legend_on;
	NhlOrientation	orient;
	NhlJustification just;
	float		box_major_ext;
	float		box_minor_ext;
	int		item_count;
	NhllgItemPlacementMode	item_placement;

	NhlBoolean	auto_manage;
	float		label_angle_add;
	float		label_off;
	float		title_off;
	NhlBoundingBox	margin;

	NhlGenArray	line_labels;
	NhlBoolean	mono_item_type;
	NhlMarkLineMode	item_type;
	NhlGenArray	item_types;
	NhlBoolean	mono_dash_index;
	NhlDashIndex	dash_index;
	NhlGenArray	dash_indexes;
	NhlBoolean	mono_marker_index;
	NhlMarkerIndex	marker_index;
	NhlGenArray	marker_indexes;
	NhlBoolean 	mono_line_color;
	NhlColorIndex	line_color;
	NhlGenArray	line_colors;
	NhlBoolean 	mono_marker_color;
	NhlColorIndex	marker_color;
	NhlGenArray	marker_colors;
	NhlBoolean	mono_line_thickness;
	float		line_thickness;
	NhlGenArray	line_thicknesses;
	NhlBoolean	mono_marker_thickness;
	float		marker_thickness;
	NhlGenArray	marker_thicknesses;
	NhlBoolean 	mono_line_label_font_height;
	float		line_label_font_height;
	NhlGenArray	line_label_font_heights;
	NhlBoolean 	mono_marker_size;
	float		marker_size;
	NhlGenArray	marker_sizes;
	NhlGenArray	label_strings;
	NhlGenArray	item_positions;
	NhlGenArray	item_order;

	NhlBoolean 	mono_line_label_color;
	NhlColorIndex	line_label_color;
	NhlGenArray	line_label_colors;

	NhlBoolean	mono_line_dash_seglen;
	float		line_dash_seglen;
	NhlGenArray	line_dash_seglens;

	NhlBoolean	line_labels_on;

	NhlFont		ll_font;
	float		ll_aspect;
	float		ll_thickness;
	NhlFontQuality 	ll_quality;
	float		ll_const_spacing;
	char		ll_func_code;

	NhlBoolean	label_auto_stride;
	int		labels_on;
	NhlPosition	label_pos;
	NhlJustification label_just;
	NhllgLabelAlignmentMode	label_alignment; 
	int		label_dir;
	float		label_angle;
	NhlFont		label_font;
	int		label_color;
	float		label_height;
	float		label_aspect;
	float		label_thickness;
	NhlFontQuality 	label_quality;
	float		label_const_spacing;
	char		label_func_code;
	NhlTextDirection label_direction;
	int     	label_stride;
	
	float		title_ext;
	char		*title_string;
	NhlBoolean	title_on_set;
	int		title_on;
	NhlPosition 	title_pos;
	NhlJustification  title_just;
	NhlBoolean     	title_direction_set;
	int     	title_direction;
	float		title_angle;
	NhlFont		title_font;
	int		title_color;
	float		title_height;
	float		title_aspect;
	float		title_thickness;
	NhlFontQuality 	title_quality;
	float		title_const_spacing;
	char		title_func_code;

	int	box_background;
	int	box_line_on;
	int	box_line_color;
	float	box_line_thickness;
	int	box_line_dash_pattern;
	float	box_line_dash_seglen;
	
	int	perim_on;
	int	perim_color;
	int     perim_fill;
	int	perim_fill_color;
	float	perim_thickness;
	int	perim_dash_pattern;
	float	perim_dash_seglen;

	int	fill_background;
	float	fill_line_thickness;

	/* private fields */

	float		lg_x;		/* base position and size */
	float  		lg_y;
	float		lg_width;
	float		lg_height;
	lgLocInfo	perim;
	NhlBoundingBox  adj_perim;	/* perimeter minus margins */
	float		adj_width;	/* width minus margins  */
	float		adj_height;	/* height minus margins */
	lgLocInfo	title;
	lgLocInfo	labels;
	NhlBoundingBox	bar;	         /* preliminary bar boundary */
	NhlBoundingBox	adj_bar;       /* after external label, label angle */
	NhlCoord	box_size;        /* size of box assuming uniform */
	NhlCoord        adj_box_size;    /* size of box after adjustments */
	float		title_off_ndc;
	float		label_off_ndc;
	float		small_axis;

	int		label_draw_count;
	int		max_label_draw_count;
	int		max_label_stride_count;

	float		*item_locs;      /* x or y depending on orientation */
	int		labels_id;       /* multitext id */
	float		const_pos;       /* constant position for labels */
	float		*label_locs;     /* locations for multitext */
	char		**stride_labels; /* subset of label_strings */
	int		title_id;
	float		title_x;
	float		title_y;
	NhlBoolean	new_draw_req;	
        NhlTransDat	*trans_dat;	/* segment transform data */

	float		ll_pheight;
	float		ll_pwidth;
	float		actual_label_height;

}NhlLegendLayerPart;

typedef struct _NhlLegendLayerRec{
	NhlBaseLayerPart	base;
	NhlViewLayerPart	view;
	NhlLegendLayerPart legend;
}NhlLegendLayerRec;

typedef struct _NhlLegendClassPart {
	char *foo;
}NhlLegendClassPart;

typedef struct _NhlLegendClassRec{
	NhlBaseClassPart base_class;
	NhlViewClassPart view_class;
	NhlLegendClassPart legend_class;
}NhlLegendClassRec;

typedef struct _NhlLegendClassRec *NhlLegendClass;
typedef struct _NhlLegendLayerRec	*NhlLegendLayer;

extern NhlLegendClassRec NhllegendClassRec;


#endif  /*_NLegendP_h*/

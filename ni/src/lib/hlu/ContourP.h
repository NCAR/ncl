/*
 *      $Id: ContourP.h,v 1.6 1994-04-05 00:51:09 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		ContourP.h
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Oct 2 15:01:59 MDT 1992
 *
 *	Description:	Contour plot object private header file
 */

#ifndef _NContourP_h
#define _NContourP_h

#include <ncarg/hlu/TransformP.h>
#include <ncarg/hlu/LogLinTransObjP.h>
#include <ncarg/hlu/OverlayI.h>
#include <ncarg/hlu/Contour.h>
#include <ncarg/hlu/WorkspaceI.h>


#define Nhl_cnDEF_ARRAY_SIZE	16
#define Nhl_cnMAX_LEVELS	256
#define Nhl_cnDEF_COLOR		NhlFOREGROUND
#define Nhl_cnDEF_PATTERN	1
#define Nhl_cnDEF_DASH_PATTERN  0
#define Nhl_cnDEF_LINE_LABEL_STRING	"LL_"
#define Nhl_cnINT_WKSPACE	1000
#define Nhl_cnFLOAT_WKSPACE	5000
#define Nhl_cnSTD_VIEW_WIDTH	1.0
#define Nhl_cnSTD_VIEW_HEIGHT	1.0


typedef struct _NhlcnLabelAttrs {
	NhlBoolean	on;
	NhlString	*text;
	NhlString	format;
	NhlFont		font;
	int		mono_color;
	int		*colors;
	NhlBoolean	height_set;
	float		height;
	float		aspect;
	float		thickness;
	int		quality;
	float		cspacing;
	float		angle;
	char		fcode[2];
	int		back_color;
	NhlBoolean	perim_on;
	NhlBoolean	perim_space_set;
	float		perim_space;
	float		perim_lthick;
	int		perim_lcolor;
	int		gks_bcolor;
	int		gks_plcolor;
	float		real_height;
	float		pheight;
	float		pwidth;
} NhlcnLabelAttrs;

typedef struct _NhlContourLayerPart {

	/* Public resources */

	float		out_of_range_val;
	float		special_val;

	int		level_count;
	int		level_selection_mode;
	int		max_level_count;
	float		level_spacing;
	NhlBoolean	label_masking;
	NhlBoolean	min_level_set;
	float		min_level_val;
	NhlBoolean	max_level_set;
	float		max_level_val;
	NhlBoolean	llabel_interval_set;
	int		llabel_interval;

	NhlBoolean	mono_level_flag;
	NhlBoolean	mono_fill_color;
	NhlBoolean	mono_fill_pattern;
	NhlBoolean	mono_fill_scale;
	NhlBoolean	mono_line_color;
	NhlBoolean	mono_line_dash_pattern;
	NhlBoolean	mono_line_thickness;
	NhlBoolean	mono_llabel_color;

	NhlGenArray	levels;
	NhlGenArray	level_flags;
	NhlGenArray	fill_colors;
	NhlGenArray	fill_patterns;
	NhlGenArray	fill_scales;

	NhlGenArray	line_colors;
	NhlGenArray	line_dash_patterns;
	NhlGenArray	line_thicknesses;
	NhlGenArray	llabel_strings;
	NhlGenArray	llabel_colors;

	NhlBoolean	line_dash_seglen_set;
	float		line_dash_seglen;
	int		llabel_position;

	NhlBoolean	low_use_high_attrs;
	NhlBoolean	high_use_line_attrs;
	NhlBoolean	line_use_info_attrs;
	NhlcnLabelAttrs info_lbl;
	NhlcnLabelAttrs line_lbls;
	NhlcnLabelAttrs high_lbls;
	NhlcnLabelAttrs low_lbls;

	float 		x_min;
	float		x_max;
	NhlBoolean	x_log;
	NhlBoolean	x_reverse;
	float 		y_min;
	float		y_max;
	NhlBoolean	y_log;
	NhlBoolean	y_reverse;
	int		display_labelbar;
	int		display_legend;
	NhlBoolean	auto_legend_labels;
	NhlGenArray	legend_labels;
	NhlString	legend_title;
	NhlBoolean	auto_labelbar_labels;
	NhlGenArray	labelbar_labels;
	NhlString	labelbar_title;

	NhlBoolean	update_req;

	/* Private Fields */

        NhlTransDat	*trans_dat;
	NhlBoolean	new_draw_req;

	NhlLayer	overlay_object;
	NhlBoolean	data_changed;
	NhlBoolean	cprect_call_req;
	float		*real_levels;
	int		*gks_fill_colors;
	int		*gks_line_colors;
	int		*gks_llabel_colors;
	NhlGenArray	dash_table;
	float		zmin;
	float		zmax;
	int		fill_count;
	float		llabel_pwidth;
	float		llabel_pheight;
	float		llabel_real_height;
	float		info_label_pwidth;
	float		info_label_pheight;
	float		info_label_real_height;
	float		high_label_pwidth;
	float		high_label_pheight;
	float		high_label_real_height;
	float		low_label_pwidth;
	float		low_label_pheight;
	float		low_label_real_height;
	NhlGenArray	ll_strings;
	NhlGenArray	ll_text_heights;
	int		*label_amap;
	int		iws_id;
	int		fws_id;
	int		label_aws_id;
	int		fill_aws_id;
	int		ezmap_aws_id;

} NhlContourLayerPart;

typedef struct _NhlContourLayerRec {
	NhlBaseLayerPart	base;
	NhlViewLayerPart	view;
	NhlTransformLayerPart	trans;
	NhlContourLayerPart	contour;
} NhlContourLayerRec;

typedef struct NhlContourLayerClassPart{
	NhlPointer		foo;
} NhlContourLayerClassPart;

typedef struct _NhlContourLayerClassRec{
	NhlBaseLayerClassPart		base_class;
	NhlViewLayerClassPart		view_class;
	NhlTransformLayerClassPart	trans_class;
	NhlContourLayerClassPart	contour_class;
} NhlContourLayerClassRec;

typedef struct _NhlContourLayerClassRec	*NhlContourLayerClass;
typedef struct _NhlContourLayerRec	*NhlContourLayer;

extern NhlContourLayerClassRec		NhlcontourLayerClassRec;

#endif  /* _NContourP_h */

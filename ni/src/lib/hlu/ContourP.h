/*
 *      $Id: ContourP.h,v 1.5 1994-03-18 02:18:07 dbrown Exp $
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
#define Nhl_cnSTD_VIEW_WIDTH	0.5
#define Nhl_cnSTD_VIEW_HEIGHT	0.5

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
	NhlBoolean	line_label_interval_set;
	int		line_label_interval;

	NhlBoolean	mono_level_flag;
	NhlBoolean	mono_fill_color;
	NhlBoolean	mono_fill_pattern;
	NhlBoolean	mono_fill_scale;
	NhlBoolean	mono_line_color;
	NhlBoolean	mono_line_dash_pattern;
	NhlBoolean	mono_line_thickness;
	NhlBoolean	mono_line_label_color;

	NhlGenArray	levels;
	NhlGenArray	level_flags;
	NhlGenArray	fill_colors;
	NhlGenArray	fill_patterns;
	NhlGenArray	fill_scales;

	NhlGenArray	line_colors;
	NhlGenArray	line_dash_patterns;
	NhlGenArray	line_thicknesses;
	NhlGenArray	line_label_strings;
	NhlGenArray	line_label_colors;

	float		line_dash_seglen;
	NhlBoolean	line_label_text_height_set;
	float		line_label_text_height;
	int		llabel_position;
	float		llabel_angle;
	int		llabel_background_color;
	NhlBoolean	llabel_perim;
	int		llabel_perim_color; /* not a Conpack option */

/* these will be replaced by contour specific resources */

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
	NhlBoolean	update_req;
	NhlGenArray	legend_labels;

	/* Private Fields */

        NhlTransDat	*trans_dat;
	NhlBoolean	new_draw_req;

	NhlLayer	overlay_object;
	NhlBoolean	data_changed;
	NhlBoolean	cprect_call_req;
	float		*real_levels;
	int		*gks_fill_colors;
	int		*gks_line_colors;
	int		*gks_line_label_colors;
	NhlGenArray	dash_table;
	float		zmin;
	float		zmax;
	float		ll_text_height_2vpw;
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

/*
 *      $Id: ContourP.h,v 1.20 1995-01-06 00:20:05 dbrown Exp $
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

#include <ncarg/hlu/DataCommP.h>
#include <ncarg/hlu/LogLinTransObjP.h>
#include <ncarg/hlu/OverlayI.h>
#include <ncarg/hlu/Contour.h>
#include <ncarg/hlu/WorkspaceI.h>
#include <ncarg/hlu/ScalarFieldFloatP.h>
#include <ncarg/hlu/FormatI.h>

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
#define NhlcnMAPVAL		99
#define NhlcnDEF_INFO_LABEL	"CONTOUR FROM $CMN$ TO $CMX$ BY $CIU$"
#define NhlcnDEF_CONSTF_LABEL	"CONSTANT FIELD - VALUE IS $ZDV$"
#define NhlcnDEF_HIGH_LABEL	"H:B:$ZDV$:E:"
#define NhlcnDEF_LOW_LABEL	"L:B:$ZDV$:E:"
#ifndef FLT_MAX
#define FLT_MAX			10.0e37
#endif
typedef enum { _cnCONSTF, _cnINFO } _cnAnnoType;

typedef struct _NhlcnLabelAttrs {
	NhlBoolean		on;
	NhlPointer		text; /* cast to NhlString or NhlString* */
	NhlFormatRec		format;
	NhlBoolean		height_set;
	float			height;
	NhlTextDirection	direction;
	NhlFont			font;
	int			mono_color;
	int			*colors;
	float			aspect;
	float			thickness;
	NhlFontQuality		quality;
	float			cspacing;
	float			angle;
	char			fcode[2];
	int			back_color;
	NhlBoolean		perim_on;
	float			perim_space;
	float			perim_lthick;
	int			perim_lcolor;
	int			gks_bcolor;
	int			gks_plcolor;
	float			real_height;
	float			pheight;
	float			pwidth;
	float			x_pos;
	float			y_pos;
	NhlJustification	just;
} NhlcnLabelAttrs;

typedef struct _NhlcnRegionAttrs {
	NhlBoolean	perim_on;
	float		perim_thick;
	int		perim_dpat;
	int		perim_color;
	int		gks_pcolor;
	int		fill_color;
	int		gks_fcolor;
	int		fill_pat;
	float		fill_scale;
} NhlcnRegionAttrs;

typedef struct _NhlcnFillAttrs {
	NhlBoolean	on;
	int		color;
	int		pattern;
	float		scale;
} NhlcnFillAttrs;

typedef struct _NhlContourDataDepLayerPart{
	/* Public resources	*/

	NhlBoolean		labels;

	/* Private fields	*/
} NhlContourDataDepLayerPart;
/* private resource */

#define NhlNcnDataChanged	".cnDataChanged"
#define NhlCcnDataChanged	".CnDataChanged"


typedef struct _NhlContourLayerPart {

	/* Public resources */

	NhlGenArray	scalar_field_data;

	NhlcnLevelSelectionMode		level_selection_mode;
	int		level_count;
	int		max_level_count;
	NhlBoolean	level_spacing_set;
	float		level_spacing;
	NhlBoolean	min_level_set;
	float		min_level_val;
	NhlBoolean	max_level_set;
	float		max_level_val;
	NhlcnFillAttrs	below_min;
	NhlcnFillAttrs  above_max;
	NhlBoolean	llabel_interval_set;
	int		llabel_interval;
	NhlDrawOrder	label_order;
	NhlBoolean	label_masking;
	NhlDrawOrder	line_order;
	NhlDrawOrder	fill_order;
	NhlBoolean	lines_on;
	NhlBoolean	fill_on;
	int		fill_background_color;

        NhlcnLabelScalingMode	label_scaling_mode;
        float		label_scale_value;
        float		label_scale_factor;
        NhlFormatRec	max_data_format;
        NhlBoolean	smoothing_on;
        float		smoothing_tension;
        float		smoothing_distance;
        NhlBoolean	check_point_distance;
        float		max_point_distance;

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
	NhlcnLineLabelSpacingMode	llabel_spacing;

	NhlBoolean	low_use_high_attrs;
	NhlBoolean	high_use_line_attrs;
	NhlBoolean	constf_use_info_attrs;
	NhlcnHighLowLabelOverlapMode	high_low_overlap;

	NhlcnLabelAttrs 	line_lbls;
	NhlcnLabelAttrs 	high_lbls;
	NhlcnLabelAttrs 	low_lbls;
	NhlString		info_string; /* before substitution */
	NhlcnLabelAttrs 	info_lbl;
	NhlAnnotationRec	info_lbl_rec;
	NhlString		constf_string; /* before substitution */
	NhlcnLabelAttrs 	constf_lbl;
	NhlAnnotationRec	constf_lbl_rec;

	NhlcnRegionAttrs	missing_val;
	NhlcnRegionAttrs	grid_bound;
	NhlcnRegionAttrs	out_of_range;

	NhlAnnotationDisplayMode	display_labelbar;
	NhlAnnotationDisplayMode	display_legend;
	NhlAnnotationDisplayMode	display_titles;
	NhlAnnotationDisplayMode	display_tickmarks;
	NhlBoolean	x_min_set;
	float 		x_min;
	NhlBoolean	x_max_set;
	float		x_max;
	NhlBoolean	x_log;
	NhlBoolean	x_reverse;
	NhlBoolean	y_min_set;
	float 		y_min;
	NhlBoolean	y_max_set;
	float		y_max;
	NhlBoolean	y_log;
	NhlBoolean	y_reverse;
	NhlBoolean	auto_legend_labels;
	NhlGenArray	legend_labels;
	NhlBoolean	draw_lgnd_line_lbls_set;
	NhlBoolean	draw_lgnd_line_lbls;
	NhlBoolean	auto_labelbar_labels;
	NhlGenArray	labelbar_labels;

	/* private resource */

	NhlBoolean	dump_area_map;
	NhlBoolean	update_req;
	NhlBoolean	data_changed;

	/* Private Fields */

        NhlTransDat	*predraw_dat;
        NhlTransDat	*draw_dat;
        NhlTransDat	*postdraw_dat;
	NhlBoolean	new_draw_req;

	NhlLayer	overlay_object;
	NhlBoolean	data_init;
	NhlBoolean	cprect_call_req;
	int		ref_level;
	float		*real_levels;
	int		real_fill_count;
	int		*real_fill_colors;
	int		*real_fill_patterns;
	float		*real_fill_scales;
	int		*gks_line_colors;
	int		*gks_llabel_colors;
	NhlGenArray	dash_table;
	float		zmin;
	float		zmax;
	NhlBoolean	const_field;
	NhlBoolean	display_constf;
	int		fill_count;
	int		line_count;
	NhlGenArray	ll_strings;
	NhlGenArray	ll_text_heights;
	int		*label_amap;
	int		iws_id;
	int		fws_id;
	int		label_aws_id;
	int		fill_aws_id;
	int		ezmap_aws_id;
	NhlBoolean	use_irr_trans;
	float		xc1,xcm,yc1,ycn; /* data bounds for Conpack */
	float		xlb,xub,ylb,yub; /* window boundaries */
	int		info_anno_id;
	int		constf_anno_id;

	NhlScalarFieldFloatLayerPart	*sfp;

	NhlString	*dtable;
	int		dtable_len;
	NhlLayer	trans_obj;
	NhlBoolean	do_lines;
	NhlBoolean	do_fill;
	NhlBoolean	do_labels;
	NhlWorkspace	*fws,*iws,*aws;
	NhlBoolean	area_ws_inited;
	float		*data;

} NhlContourLayerPart;

typedef struct _NhlContourDataDepLayerRec{
	NhlObjLayerPart			base;
	NhlDataSpecLayerPart		dataspec;
	NhlContourDataDepLayerPart	cndata;
} NhlContourDataDepLayerRec;

typedef struct _NhlContourLayerRec {
	NhlBaseLayerPart	base;
	NhlViewLayerPart	view;
	NhlTransformLayerPart	trans;
	NhlDataCommLayerPart	datacomm;
	NhlContourLayerPart	contour;
} NhlContourLayerRec;

typedef struct _NhlContourDataDepLayerClassPart{
	NhlPointer		foo;
} NhlContourDataDepLayerClassPart;

typedef struct NhlContourLayerClassPart{
	NhlPointer		foo;
} NhlContourLayerClassPart;

typedef struct _NhlContourDataDepLayerClassRec{
	NhlObjLayerClassPart		base_class;
	NhlDataSpecLayerClassPart	dataspec_class;
	NhlContourDataDepLayerClassPart	cndata_class;
} NhlContourDataDepLayerClassRec;

typedef struct _NhlContourLayerClassRec{
	NhlBaseLayerClassPart		base_class;
	NhlViewLayerClassPart		view_class;
	NhlTransformLayerClassPart	trans_class;
	NhlDataCommLayerClassPart	datacomm_class;
	NhlContourLayerClassPart	contour_class;
} NhlContourLayerClassRec;

typedef struct _NhlContourDataDepLayerClassRec	*NhlContourDataDepLayerClass;
typedef struct _NhlContourDataDepLayerRec	*NhlContourDataDepLayer;

typedef struct _NhlContourLayerClassRec		*NhlContourLayerClass;
typedef struct _NhlContourLayerRec		*NhlContourLayer;

extern NhlLayerClass			NhlcontourDataDepLayerClass;
extern NhlContourDataDepLayerClassRec	NhlcontourDataDepLayerClassRec;
extern NhlContourLayerClassRec		NhlcontourLayerClassRec;

#endif  /* _NContourP_h */

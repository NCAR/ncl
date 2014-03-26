/*
 *      $Id: ContourPlotP.h,v 1.36.8.1 2010-03-17 20:47:07 brownrig Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		ContourPlotP.h
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Oct 2 15:01:59 MDT 1992
 *
 *	Description:	ContourPlot plot object private header file
 */

#ifndef _NCONTOURPLOTP_h
#define _NCONTOURPLOTP_h

#include <ncarg/hlu/DataCommP.h>
#include <ncarg/hlu/LogLinTransObjP.h>
#include <ncarg/hlu/PlotManagerI.h>
#include <ncarg/hlu/ContourPlot.h>
#include <ncarg/hlu/ContourPlotI.h>
#include <ncarg/hlu/WorkspaceI.h>
#include <ncarg/hlu/ScalarFieldFloatP.h>
#include <ncarg/hlu/FormatI.h>


#define Nhl_cnDEF_ARRAY_SIZE	16
#define Nhl_cnMAX_LEVELS	255
#define Nhl_cnDEF_COLOR		NhlFOREGROUND
#define Nhl_cnCOLOR_ARRAY_START 2
#define Nhl_cnDEF_PATTERN	1
#define Nhl_cnDEF_DASH_PATTERN  0
#define Nhl_cnDEF_LINE_LABEL_STRING	"LL_"
#define Nhl_cnINT_WKSPACE	1000
#define Nhl_cnFLOAT_WKSPACE	5000
#define NhlcnMAPVAL		99
#define NhlcnAREAID_OFFSET	100
#define NhlcnDEF_INFO_LABEL	"CONTOUR FROM $CMN$ TO $CMX$ BY $CIU$"
#define NhlcnDEF_NODATA_LABEL	"NO CONTOUR DATA"
#define NhlcnDEF_CONSTF_LABEL	"CONSTANT FIELD - VALUE IS $ZDV$"
#define NhlcnDEF_HIGH_LABEL	"H:B:$ZDV$:E:"
#define NhlcnDEF_LOW_LABEL	"L:B:$ZDV$:E:"
#define NhlcnDEF_FORMAT		"*+^sg"
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
	NhlBoolean		mono_color;
	NhlColorIndex		color;
	NhlColorIndex		gks_color;
	NhlColorIndex		*colors;
	float			aspect;
	float			thickness;
	NhlFontQuality		quality;
	float			cspacing;
	float			angle;
	char			fcode[2];
	NhlColorIndex		back_color;
	NhlBoolean		perim_on;
	float			perim_space;
	float			perim_lthick;
	NhlColorIndex		perim_lcolor;
	NhlColorIndex		gks_bcolor;
	NhlColorIndex		gks_plcolor;
	float			real_height;
	float			pheight;
	float			pwidth;
	float			x_pos;
	float			y_pos;
	NhlJustification	just;
	int			count;   /* get only count of number of labels drawn */
} NhlcnLabelAttrs;

typedef struct _NhlcnRegionAttrs {
	NhlBoolean	perim_on;
	float		perim_thick;
	NhlDashIndex	perim_dpat;
	NhlColorIndex	perim_color;
	NhlColorIndex	gks_pcolor;
	NhlColorIndex	fill_color;
	NhlColorIndex	gks_fcolor;
	NhlFillIndex	fill_pat;
	float		fill_scale;
} NhlcnRegionAttrs;


typedef struct _NhlContourPlotDataDepLayerPart{
	/* Public resources	*/

	int		foo;

	/* Private fields	*/
} NhlContourPlotDataDepLayerPart;
/* private resource */

#define NhlNcnDataChanged	".cnDataChanged"
#define NhlCcnDataChanged	".CnDataChanged"


typedef struct _NhlContourPlotLayerPart {

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
	NhlBoolean	llabel_interval_set;
	int		llabel_interval;
	NhlDrawOrder	label_order;
	NhlBoolean	label_masking;
	NhlDrawOrder	line_order;
	NhlDrawOrder	fill_order;
	NhlBoolean	lines_on;
	NhlBoolean      fill_on_set;
	NhlBoolean	fill_on;
	NhlBoolean      fill_mode_set;
	NhlcnFillMode   fill_mode;
	NhlColorIndex	fill_background_color;

        NhlcnLabelScalingMode	label_scaling_mode;
        float		label_scale_value;
        float		label_scale_factor;
        NhlFormatRec	max_data_format;
        NhlBoolean	smoothing_on;
        float		smoothing_tension;
        float		smoothing_distance;
        float		max_point_distance;

	NhlBoolean	explicit_line_labels_on;
	NhlBoolean	explicit_lbar_labels_on;
	NhlBoolean      lbar_end_labels_on_set;
	NhlBoolean	lbar_end_labels_on;
	NhlBoolean      lbar_end_style_set;
	NhlLabelBarEndStyle lbar_end_style;
	NhlBoolean	explicit_lgnd_labels_on;
	NhlGenArray	lgnd_level_flags;
	NhlBoolean      raster_mode_on_set;
	NhlBoolean	raster_mode_on;
	NhlBoolean	cell_size_set;
	float		cell_size;
        NhlBoolean	raster_smoothing_on;
        float		raster_sample_factor;
        float		min_cell_size;
	NhlBoolean	cyclic_mode_on;
	NhlColorIndex   cell_fill_edge_color;
	NhlColorIndex   cell_fill_missing_val_edge_color;

	NhlGenArray	levels;
	NhlBoolean	mono_level_flag;
	NhlcnLevelUseMode	level_flag;
	NhlGenArray	level_flags;
	NhlGenArray     fill_palette;
	NhlBoolean      span_fill_palette;
	NhlBoolean	mono_fill_color;
	NhlColorIndex	fill_color;
	NhlGenArray	fill_colors;
	float           fill_opacity;
	NhlBoolean	mono_fill_pattern;
	NhlFillIndex	fill_pattern;
	NhlGenArray	fill_patterns;
	NhlBoolean	mono_fill_scale;
	float		fill_scale;
	NhlGenArray	fill_scales;
	float           fill_dot_size;

	NhlBoolean	mono_line_color;
	NhlGenArray     line_palette;
	NhlBoolean      span_line_palette;
	NhlGenArray	line_colors;
	NhlColorIndex	line_color;
	NhlBoolean	mono_line_dash_pattern;
	NhlDashIndex	line_dash_pattern;
	NhlGenArray	line_dash_patterns;
	NhlBoolean	mono_line_thickness;
	float		line_thickness;
	NhlGenArray	line_thicknesses;
	NhlGenArray	llabel_colors;
	NhlGenArray	llabel_strings;

	NhlBoolean	line_dash_seglen_set;
	float		line_dash_seglen;
	NhlcnLineLabelPlacementMode	llabel_placement;
	float           llabel_density;

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
	NhlString		no_data_string; /* before substitution */
	NhlBoolean		no_data_label_on;
	NhlString		constf_string; /* before substitution */
	NhlcnLabelAttrs 	constf_lbl;
	NhlAnnotationRec	constf_lbl_rec;
	NhlBoolean              constf_enable_fill;

	NhlcnRegionAttrs	missing_val;
	NhlBoolean		missing_val_perim_grid_bound_on;
	NhlcnRegionAttrs	grid_bound;
	NhlcnRegionAttrs	out_of_range;

	NhlAnnotationDisplayMode	display_labelbar;
	NhlAnnotationDisplayMode	display_legend;
	NhlAnnotationDisplayMode	display_titles;
	NhlAnnotationDisplayMode	display_tickmarks;
	NhlGenArray	lbar_labels_res;
	char		lbar_func_code;
	NhlBoolean	lbar_alignment_set;
	NhllbLabelAlignmentMode lbar_alignment;
	NhlGenArray	lgnd_labels_res;
	char		lgnd_func_code;
	NhlBoolean	draw_lgnd_line_lbls_set;
	NhlBoolean	draw_lgnd_line_lbls;
        float		x_tension;
        float		y_tension;
	NhlGenArray	conpack_params;
        
	/* private resource */

	NhlBoolean	dump_area_map;
	NhlBoolean	fix_fill_bleed;
	NhlBoolean      output_gridded_data;
	NhlString       output_file_name;
	NhlBoolean      verbose_triangle_info;
	int		amap_crange;
	NhlBoolean	update_req;
	NhlBoolean	data_changed;
	NhlBoolean      trans_updated;

	/* Private Fields */

        NhlTransDat	*predraw_dat;
        NhlTransDat	*draw_dat;
        NhlTransDat	*postdraw_dat;
	NhlTransDat	*current_trans_dat;
	NhlBoolean	new_draw_req;
	float		out_of_range_val;

	NhlLayer	overlay_object;
	NhlBoolean	data_init;
	NhlBoolean	levels_set;
	int		ref_level;
	NhlColorIndex	*gks_fill_colors;
	NhlColorIndex	*gks_line_colors;
	NhlColorIndex	*gks_llabel_colors;
	NhlGenArray	dash_table;
	float		zmin;
	float		zmax;
	NhlBoolean	const_field;
	NhlBoolean      do_constf_fill;
	NhlBoolean	display_constf_no_data;
	NhlString	constf_no_data_string;
	int		fill_count;
	NhlGenArray	ll_strings;
	int		*label_amap;
	int		iws_id;
	int		fws_id;
	int		cws_id;
	int		aws_id;
	float		xc1,xcm,yc1,ycn; /* data bounds for Conpack */
	float		xlb,xub,ylb,yub; /* window boundaries */
	int		info_anno_id;
	int		constf_anno_id;

	NhlScalarFieldFloatLayerPart	*sfp;
	NhlScalarFieldFloatLayerPart	*osfp;
	NhlGenArray    x_arr;
	NhlGenArray    y_arr;
        NhlGridType    grid_type;

	NhlString	*dtable;
	int		dtable_len;
	NhlLayer	trans_obj;
	NhlBoolean	do_lines;
	NhlBoolean	do_fill;
	NhlBoolean	do_labels;
	NhlWorkspace	*fws;
	NhlWorkspace	*iws;
	NhlWorkspace	*aws;
	NhlWorkspace	*cws;
	NhlBoolean	wk_active;
	float		*data;
	NhlBoolean	do_low_level_log;
	NhlBoolean	low_level_log_on;
	NhlLayer        render_obj;
	int             render_update_mode;
	int             hlb_val; /* value of Conpack Param HLB */

	/* labelbar and legend stuff */

	NhlBoolean	lbar_labels_res_set;
	NhlBoolean	lbar_labels_set;
	NhlGenArray	lbar_labels;
	NhlGenArray     lbar_fill_colors;
	NhlGenArray     lbar_fill_patterns;
	NhlGenArray     lbar_fill_scales;
	int             lbar_fill_count;

	NhlBoolean	lgnd_labels_res_set;
	NhlBoolean	lgnd_labels_set;

	int		lgnd_line_count;
	NhlGenArray	lgnd_labels;
	NhlGenArray	lgnd_l_colors;
	NhlGenArray	lgnd_l_dash_pats;
	NhlGenArray	lgnd_l_thicknesses;
	NhlGenArray	lgnd_ll_font_colors;
	NhlGenArray	lgnd_ll_strings;
        NhlBoolean	sticky_cell_size_set;
	NhlBoolean	llabel_interval_mode;

} NhlContourPlotLayerPart;

typedef struct _NhlContourPlotDataDepLayerRec{
	NhlBaseLayerPart		base;
	NhlDataSpecLayerPart		dataspec;
	NhlContourPlotDataDepLayerPart	cndata;
} NhlContourPlotDataDepLayerRec;

typedef struct _NhlContourPlotLayerRec {
	NhlBaseLayerPart	base;
	NhlViewLayerPart	view;
	NhlTransformLayerPart	trans;
	NhlDataCommLayerPart	datacomm;
	NhlContourPlotLayerPart	contourplot;
} NhlContourPlotLayerRec;

typedef struct _NhlContourPlotDataDepClassPart{
	NhlPointer		foo;
} NhlContourPlotDataDepClassPart;

typedef struct NhlContourPlotClassPart{
	NhlPointer		foo;
} NhlContourPlotClassPart;

typedef struct _NhlContourPlotDataDepClassRec{
	NhlBaseClassPart		base_class;
	NhlDataSpecClassPart	dataspec_class;
	NhlContourPlotDataDepClassPart	cndata_class;
} NhlContourPlotDataDepClassRec;

typedef struct _NhlContourPlotClassRec{
	NhlBaseClassPart		base_class;
	NhlViewClassPart		view_class;
	NhlTransformClassPart	trans_class;
	NhlDataCommClassPart	datacomm_class;
	NhlContourPlotClassPart	contourplot_class;
} NhlContourPlotClassRec;

typedef struct _NhlContourPlotDataDepClassRec
					*NhlContourPlotDataDepClass;
typedef struct _NhlContourPlotDataDepLayerRec	*NhlContourPlotDataDepLayer;

typedef struct _NhlContourPlotClassRec	*NhlContourPlotClass;
typedef struct _NhlContourPlotLayerRec		*NhlContourPlotLayer;

extern NhlErrorTypes _NhlContourRender(
#if	NhlNeedProto
	NhlLayer		instance,
        NhlContourPlotLayer     cnl,
	NhlDrawOrder            order,
	NhlString		entry_name
#endif
        );

extern  NhlIsoLine  *_NhlGetIsoLines(
#if     NhlNeedProto
        NhlLayer                instance,
        NhlContourPlotLayer     cnl,
        int			n_levels,
        float 			*levels,
	NhlString		entry_name
#endif
	);

extern NhlClass			NhlcontourPlotDataDepClass;
extern NhlContourPlotDataDepClassRec NhlcontourPlotDataDepClassRec;
extern NhlContourPlotClassRec	NhlcontourPlotClassRec;

#endif  /* _NCONTOURPLOTP_h */

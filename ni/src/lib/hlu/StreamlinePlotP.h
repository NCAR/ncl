/*
 *      $Id: StreamlinePlotP.h,v 1.14.12.1 2010-03-17 20:47:07 brownrig Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		StreamlinePlotP.h
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Sep 28 11:44:30 MDT 1995
 *
 *	Description:	StreamlinePlot plot object private header file
 */

#ifndef _NSTREAMLINEPLOTP_h
#define _NSTREAMLINEPLOTP_h

#include <ncarg/hlu/DataCommP.h>
#include <ncarg/hlu/LogLinTransObjP.h>
#include <ncarg/hlu/PlotManagerI.h>
#include <ncarg/hlu/StreamlinePlot.h>
#include <ncarg/hlu/WorkspaceI.h>
#include <ncarg/hlu/VectorFieldFloatP.h>
#include <ncarg/hlu/ScalarFieldFloatP.h>
#include <ncarg/hlu/FormatI.h>

#define Nhl_stDEF_ARRAY_SIZE	16
#define Nhl_stMAX_LEVELS	255
#define Nhl_stDEF_COLOR		NhlFOREGROUND
#define Nhl_stCOLOR_ARRAY_START 2
#define Nhl_stDEF_PATTERN	1
#define Nhl_stDEF_DASH_PATTERN  0
#define NhlstMAPVAL		99
#define NhlstDEF_NODATA_LABEL	"NO STREAMLINE DATA"
#define NhlstDEF_ZEROF_LABEL	"ZERO FIELD"
#define NhlstDEF_FORMAT		"*+^sg"
#define NhlstPRECISION		6
#ifndef FLT_MAX
#define FLT_MAX			10.0e37
#endif
typedef enum { _stZEROF, _stREFVECANNO, _stMINVECANNO } _stAnnoType;

typedef struct _NhlstLabelAttrs {
	NhlString		name;
	NhlBoolean		on;
	NhlOrientation		orientation;
	NhlBoolean		string1_on;
	NhlString		string1;
	NhlBoolean		string2_on;
	NhlString		string2;
	NhlString		text1;  /* after substitution */
	NhlString		text2;  /* after substitution */
	NhlBoolean		height_set;
	float			height;
	NhlTextDirection	direction;
	NhlFont			font;
	NhlColorIndex		color;
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
	float			real_height;
	float			pheight;
	float			pwidth;
	float			x_pos;
	float			y_pos;
	NhlJustification	just;
} NhlstLabelAttrs;

typedef struct _NhlstScaleInfo {
	NhlScalingMode		mode;
        float			scale_value;
        float			scale_factor;
	float			max_val;
	float			min_val;
        NhlFormatRec		format;
	int			left_sig_digit;
	int			sig_digits;
} NhlstScaleInfo;


typedef struct _NhlStreamlinePlotDataDepLayerPart{
	/* Public resources	*/

	int		foo;

	/* Private fields	*/
} NhlStreamlinePlotDataDepLayerPart;

/* private resource */

#define NhlNstDataChanged	".stDataChanged"
#define NhlCstDataChanged	".StDataChanged"
#define NhlNstFoo		".stFoo"
#define NhlCstFoo		".StFoo"

typedef struct _NhlStreamlinePlotLayerPart {

	/* Public resources */

	NhlGenArray		vector_field_data;
	NhlGenArray		scalar_field_data;
	NhlDrawOrder		streamline_order;
	NhlBoolean		map_direction;

	float			line_thickness;
	NhlBoolean		mono_line_color;
	NhlColorIndex		line_color;
	float			line_opacity;
	NhlBoolean		arrow_length_set;
	float			arrow_length;
	NhlBoolean		step_size_set;
	float			step_size;
#if 0        
	NhlBoolean		min_line_length_set;
	float			min_line_length;
#endif        
	NhlBoolean		min_line_spacing_set;
	float			min_line_spacing;
	NhlBoolean		min_arrow_spacing_set;
	float			min_arrow_spacing;
	float			min_step_factor;
	int			length_check_count;
	int			crossover_check_count;
	int			line_start_stride;
	int			arrow_stride;
	
	NhlBoolean		curly_vector_mode;
	float			ref_magnitude;
	NhlBoolean		ref_length_set;
	float			ref_length;
	float			min_frac_len;
	int			position_mode;
	float			arrow_frac_len;
	NhlBoolean		min_distance_set;
	float			min_distance;
	float			min_magnitude;

	NhlGenArray		levels;
	int			level_count;
	NhlLevelSelectionMode	level_selection_mode;
	int			max_level_count;
	NhlBoolean		level_spacing_set;
	float			level_spacing;
	NhlBoolean		min_level_set;
	float			min_level_val;
	NhlBoolean		max_level_set;
	float			max_level_val;
	NhlGenArray	        level_palette;
	NhlBoolean              span_level_palette;
	NhlGenArray		level_colors;
	NhlBoolean		use_scalar_array;
	NhlColorIndex		scalar_mval_color;
	NhlBoolean		scalar_data_init;
	NhlScalarFieldFloatLayerPart	*sfp;
	NhlScalarFieldFloatLayerPart	*osfp;
	float			scalar_min, scalar_max;
	
	NhlString		zerof_string; /* before substitution */
	NhlstLabelAttrs 	zerof_lbl;
	NhlAnnotationRec	zerof_lbl_rec;

	NhlBoolean		explicit_lbar_labels_on;
	NhlBoolean		lbar_end_labels_on;
	NhlstScaleInfo		scale;

/* intercepted resources */

	NhlAnnotationDisplayMode	display_labelbar;
	NhlAnnotationDisplayMode	display_legend;
	NhlAnnotationDisplayMode	display_titles;
	NhlAnnotationDisplayMode	display_tickmarks;
	float		x_tension;
	float		y_tension;
	NhlGenArray	lbar_labels_res;
	char		lbar_func_code;
	NhllbLabelAlignmentMode lbar_alignment;

	/* private resource */

	NhlBoolean	dump_area_map;
	int		amap_crange;
	NhlBoolean	update_req;
	NhlBoolean	data_changed;

	/* Private Fields */

        NhlTransDat	*predraw_dat;
        NhlTransDat	*draw_dat;
        NhlTransDat	*postdraw_dat;
	NhlTransDat	*current_trans_dat;
	NhlBoolean	new_draw_req;
	float		out_of_range_val;

	NhlLayer	overlay_object;
	NhlBoolean	data_init;
	NhlBoolean      levels_set;
	NhlVectorFieldFloatLayerPart	*vfp;
	NhlVectorFieldFloatLayerPart	*ovfp;
	float		zmin;
	float		zmax;
	float		umin,umax,vmin,vmax;
	NhlBoolean	zero_field;
	NhlBoolean	display_zerof_no_data;
	NhlString	zerof_no_data_string;
	float		xc1,xcm,yc1,ycn; /* data bounds for stinit/stream */
	float		xlb,xub,ylb,yub; /* window boundaries */
	int		zerof_anno_id;
	float		real_ref_length;

	NhlLayer	trans_obj;
	NhlBoolean	wk_active;
	NhlBoolean	do_low_level_log;
	NhlBoolean	low_level_log_on;

	/* labelbar stuff */

	NhlString	*level_strings;
	NhlBoolean	lbar_labels_res_set;
	NhlBoolean	lbar_labels_set;
	NhlGenArray	lbar_labels;

	/* workspace */

	int		fws_id;
	NhlWorkspace	*fws;

	float		grid_cell_size;

} NhlStreamlinePlotLayerPart;

typedef struct _NhlStreamlinePlotDataDepLayerRec{
	NhlBaseLayerPart			base;
	NhlDataSpecLayerPart			dataspec;
	NhlStreamlinePlotDataDepLayerPart	stdata;
} NhlStreamlinePlotDataDepLayerRec;

typedef struct _NhlStreamlinePlotLayerRec {
	NhlBaseLayerPart		base;
	NhlViewLayerPart		view;
	NhlTransformLayerPart		trans;
	NhlDataCommLayerPart		datacomm;
	NhlStreamlinePlotLayerPart	streamlineplot;
} NhlStreamlinePlotLayerRec;

typedef struct _NhlStreamlinePlotDataDepClassPart{
	NhlPointer		foo;
} NhlStreamlinePlotDataDepClassPart;

typedef struct NhlStreamlinePlotClassPart{
	NhlPointer		foo;
} NhlStreamlinePlotClassPart;

typedef struct _NhlStreamlinePlotDataDepClassRec{
	NhlBaseClassPart			base_class;
	NhlDataSpecClassPart			dataspec_class;
	NhlStreamlinePlotDataDepClassPart	stdata_class;
} NhlStreamlinePlotDataDepClassRec;

typedef struct _NhlStreamlinePlotClassRec{
	NhlBaseClassPart		base_class;
	NhlViewClassPart		view_class;
	NhlTransformClassPart		trans_class;
	NhlDataCommClassPart		datacomm_class;
	NhlStreamlinePlotClassPart	streamlineplot_class;
} NhlStreamlinePlotClassRec;

typedef struct 
	_NhlStreamlinePlotDataDepClassRec	*NhlStreamlinePlotDataDepClass;
typedef struct 
	_NhlStreamlinePlotDataDepLayerRec	*NhlStreamlinePlotDataDepLayer;

typedef struct _NhlStreamlinePlotClassRec	*NhlStreamlinePlotClass;
typedef struct _NhlStreamlinePlotLayerRec	*NhlStreamlinePlotLayer;

extern NhlClass			NhlstreamlinePlotDataDepClass;
extern NhlStreamlinePlotDataDepClassRec NhlstreamlinePlotDataDepClassRec;
extern NhlStreamlinePlotClassRec	NhlstreamlinePlotClassRec;

#endif  /* _NSTREAMLINEPLOTP_h */

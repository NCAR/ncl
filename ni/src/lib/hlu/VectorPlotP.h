/*
 *      $Id: VectorPlotP.h,v 1.3 1996-01-19 18:06:40 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		VectorPlotP.h
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Sep 28 11:44:30 MDT 1995
 *
 *	Description:	VectorPlot plot object private header file
 */

#ifndef _NVECTORPLOTP_h
#define _NVECTORPLOTP_h

#include <ncarg/hlu/DataCommP.h>
#include <ncarg/hlu/LogLinTransObjP.h>
#include <ncarg/hlu/PlotManagerI.h>
#include <ncarg/hlu/VectorPlot.h>
#include <ncarg/hlu/WorkspaceI.h>
#include <ncarg/hlu/VectorFieldFloatP.h>
#include <ncarg/hlu/ScalarFieldFloatP.h>
#include <ncarg/hlu/FormatI.h>
#include <ncarg/hlu/VecAnno.h>

#define Nhl_vcDEF_ARRAY_SIZE	16
#define Nhl_vcMAX_LEVELS	255
#define Nhl_vcDEF_COLOR		NhlFOREGROUND
#define Nhl_vcDEF_PATTERN	1
#define Nhl_vcDEF_DASH_PATTERN  0
#define Nhl_vcSTD_VIEW_WIDTH	0.6
#define Nhl_vcSTD_VIEW_HEIGHT	0.6
#define NhlvcMAPVAL		99
#define NhlvcDEF_REFVEC_STRING1	"$VMG$"
#define NhlvcDEF_REFVEC_STRING2	"Reference Vector"
#define NhlvcDEF_MINVEC_STRING1	"$VMG$"
#define NhlvcDEF_MINVEC_STRING2	"Minimum Vector"
#define NhlvcDEF_NODATA_LABEL	"NO VECTOR DATA"
#define NhlvcDEF_ZEROF_LABEL	"ZERO FIELD"
#define NhlvcDEF_FORMAT		"*+^sg"
#ifndef FLT_MAX
#define FLT_MAX			10.0e37
#endif
typedef enum { _vcZEROF, _vcREFVECANNO, _vcMINVECANNO } _vcAnnoType;

typedef struct _NhlvcArrowAttrs {
	float			vec_len;
	float			vec_mag;
	float			real_vec_mag;
	NhlColorIndex		arrow_line_color;
	NhlColorIndex		real_arrow_line_color;
	NhlColorIndex		arrow_fill_color;
	NhlColorIndex		real_arrow_fill_color;
	NhlBoolean		use_vec_color;
	float			arrow_angle;
	float			arrow_space;
	float			arrow_min_offset;
} NhlvcArrowAttrs;

typedef struct _NhlvcLabelAttrs {
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
	NhlvcArrowAttrs		*aap;
} NhlvcLabelAttrs;

typedef struct _NhlvcRegionAttrs {
	NhlBoolean	perim_on;
	float		perim_thick;
	NhlDashIndex	perim_dpat;
	NhlColorIndex	perim_color;
	NhlColorIndex	gks_pcolor;
	NhlColorIndex	fill_color;
	NhlColorIndex	gks_fcolor;
	NhlFillIndex	fill_pat;
	float		fill_scale;
} NhlvcRegionAttrs;

typedef struct _NhlvcScaleInfo {
	NhlScalingMode		mode;
        float			scale_value;
        float			scale_factor;
	float			max_val;
	float			min_val;
        NhlFormatRec		format;
	int			left_sig_digit;
	int			sig_digits;
} NhlvcScaleInfo;


typedef struct _NhlVectorPlotDataDepLayerPart{
	/* Public resources	*/

	int		foo;

	/* Private fields	*/
} NhlVectorPlotDataDepLayerPart;

/* private resource */

#define NhlNvcDataChanged	".vcDataChanged"
#define NhlCvcDataChanged	".VcDataChanged"
#define NhlNvcFoo		".vcFoo"
#define NhlCvcFoo		".VcFoo"

typedef struct _NhlVectorPlotLayerPart {

	/* Public resources */

	NhlGenArray		vector_field_data;
	NhlGenArray		scalar_field_data;
	NhlBoolean		map_direction;
	NhlVectorPositionMode	position_mode;

	float			min_vec_dist;
	float			min_frac_len;
	float			min_magnitude;
	float			max_magnitude;
	float			ref_magnitude;
	NhlBoolean		ref_length_set;
	float			ref_length;
	
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

	float			arrow_min_size;
	float			arrow_max_size;
	NhlBoolean		filled_arrows_on;
	float			arrow_width;
	float			arrow_min_width;
	float			arrowhead_x;
	float			arrowhead_min_x;
	float			arrowhead_y;
	float			arrowhead_min_y;
	float			arrowhead_interior;
	NhlBoolean		fill_over_line;
	float			line_thickness;
	NhlBoolean		use_scalar_array;
	NhlBoolean		mono_vector_line_color;
	NhlColorIndex		vector_line_color;
	NhlBoolean		mono_vector_fill_color;
	NhlColorIndex		vector_fill_color;
	NhlGenArray		vector_colors;
	NhlColorIndex		scalar_mval_color;
	NhlDrawOrder		vector_order;

	NhlBoolean		use_refvec_anno_attrs;
	NhlvcLabelAttrs 	refvec_anno;
	NhlvcArrowAttrs		ref_attrs;
	NhlAnnotationRec	refvec_anno_rec;
	NhlvcLabelAttrs 	minvec_anno;
	NhlvcArrowAttrs		min_attrs;
	NhlAnnotationRec	minvec_anno_rec;
	NhlString		zerof_string; /* before substitution */
	NhlvcLabelAttrs 	zerof_lbl;
	NhlAnnotationRec	zerof_lbl_rec;

	NhlvcScaleInfo		mag_scale;
	NhlvcScaleInfo		svalue_scale;

	NhlvcLabelAttrs 	lbls;
	NhlBoolean		labels_use_vec_color;

	NhlBoolean		explicit_lbar_labels_on;
	NhlBoolean		lbar_end_labels_on;


/* intercepted resources */

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
	NhlGenArray	lbar_labels_res;
	char		lbar_func_code;
	NhllbLabelAlignmentMode lbar_alignment;

	/* private resource */

	NhlBoolean	dump_area_map;
	int		amap_crange;
	NhlGenArray	conpack_params;
	NhlBoolean	update_req;
	NhlBoolean	data_changed;

	/* Private Fields */

        NhlTransDat	*predraw_dat;
        NhlTransDat	*draw_dat;
        NhlTransDat	*postdraw_dat;
	NhlBoolean	new_draw_req;
	float		out_of_range_val;

	NhlLayer	overlay_object;
	NhlBoolean	data_init;
	NhlVectorFieldFloatLayerPart	*vfp;
	NhlVectorFieldFloatLayerPart	*ovfp;
	float		zmin;
	float		zmax;
	float		umin,umax,vmin,vmax;
	NhlBoolean	scalar_data_init;
	NhlScalarFieldFloatLayerPart	*sfp;
	NhlScalarFieldFloatLayerPart	*osfp;
	float		scalar_min, scalar_max;
	NhlBoolean	zero_field;
	NhlBoolean	display_zerof_no_data;
	NhlString	zerof_no_data_string;
	NhlBoolean	use_irr_trans;
	float		xc1,xcm,yc1,ycn; /* data bounds for vvinit/vvectr */
	float		xlb,xub,ylb,yub; /* window boundaries */
	int		refvec_anno_id;
	int		minvec_anno_id;
	int		zerof_anno_id;
	NhlColorIndex	*gks_vector_colors;
	float		real_ref_length;

	NhlLayer	trans_obj;
	NhlBoolean	wk_active;
	NhlBoolean	seg_open;
	NhlBoolean	do_low_level_log;
	NhlString	*level_strings;

	/* labelbar stuff */

	NhlBoolean	lbar_labels_res_set;
	NhlBoolean	lbar_labels_set;
	NhlGenArray	lbar_labels;

	/* workspace */

	int		fws_id;
	NhlWorkspace	*fws;

	/* for VecAnno */

	_NhlvaArrowParams a_params;
	_NhlvaDrawParams  d_params;

} NhlVectorPlotLayerPart;

typedef struct _NhlVectorPlotDataDepLayerRec{
	NhlBaseLayerPart		base;
	NhlDataSpecLayerPart		dataspec;
	NhlVectorPlotDataDepLayerPart	vcdata;
} NhlVectorPlotDataDepLayerRec;

typedef struct _NhlVectorPlotLayerRec {
	NhlBaseLayerPart	base;
	NhlViewLayerPart	view;
	NhlTransformLayerPart	trans;
	NhlDataCommLayerPart	datacomm;
	NhlVectorPlotLayerPart	vectorplot;
} NhlVectorPlotLayerRec;

typedef struct _NhlVectorPlotDataDepClassPart{
	NhlPointer		foo;
} NhlVectorPlotDataDepClassPart;

typedef struct NhlVectorPlotClassPart{
	NhlPointer		foo;
} NhlVectorPlotClassPart;

typedef struct _NhlVectorPlotDataDepClassRec{
	NhlBaseClassPart		base_class;
	NhlDataSpecClassPart		dataspec_class;
	NhlVectorPlotDataDepClassPart	vcdata_class;
} NhlVectorPlotDataDepClassRec;

typedef struct _NhlVectorPlotClassRec{
	NhlBaseClassPart	base_class;
	NhlViewClassPart	view_class;
	NhlTransformClassPart	trans_class;
	NhlDataCommClassPart	datacomm_class;
	NhlVectorPlotClassPart	vectorplot_class;
} NhlVectorPlotClassRec;

typedef struct _NhlVectorPlotDataDepClassRec
					*NhlVectorPlotDataDepClass;
typedef struct _NhlVectorPlotDataDepLayerRec	*NhlVectorPlotDataDepLayer;

typedef struct _NhlVectorPlotClassRec	*NhlVectorPlotClass;
typedef struct _NhlVectorPlotLayerRec		*NhlVectorPlotLayer;

extern NhlClass			NhlvectorPlotDataDepClass;
extern NhlVectorPlotDataDepClassRec NhlvectorPlotDataDepClassRec;
extern NhlVectorPlotClassRec	NhlvectorPlotClassRec;

#endif  /* _NVECTORPLOTP_h */

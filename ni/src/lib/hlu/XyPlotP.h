/*
 *      $Id: XyPlotP.h,v 1.15 1999-03-24 19:09:51 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		XyPlotP.h
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Dec 30 13:17:21 MST 1992
 *
 *	Description:	
 */
#ifndef _NXYPLOTP_h
#define _NXYPLOTP_h

#include <ncarg/hlu/DataCommP.h>
#include <ncarg/hlu/PlotManagerI.h>
#include <ncarg/hlu/XyPlot.h>
#include <ncarg/hlu/TickMark.h>

#define	_NhlNxyDSpecChanged	"xyDspec.Changed"
#define	_NhlCxyDSpecChanged	"XyDspec.Changed"

typedef struct _NhlXyDataSpecLayerPart{
	/* Public resources	*/
	NhlDashIndex		dash;
	NhlGenArray		dashes;
	NhlBoolean		mono_dash;

	NhlMarkLineMode		marker_mode;
	NhlGenArray		marker_modes;
	NhlBoolean		mono_marker_mode;

	NhlGenArray		lg_label_strings;

	NhlColorIndex		color;
	NhlGenArray		colors;
	NhlBoolean		mono_color;
        float                   line_opacity;
        NhlGenArray             line_opacities;

	NhlColorIndex		label_color;
	NhlGenArray		label_colors;
	NhlBoolean		mono_label_color;

	NhlLineLabelMode	label_mode;
	NhlGenArray		labels;

	float			line_thickness;
	NhlGenArray		line_thicknesses;
	NhlBoolean		mono_line_thickness;

	NhlColorIndex		marker_color;
	NhlGenArray		marker_colors;
	NhlBoolean		mono_marker_color;
        float                   marker_opacity;
        NhlGenArray             marker_opacities;

	NhlMarkerIndex		marker;
	NhlGenArray		markers;
	NhlBoolean		mono_marker;
	
	NhlBoolean		marker_size_set;
	float			marker_size;
	NhlGenArray		marker_sizes;
	NhlBoolean		mono_marker_size;
	
	float			marker_thickness;
	NhlGenArray		marker_thicknesses;
	NhlBoolean		mono_marker_thickness;
	
	NhlBoolean		dash_seg_len_set;
	float			dash_seg_len;

	NhlBoolean		llabel_fheight_set;
	float			llabel_fheight;

	NhlFont			llabel_font;
	float			llabel_faspect;
	float			llabel_fthickness;
	NhlFontQuality		llabel_fquality;
	float			llabel_cspacing;
	char			llabel_func_code;
	
	/* Private fields	*/
} NhlXyDataSpecLayerPart;

typedef struct _NhlXyPlotLayerPart {
	/* Publically setable resources */

	NhlGenArray		curve_data;
	NhlGenArray		dspeclist;

	NhlBoolean		x_style_set;
	NhlTickMarkStyle	x_style;
	NhlBoolean		y_style_set;
	NhlTickMarkStyle 	y_style;

	float			x_tension;
	float			y_tension;

	NhlGenArray		x_irregular_points;
	NhlGenArray		y_irregular_points;
	NhlBoolean		comp_x_min_set;
	NhlBoolean		compute_x_min;
	NhlBoolean		comp_x_max_set;
	NhlBoolean		compute_x_max;
	NhlBoolean		comp_y_max_set;
	NhlBoolean		compute_y_max;
	NhlBoolean		comp_y_min_set;
	NhlBoolean		compute_y_min;
	NhlDrawOrder		curve_order;

/*
 * These resources have not been implimented yet...
 */
#ifdef	NOT
	NhlAlternatePlace	x_alternate;
	NhlAlternatePlace	y_alternate;

	NhlGenArray		x_alternate_coords;
	NhlGenArray		x_original_coords;
	NhlGenArray		y_alternate_coords;
	NhlGenArray		y_original_coords;
#endif

	NhlAnnotationDisplayMode	display_legend;
	NhlAnnotationDisplayMode	display_titles;
	NhlAnnotationDisplayMode	display_tickmarks;
	NhlBoolean			update_req;	

	/* Private fields */
	NhlLayer	thetrans;
	NhlBoolean	have_irreg_trans;
#if 0        
	NhlBoolean	fake_x;
	float		fake_x_max;
	float		fake_x_min;
	NhlBoolean	fake_y;
	float		fake_y_max;
	float		fake_y_min;
#endif
	NhlLayer	overlay;

	NhlBoolean	data_ranges_set;
	NhlBoolean	check_ranges;

	float		x_data_min;
	float		x_data_max;

	float		y_data_min;
	float		y_data_max;

	float		x_irreg_min;
	float		x_irreg_max;
	float		y_irreg_min;
	float		y_irreg_max;

	float		vp_average;

	int		num_cpairs;
	int		size_cpair_arrays;

	NhlGenArray	dash_indexes;
	NhlGenArray	item_types;
	NhlGenArray	lg_label_strings;
	NhlGenArray	line_colors;
        NhlGenArray     line_opacities;
	NhlGenArray	dash_seg_lens;
	NhlGenArray	llabel_colors;
	NhlGenArray	llabel_strings;
	NhlGenArray	llabel_fheights;
	NhlGenArray	line_thicknesses;
	NhlGenArray	marker_colors;
	NhlGenArray	marker_indexes;
	NhlGenArray	marker_sizes;
	NhlGenArray	marker_thicknesses;
        NhlGenArray     marker_opacities;     
	NhlGenArray	xvectors;
	NhlGenArray	yvectors;
	NhlGenArray	len_vectors;
	NhlGenArray	missing_set;
	NhlGenArray	xmissing;
	NhlGenArray	ymissing;
	NhlGenArray	llabel_fonts;
	NhlGenArray	llabel_faspects;
	NhlGenArray	llabel_fthicknesses;
	NhlGenArray	llabel_fqualities;
	NhlGenArray	llabel_cspacings;
	NhlGenArray	llabel_func_codes;

	NhlBoolean	dspec_changed;
	NhlBoolean	new_draw_req;
	NhlTransDat	*predraw_dat;
        NhlTransDat	*draw_dat;
        NhlTransDat	*postdraw_dat;
}NhlXyPlotLayerPart;

typedef struct _NhlXyDataSpecLayerRec{
	NhlBaseLayerPart	base;
	NhlDataSpecLayerPart	dataspec;
	NhlXyDataSpecLayerPart	xydata;
} NhlXyDataSpecLayerRec;

typedef struct _NhlXyPlotLayerRec {
	NhlBaseLayerPart		base;
	NhlViewLayerPart		view;
	NhlTransformLayerPart		trans;
	NhlDataCommLayerPart		datacomm;
	NhlXyPlotLayerPart		xyplot;
}NhlXyPlotLayerRec;

typedef struct _NhlXyDataSpecClassPart{
	int	foo;
} NhlXyDataSpecClassPart;

typedef struct _NhlXyPlotClassPart {
	char *foo;
} NhlXyPlotClassPart;

typedef struct _NhlXyDataSpecClassRec{
	NhlBaseClassPart		base_class;
	NhlDataSpecClassPart	dataspec_class;
	NhlXyDataSpecClassPart	xydata_class;
} NhlXyDataSpecClassRec;

typedef struct _NhlXyPlotClassRec {
	NhlBaseClassPart		base_class;
	NhlViewClassPart		view_class;
	NhlTransformClassPart	trans_class;
	NhlDataCommClassPart	datacomm_class;
	NhlXyPlotClassPart		xyplot_class;
}NhlXyPlotClassRec;

extern NhlXyDataSpecClassRec NhlxyDataSpecClassRec;
extern NhlXyPlotClassRec NhlxyPlotClassRec;

extern NhlClass NhlxyDataSpecClass;

typedef struct _NhlXyDataSpecClassRec *NhlXyDataSpecClass;
typedef struct _NhlXyDataSpecLayerRec *NhlXyDataSpecLayer;

typedef struct _NhlXyPlotClassRec *NhlXyPlotClass;
typedef struct _NhlXyPlotLayerRec *NhlXyPlotLayer;

#endif /* _NXYPLOTP_h */

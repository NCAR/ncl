/*
 *      $Id: PlotManagerP.h,v 1.14 2000-02-16 01:43:31 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		PlotManagerP.h
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Oct 2 15:01:59 MDT 1992
 *
 *	Description:	
 */

#ifndef _NPLOTMANAGERP_h
#define _NPLOTMANAGERP_h

#include <ncarg/hlu/TransformP.h>
#include <ncarg/hlu/PlotManagerI.h>

#define NhlOV_ALLOC_UNIT	8
#define NhlOV_IRR_COUNT		16
#define NhlOV_DEF_TICKMARK_ZONE 2
#define NhlOV_DEF_TITLE_ZONE	4
#define NhlOV_DEF_LABELBAR_ZONE 6
#define NhlOV_DEF_LEGEND_ZONE	7

/* private resources */

#define NhlNpmPlotManagerRecs	".pmPlotManagerRecs"

#define NhlCpmPlotManagerRecs	".PmPlotManagerRecs"

typedef enum { ovTICKMARK, ovTITLE, ovLEGEND, ovLABELBAR, ovEXTERNAL }
ovAnnoType;

typedef struct _NhlAnnoRec {
	NhlLayer		ovl;
	int			anno_id;
	int			plot_id;
	NhlBoolean		resize_notify;
	int			zone;
	NhlPosition		side;
	NhlJustification	just;
	float			para_pos;
	float			ortho_pos;
	ovAnnoType		type;
	int			status;
	NhlBoolean		viewable;
	NhlBoolean		track_data;
	float			data_x;
	float			data_y;
	NhlBoolean		out_of_range;
	float			orig_x;
	float			orig_y;
	float			orig_height;
	float			orig_width;
	struct _NhlAnnoRec	*next;
} NhlAnnoRec;
	
typedef struct _NhlpmRec {
	NhlTransformLayer	plot;	   /* overlay plot (member or base) */
	NhlLayer		ov_obj;	   /* plot's PlotManager object */
	NhlAnnoRec		*anno_list; /* list of annotation records */
	int			max_zone;  /* the max annotation zone */
	float			ox,oy;		/* original vp values */
	float			owidth,oheight;
} NhlpmRec;

typedef struct _NhlPlotManagerLayerPart {

	/* Public resource fields */

	NhlGenArray		overlay_seq_ids;	/* read only */
	NhlGenArray		pre_draw_order;
	NhlGenArray		post_draw_order;

	NhlGenArray		anno_view_ids;
	NhlGenArray		annomanager_ids;
	NhlBoolean		fit_to_bb;
	float			bb_left;
	float			bb_right;
	float			bb_top;
	float			bb_bottom;

	NhlAnnotationDisplayMode	display_tickmarks;
	int				tickmark_zone;
	NhlAnnotationDisplayMode	display_titles;
	int				title_zone;
	NhlAnnotationDisplayMode	display_labelbar;
	int				labelbar_zone;
	NhlAnnotationDisplayMode	display_legend;
	int				legend_zone;
#if 0
	/* intercepted tickmark resources */

	float			x_b_data_left;
	float			x_b_data_right;
	float			y_l_data_bottom;
	float			y_l_data_top;
	int			x_log;
	int			y_log;
	float			x_min;
	float			y_min;
	float			x_max;
	float			y_max;
	int			x_reverse;
	int			y_reverse;
	float			x_tension;
	float			y_tension;
#endif
	/* intercepted title resources */

	float			ti_main_offset_x;
	float			ti_x_axis_offset_x;
	float			ti_y_axis_offset_y;
	NhlTitlePositions	ti_main_position;
	NhlTitlePositions	ti_x_axis_position;
	NhlTitlePositions	ti_y_axis_position;
	NhlBoolean		ti_main_font_height_set;
	float			ti_main_font_height;
	NhlBoolean		ti_x_axis_font_height_set;
	float			ti_x_axis_font_height;
	NhlBoolean		ti_y_axis_font_height_set;
	float			ti_y_axis_font_height;

	/* labelbar resources */

	NhlBoolean		lbar_on;
	NhlOrientation		lbar_orient;
	NhlBoolean		lbar_width_set;
	float			lbar_width;
	NhlBoolean		lbar_height_set;
	float			lbar_height;
	NhlBoolean		lbar_keep_aspect;
	NhlPosition		lbar_side;
	float			lbar_para_pos;
	float			lbar_ortho_pos;
	NhlJustification	lbar_just;

	float			lbar_x_off;
	float			lbar_y_off;
	NhlPosition		lbar_pos;

	/* legend resources */


	NhlBoolean		lgnd_on;
	NhlOrientation		lgnd_orient;
	NhlBoolean		lgnd_width_set;
	float			lgnd_width;
	NhlBoolean		lgnd_height_set;
	float			lgnd_height;
	NhlBoolean		lgnd_keep_aspect;
	NhlPosition		lgnd_side;
	float			lgnd_para_pos;
	float			lgnd_ortho_pos;
	NhlJustification	lgnd_just;

	float			lgnd_x_off;
	float			lgnd_y_off;
	NhlPosition		lgnd_pos;
	
	/* Private resource fields */

	NhlGenArray		pm_rec_list;
	NhlBoolean		update_req;
	NhlBoolean		update_anno_req;
	int			trans_change_count;
	NhlBoolean		trans_changed;

	/* Private Fields */

	int			overlay_alloc;
	int			overlay_count;
	NhlpmRec		**pm_recs;

	int			anno_alloc;
	int			anno_count;
	int			anno_ix;
	int			*view_ids;
	int			*anno_ids;
	NhlLayer		tickmarks;
	NhlLayer		titles;
	NhlLayer		labelbar;
	NhlLayer		legend;

	float			lbar_x;
	float			lbar_y;
	NhlJustification	real_lbar_just;
	float			lgnd_x;
	float			lgnd_y;
	NhlJustification	real_lgnd_just;

	float			ti_x;
	float			ti_y;
	float			ti_width;
	float			ti_height;
	float			real_main_offset_x;
	float			real_y_axis_offset_y;
	float			real_x_axis_offset_x;

	NhlTickMarkStyle	x_tm_style;
	NhlTickMarkStyle	y_tm_style;
	int			x_irr_count;
	int			y_irr_count;
	NhlGenArray		x_irr;
	NhlGenArray		y_irr;

} NhlPlotManagerLayerPart;

typedef struct _NhlPlotManagerLayerRec {
	NhlBaseLayerPart	base;
	NhlViewLayerPart	view;
	NhlTransformLayerPart	trans;
	NhlPlotManagerLayerPart	plotmanager;
} NhlPlotManagerLayerRec;

typedef struct NhlPlotManagerClassPart{
	NhlPointer		foo;
} NhlPlotManagerClassPart;

typedef struct _NhlPlotManagerClassRec{
	NhlBaseClassPart		base_class;
	NhlViewClassPart		view_class;
	NhlTransformClassPart	trans_class;
	NhlPlotManagerClassPart	plotmanager_class;
} NhlPlotManagerClassRec;

typedef struct _NhlPlotManagerClassRec *NhlPlotManagerClass;
typedef struct _NhlPlotManagerLayerRec	*NhlPlotManagerLayer;

extern NhlPlotManagerClassRec	NhlplotManagerClassRec;

#endif  /* _NPLOTMANAGERP_h */

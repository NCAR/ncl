/*
 *      $Id: OverlayP.h,v 1.10 1994-06-24 00:39:53 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		OverlayP.h
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Oct 2 15:01:59 MDT 1992
 *
 *	Description:	
 */

#ifndef _NOverlayP_h
#define _NOverlayP_h

#include <ncarg/hlu/TransformP.h>
#include <ncarg/hlu/OverlayI.h>

#define NhlOV_ALLOC_UNIT	8
#define NhlOV_IRR_COUNT		16
#define NhlOV_STD_VIEW_WIDTH	0.5
#define NhlOV_STD_VIEW_HEIGHT	0.5
#define NhlOV_DEF_TICKMARK_ZONE 2
#define NhlOV_DEF_TITLE_ZONE	4
#define NhlOV_DEF_LABELBAR_ZONE 6
#define NhlOV_DEF_LEGEND_ZONE	7
#define NhlOV_DEF_TITLE_HEIGHT  0.02

/* Zone/Display flags */

#define NhlovMAXZONE		24

/* private resources */

#define NhlNovOverlayRecs	".ovOverlayRecs"

#define NhlCovOverlayRecs	".OvOverlayRecs"

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
	NhlBoolean		track_data;
	float			data_x;
	float			data_y;
	NhlBoolean		out_of_range;
	struct _NhlAnnoRec	*next;
} NhlAnnoRec;
	
typedef struct _NhlovRec {
	NhlTransformLayer	plot;	   /* overlay plot (member or base) */
	NhlLayer		ov_obj;	   /* plot's overlay object */
	NhlAnnoRec		*anno_list; /* list of annotation records */
	int			max_zone;  /* the max annotation zone */
} NhlovRec;

typedef struct _NhlOverlayLayerPart {

	/* Public resource fields */

	NhlGenArray		overlay_ids;	/* read only */
	NhlGenArray		pre_draw_order;
	NhlGenArray		post_draw_order;

	NhlAnnotationDisplayMode	display_tickmarks;
	int				tickmark_zone;
	NhlAnnotationDisplayMode	display_titles;
	int				title_zone;
	NhlAnnotationDisplayMode	display_labelbar;
	int				labelbar_zone;
	NhlAnnotationDisplayMode	display_legend;
	int				legend_zone;

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
	float			lbar_width;
	float			lbar_height;
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
	float			lgnd_width;
	float			lgnd_height;
	NhlPosition		lgnd_side;
	float			lgnd_para_pos;
	float			lgnd_ortho_pos;
	NhlJustification	lgnd_just;

	float			lgnd_x_off;
	float			lgnd_y_off;
	NhlPosition		lgnd_pos;
	
	/* Private resource fields */

	NhlGenArray		ov_rec_list;
	NhlBoolean		update_req;

	/* Private Fields */

	int			overlay_alloc;
	int			overlay_count;
	NhlovRec		**ov_recs;

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

	NhlTickMarkStyles	x_tm_style;
	NhlTickMarkStyles	y_tm_style;
	int			x_irr_count;
	int			y_irr_count;
	NhlGenArray		x_irr;
	NhlGenArray		y_irr;

} NhlOverlayLayerPart;

typedef struct _NhlOverlayLayerRec {
	NhlBaseLayerPart	base;
	NhlViewLayerPart	view;
	NhlTransformLayerPart	trans;
	NhlOverlayLayerPart	overlay;
} NhlOverlayLayerRec;

typedef struct NhlOverlayLayerClassPart{
	NhlPointer		foo;
} NhlOverlayLayerClassPart;

typedef struct _NhlOverlayLayerClassRec{
	NhlBaseLayerClassPart		base_class;
	NhlViewLayerClassPart		view_class;
	NhlTransformLayerClassPart	trans_class;
	NhlOverlayLayerClassPart	overlay_class;
} NhlOverlayLayerClassRec;

typedef struct _NhlOverlayLayerClassRec *NhlOverlayLayerClass;
typedef struct _NhlOverlayLayerRec	*NhlOverlayLayer;

extern NhlOverlayLayerClassRec		NhloverlayLayerClassRec;

#endif  /* _NOverlayP_h */

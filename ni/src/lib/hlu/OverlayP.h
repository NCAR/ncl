/*
 *      $Id: OverlayP.h,v 1.4 1994-01-24 23:57:46 dbrown Exp $
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
#include <ncarg/hlu/Overlay.h>

#define NhlOV_ALLOC_UNIT 8
#define NhlOV_IRR_COUNT 16

/* flags for Bounding Box include */

#define NhlovTICKMARKS	1<<0
#define NhlovTITLES	1<<1
#define NhlovLABELBAR	1<<2
#define NhlovLEGEND	1<<3
#define NhlovALL   NhlovTICKMARKS | NhlovTITLES | NhlovLABELBAR | NhlovLEGEND

/* private resources */

#define NhlNovOverlayRecs	".ovOverlayRecs"
#define NhlNovUpdateReq		".ovUpdateReq"

#define NhlCovOverlayRecs	".OvOverlayRecs"
#define NhlCovUpdateReq		".OvUpdateReq"

typedef struct _ovRec {
	TransformLayer	plot;		/* overlay plot */
	Layer		ov_obj;		/* overlay object associated w/ plot */
} ovRec;

typedef struct OverlayLayerPart {

	/* Public resource fields */

	NhlGenArray		overlay_ids;	/* read only */
	NhlGenArray		pre_draw_order;
	NhlGenArray		post_draw_order;

	int			display_tickmarks;
	int			display_titles;
	int			display_labelbar;
	int			display_legend;

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
	TitlePositions		ti_main_position;
	TitlePositions		ti_x_axis_position;
	TitlePositions		ti_y_axis_position;

	/* labelbar resources */

	float			lbar_width;
	float			lbar_height;
	float			lbar_x_off;
	float			lbar_y_off;
	NhlPosition		lbar_side;
	NhlPosition		lbar_pos;
	NhlJustification	lbar_just;
	NhlOrientation		lbar_orient;
	NhlBoolean		lbar_on;

	/* legend resources */

	float			lgnd_width;
	float			lgnd_height;
	float			lgnd_x_off;
	float			lgnd_y_off;
	NhlPosition		lgnd_side;
	NhlPosition		lgnd_pos;
	NhlJustification	lgnd_just;
	NhlOrientation		lgnd_orient;
	NhlBoolean		lgnd_on;
	
	/* Private resource fields */

	NhlGenArray		ov_rec_list;
	NhlBoolean		update_req;

	/* Private Fields */

	int			overlay_alloc;
	int			overlay_count;
	ovRec			**ov_recs;

	Layer			tickmarks;
	Layer			titles;
	Layer			labelbar;
	Layer			legend;

	float			lbar_x;
	float			lbar_y;
	float			lgnd_x;
	float			lgnd_y;

	float			ti_x;
	float			ti_y;
	float			ti_width;
	float			ti_height;
	float			real_main_offset_x;
	float			real_y_axis_offset_y;
	float			real_x_axis_offset_x;

	TickMarkStyles		x_tm_style;
	TickMarkStyles		y_tm_style;
	int			x_irr_count;
	int			y_irr_count;
	NhlGenArray		x_irr;
	NhlGenArray		y_irr;

} OverlayLayerPart;

typedef struct _OverlayLayerRec {
	BaseLayerPart		base;
	ViewLayerPart		view;
	TransformLayerPart	trans;
	OverlayLayerPart	overlay;
} OverlayLayerRec;

typedef struct OverlayLayerClassPart{
	void *foo;
} OverlayLayerClassPart;

typedef struct _OverlayLayerClassRec{
	BaseLayerClassPart	base_class;
	ViewLayerClassPart	view_class;
	TransformLayerClassPart	trans_class;
	OverlayLayerClassPart	overlay_class;
} OverlayLayerClassRec;

extern OverlayLayerClassRec overlayLayerClassRec;

#endif  /* _NOverlayP_h */

/*
 *      $Id: OverlayP.h,v 1.5 1994-01-27 21:25:30 boote Exp $
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

typedef struct _NhlovRec {
	NhlTransformLayer	plot;	/* overlay plot */
	NhlLayer		ov_obj;	/* overlay object associated w/ plot */
} NhlovRec;

typedef struct _NhlOverlayLayerPart {

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
	NhlTitlePositions		ti_main_position;
	NhlTitlePositions		ti_x_axis_position;
	NhlTitlePositions		ti_y_axis_position;

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
	NhlovRec		**ov_recs;

	NhlLayer		tickmarks;
	NhlLayer		titles;
	NhlLayer		labelbar;
	NhlLayer		legend;

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

	NhlTickMarkStyles		x_tm_style;
	NhlTickMarkStyles		y_tm_style;
	int			x_irr_count;
	int			y_irr_count;
	NhlGenArray		x_irr;
	NhlGenArray		y_irr;

} NhlOverlayLayerPart;

typedef struct _NhlOverlayLayerRec {
	NhlBaseLayerPart		base;
	NhlViewLayerPart		view;
	NhlTransformLayerPart	trans;
	NhlOverlayLayerPart	overlay;
} NhlOverlayLayerRec;

typedef struct NhlOverlayLayerClassPart{
	void *foo;
} NhlOverlayLayerClassPart;

typedef struct _NhlOverlayLayerClassRec{
	NhlBaseLayerClassPart	base_class;
	NhlViewLayerClassPart	view_class;
	NhlTransformLayerClassPart	trans_class;
	NhlOverlayLayerClassPart	overlay_class;
} NhlOverlayLayerClassRec;

typedef struct _NhlOverlayLayerClassRec *NhlOverlayLayerClass;
typedef struct _NhlOverlayLayerRec *NhlOverlayLayer;

extern NhlOverlayLayerClassRec NhloverlayLayerClassRec;

#endif  /* _NOverlayP_h */

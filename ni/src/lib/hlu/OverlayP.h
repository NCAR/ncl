/*
 *      $Id: OverlayP.h,v 1.2 1993-12-22 00:56:19 dbrown Exp $
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

/* private resources */

#define NhlNovMasterOverlay	".ovMasterOverlay"
#define NhlNovOverlayRecs	".ovOverlayRecs"

#define NhlCovMasterOverlay	".OvMasterOverlay"
#define NhlCovOverlayRecs	".OvOverlayRecs"

typedef struct _ovRec {
	TransformLayer	plot;		/* overlay plot */
	Layer		ov_obj;		/* overlay object associated w/ plot */
} ovRec;

typedef struct OverlayLayerPart {

	/* Public resource fields */

	NhlGenArray		overlay_ids;	/* read only */
	NhlGenArray		pre_draw_order;
	NhlGenArray		post_draw_order;

	/* Private resource fields */

	NhlBoolean		master_overlay;
	NhlGenArray		sub_ov_recs;

	/* Private Fields */

	Layer			overlay_trans_obj;

	int			overlay_alloc;
	int			overlay_count;
	ovRec			**ov_recs;

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

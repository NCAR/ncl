/*
 *      $Id: AnnoManagerP.h,v 1.1 1995-04-01 00:03:53 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		AnnoManagerP.h
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri May 20 14:22:36 MDT 1994
 *
 *	Description:	Private header file for AnnoManager class
 */
#ifndef _NANNOMANAGERP_h
#define _NANNOMANAGERP_h

#include <ncarg/hlu/BaseP.h>
#include <ncarg/hlu/View.h>
#include <ncarg/hlu/AnnoManager.h>
#include <ncarg/hlu/TransformI.h>

/* private resource */

#define NhlNamOverlayId		".amOverlayId"
#define NhlCamOverlayId		".AmOverlayId"

typedef struct _NhlAnnoManagerLayerPart{

	/* public resource fields */

	NhlBoolean		on;
	int			view_id;
	NhlBoolean		resize_notify;
	int			zone;
	NhlPosition		side;
	NhlJustification	just;
	float			para_pos;
	float			ortho_pos;
	NhlBoolean		track_data;
	float			data_x;
	float			data_y;

	/* private resource */

	int			overlay_id;

}NhlAnnoManagerLayerPart;

typedef struct _NhlAnnoManagerLayerRec{
	NhlObjLayerPart		base;
	NhlAnnoManagerLayerPart annomanager;
}NhlAnnoManagerLayerRec;

typedef struct _NhlAnnoManagerLayerClassPart {
	char *foo;
}NhlAnnoManagerLayerClassPart;

typedef struct _NhlAnnoManagerLayerClassRec{
	NhlObjLayerClassPart		base_class;
	NhlAnnoManagerLayerClassPart	annomanager_class;
}NhlAnnoManagerLayerClassRec;

typedef struct _NhlAnnoManagerLayerClassRec	*NhlAnnoManagerLayerClass;
typedef struct _NhlAnnoManagerLayerRec		*NhlAnnoManagerLayer;

extern NhlAnnoManagerLayerClassRec		NhlannoManagerLayerClassRec;

#endif  /*_NANNOMANAGERP_h*/

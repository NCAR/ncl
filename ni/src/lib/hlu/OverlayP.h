/*
 *      $Id: OverlayP.h,v 1.1 1993-11-20 01:06:22 dbrown Exp $
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
 *	Description:	Provides generic hooks for plot classes to assign
 *			functions that compute the forward and backward 
 *			data transformations to support point-n-click 
 *			features.
 */

#ifndef _NOverlayP_h
#define _NOverlayP_h

#include <ncarg/hlu/TransformP.h>
#include <ncarg/hlu/Overlay.h>

typedef struct OverlayLayerPart {
	/* User settable resource fields */
	int foo;
	/* Private Fields */
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

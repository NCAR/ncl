
/*
 *      $Id: Overlay.h,v 1.1 1993-11-20 01:06:20 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Overlay.h
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Nov 16 15:18:58 MST 1993
 *
 *	Description:	Public header for Overlay class.
 */

#ifndef _NOverlay_h
#define _NOverlay_h

#include <ncarg/hlu/Transform.h>
#include <ncarg/hlu/TickMark.h>
#include <ncarg/hlu/Title.h>
#include <ncarg/hlu/LabelBar.h>
#include <ncarg/hlu/Legend.h>

/*
 * Overlay class resources
 */

#define NhlNovOverlayTrans		"ovOverlayTrans"
typedef struct _OverlayLayerClassRec *OverlayLayerClass;
typedef struct _OverlayLayerRec *OverlayLayer;

extern LayerClass overlayLayerClass;

#endif /*_NOverlay_h */

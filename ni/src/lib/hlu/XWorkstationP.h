/*
 *      $Id: XWorkstationP.h,v 1.8 2000-03-29 03:59:07 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		XWorkstationP.h
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Sep 14 17:03:13 MDT 1992
 *
 *	Description:	Private header file for XWorkstation class
 */
#ifndef _XWorkstation_h
#define _XWorkstation_h
#include <ncarg/hlu/WorkstationP.h>
#include <ncarg/hlu/XWorkstation.h>
#include <ncarg/gksP.h>

#define NCGM_DEFAULT_CONID -1
#define NCGM_WORKSTATION_TYPE 1

typedef struct _NhlXWorkstationLayerPart {

	NhlBoolean	window_id_set;
	int		window_id;

	NhlXColorMode	xcolor_mode;

	/*
	 * Pause is forced to FALSE if the user provides a window id.
	 */
	NhlBoolean	pause_set;
	NhlBoolean	pause;

	_NGCXWinConfig	xwinconfig;

	/*
	 * Private Fields...
	 */
	NhlXPixel	xpixels[_NhlMAX_COLOR_MAP];
} NhlXWorkstationLayerPart;

typedef struct _NhlXWorkstationLayerRec {
	NhlBaseLayerPart		base;
	NhlWorkstationLayerPart		work;
	NhlXWorkstationLayerPart	xwork;
} NhlXWorkstationLayerRec;

typedef struct _NhlXWorkstationClassPart {
	char *foo;
} NhlXWorkstationClassPart;

typedef struct _NhlXWorkstationClassRec {
	NhlBaseClassPart		base_class;
	NhlWorkstationClassPart	work_class;
	NhlXWorkstationClassPart	xwork_class;
} NhlXWorkstationClassRec;

typedef struct _NhlXWorkstationLayerRec *NhlXWorkstationLayer;
typedef struct _NhlXWorkstationClassRec *NhlXWorkstationClass;

extern NhlXWorkstationClassRec NhlxWorkstationClassRec;

extern NhlErrorTypes
_NhlGetXPixel(
#if	NhlNeedProto
	NhlLayer	l,
	int		hluci,
	NhlXPixel	*xpix
#endif
);

#endif /* _XWorkstation_h */

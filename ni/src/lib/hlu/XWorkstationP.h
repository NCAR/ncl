/*
 *      $Id: XWorkstationP.h,v 1.3 1995-04-07 10:44:29 boote Exp $
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

#define NCGM_DEFAULT_CONID -1
#define NCGM_WORKSTATION_TYPE 1

typedef struct _NhlXWorkstationLayerPart {
	/* User setable resource fields */

	int	window_id;
	int	color_map_id;

	/*
	 * This resource is forced to FALSE if the user provides a window id.
	 */
	NhlBoolean	pause;

	/* Private internal fields */

	NhlBoolean	window_id_set;
	NhlBoolean	color_map_id_set;
	NhlBoolean	pause_set;
	
	/* Export Values */
	/* Import Values */
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

#endif /* _XWorkstation_h */

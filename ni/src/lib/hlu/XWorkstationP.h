
/*
 *      $Id: XWorkstationP.h,v 1.1 1993-04-30 17:26:19 boote Exp $
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

typedef struct _XWorkstationLayerPart {
	/* User setable resource fields */

	int	window_id;
	int	color_map_id;

	/*
	 * This resource is forced to FALSE if the user provides a window id.
	 */
	NhlBoolean	pause;

	/* Private internal fields */
	
	/* Export Values */
	/* Import Values */
} XWorkstationLayerPart;

typedef struct _XWorkstationLayerRec {
	BaseLayerPart	base;
	WorkstationLayerPart	work;
	XWorkstationLayerPart	xwork;
} XWorkstationLayerRec;

typedef struct _XWorkstationLayerClassPart {
	char *foo;
} XWorkstationLayerClassPart;

typedef struct _XWorkstationLayerClassRec {
	BaseLayerClassPart	base_class;
	WorkstationLayerClassPart	work_class;
	XWorkstationLayerClassPart	xwork_class;
} XWorkstationLayerClassRec;

extern XWorkstationLayerClassRec xWorkstationLayerClassRec;

 

#endif /* _XWorkstation_h */

/*
 *      $Id: ImageWorkstationP.h,v 1.1 2004-03-20 00:16:24 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		ImageWorkstationP.h
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Mar 17 12:28:04 MST 2004
 *
 *	Description:	Private header file for ImageWorkstation class
 */
#ifndef _ImageWorkstation_h
#define _ImageWorkstation_h
#include <ncarg/hlu/XWorkstationP.h>
#include <ncarg/hlu/ImageWorkstation.h>
#include <ncarg/gksP.h>


typedef struct _NhlImageWorkstationLayerPart {

	
	NhlBoolean	window_id_set;
	int		window_id;
	NhlImageFormat  format;
	NhlString       filename;
	NhlXColorMode	xcolor_mode;

	/*
	 * Pause is forced to FALSE if the user provides a window id.
	 */
	NhlBoolean	pause_set;
	NhlBoolean	pause;

	_NGCPixConfig	pixconfig;

	/*
	 * Private Fields...
	 */
	NhlXPixel	xpixels[_NhlMAX_COLOR_MAP];
} NhlImageWorkstationLayerPart;

typedef struct _NhlImageWorkstationLayerRec {
	NhlBaseLayerPart		base;
	NhlWorkstationLayerPart		work;
	NhlImageWorkstationLayerPart	imagework;
} NhlImageWorkstationLayerRec;

typedef struct _NhlImageWorkstationClassPart {
	char *foo;
} NhlImageWorkstationClassPart;

typedef struct _NhlImageWorkstationClassRec {
	NhlBaseClassPart		base_class;
	NhlWorkstationClassPart		work_class;
	NhlImageWorkstationClassPart	imagework_class;
} NhlImageWorkstationClassRec;

typedef struct _NhlImageWorkstationLayerRec *NhlImageWorkstationLayer;
typedef struct _NhlImageWorkstationClassRec *NhlImageWorkstationClass;

extern NhlImageWorkstationClassRec NhlimageWorkstationClassRec;


#endif /* _ImageWorkstation_h */

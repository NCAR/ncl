
/*
 *      $Id: Workstation.h,v 1.1 1993-04-30 17:26:04 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Workstation.c
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Sep 9 09:51:36 MDT 1992
 *
 *	Description:	Main workstation class from which all GKS workstations
 *			are subclassed. This main class manages the children
 *			of the workstation, a color map, the workstation ID, and
 *			the workstation type.
 */


#ifndef _NWorkstation_h
#define _NWorkstation_h

#include <ncarg/hlu/Base.h>

#define NhlNwkColorMap		"wkColorMap"
#define NhlCwkColorMap		"WkColorMap"
#define NhlNwkColorMapLen	"wkColorMapLen"
#define NhlCwkColorMapLen	"WkColorMapLen"
#define NhlNwkBkgndColor	"wkBkgndColor"
#define NhlCwkBkgndColor	"WkBkgndColor"

typedef struct _NhlColor {
	float	red;
	float	green;
	float	blue;
} NhlColor;

#define NhlTColorPtr "colorptr"
#define NhlTColor    "color"


extern LayerClass workstationLayerClass;

typedef struct _WorkstationLayerRec *WorkstationLayer;
typedef struct _WorkstationLayerClassRec *WorkstationLayerClass;

#endif	/* _NWorkstation_h */

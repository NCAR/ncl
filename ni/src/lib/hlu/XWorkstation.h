#ifndef _NXWorkstation_h
/*
 *      $Id: XWorkstation.h,v 1.1 1993-04-30 17:26:16 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		XWorkstation.h
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Sep 14 17:03:36 MDT 1992
 *
 *	Description:	Public header for XWorkstation class
 */
#define	_NXWorkstation_h

#include <ncarg/hlu/Workstation.h>

#define NhlNwkWindowId	"wkWindowId"
#define NhlCwkWindowId	"WkWindowId"
#define NhlNwkColorMapId	"wkColorMapId"
#define NhlCwkColorMapId	"WkColorMapId"
#define NhlNwkPause	"wkPause"
#define NhlCwkPause	"WkPause"

extern LayerClass xWorkstationLayerClass;

typedef struct _XWorkstationLayerRec *XWorkstationLayer;
typedef struct _XWorkstationLayerClassRec *XWorkstationLayerClass;


#endif /* _NXWorkstation_h */

/*
 *      $Id: XWorkstation.h,v 1.3 1995-04-07 10:44:28 boote Exp $
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
#ifndef _NXWorkstation_h
#define	_NXWorkstation_h

#include <ncarg/hlu/Workstation.h>

#define NhlNwkWindowId	"wkWindowId"
#define NhlCwkWindowId	"WkWindowId"
#define NhlNwkColorMapId	"wkColorMapId"
#define NhlCwkColorMapId	"WkColorMapId"
#define NhlNwkPause	"wkPause"
#define NhlCwkPause	"WkPause"

extern NhlClass NhlxWorkstationClass;

#endif /* _NXWorkstation_h */

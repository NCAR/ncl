/*
 *      $Id: XWorkstation.h,v 1.8 1997-08-25 20:20:39 boote Exp $
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

#define NhlNwkWindowId		"wkWindowId"
#define NhlCwkWindowId		"WkWindowId"
#define NhlNwkXColorMode	"wkXColorMode"
#define NhlCwkXColorMode	"WkXColorMode"
#define NhlNwkPause		"wkPause"
#define NhlCwkPause		"WkPause"

#define NhlNwkX			"wkX"
#define NhlCwkX			"WkX"
#define NhlNwkY			"wkY"
#define NhlCwkY			"WkY"
#define NhlNwkWidth		"wkWidth"
#define NhlCwkWidth		"WkWidth"
#define NhlNwkHeight		"wkHeight"
#define NhlCwkHeight		"WkHeight"
#define	NhlNwkTitle		"wkTitle"
#define	NhlCwkTitle		"WkTitle"
#define	NhlNwkIconTitle		"wkIconTitle"
#define	NhlCwkIconTitle		"WkIconTitle"

#define	NhlTXColorMode		"XColorMode"
typedef enum _NhlXColorMode{
	NhlSHARE = 0,
	NhlPRIVATE = 1,
	NhlMIXED = 2
} NhlXColorMode;

typedef unsigned long NhlXPixel;

extern NhlErrorTypes
NhlGetXPixel(
#if	NhlNeedProto
	int		id,
	int		hluci,
	NhlXPixel	*xpix
#endif
);

extern NhlClass NhlxWorkstationClass;

#endif /* _NXWorkstation_h */

/*
 *      $Id: XWorkstation.h,v 1.5 1995-04-27 22:34:18 boote Exp $
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

#define	NhlTXColorMode		"XColorMode"
typedef enum _NhlXColorMode{
	NhlSHARE = 0,
	NhlPRIVATE = 1
} NhlXColorMode;

extern NhlClass NhlxWorkstationClass;

#endif /* _NXWorkstation_h */

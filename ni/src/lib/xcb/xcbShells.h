/*
 *      $Id: xcbShells.h,v 1.1 1997-06-11 20:49:24 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1997			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		xcbShells.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Sat May 24 12:39:00 MDT 1997
 *
 *	Description:	
 */
#ifndef	XCB_SHELLS_H
#define	XCB_SHELLS_H

#include <Xm/Xm.h>
#include <Xcb/xcb.h>

#define	XcbNcolorBroker		"colorBroker"
#define	XcbCcolorBroker		"ColorBroker"
#define XcbNparentBroker	"parentBroker"
#define XcbCparentBroker	"ParentBroker"
#define	XcbNcolorMode		"colorMode"
#define	XcbCcolorMode		"ColorMode"
#define	XcbNmaxColorCells	"maxColorCells"
#define	XcbCmaxColorCells	"MaxColorCells"
#define	XcbNminColorCells	"minColorCells"
#define	XcbCminColorCells	"MinColorCells"
#define	XcbNredLevels		"redLevels"
#define	XcbCredLevels		"RedLevels"
#define	XcbNgreenLevels		"greenLevels"
#define	XcbCgreenLevels		"GreenLevels"
#define	XcbNblueLevels		"blueLevels"
#define	XcbCblueLevels		"BlueLevels"
#define	XcbNrgbError		"rgbError"
#define	XcbCrgbError		"RgbError"

#define XcbRPixel	"XcbPixel"
#define XcbRColorMode	"XcbColorMode"

extern Boolean
XcbIsXcbShell(
	Widget	w
);

extern Xcb
XcbGetXcbFromWidget(
	Widget	w
);

typedef struct XcbApplicationShellClassRec *XcbApplicationShellClass;
extern WidgetClass xcbApplicationShellWidgetClass;

#endif	/* XCB_SHELLS_H */

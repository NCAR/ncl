/*
 *      $Id: appSP.h,v 1.1 1997-06-11 20:49:22 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1997			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		appSP.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Apr 18 12:55:58 MDT 1997
 *
 *	Description:	
 */
#ifndef	XCB_APPSP_H
#define	XCB_APPSP_H

#include <Xcb/xcbShellsP.h>

#define XcbApplicationShellIndex	(XmApplicationShellIndex + 1)

typedef struct XcbApplicationShellClassRec XcbApplicationShellClassRec;
struct XcbApplicationShellClassRec{
	CoreClassPart			core_class;
	CompositeClassPart		composite_class;
	ShellClassPart			shell_class;
	WMShellClassPart		wm_shell_class;
	VendorShellClassPart		vendor_shell_class;
	TopLevelShellClassPart		top_level_shell_class;
	ApplicationShellClassPart	application_shell_class;
	XcbShellClassPart		xcb_application_shell_class;
};

extern XcbApplicationShellClassRec xcbApplicationShellClassRec;

typedef struct XcbApplicationShellRec
			XcbApplicationShellRec, *XcbApplicationShellWidget;

struct XcbApplicationShellRec{
	CorePart		core;
	CompositePart		composite;
	ShellPart		shell;
	WMShellPart		wm;
	VendorShellPart		vendor;
	TopLevelShellPart	topLevel;
	ApplicationShellPart	application;
	XcbShellPart		xcb;
};

#endif	/* XCB_APPSP_H */

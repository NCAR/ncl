/*
 *      $Id: xapp.h,v 1.6 1997-09-04 17:05:46 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		xapp.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jul 24 12:04:46 MDT 1996
 *
 *	Description:	
 */
#ifndef	_NG_XAPPMGR_H
#define	_NG_XAPPMGR_H

#include <ncarg/ngo/app.h>

#include <X11/Intrinsic.h>
#include <Xm/Xm.h>

#include <Xcb/xcbShells.h>

#define	NgNxappDpy	"ngxappDpy"
#define	NgCxappDpy	"ngXappDpy"
#define	NgNxappContext	"ngxappContext"
#define	NgCxappContext	"ngXappContext"

#define NgNxappArgc	"ngxappArgc"
#define NgCxappArgc	"ngXappArgc"
#define NgNxappArgv	"ngxappArgv"
#define NgCxappArgv	"ngXappArgv"

#define NgNxappExport	"ngxappExport"
#define NgCxappExport	"ngXappExport"

#define	NgNxappAddFile	"ngxappAddFile"
#define	NgCxappAddFile	"ngXappAddFile"

#define	NgNxappLoadFile	"ngxappLoadFile"
#define	NgCxappLoadFile	"ngXappLoadFile"

extern NhlClass NgxappMgrClass;

/*
 * Public api
 */

typedef struct NgXAppExportRec NgXAppExportRec, *NgXAppExport;
struct NgXAppExportRec{
	XtAppContext	app;
	Display		*dpy;

	Atom		wm_delete_window;
	Cursor		wait;

	Xcb		xcb;
};

/*
 * The xapp object may someday maintain a cache of XmStrings so not too
 * many are used since the same strings are often used multiple times.
 * But for now, these functions should still be used to allocate and
 * free XmStrings so the code used to do this can be localized.
 */
extern XmString NgXAppCreateXmString(
	int	xapp,
	char	*str
);

extern void NgXAppFreeXmString(
	int		xapp,
	XmString	xmstr
);

#endif	/* _NG_XAPPMGR_H */

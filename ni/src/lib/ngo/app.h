/*
 *      $Id: app.h,v 1.3 1997-01-03 01:37:59 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		app.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jul 24 12:04:46 MDT 1996
 *
 *	Description:	
 */
#ifndef	_NG_APPMGR_H
#define	_NG_APPMGR_H

#include <ncarg/hlu/Base.h>
#include <ncarg/ngo/ngo.h>

#define NgNappName			"ngappName"
#define NgCappName			"ngAppName"
#define NgNappClass			"ngappClass"
#define NgCappClass			"ngAppClass"
#define NgNappAppId			"ngappAppId"
#define NgCappAppId			"ngAppAppId"
#define	NgNappNclState			"ngappNclState"
#define	NgCappNclState			"NgAppNclState"

extern NhlClass NgappMgrClass;

/*
 * Public api
 */

extern void NgAppRun(
	int	appmgrid
);

extern void NgAppQuit(
	int	appmgrid
);

/*
 * Work proc's should return True if they are "done", and False if
 * they need to be called again.
 */
typedef NhlBoolean (*NgWorkProc)(
	NhlPointer	cdata
);

extern void NgAddWorkProc(
	int		appmgrid,
	NgWorkProc	work_proc,
	NhlPointer	cdata
);

extern void NgAddPriorityWorkProc(
	int		appmgrid,
	NgWorkProc	work_proc,
	NhlPointer	cdata
);

extern void NgRemoveWorkProc(
	int		appmgrid,
	NgWorkProc	work_proc,
	NhlPointer	cdata
);

extern int NgAppGetID(
	void
);

extern void NgAppAddGO(
	int	appid,
	int	goid
);

extern void NgAppRemoveGO(
	int	appid,
	int	goid
);

extern void NgAppAddNclEditor(
	int	appid,
	int	goid
);

extern void NgAppRemoveNclEditor(
	int	appid,
	int	goid
);

extern int NgAppGetNclEditor(
	int		appid,
	NhlBoolean	new
);

extern void NgAppGrabFocus(
	int	appid,
	int	goid
);

extern void NgAppReleaseFocus(
	int	appid,
	int	goid
);

#endif	/* _NG_APPMGR_H */

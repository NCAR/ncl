/*
 *      $Id: go.h,v 1.1 1996-10-10 18:55:21 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		go.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Sep 19 13:33:12 MDT 1996
 *
 *	Description:	
 */
#ifndef	_NG_GO_H
#define	_NG_GO_H

#include <ncarg/hlu/Base.h>
#include <ncarg/ngo/ngo.h>

#include <X11/Intrinsic.h>

extern NhlClass NggOClass;

/*
 * Public api
 */

#define	NgNgoTitle	"gotitle"
#define	NgCgoTitle	"Gotitle"

/*
 * resources retrieved through Xt
 */
#define	NgNglobalTranslations	"globalTranslations"
#define	NgCglobalTranslations	"GlobalTranslations"

extern void NgGOCreateWindow(
	int	goid
);

extern void NgGOSensitive(
	int		goid,
	NhlBoolean	sensitive
);

extern void NgGOPopup(
	int	goid
);

extern void NgGOPopdown(
	int	goid
);

extern int NgGOWidgetToGoId(
	Widget	w
);

#endif	/* _NG_GO_H */

/*
 *      $Id: go.h,v 1.3 1999-03-12 23:33:02 dbrown Exp $
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

#define	NgNgoTitle		"gotitle"
#define	NgCgoTitle		"Gotitle"
#define NgNgoEditFieldColor	"goEditFieldColor"
#define NgCgoEditFieldColor	"GoEditFieldColor"
#define NgNgoSelectColor	"goSelectColor"
#define NgCgoSelectColor	"GoSelectColor"

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

extern void NgGOClose(
	int	goid
);

extern int NgGOWidgetToGoId(
	Widget	w
);

#endif	/* _NG_GO_H */

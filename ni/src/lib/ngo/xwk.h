/*
 *      $Id: xwk.h,v 1.5 1999-05-22 00:36:29 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1997			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		xwk.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Feb 14 11:26:52 MST 1997
 *
 *	Description:	
 */
#ifndef	_NG_XWK_H
#define	_NG_XWK_H

#include <ncarg/ngo/go.h>
#include <ncarg/hlu/XWorkstation.h>

extern NhlClass NgxWkClass;

/*
 * Public api
 */

#define	NgNxwkWork		"ngxwkWork"
#define	NgCxwkWork		"ngXwkWork"

#define	NgNxwkSelectedView	"ngxwkSelectedView"
#define	NgCxwkSelectedView	"ngXwkSelectedView"

#define	NgNxwkDrawSelectedViewOnly	"ngxwkDrawSelectedViewOnly"
#define	NgCxwkDrawSelectedViewOnly	"ngXwkDrawSelectedViewOnly"

#define	NgNxwkAutoUpdate	"ngxwkAutoUpdate"
#define	NgCxwkAutoUpdate	"ngXwkAutoUpdate"

void NgXWorkPopup
(
	int appmgr,
	int xwkid
);


#endif	/* _NG_XWK_H */

/*
 *      $Id: mwinP.h,v 1.3 1998-11-18 19:45:20 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		mwinP.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Dec 10 11:36:34 MST 1996
 *
 *	Description:	
 */
#ifndef	_NG_MWINP_H_
#define	_NG_MWINP_H_

#include <ncarg/ngo/goP.h>

#include <ncarg/ngo/mwin.h>

#define DEBUG_MWIN 0

typedef struct _NgMWinClassRec *NgMWinClass;
typedef struct _NgMWinRec *NgMWin;

typedef struct _NgMWinPart {
/* required fields */
	int		foo;
/* private fields */
	Widget		cwki;

} NgMWinPart;

typedef struct _NgMWinRec {
	NhlObjLayerPart	base;
	NgGOPart	go;
	NgMWinPart	mwin;
} NgMWinRec;

typedef struct _NgMWinClassPart {
	int		foo;
} NgMWinClassPart;

typedef struct _NgMWinClassRec {
	NhlObjClassPart		base_class;
	NgGOClassPart		go_class;
	NgMWinClassPart		mwin_class;
} NgMWinClassRec;

extern NgMWinClassRec	NgmWinClassRec;

#endif	/* _NG_MWINP_H_ */

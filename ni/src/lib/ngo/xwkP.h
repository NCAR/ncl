/*
 *      $Id: xwkP.h,v 1.5 1998-10-19 20:25:55 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1997			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		xwkP.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Feb 14 11:28:50 MST 1997
 *
 *	Description:	
 */
#ifndef	_NG_XWKP_H_
#define	_NG_XWKP_H_

#include <ncarg/ngo/goP.h>
#include <ncarg/ngo/xwk.h>

#include <ncarg/hlu/XWorkstationP.h>

typedef struct _NgXWkClassRec *NgXWkClass;
typedef struct _NgXWkRec *NgXWk;

typedef struct _NgXWkPart {
/* required fields */
	NhlXWorkstationLayer	xwork;

/* private fields */
	NhlBoolean		mapped;

	NgCBWP			xwork_destroycb;

	_NhlCB			appdestroycb;
	_NhlCB			nsdestroycb;

	NhlBoolean		my_broker;
	Xcb			xcb;
	Xcb			pxcb;
	_NhlCB			broker_destroyCB;
	_NhlCB			broker_cfaultCB;

	Widget			graphicsSW;
	Widget			graphics;

	Dimension		grw;
	Dimension		grh;

	Widget			size;

	int			cmap_editor;
} NgXWkPart;

typedef struct _NgXWkRec {
	NhlObjLayerPart	base;
	NgGOPart	go;
	NgXWkPart	xwk;
} NgXWkRec;

typedef struct _NgXWkClassPart {
	int		foo;
} NgXWkClassPart;

typedef struct _NgXWkClassRec {
	NhlObjClassPart		base_class;
	NgGOClassPart		go_class;
	NgXWkClassPart		xwk_class;
} NgXWkClassRec;

extern NgXWkClassRec	NgxWkClassRec;

#endif	/* _NG_XWKP_H_ */

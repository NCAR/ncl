/*
 *      $Id: xappP.h,v 1.1 1996-10-10 18:55:31 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		xappP.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Aug 28 17:41:23 MDT 1996
 *
 *	Description:	
 */
#ifndef	_NG_XAPPP_H_
#define	_NG_XAPPP_H_

#include <ncarg/ngo/appP.h>

#include <ncarg/ngo/xapp.h>

#define	_NgNUMXmSTRINGBUCKETS	(211)
typedef struct _NgXmStringBucketRec _NgXmStringBucketRec, *_NgXmStringBucket;
struct _NgXmStringBucketRec{
	NhlString		str;
	XmString		xmstr;
	int			refs;
	_NgXmStringBucket	next;
};

typedef struct _NgXAppMgrPart {
	int			argc;
	char			**argv;

	NgXAppExportRec		x;
	_NgXmStringBucket	xmstrings[_NgNUMXmSTRINGBUCKETS];

} NgXAppMgrPart;

typedef struct _NgXAppMgrRec {
	NhlObjLayerPart	base;
	NgAppMgrPart	app;
	NgXAppMgrPart	xapp;
} NgXAppMgrRec;

typedef struct _NgXAppMgrClassPart {
	int	foo;
} NgXAppMgrClassPart;

typedef struct _NgXAppMgrClassRec {
	NhlObjClassPart		base_class;
	NgAppMgrClassPart	app_class;
	NgXAppMgrClassPart	xapp_class;
} NgXAppMgrClassRec;

typedef struct _NgXAppMgrClassRec *NgXAppMgrClass;
typedef struct _NgXAppMgrRec *NgXAppMgr;

#endif	/* _NG_XAPPP_H_ */

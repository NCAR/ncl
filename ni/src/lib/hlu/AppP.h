/*
 *      $Id: AppP.h,v 1.3 1995-02-17 10:22:54 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1994			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		AppP.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Jul 29 12:33:46 MDT 1994
 *
 *	Description:	
 */
#ifndef _NAppP_h
#define _NAppP_h

#include <ncarg/hlu/BaseP.h>
#include <ncarg/hlu/AppI.h>

typedef struct _NhlAppLayerRec *NhlAppLayer;
typedef struct _NhlAppLayerClassRec *NhlAppLayerClass;

typedef struct _NhlAppLayerPart {
	/* public resource fields */
	NhlString	usr_appdir;
	NhlString	sys_appdir;
	NhlString	file_suffix;
	NhlBoolean	default_parent;

	/* private fields */
	_NhlC_OR_F	init_mode;
	NhlBoolean	default_app;
	NhlBoolean	no_appDB;
	NrmDatabase	appDB;
} NhlAppLayerPart;

typedef struct _NhlAppLayerRec {
	NhlBaseLayerPart	base;
	NhlAppLayerPart		app;
} NhlAppLayerRec;

typedef struct _NhlAppTableRec NhlAppTableRec, *NhlAppTable;
struct _NhlAppTableRec{
	NhlAppLayer	app;
	NhlAppTable	next;
};

typedef struct _NhlAppLayerClassPart {
	NhlAppLayer		default_app;
	NhlAppLayer		current_app;
	NrmDatabase		baseDB;
	int			error_id;
	int			workspace_id;
	NhlAppTable		app_objs;	/* except default_app */
} NhlAppLayerClassPart;

typedef struct _NhlAppLayerClassRec {
	NhlBaseLayerClassPart	base_class;
	NhlAppLayerClassPart	app_class;
} NhlAppLayerClassRec;

extern NhlAppLayerClassRec NhlappLayerClassRec;

#endif /* _NAppP_h */	

/*
 *      $Id: AppP.h,v 1.8 1997-02-27 20:13:00 boote Exp $
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
#include <ncarg/hlu/ResourcesP.h>

typedef struct _NhlAppLayerRec *NhlAppLayer;
typedef struct _NhlAppClassRec *NhlAppClass;

typedef struct _NhlAppLayerPart {
	/* public resource fields */
	NhlString	usr_appdir;
	NhlString	sys_appdir;
	NhlString	file_suffix;

	/* post-resdb resources */
	NhlBoolean	default_parent;
	NhlGenArray	resources;

	/* private fields */
	NhlPointer	res_strings;
	NhlPointer	clineopts;
	int		*argc_in_out;
	NhlString	*argv_in_out;
	_NhlC_OR_F	init_mode;
	NhlBoolean	default_app;
	NhlBoolean	no_appDB;
	NrmDatabase	appDB;

	NhlString	*values;
	NrmResourceList	res;
	int		nres;

	_NhlArgList	args;
	int		nargs;
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

typedef struct _NhlAppClassPart {
	NhlResourceList		resources;
	int			num_resources;
	NhlAppLayer		default_app;
	NhlAppLayer		current_app;
	NrmDatabase		baseDB;
	int			error_id;
	int			workspace_id;
	NhlAppTable		app_objs;	/* except default_app */
	NhlPointer		default_guidata;
} NhlAppClassPart;

typedef struct _NhlAppClassRec {
	NhlBaseClassPart	base_class;
	NhlAppClassPart		app_class;
} NhlAppClassRec;

extern NhlAppClassRec NhlappClassRec;

#endif /* _NAppP_h */	

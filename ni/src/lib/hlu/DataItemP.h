/*
 *      $Id: DataItemP.h,v 1.5 1994-12-16 20:04:08 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		DataItemP.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Jun 24 10:19:49 MDT 1993
 *
 *	Description:	Private declarations for the DataItem class.
 */
#ifndef _NDataItemP_h
#define _NDataItemP_h

#include <ncarg/hlu/BaseP.h>
#include <ncarg/hlu/DataItem.h>
#include <ncarg/hlu/DataMgr.h>

#define	NhlNnoManager	"no.Manager"
#define	NhlCnoManager	"No.Manager"

typedef struct _NhlDataItemLayerPart{
	/* User setable resource fields */

	/*
	 * type converter setable fields - this resource is setable
	 * programatically only - and only by things that include the P.h file
	 */
	NhlBoolean	no_manager;
	/*
	 * Can't even do missing val's or min, max - because we don't know
	 * what type the data is yet. - I guess it will have to be in
	 * the sub-class.  Perhaps we need to define the Resource names
	 * in the public.h file so they at least all use the same name.
	 */
	/* Private Fields */
	NhlLayer	manager;
	NhlBoolean	change_called;
} NhlDataItemLayerPart;

typedef struct _NhlDataItemLayerRec{
	NhlBaseLayerPart	base;
	NhlDataItemLayerPart	dataitem;
} NhlDataItemLayerRec;

typedef struct _NhlDataItemLayerClassPart{
	int	foo;
} NhlDataItemLayerClassPart;

typedef struct _NhlDataItemLayerClassRec{
	NhlBaseLayerClassPart	base_class;
	NhlDataItemLayerClassPart	dataitem_class;
} NhlDataItemLayerClassRec;

typedef struct _NhlDataItemLayerClassRec *NhlDataItemLayerClass;
typedef struct _NhlDataItemLayerRec *NhlDataItemLayer;

extern NhlDataItemLayerClassRec NhldataItemLayerClassRec;

/*
 * Private API for DataItem sub-classes
 */
extern void _NhlDataChanged(
#if	NhlNeedProto
	NhlDataItemLayer	l,
	NhlBoolean		status
#endif
);

#endif  /* _NDataItemP_h */

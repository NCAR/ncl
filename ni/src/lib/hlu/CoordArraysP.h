/*
 *      $Id: CoordArraysP.h,v 1.3 1994-01-27 21:22:18 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		CoordArraysP.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jul 28 11:34:08 MDT 1993
 *
 *	Description:	Private declarations for CoordArrays object.
 */
#ifndef _NCoordArraysP_h
#define _NCoordArraysP_h

#include <ncarg/hlu/DataItemP.h>
#include <ncarg/hlu/CoordArrays.h>
#include <ncarg/hlu/CoordArraysIntP.h>
#include <ncarg/hlu/CoordArraysFloatP.h>

/*
 * Private Resource Names
 */
typedef struct _NhlCoordArraysLayerPart{
	/* User setable resource fields */
	NhlString		type_string;

	/* Private Fields */
	NrmQuark		type;
	NhlLayer		child;
} NhlCoordArraysLayerPart;

typedef struct _NhlCoordArraysLayerRec{
	NhlBaseLayerPart		base;
	NhlDataItemLayerPart		dataitem;
	NhlCoordArraysLayerPart		carr;
} NhlCoordArraysLayerRec;

typedef struct _NhlCoordArraysLayerClassPart{
	int	foo;
} NhlCoordArraysLayerClassPart;

typedef struct _NhlCoordArraysLayerClassRec{
	NhlBaseLayerClassPart		base_class;
	NhlDataItemLayerClassPart	dataitem_class;
	NhlCoordArraysLayerClassPart	carr_class;
} NhlCoordArraysLayerClassRec;

typedef struct _NhlCoordArraysLayerClassRec *NhlCoordArraysLayerClass;
typedef struct _NhlCoordArraysLayerRec *NhlCoordArraysLayer;

extern NhlCoordArraysLayerClassRec NhlcoordArraysLayerClassRec;

#endif  /* _NCoordArraysP_h */

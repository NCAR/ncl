/*
 *      $Id: CoordArrTableP.h,v 1.2 1994-01-27 21:22:02 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		CoordArrTableP.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jul 28 11:34:08 MDT 1993
 *
 *	Description:	Private declarations for CoordArrTable object.
 */
#ifndef _NCoordArrTableP_h
#define _NCoordArrTableP_h

#include <ncarg/hlu/DataItemP.h>
#include <ncarg/hlu/CoordArrTable.h>
#include <ncarg/hlu/CoordArrTableIntP.h>
#include <ncarg/hlu/CoordArrTableFloatP.h>

typedef struct _NhlCoordArrTableLayerPart{
	/* User setable resource fields */
	NhlString		type_string;

	/* Private Fields */
	NrmQuark		type;
	NhlLayer		child;
} NhlCoordArrTableLayerPart;

typedef struct _NhlCoordArrTableLayerRec{
	NhlBaseLayerPart		base;
	NhlDataItemLayerPart		dataitem;
	NhlCoordArrTableLayerPart	cat;
} NhlCoordArrTableLayerRec;

typedef struct _NhlCoordArrTableLayerClassPart{
	int	foo;
} NhlCoordArrTableLayerClassPart;

typedef struct _NhlCoordArrTableLayerClassRec{
	NhlBaseLayerClassPart		base_class;
	NhlDataItemLayerClassPart	dataitem_class;
	NhlCoordArrTableLayerClassPart	cat_class;
} NhlCoordArrTableLayerClassRec;

typedef struct _NhlCoordArrTableLayerClassRec *NhlCoordArrTableLayerClass;
typedef struct _NhlCoordArrTableLayerRec *NhlCoordArrTableLayer;

extern NhlCoordArrTableLayerClassRec NhlcoordArrTableLayerClassRec;

#endif  /* _NCoordArrTableP_h */

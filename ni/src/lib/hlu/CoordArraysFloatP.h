/*
 *      $Id: CoordArraysFloatP.h,v 1.1 1993-09-15 22:10:38 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		CoordArraysFloatP.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jul 28 12:09:03 MDT 1993
 *
 *	Description:	This file contains all the declarations for the
 *			CoordArraysFloat object.  This object is a private
 *			object to be used internally by DataComm sub-classes
 *			and by the CoordArrays object so there is no
 *			public header file.
 */
#ifndef _NCoordArraysFloatP_h
#define _NCoordArraysFloatP_h
#include <ncarg/hlu/BaseP.h>

typedef struct _CoordArraysFloatLayerClassRec *CoordArraysFloatLayerClass;
typedef struct _CoordArraysFloatLayerRec *CoordArraysFloatLayer;

extern LayerClass coordArraysFloatLayerClass;

typedef struct _CoordArraysFloatLayerPart{
	/* User setable resource fields */
	int		foo;
	/* Private Fields */
	float		max_x;
	float		max_y;
	float		min_x;
	float		min_y;

	NhlBoolean	missing_x_set;
	NhlBoolean	missing_y_set;
	float		missing_x;
	float		missing_y;
} CoordArraysFloatLayerPart;

typedef struct _CoordArraysFloatLayerRec{
	ObjLayerPart			base;
	CoordArraysFloatLayerPart	carraysfloat;
} CoordArraysFloatLayerRec;

typedef struct _CoordArraysFloatLayerClassPart{
	int	foo;
} CoordArraysFloatLayerClassPart;

typedef struct _CoordArraysFloatLayerClassRec{
	ObjLayerClassPart			base_class;
	CoordArraysFloatLayerClassPart		ccarraysfloat_class;
} CoordArraysFloatLayerClassRec;

extern CoordArraysFloatLayerClassRec coordArraysFloatLayerClassRec;

#endif	/* _NCoordArraysFloatP_h */

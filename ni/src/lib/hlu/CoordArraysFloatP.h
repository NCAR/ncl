/*
 *      $Id: CoordArraysFloatP.h,v 1.2 1994-01-21 19:29:33 boote Exp $
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
	NhlGenArray	xarray;
	NhlGenArray	yarray;

	int		x_cast;
	int		y_cast;

	NhlBoolean	copy_arrays;

	float		missing_x;
	float		missing_y;
	float		max_x;
	float		max_y;
	float		min_x;
	float		min_y;

	/* Private Fields */
	NhlBoolean	missing_x_set;
	NhlBoolean	missing_y_set;
	NhlBoolean	max_x_set;
	NhlBoolean	max_y_set;
	NhlBoolean	min_x_set;
	NhlBoolean	min_y_set;
	NhlBoolean	x_cast_set;
	NhlBoolean	y_cast_set;
} CoordArraysFloatLayerPart;

typedef struct _CoordArraysFloatLayerRec{
	ObjLayerPart			base;
	CoordArraysFloatLayerPart	carrfloat;
} CoordArraysFloatLayerRec;

typedef struct _CoordArraysFloatLayerClassPart{
	int	foo;
} CoordArraysFloatLayerClassPart;

typedef struct _CoordArraysFloatLayerClassRec{
	ObjLayerClassPart			base_class;
	CoordArraysFloatLayerClassPart		ccarrfloat_class;
} CoordArraysFloatLayerClassRec;

extern CoordArraysFloatLayerClassRec coordArraysFloatLayerClassRec;

#endif	/* _NCoordArraysFloatP_h */

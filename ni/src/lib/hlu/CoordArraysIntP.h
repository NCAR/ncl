/*
 *      $Id: CoordArraysIntP.h,v 1.2 1994-01-21 19:29:36 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		CoordArraysIntP.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jul 28 12:09:03 MDT 1993
 *
 *	Description:	This file contains all the declarations for the
 *			CoordArraysInt object.  This object is a private
 *			object to be used internally by DataComm sub-classes
 *			and by the CoordArrays object so there is no
 *			public header file.
 */
#ifndef _NCoordArraysIntP_h
#define _NCoordArraysIntP_h
#include <ncarg/hlu/BaseP.h>

typedef struct _CoordArraysIntLayerClassRec *CoordArraysIntLayerClass;
typedef struct _CoordArraysIntLayerRec *CoordArraysIntLayer;

extern LayerClass coordArraysIntLayerClass;

typedef struct _CoordArraysIntLayerPart{
	/* User setable resource fields */
	NhlGenArray	xarray;
	NhlGenArray	yarray;

	int		x_cast;
	int		y_cast;

	NhlBoolean	copy_arrays;

	int		missing_x;
	int		missing_y;
	int		max_x;
	int		max_y;
	int		min_x;
	int		min_y;

	/* Private Fields */
	NhlBoolean	missing_x_set;
	NhlBoolean	missing_y_set;
	NhlBoolean	max_x_set;
	NhlBoolean	max_y_set;
	NhlBoolean	min_x_set;
	NhlBoolean	min_y_set;
	NhlBoolean	x_cast_set;
	NhlBoolean	y_cast_set;
} CoordArraysIntLayerPart;

typedef struct _CoordArraysIntLayerRec{
	ObjLayerPart			base;
	CoordArraysIntLayerPart		carrint;
} CoordArraysIntLayerRec;

typedef struct _CoordArraysIntLayerClassPart{
	int	foo;
} CoordArraysIntLayerClassPart;

typedef struct _CoordArraysIntLayerClassRec{
	ObjLayerClassPart		base_class;
	CoordArraysIntLayerClassPart	carrint_class;
} CoordArraysIntLayerClassRec;

extern CoordArraysIntLayerClassRec coordArraysIntLayerClassRec;

#endif /*_NCoordArraysIntP_h */

/*
 *      $Id: CoordArraysIntP.h,v 1.1 1993-09-15 22:10:41 boote Exp $
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
	int		foo;
	/* Private Fields */
	int		max_x;
	int		max_y;
	int		min_x;
	int		min_y;
} CoordArraysIntLayerPart;

typedef struct _CoordArraysIntLayerRec{
	ObjLayerPart			base;
	CoordArraysIntLayerPart		carraysint;
} CoordArraysIntLayerRec;

typedef struct _CoordArraysIntLayerClassPart{
	int	foo;
} CoordArraysIntLayerClassPart;

typedef struct _CoordArraysIntLayerClassRec{
	ObjLayerClassPart		base_class;
	CoordArraysIntLayerClassPart	carraysint_class;
} CoordArraysIntLayerClassRec;

extern CoordArraysIntLayerClassRec coordArraysIntLayerClassRec;

#endif /*_NCoordArraysIntP_h */

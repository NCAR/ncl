/*
 *      $Id: CoordArrTableFloatP.h,v 1.1 1993-09-15 22:10:19 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		CoordArrTableFloatP.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jul 28 12:09:03 MDT 1993
 *
 *	Description:	This file contains all the declarations for the
 *			CoordArrTableFloat object.  This object is a private
 *			object to be used internally by DataComm sub-classes
 *			and by the CoordArrTable object so there is no
 *			public header file.
 */
#ifndef _NCoordArrTableFloatP_h
#define _NCoordArrTableFloatP_h
#include <ncarg/hlu/BaseP.h>

typedef struct _CoordArrTableFloatLayerClassRec *CoordArrTableFloatLayerClass;
typedef struct _CoordArrTableFloatLayerRec *CoordArrTableFloatLayer;

extern LayerClass coordArrTableFloatLayerClass;

typedef struct _CoordArrTableFloatLayerPart{
	/* User setable resource fields */
	NhlGenArray	xtable;
	NhlGenArray	ytable;
	NhlGenArray	xtable_lens;
	NhlGenArray	ytable_lens;

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
} CoordArrTableFloatLayerPart;

typedef struct _CoordArrTableFloatLayerRec{
	ObjLayerPart			base;
	CoordArrTableFloatLayerPart	catfloat;
} CoordArrTableFloatLayerRec;

typedef struct _CoordArrTableFloatLayerClassPart{
	int	foo;
} CoordArrTableFloatLayerClassPart;

typedef struct _CoordArrTableFloatLayerClassRec{
	ObjLayerClassPart			base_class;
	CoordArrTableFloatLayerClassPart	catfloat_class;
} CoordArrTableFloatLayerClassRec;

extern CoordArrTableFloatLayerClassRec coordArrTableFloatLayerClassRec;

#endif	/* _NCoordArrTableFloatP_h */

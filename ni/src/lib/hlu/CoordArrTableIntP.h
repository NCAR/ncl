/*
 *      $Id: CoordArrTableIntP.h,v 1.2 1993-10-06 01:55:07 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		CoordArrTableIntP.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jul 28 12:09:03 MDT 1993
 *
 *	Description:	This file contains all the declarations for the
 *			CoordArrTableInt object.  This object is a private
 *			object to be used internally by DataComm sub-classes
 *			and by the CoordArrTable object so there is no
 *			public header file.
 */
#ifndef _NCoordArrTableIntP_h
#define _NCoordArrTableIntP_h

typedef struct _CoordArrTableIntLayerClassRec *CoordArrTableIntLayerClass;
typedef struct _CoordArrTableIntLayerRec *CoordArrTableIntLayer;

extern LayerClass coordArrTableIntLayerClass;

typedef struct _CoordArrTableIntLayerPart{
	/* User setable resource fields */
	NhlGenArray	xtable;
	NhlGenArray	ytable;
	NhlGenArray	xtable_lens;
	NhlGenArray	ytable_lens;

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
} CoordArrTableIntLayerPart;

typedef struct _CoordArrTableIntLayerRec{
	ObjLayerPart			base;
	CoordArrTableIntLayerPart	catint;
} CoordArrTableIntLayerRec;

typedef struct _CoordArrTableIntLayerClassPart{
	int	foo;
} CoordArrTableIntLayerClassPart;

typedef struct _CoordArrTableIntLayerClassRec{
	ObjLayerClassPart		base_class;
	CoordArrTableIntLayerClassPart	catint_class;
} CoordArrTableIntLayerClassRec;

extern CoordArrTableIntLayerClassRec coordArrTableIntLayerClassRec;

#endif /*_NCoordArrTableIntP_h */

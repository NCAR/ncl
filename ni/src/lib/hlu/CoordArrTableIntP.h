/*
 *      $Id: CoordArrTableIntP.h,v 1.4 1994-01-27 21:22:00 boote Exp $
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

typedef struct _NhlCoordArrTableIntLayerClassRec *NhlCoordArrTableIntLayerClass;
typedef struct _NhlCoordArrTableIntLayerRec *NhlCoordArrTableIntLayer;

extern NhlLayerClass NhlcoordArrTableIntLayerClass;

typedef struct _NhlCoordArrTableIntLayerPart{
	/* User setable resource fields */
	NhlGenArray	xtable;
	NhlGenArray	ytable;
	NhlGenArray	xtable_lens;
	NhlGenArray	ytable_lens;

	NhlBoolean	copy_tables;

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
	NhlBoolean	own_x;
	NhlBoolean	own_y;
} NhlCoordArrTableIntLayerPart;

typedef struct _NhlCoordArrTableIntLayerRec{
	NhlObjLayerPart			base;
	NhlCoordArrTableIntLayerPart	catint;
} NhlCoordArrTableIntLayerRec;

typedef struct _NhlCoordArrTableIntLayerClassPart{
	int	foo;
} NhlCoordArrTableIntLayerClassPart;

typedef struct _NhlCoordArrTableIntLayerClassRec{
	NhlObjLayerClassPart			base_class;
	NhlCoordArrTableIntLayerClassPart	catint_class;
} NhlCoordArrTableIntLayerClassRec;

extern NhlCoordArrTableIntLayerClassRec NhlcoordArrTableIntLayerClassRec;

#endif /*_NCoordArrTableIntP_h */

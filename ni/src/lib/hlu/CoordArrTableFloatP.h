/*
 *      $Id: CoordArrTableFloatP.h,v 1.5 1994-07-12 20:51:35 boote Exp $
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

typedef struct _NhlCoordArrTableFloatLayerClassRec
					*NhlCoordArrTableFloatLayerClass;
typedef struct _NhlCoordArrTableFloatLayerRec *NhlCoordArrTableFloatLayer;

extern NhlLayerClass NhlcoordArrTableFloatLayerClass;

typedef struct _NhlCoordArrTableFloatLayerPart{
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

	/*
	 * Private Fields
	 */
	NhlBoolean	missing_x_set;
	NhlBoolean	missing_y_set;
} NhlCoordArrTableFloatLayerPart;

typedef struct _NhlCoordArrTableFloatLayerRec{
	NhlObjLayerPart			base;
	NhlCoordArrTableFloatLayerPart	flt;
} NhlCoordArrTableFloatLayerRec;

typedef struct _NhlCoordArrTableFloatLayerClassPart{
	int	foo;
} NhlCoordArrTableFloatLayerClassPart;

typedef struct _NhlCoordArrTableFloatLayerClassRec{
	NhlObjLayerClassPart			base_class;
	NhlCoordArrTableFloatLayerClassPart	flt_class;
} NhlCoordArrTableFloatLayerClassRec;

extern NhlCoordArrTableFloatLayerClassRec NhlcoordArrTableFloatLayerClassRec;

#endif	/* _NCoordArrTableFloatP_h */

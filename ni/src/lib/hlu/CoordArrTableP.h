/*
 *      $Id: CoordArrTableP.h,v 1.1 1993-09-15 22:10:26 boote Exp $
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

typedef struct _CoordArrTableLayerPart{
	/* User setable resource fields */
	NhlString		type_string;

	/* Private Fields */
	NrmQuark		type;
	Layer			child;
} CoordArrTableLayerPart;

typedef struct _CoordArrTableLayerRec{
	BaseLayerPart			base;
	DataItemLayerPart		dataitem;
	CoordArrTableLayerPart		cat;
} CoordArrTableLayerRec;

typedef struct _CoordArrTableLayerClassPart{
	int	foo;
} CoordArrTableLayerClassPart;

typedef struct _CoordArrTableLayerClassRec{
	BaseLayerClassPart		base_class;
	DataItemLayerClassPart		dataitem_class;
	CoordArrTableLayerClassPart	cat_class;
} CoordArrTableLayerClassRec;

extern CoordArrTableLayerClassRec coordArrTableLayerClassRec;

#endif  /* _NCoordArrTableP_h */

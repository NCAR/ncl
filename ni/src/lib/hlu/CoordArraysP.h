/*
 *      $Id: CoordArraysP.h,v 1.2 1994-01-21 19:29:39 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		CoordArraysP.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jul 28 11:34:08 MDT 1993
 *
 *	Description:	Private declarations for CoordArrays object.
 */
#ifndef _NCoordArraysP_h
#define _NCoordArraysP_h

#include <ncarg/hlu/DataItemP.h>
#include <ncarg/hlu/CoordArrays.h>
#include <ncarg/hlu/CoordArraysIntP.h>
#include <ncarg/hlu/CoordArraysFloatP.h>

/*
 * Private Resource Names
 */
typedef struct _CoordArraysLayerPart{
	/* User setable resource fields */
	NhlString		type_string;

	/* Private Fields */
	NrmQuark		type;
	Layer			child;
} CoordArraysLayerPart;

typedef struct _CoordArraysLayerRec{
	BaseLayerPart			base;
	DataItemLayerPart		dataitem;
	CoordArraysLayerPart		carr;
} CoordArraysLayerRec;

typedef struct _CoordArraysLayerClassPart{
	int	foo;
} CoordArraysLayerClassPart;

typedef struct _CoordArraysLayerClassRec{
	BaseLayerClassPart		base_class;
	DataItemLayerClassPart		dataitem_class;
	CoordArraysLayerClassPart	carr_class;
} CoordArraysLayerClassRec;

extern CoordArraysLayerClassRec coordArraysLayerClassRec;

#endif  /* _NCoordArraysP_h */

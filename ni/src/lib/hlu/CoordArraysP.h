/*
 *      $Id: CoordArraysP.h,v 1.1 1993-09-15 22:10:44 boote Exp $
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
#define	_NhlNcaXarray	"ca.x.array"
#define	_NhlCcaXarray	"Ca.x.array"
#define	_NhlNcaYarray	"ca.y.array"
#define	_NhlCcaYarray	"Ca.y.array"

typedef struct _CoordArraysLayerPart{
	/* User setable resource fields */
	int			foo;

	/* Private Fields */
	NrmQuark		type_child;
	int			child;
} CoordArraysLayerPart;

typedef struct _CoordArraysLayerRec{
	BaseLayerPart			base;
	DataItemLayerPart		dataitem;
	CoordArraysLayerPart		carrays;
} CoordArraysLayerRec;

typedef struct _CoordArraysLayerClassPart{
	int	foo;
} CoordArraysLayerClassPart;

typedef struct _CoordArraysLayerClassRec{
	BaseLayerClassPart		base_class;
	DataItemLayerClassPart		dataitem_class;
	CoordArraysLayerClassPart	carrays_class;
} CoordArraysLayerClassRec;

extern CoordArraysLayerClassRec coordArraysLayerClassRec;

#endif  /* _NCoordArraysP_h */

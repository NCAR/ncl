/*
 *      $Id: CoordArraysP.h,v 1.5 1994-07-28 22:11:55 boote Exp $
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

/*
 * Private Resource Names
 */
typedef struct _NhlCoordArraysLayerPart{
	/* User setable resource fields */

	NhlGenArray		xarray;
	NhlGenArray		yarray;

	NhlcaCastMode		xcast;
	NhlcaCastMode		ycast;

	NhlBoolean		copy_arrays;

	NhlGenArray		missing_x;
	NhlGenArray		missing_y;

	NhlGenArray		max_x;
	NhlGenArray		max_y;
	NhlGenArray		min_x;
	NhlGenArray		min_y;

	/* Private Fields */

	NhlBoolean		xcast_set;
	NhlBoolean		ycast_set;

	NhlGenArray		my_xarray;
	NhlGenArray		my_yarray;

	_NhlConvertContext	xctxt;
	_NhlConvertContext	yctxt;

	NhlGenArray		my_missing_x;
	NhlGenArray		my_missing_y;

	NhlBoolean		sticky_max_x;
	NhlBoolean		sticky_max_y;
	NhlBoolean		sticky_min_x;
	NhlBoolean		sticky_min_y;
} NhlCoordArraysLayerPart;

typedef struct _NhlCoordArraysLayerRec{
	NhlBaseLayerPart		base;
	NhlDataItemLayerPart		dataitem;
	NhlCoordArraysLayerPart		carr;
} NhlCoordArraysLayerRec;

typedef struct _NhlCoordArraysLayerClassPart{
	int	foo;
} NhlCoordArraysLayerClassPart;

typedef struct _NhlCoordArraysLayerClassRec{
	NhlBaseLayerClassPart		base_class;
	NhlDataItemLayerClassPart	dataitem_class;
	NhlCoordArraysLayerClassPart	carr_class;
} NhlCoordArraysLayerClassRec;

typedef struct _NhlCoordArraysLayerClassRec *NhlCoordArraysLayerClass;
typedef struct _NhlCoordArraysLayerRec *NhlCoordArraysLayer;

extern NhlCoordArraysLayerClassRec NhlcoordArraysLayerClassRec;

#endif  /* _NCoordArraysP_h */

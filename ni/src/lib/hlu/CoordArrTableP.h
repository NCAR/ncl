/*
 *      $Id: CoordArrTableP.h,v 1.4 1995-04-07 10:41:18 boote Exp $
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
#include <ncarg/hlu/CoordArrTableFloatP.h>

typedef struct _NhlCoordArrTableLayerPart{

	NhlString		xtype;
	NhlString		ytype;

	int			xsize;
	int			ysize;

	NhlGenArray		xtable;
	NhlGenArray		ytable;
	NhlGenArray		xtable_lens;
	NhlGenArray		ytable_lens;

	NhlBoolean		copy_tables;

	NhlGenArray		missing_x;
	NhlGenArray		missing_y;
	NhlGenArray		max_x;
	NhlGenArray		max_y;
	NhlGenArray		min_x;
	NhlGenArray		min_y;

	/*
	 * Private Fields
	 */
	NrmQuark		xtypeQ;
	NrmQuark		ytypeQ;
	NhlGenArray		own_x;
	NhlGenArray		own_y;
	_NhlConvertContext	conv_x;
	_NhlConvertContext	conv_y;
	NhlGenArray		own_miss_x;
	NhlGenArray		own_miss_y;
	NhlBoolean		sticky_max_x;
	NhlBoolean		sticky_min_x;
	NhlBoolean		sticky_max_y;
	NhlBoolean		sticky_min_y;
} NhlCoordArrTableLayerPart;

typedef struct _NhlCoordArrTableLayerRec{
	NhlBaseLayerPart		base;
	NhlDataItemLayerPart		dataitem;
	NhlCoordArrTableLayerPart	cat;
} NhlCoordArrTableLayerRec;

typedef struct _NhlCoordArrTableClassPart{
	int	foo;
} NhlCoordArrTableClassPart;

typedef struct _NhlCoordArrTableClassRec{
	NhlBaseClassPart		base_class;
	NhlDataItemClassPart	dataitem_class;
	NhlCoordArrTableClassPart	cat_class;
} NhlCoordArrTableClassRec;

typedef struct _NhlCoordArrTableClassRec *NhlCoordArrTableClass;
typedef struct _NhlCoordArrTableLayerRec *NhlCoordArrTableLayer;

extern NhlCoordArrTableClassRec NhlcoordArrTableClassRec;

#endif  /* _NCoordArrTableP_h */

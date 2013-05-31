/*
 *      $Id: PrimitiveP.h,v 1.1 2000-06-28 19:04:00 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		PrimitiveP.h
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jun 21 17:04:23 MDT 2000
 *
 *	Description:	Private header file for Primitive class
 */
#ifndef _NPRIMITIVEP_h
#define _NPRIMITIVEP_h

#include <ncarg/hlu/BaseP.h>
#include <ncarg/hlu/Primitive.h>

typedef struct _NhlPrimitiveLayerPart{

	/* public resource fields */

	NhlGenArray		x_arr;
	NhlGenArray		y_arr;
	NhlPolyType		poly_type;
	NhlLayer		graphic_style;

}NhlPrimitiveLayerPart;

typedef struct _NhlPrimitiveLayerRec{
	NhlObjLayerPart		base;
	NhlPrimitiveLayerPart	primitive;
}NhlPrimitiveLayerRec;

typedef struct _NhlPrimitiveClassPart {
	char *foo;
}NhlPrimitiveClassPart;

typedef struct _NhlPrimitiveClassRec{
	NhlObjClassPart		base_class;
	NhlPrimitiveClassPart	primitive_class;
}NhlPrimitiveClassRec;

typedef struct _NhlPrimitiveClassRec	*NhlPrimitiveClass;
typedef struct _NhlPrimitiveLayerRec		*NhlPrimitiveLayer;

extern NhlPrimitiveClassRec		NhlprimitiveClassRec;

#endif  /*_NPRIMITIVEP_h*/

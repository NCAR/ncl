/*
 *      $Id: ScalarFieldFloatP.h,v 1.1 1994-04-29 21:31:31 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		ScalarFieldFloatP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Apr  6 17:53:29 MDT 1994
 *
 *	Description:	This file contains all the declarations for the
 *			ScalarFieldFloat object.  This object is a private
 *			object to be used internally by DataComm sub-classes
 *			and by the ScalarField object so there is no
 *			public header file.
 */
#ifndef _NScalarFieldFloatP_h
#define _NScalarFieldFloatP_h
#include <ncarg/hlu/BaseP.h>


typedef struct _NhlScalarFieldFloatLayerClassRec 
				*NhlScalarFieldFloatLayerClass;
typedef struct _NhlScalarFieldFloatLayerRec *NhlScalarFieldFloatLayer;

extern NhlLayerClass NhlscalarFieldFloatLayerClass;

typedef struct _NhlScalarFieldFloatLayerPart{

	/* Public resources */

	NhlGenArray	d_arr;
	NhlGenArray	x_arr;
	NhlGenArray	y_arr;

	NhlBoolean	copy_arrays;
	NhlBoolean	data_order_set;
	int		data_order;

	NhlBoolean	missing_value_set;
	float		missing_value;
	NhlBoolean	data_max_set;
	float		data_max;
	NhlBoolean	data_min_set;
	float		data_min;
	NhlBoolean	x_max_set;
	float		x_max;
	NhlBoolean	x_min_set;
	float		x_min;
	NhlBoolean	y_max_set;
	float		y_max;
	NhlBoolean	y_min_set;
	float		y_min;

} NhlScalarFieldFloatLayerPart;

typedef struct _NhlScalarFieldFloatLayerRec{
	NhlObjLayerPart			base;
	NhlScalarFieldFloatLayerPart	sfieldfloat;
} NhlScalarFieldFloatLayerRec;

typedef struct _NhlScalarFieldFloatLayerClassPart{
	int	foo;
} NhlScalarFieldFloatLayerClassPart;

typedef struct _NhlScalarFieldFloatLayerClassRec{
	NhlObjLayerClassPart			base_class;
	NhlScalarFieldFloatLayerClassPart	sfieldfloat_class;
} NhlScalarFieldFloatLayerClassRec;

extern NhlScalarFieldFloatLayerClassRec NhlscalarFieldFloatLayerClassRec;

#endif	/* _NScalarFieldFloatP_h */

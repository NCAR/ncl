/*
 *      $Id: ScalarFieldFloatP.h,v 1.2 1994-05-17 22:26:20 dbrown Exp $
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
#include <ncarg/hlu/ScalarField.h>


typedef struct _NhlScalarFieldFloatLayerClassRec 
				*NhlScalarFieldFloatLayerClass;
typedef struct _NhlScalarFieldFloatLayerRec *NhlScalarFieldFloatLayer;

extern NhlLayerClass NhlscalarFieldFloatLayerClass;

typedef struct _NhlScalarFieldFloatLayerPart{

	/* Public resources */

	NhlGenArray	d_arr;
	NhlGenArray	x_arr;
	NhlGenArray	y_arr;

	NhlBoolean	missing_value_set;
	float		missing_value;
	float		data_min;
	float		data_max;
	float		x_start;
	float		x_end;
	float		y_start;
	float		y_end;

	/* private resources */

	int		begin;
	int		fast_dim;
	int		fast_len;
	int		slow_len;

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

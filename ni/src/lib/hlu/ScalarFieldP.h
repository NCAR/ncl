/*
 *      $Id: ScalarFieldP.h,v 1.7 1997-08-11 18:22:21 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		ScalarFieldP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Apr  6 17:53:29 MDT 1994
 *
 *	Description:	Private declarations for ScalarField object.
 */
#ifndef _NScalarFieldP_h
#define _NScalarFieldP_h

#include <ncarg/hlu/DataItemP.h>
#include <ncarg/hlu/ScalarField.h>
#include <ncarg/hlu/ScalarFieldFloatP.h>

#ifndef FLT_MAX
#define FLT_MAX			10.0e37
#endif

typedef struct _NhlScalarFieldLayerPart{

	NhlString		type_string;

	/* Public resources */

	NhlGenArray	d_arr;
	NhlGenArray	x_arr;
	NhlGenArray	y_arr;

	NhlBoolean	subset_by_index;
	NhlBoolean	copy_arrays;
	NhlBoolean	exchange_dimensions;

	NhlGenArray	missing_value;
	NhlGenArray	data_min;
	NhlGenArray	data_max;
	NhlGenArray	x_start;
	NhlGenArray	x_end;
	NhlGenArray	y_start;
	NhlGenArray	y_end;

	NhlGenArray	x_subset_start;
	NhlGenArray	x_subset_end;
	NhlGenArray	y_subset_start;
	NhlGenArray	y_subset_end;

	int		x_index_start;
	int		x_index_end;
	int		y_index_start;
	int		y_index_end;
	
	int		x_stride;
	int		y_stride;

	float		x_actual_start;
	float		x_actual_end;
	float		y_actual_start;
	float		y_actual_end;

	/* private fields */

	int		ix_start;
	int		ix_end;
	int		iy_start;
	int		iy_end;

        NhlBoolean	up_to_date;
	NhlScalarFieldFloatLayer	sffloat;

} NhlScalarFieldLayerPart;

typedef struct _NhlScalarFieldLayerRec{
	NhlBaseLayerPart		base;
	NhlDataItemLayerPart		dataitem;
	NhlScalarFieldLayerPart		sfield;
} NhlScalarFieldLayerRec;

typedef struct _NhlScalarFieldClassPart{
	int	foo;
} NhlScalarFieldClassPart;

typedef struct _NhlScalarFieldClassRec{
	NhlBaseClassPart		base_class;
	NhlDataItemClassPart	dataitem_class;
	NhlScalarFieldClassPart	sfield_class;
} NhlScalarFieldClassRec;

typedef struct _NhlScalarFieldClassRec *NhlScalarFieldClass;
typedef struct _NhlScalarFieldLayerRec *NhlScalarFieldLayer;

extern NhlScalarFieldClassRec NhlscalarFieldClassRec;

#endif  /* _NScalarFieldP_h */


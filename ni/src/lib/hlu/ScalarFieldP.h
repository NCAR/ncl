/*
 *      $Id: ScalarFieldP.h,v 1.13 2004-07-23 21:24:55 dbrown Exp $
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

typedef struct _NhlScalarFieldLayerPart{

	/* Public resources */

	NhlGenArray	d_arr;
	NhlGenArray	x_arr;
	NhlGenArray	y_arr;
	NhldiGridType   grid_type;

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
	int             xc_el_count;  /* x/y array x dim len */
        int		xd_el_count;  /* data array x dim len */
	float		y_actual_start;
	float		y_actual_end;
	int             yc_el_count;  /* x/y array y dim len */
        int		yd_el_count;  /* data array y dim len */

	/* private fields */

	int		ix_start;
	int		ix_end;
	int		iy_start;
	int		iy_end;

	int		xc_start_el; /* 1D: same as ix_start/ix_end */
	int		xc_end_el;   /* 2D: element # of flattened array */
	int		yc_start_el;
	int		yc_end_el;
        
        NhlBoolean	xstart_byindex;
        NhlBoolean	xend_byindex;
        NhlBoolean	ystart_byindex;
        NhlBoolean	yend_byindex;

	NhlBoolean	xc_is_bounds;
	NhlBoolean	yc_is_bounds;
	NhlBoolean	xc_is_linear;
	NhlBoolean	yc_is_linear;

        NhlBoolean	up_to_date;
	NhlScalarFieldFloatLayer	sffloat;
	int		changed;

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


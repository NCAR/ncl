/*
 *      $Id: MeshScalarFieldP.h,v 1.2 2004-08-11 23:52:50 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		MeshScalarFieldP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jun 23 18:48:07 MDT 2004
 *
 *	Description:	Private declarations for MeshScalarField object.
 */
#ifndef _NMeshScalarFieldP_h
#define _NMeshScalarFieldP_h

#include <ncarg/hlu/ScalarFieldP.h>
#include <ncarg/hlu/MeshScalarField.h>
#include <ncarg/hlu/ScalarFieldFloatP.h>

typedef struct _NhlMeshScalarFieldLayerPart{

	/* Public resources */

	NhlGenArray	d_arr;
	NhlGenArray	x_arr;
	NhlGenArray	y_arr;
	NhlGenArray     element_nodes;
	NhlGenArray     node_indexes;
	NhlGenArray     x_cell_bounds;
	NhlGenArray     y_cell_bounds;
	int		first_node_index;		
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
	float		y_actual_start;
	float		y_actual_end;
	int             yc_el_count;  /* x/y array y dim len */
        int		d_el_count;  /* data array dim len */

	/* private fields */

	int		istart;
	int		iend;
	int             istride;
        
        NhlBoolean	xstart_byindex;
        NhlBoolean	xend_byindex;
        NhlBoolean	ystart_byindex;
        NhlBoolean	yend_byindex;

	NhlBoolean	xc_is_bounds;
	NhlBoolean	yc_is_bounds;

        NhlBoolean	up_to_date;
	NhlScalarFieldFloatLayer	sffloat;
	int		changed;

} NhlMeshScalarFieldLayerPart;

typedef struct _NhlMeshScalarFieldLayerRec{
	NhlBaseLayerPart		base;
	NhlDataItemLayerPart		dataitem;
	NhlScalarFieldLayerPart		sfield;
	NhlMeshScalarFieldLayerPart	msfield;
} NhlMeshScalarFieldLayerRec;

typedef struct _NhlMeshScalarFieldClassPart{
	int	foo;
} NhlMeshScalarFieldClassPart;

typedef struct _NhlMeshScalarFieldClassRec{
	NhlBaseClassPart		base_class;
	NhlDataItemClassPart		dataitem_class;
	NhlScalarFieldClassPart		sfield_class;
	NhlMeshScalarFieldClassPart	msfield_class;
} NhlMeshScalarFieldClassRec;

typedef struct _NhlMeshScalarFieldClassRec *NhlMeshScalarFieldClass;
typedef struct _NhlMeshScalarFieldLayerRec *NhlMeshScalarFieldLayer;

extern NhlMeshScalarFieldClassRec NhlmeshScalarFieldClassRec;

#endif  /* _NMeshScalarFieldP_h */


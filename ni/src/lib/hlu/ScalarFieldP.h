/*
 *      $Id: ScalarFieldP.h,v 1.1 1994-04-29 21:31:33 dbrown Exp $
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

/*
 * Private Resource Names 
 * (Used to communicate the variable type public resources to the 
 * ScalarFieldFloat object.)
 */

#define	NhlNsfMissingValueF	".sfMissingValueF"
#define	NhlNsfDataMaxF		".sfDataMaxF"
#define	NhlNsfDataMinF		".sfDataMinF"
#define	NhlNsfXMaxF		".sfXMaxF"
#define	NhlNsfYMaxF		".sfYMaxF"
#define	NhlNsfXMinF		".sfXMinF"
#define	NhlNsfYMinF		".sfYMinF"

#define	NhlCsfMissingValueF	".SfMissingValueF"
#define	NhlCsfDataMaxF		".SfDataMaxF"
#define	NhlCsfDataMinF		".SfDataMinF"
#define	NhlCsfXMaxF		".SfXMaxF"
#define	NhlCsfYMaxF		".SfYMaxF"
#define	NhlCsfXMinF		".SfXMinF"
#define	NhlCsfYMinF		".SfYMinF"

typedef struct _NhlScalarFieldLayerPart{

	NhlString		type_string;

	/* Public resources */

	NhlGenArray	d_arr;
	NhlGenArray	x_arr;
	NhlGenArray	y_arr;

	NhlBoolean	copy_arrays;
	NhlBoolean	data_order_set;
	int		data_order;

	NhlGenArray	missing_value;
	NhlGenArray	data_min;
	NhlGenArray	data_max;
	NhlGenArray	x_min;
	NhlGenArray	x_max;
	NhlGenArray	y_min;
	NhlGenArray	y_max;

	NhlGenArray	x_subset_min;
	NhlGenArray	x_subset_max;
	NhlGenArray	y_subset_min;
	NhlGenArray	y_subset_max;

	int		x_index_min;
	int		x_index_max;
	int		y_index_min;
	int		y_index_max;
	
	int		x_stride;
	int		y_stride;

	/* Private Fields */
	NrmQuark		type;
	NhlLayer		child;

} NhlScalarFieldLayerPart;

typedef struct _NhlScalarFieldLayerRec{
	NhlBaseLayerPart		base;
	NhlDataItemLayerPart		dataitem;
	NhlScalarFieldLayerPart		sfield;
} NhlScalarFieldLayerRec;

typedef struct _NhlScalarFieldLayerClassPart{
	int	foo;
} NhlScalarFieldLayerClassPart;

typedef struct _NhlScalarFieldLayerClassRec{
	NhlBaseLayerClassPart		base_class;
	NhlDataItemLayerClassPart	dataitem_class;
	NhlScalarFieldLayerClassPart	sfield_class;
} NhlScalarFieldLayerClassRec;

typedef struct _NhlScalarFieldLayerClassRec *NhlScalarFieldLayerClass;
typedef struct _NhlScalarFieldLayerRec *NhlScalarFieldLayer;

extern NhlScalarFieldLayerClassRec NhlscalarFieldLayerClassRec;

#endif  /* _NScalarFieldP_h */


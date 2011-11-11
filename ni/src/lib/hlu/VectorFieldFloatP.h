/*
 *      $Id: VectorFieldFloatP.h,v 1.5 2003-09-10 21:30:00 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		VectorFieldFloatP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Sep 28 11:47:36 MDT 1995
 *
 *	Description:	This file contains all the declarations for the
 *			VectorFieldFloat object.  This object is a private
 *			object to be used internally by DataComm sub-classes
 *			and by the VectorField object so there is no
 *			public header file.
 */
#ifndef _NVectorFieldFloatP_h
#define _NVectorFieldFloatP_h
#include <ncarg/hlu/BaseP.h>
#include <ncarg/hlu/VectorField.h>


typedef enum _NhlvfMissMode { 
	vfNONE,
	vfBOTH,
	vfUONLY,
	vfVONLY 
} NhlvfMissMode;

typedef struct _NhlVectorFieldFloatClassRec 
				*NhlVectorFieldFloatClass;
typedef struct _NhlVectorFieldFloatLayerRec *NhlVectorFieldFloatLayer;

extern NhlClass NhlvectorFieldFloatClass;

#define _NhlvfDARR_CHANGED 1
#define _NhlvfUARR_CHANGED 2
#define _NhlvfVARR_CHANGED 4
#define _NhlvfXARR_CHANGED 8
#define _NhlvfYARR_CHANGED 16

typedef struct _NhlVectorFieldFloatLayerPart{

	/* all fields are private and set directly by the converter */

	NhlGenArray	d_arr;
	NhlGenArray	u_arr;
	NhlGenArray	v_arr;
	NhlGenArray	x_arr;
	NhlGenArray	y_arr;

	NhldiGridType   grid_type;
	NhlBoolean      xc_is_linear;
	NhlBoolean      yc_is_linear;

	NhlvfMissMode	miss_mode;
	float		u_missing_value;
	float		v_missing_value;
	float		mag_min;
	float		mag_max;
	float		u_min;
	float		u_max;
	float		v_min;
	float		v_max;
	float		x_start;
	float		x_end;
	float		y_start;
	float		y_end;
        
        int		ix_start;
        int		ix_end;
        int		iy_start;
        int		iy_end;

	int		begin;
	int		fast_dim;
	int		fast_len;
	int		slow_len;
	int		x_stride;
	int		y_stride;
	NhlBoolean	polar_data;
	int		changed;
} NhlVectorFieldFloatLayerPart;

typedef struct _NhlVectorFieldFloatLayerRec{
	NhlObjLayerPart			base;
	NhlVectorFieldFloatLayerPart	vfieldfloat;
} NhlVectorFieldFloatLayerRec;

typedef struct _NhlVectorFieldFloatClassPart{
	int	foo;
} NhlVectorFieldFloatClassPart;

typedef struct _NhlVectorFieldFloatClassRec{
	NhlObjClassPart			base_class;
	NhlVectorFieldFloatClassPart	vfieldfloat_class;
} NhlVectorFieldFloatClassRec;

extern NhlVectorFieldFloatClassRec NhlvectorFieldFloatClassRec;

#endif	/* _NVectorFieldFloatP_h */

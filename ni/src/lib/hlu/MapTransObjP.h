/*
 *      $Id: MapTransObjP.h,v 1.4 1994-06-24 00:39:45 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Oct 28 13:42:41 MST 1992
 *
 *	Description:	
 */
#ifndef _NMapTransObjP_h
#define _NMapTransObjP_h

#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/WorkstationI.h>
#include <ncarg/hlu/TransObjP.h>
#include <ncarg/hlu/MapTransObj.h>

typedef struct _NhlMapTransObjLayerPart{

	/* User resource fields */

	NhlProjection	projection;

	float	center_lat;
	float	center_lon;
	float	center_rot;

	NhlMapLimitMode	map_limit_mode;
	float		min_lat;
	float		max_lat;
	float		min_lon;
	float		max_lon;
	float		left_angle;
	float		right_angle;
	float		bottom_angle;
	float		top_angle;
	float		actual_min_lat;
	float		actual_max_lat;
	float		actual_min_lon;
	float		actual_max_lon;
	
	float		left_corner_lat;
	float		left_corner_lon;
	float		right_corner_lat;
	float		right_corner_lon;

	float		left_window;
	float		right_window;
	float		bottom_window;
	float		top_window;

	float	lambert_parallel_1;
	float	lambert_parallel_2;
	float	lambert_meridian;
	float	satellite_dist;
	float	satellite_angle_1;
	float	satellite_angle_2;
	int	elliptical_boundary;

	char	*rect_limit_type;
	float	*rect_limit_1;
	float	*rect_limit_2;
	float	*rect_limit_3;
	float	*rect_limit_4;

	/* Private Fields */
	float	aspect;
	float	map_pos_l;
	float	map_pos_r;
	float 	map_pos_t;
	float	map_pos_b;
	float	ul;
	float	ur;
	float 	ut;
	float	ub;
	NhlBoolean updated;
}NhlMapTransObjLayerPart;

typedef struct _NhlMapTransObjLayerRec {
	NhlObjLayerPart		base;
	NhlTransObjLayerPart	trobj;
	NhlMapTransObjLayerPart	mptrans;
} NhlMapTransObjLayerRec;

typedef struct _NhlMapTransObjLayerClassPart {
	char *foo;
} NhlMapTransObjLayerClassPart;

typedef struct _NhlMapTransObjLayerClassRec {
	NhlObjLayerClassPart		base_class;
	NhlTransObjLayerClassPart	trobj_class;
	NhlMapTransObjLayerClassPart	mptrans_class;
} NhlMapTransObjLayerClassRec;

typedef struct _NhlMapTransObjLayerRec *NhlMapTransObjLayer;
typedef struct _NhlMapTransObjLayerClassRec *NhlMapTransObjLayerClass;

extern NhlMapTransObjLayerClassRec NhlmapTransObjLayerClassRec;

#endif /*_NMapTransObjP_h*/

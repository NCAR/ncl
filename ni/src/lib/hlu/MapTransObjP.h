
/*
 *      $Id: MapTransObjP.h,v 1.1 1993-04-30 17:23:00 boote Exp $
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
#include <ncarg/hlu/TransObjP.h>
#include <ncarg/hlu/MapTransObj.h>

typedef struct _MapTransObjLayerPart{
	/* User setable resource fields */
	char	*projection;
	char	*outline_type;
	float	center_lat;
	float	center_lon;
	float	center_rot;
	char	*rect_limit_type;
	float	*rect_limit_1;
	float	*rect_limit_2;
	float	*rect_limit_3;
	float	*rect_limit_4;
	float	lambert_parallel_1;
	float	lambert_parallel_2;
	float	lambert_meridian;
	float	satellite_dist;
	float	satellite_angle_1;
	float	satellite_angle_2;
	int	elliptical_boundary;
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
}MapTransObjLayerPart;

typedef struct _MapTransObjLayerRec {
	BaseLayerPart	base;
	TransObjLayerPart trobj;
	MapTransObjLayerPart mptrans;
} MapTransObjLayerRec;

typedef struct _MapTransObjLayerClassPart {
	char *foo;
} MapTransObjLayerClassPart;

typedef struct _MapTransObjLayerClassRec {
	BaseLayerClassPart	base_class;
	TransObjLayerClassPart	trobj_class;
	MapTransObjLayerClass	mptrans_class;
} MapTransObjLayerClassRec;

extern MapTransObjLayerClassRec mapTransObjLayerClassRec;

#endif /*_NMapTransObjP_h*/

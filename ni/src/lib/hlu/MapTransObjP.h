/*
 *      $Id: MapTransObjP.h,v 1.3 1994-01-27 21:24:53 boote Exp $
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

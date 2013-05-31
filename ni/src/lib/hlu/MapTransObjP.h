/*
 *      $Id: MapTransObjP.h,v 1.11 2000-05-16 01:35:27 dbrown Exp $
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

/* read-only private resource used by the MapPlot to find out if the
 * trans obj has changed.
 */

#define NhlNmpTransChanged	".mpTransChanged"

/* undocumented debugging aid */
#define NhlNmpDumpPolygonAreaMap "mpDumpPolygonAreaMap"
#define NhlCmpDumpPolygonAreaMap "MpDumpPolygonAreaMap"

typedef struct _NhlMapTransObjLayerPart{

	/* User resource fields */

	NhlProjection	projection;

	float		center_lat;
	float		center_lon;
	float		center_rot;
	NhlBoolean	rel_center_lat;
	NhlBoolean	rel_center_lon;
	NhlBoolean	preserve_aspect;

	float		map_pos_l;    /* read only */
	float		map_pos_r;    /* read only */
	float 		map_pos_t;    /* read only */
	float		map_pos_b;    /* read only */

	NhlMapLimitMode	map_limit_mode;

	float		min_lat;
	float		max_lat;
	float		min_lon;
	float		max_lon;

	float		left_angle;
	float		right_angle;
	float		bottom_angle;
	float		top_angle;

	float		left_npc;
	float		right_npc;
	float		bottom_npc;
	float		top_npc;

	float		left_ndc;
	float		right_ndc;
	float		bottom_ndc;
	float		top_ndc;

	float		left_corner_lat;
	float		left_corner_lon;
	float		right_corner_lat;
	float		right_corner_lon;

	float		left_point_lon;
	float		left_point_lat;
	float		right_point_lon;
	float		right_point_lat;
	float		bottom_point_lon;
	float		bottom_point_lat;
	float		top_point_lon;
	float		top_point_lat;

	float		left_window;
	float		right_window;
	float		bottom_window;
	float		top_window;

	float		lambert_parallel_1;
	float		lambert_parallel_2;
	float		lambert_meridian;
	float		satellite_dist;
	float		satellite_angle_1;
	float		satellite_angle_2;
	NhlBoolean	elliptical_boundary;
	NhlBoolean	great_circle_lines_on;
	NhlMapPolyMode	map_poly_mode;

	float		actual_min_lat;
	float		actual_max_lat;
	float		actual_min_lon;
	float		actual_max_lon;
        float   	data_xmin;
        float   	data_xmax;

	/* Private resources */

	NhlBoolean trans_changed;
	NhlBoolean dump_polygon_area_map;

	/* Private Fields */

	float	aspect;
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

typedef struct _NhlMapTransObjClassPart {
	int aws_id;
} NhlMapTransObjClassPart;

typedef struct _NhlMapTransObjClassRec {
	NhlObjClassPart		base_class;
	NhlTransObjClassPart	trobj_class;
	NhlMapTransObjClassPart	mptrans_class;
} NhlMapTransObjClassRec;

typedef struct _NhlMapTransObjLayerRec *NhlMapTransObjLayer;
typedef struct _NhlMapTransObjClassRec *NhlMapTransObjClass;

extern NhlMapTransObjClassRec NhlmapTransObjClassRec;

#endif /*_NMapTransObjP_h*/

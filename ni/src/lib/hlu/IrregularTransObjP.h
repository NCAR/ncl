/*
 *      $Id: IrregularTransObjP.h,v 1.7 1995-05-03 03:11:13 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		IrregularTransObjP.h
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Oct 16 13:04:03 MDT 1992
 *
 *	Description:	Private header file for irregular type 2 grid 
 *			transformations.
 */
#ifndef _NIrregularTransObjP_h
#define _NIrregularTransObjP_h

#include <ncarg/hlu/TransObjP.h>
#include <ncarg/hlu/IrregularTransObj.h>

#include <ncarg/hlu/CoordApprox.h>

#define NhlNtrLowLevelLogOn ".trLowLevelLogOn"
#define NhlCtrLowLevelLogOn ".TrLowLevelLogOn"

typedef struct _NhlIrregularTransObjLayerPart {
	/* Publically settable resources */

	NhlAxisType	x_axis_type;
	NhlGenArray	x_coord_points_ga;
	NhlGenArray	x_inter_points_ga;
	NhlBoolean	x_min_set;
	float		x_min;
	NhlBoolean	x_max_set;
	float		x_max;
	NhlBoolean	x_reverse;
	float		x_tension;
	int		x_samples;
	NhlAxisType	y_axis_type;
	NhlGenArray	y_coord_points_ga;
	NhlGenArray	y_inter_points_ga;
	NhlBoolean	y_min_set;
	float		y_min;
	NhlBoolean	y_max_set;
	float		y_max;
	NhlBoolean	y_reverse;
	float		y_tension;
	int		y_samples;
	
	/* Private fields */
	int 		x_num_points;
	float		*x_coord_points;
	float		*x_inter_points;
	NhlBoolean	x_use_log;
	float		x_log_lin_points[3];
	int 		y_num_points;
	float		*y_coord_points;
	float		*y_inter_points;
	NhlBoolean	y_use_log;
	float		y_log_lin_points[3];
	NhlCoordDat	thecoord;
	int		xstatus;
	int		ystatus;
	float		ur;
	float 		ul;
	float		ut;
	float		ub;
	float		ur_save;
	float 		ul_save;
	float		ut_save;
	float		ub_save;
	float 		compc_x_min;
	float		compc_x_max;
	float		compc_y_min;
	float 		compc_y_max;
	int 		log_lin_value;
	NhlCompareDat	*xmin_dat;
	NhlCompareDat	*xmax_dat;
	NhlCompareDat	*ymin_dat;
	NhlCompareDat	*ymax_dat;
	NhlCompareDat	*compc_xmin_dat;
	NhlCompareDat	*compc_xmax_dat;
	NhlCompareDat	*compc_ymin_dat;
	NhlCompareDat	*compc_ymax_dat;
	NhlBoolean	low_level_log_on;

} NhlIrregularTransObjLayerPart;

typedef struct _NhlIrregularTransObjLayerRec {
	NhlObjLayerPart			base;
	NhlTransObjLayerPart		trobj;
	NhlIrregularTransObjLayerPart	irtrans;
} NhlIrregularTransObjLayerRec;

typedef struct _NhlIrregularTransObjClassPart {
	char *foo;
}NhlIrregularTransObjClassPart;

typedef struct _NhlIrregularTransObjClassRec {
	NhlObjClassPart			base_class;
	NhlTransObjClassPart		trobj_class;
	NhlIrregularTransObjClassPart	irtrans_class;
}NhlIrregularTransObjClassRec;

typedef struct _NhlIrregularTransObjClassRec	*NhlIrregularTransObjClass;
typedef struct _NhlIrregularTransObjLayerRec	*NhlIrregularTransObjLayer;

extern NhlIrregularTransObjClassRec		NhlirregularTransObjClassRec;

#endif /* _NIrregularTransObjP_h */

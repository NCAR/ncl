/*
 *      $Id: IrregularTransObjP.h,v 1.5 1994-01-27 21:23:20 boote Exp $
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
 *	Description:	Private header file for irregular grid transformations.
 */


#ifndef _NIrregularTransObjP_h
#define _NIrregularTransObjP_h


#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/TransObjP.h>
#include <ncarg/hlu/IrregularTransObj.h>

#include <ncarg/hlu/CoordApprox.h>

typedef struct _NhlIrregularTransObjLayerPart {
	/* Publically setable resources */
	float 	*x_coord_points;
	float	*x_inter_points;
	float   x_min;
	float   x_max;
	int 	x_num_points;
	int	x_reverse;
	float	x_tension;
	int	x_samples;
	int	x_use_log;
	float 	*y_coord_points;
	float	*y_inter_points;
	float 	y_min;
	float	y_max;
	int 	y_num_points;
	int	y_reverse;
	float	y_tension;
	int	y_samples;
	int	y_use_log;
	
	/* Private fields */
	NhlCoordDat thecoord;
	int xstatus;
	int ystatus;
	float	ur;
	float 	ul;
	float	ut;
	float	ub;
	float   compc_x_min;
	float   compc_x_max;
	float   compc_y_min;
	float	compc_y_max;
	int log_lin_value;
} NhlIrregularTransObjLayerPart;

typedef struct _NhlIrregularTransObjLayerRec {
	NhlObjLayerPart			base;
	NhlTransObjLayerPart		trobj;
	NhlIrregularTransObjLayerPart	irtrans;
} NhlIrregularTransObjLayerRec;

typedef struct _NhlIrregularTransObjLayerClassPart {
	char *foo;
}NhlIrregularTransObjLayerClassPart;

typedef struct _NhlIrregularTransObjLayerClassRec {
	NhlObjLayerClassPart			base_class;
	NhlTransObjLayerClassPart		trobj_class;
	NhlIrregularTransObjLayerClassPart	irtrans_class;
}NhlIrregularTransObjLayerClassRec;

typedef struct _NhlIrregularTransObjLayerClassRec
					*NhlIrregularTransObjLayerClass;
typedef struct _NhlIrregularTransObjLayerRec	*NhlIrregularTransObjLayer;

extern NhlIrregularTransObjLayerClassRec NhlirregularTransObjLayerClassRec;

#endif /* _NIrregularTransObjP_h */

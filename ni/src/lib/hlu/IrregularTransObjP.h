/*
 *      $Id: IrregularTransObjP.h,v 1.2 1993-10-19 17:50:57 boote Exp $
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

typedef struct _IrregularTransObjLayerPart {
	/* Publically setable resources */
	float 	*x_coord_points;
	float	*x_inter_points;
	int 	x_num_points;
	int	x_reverse;
	float	x_tension;
	int	x_samples;
	int	x_use_log;
	float 	*y_coord_points;
	float	*y_inter_points;
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
	int log_lin_value;
} IrregularTransObjLayerPart;

typedef struct _IrregularTransObjLayerRec {
	ObjLayerPart	base;
	TransObjLayerPart trobj;
	IrregularTransObjLayerPart irtrans;
} IrregularTransObjLayerRec;

typedef struct _IrregularTransObjLayerClassPart {
	char *foo;
}IrregularTransObjLayerClassPart;

typedef struct _IrregularTransObjLayerClassRec {
	ObjLayerClassPart	base_class;
	TransObjLayerClassPart	trobj_class;
	IrregularTransObjLayerClassPart	irtrans_class;
}IrregularTransObjLayerClassRec;

extern IrregularTransObjLayerClassRec irregularTransObjLayerClassRec;

#endif /* _NIrregularTransObjP_h */

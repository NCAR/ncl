
/*
 *      $Id: IrregularType2TransObjP.h,v 1.1 1993-04-30 17:22:35 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		IrregularType2TransObjP.h
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


#ifndef _NIrregularType2TransObjP_h
#define _NIrregularType2TransObjP_h


#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/TransObjP.h>
#include <ncarg/hlu/IrregularType2TransObj.h>

#include <ncarg/hlu/CoordApprox.h>

typedef struct _IrregularType2TransObjLayerPart {
	/* Publically setable resources */
	float 	*x_coord_points;
	float	*x_inter_points;
	float	x_min;
	float	x_max;
	int 	x_num_points;
	int	x_reverse;
	float	x_tension;
	int	x_samples;
	int	x_use_log;
	float 	*y_coord_points;
	float	*y_inter_points;
	float	y_min;
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
	int log_lin_value;
} IrregularType2TransObjLayerPart;

typedef struct _IrregularType2TransObjLayerRec {
	BaseLayerPart	base;
	TransObjLayerPart trobj;
	IrregularType2TransObjLayerPart ir2trans;
} IrregularType2TransObjLayerRec;

typedef struct _IrregularType2TransObjLayerClassPart {
	char *foo;
}IrregularType2TransObjLayerClassPart;

typedef struct _IrregularType2TransObjLayerClassRec {
	BaseLayerClassPart	base_class;
	TransObjLayerClassPart	trobj_class;
	IrregularType2TransObjLayerClassPart	ir2trans_class;
}IrregularType2TransObjLayerClassRec;

extern IrregularType2TransObjLayerClassRec irregularType2TransObjLayerClassRec;

#endif /* _NIrregularType2TransObjP_h */

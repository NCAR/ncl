
/*
 *      $Id: LogLinTransObjP.h,v 1.1 1993-04-30 17:22:47 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		LogLinTransObjP.h
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Oct 16 13:04:03 MDT 1992
 *
 *	Description:	Private header file for logLin grid transformations.
 */


#ifndef _NLogLinTransObjP_h
#define _NLogLinTransObjP_h


#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/TransObjP.h>
#include <ncarg/hlu/LogLinTransObj.h>


typedef struct _LogLinTransObjLayerPart {
	/* Publically setable resources */
	float 	x_min;
	float	x_max;
	int 	x_reverse;
	int	x_log;
	float 	y_min;
	float	y_max;
	int 	y_reverse;
	int	y_log;
	
	/* Private fields */
	float	ur;
	float 	ul;
	float	ut;
	float	ub;
	int	log_lin_value;
} LogLinTransObjLayerPart;

typedef struct _LogLinTransObjLayerRec {
	BaseLayerPart	base;
	TransObjLayerPart trobj;
	LogLinTransObjLayerPart lltrans;
} LogLinTransObjLayerRec;

typedef struct _LogLinTransObjLayerClassPart {
	char *foo;
}LogLinTransObjLayerClassPart;

typedef struct _LogLinTransObjLayerClassRec {
	BaseLayerClassPart	base_class;
	TransObjLayerClassPart	trobj_class;
	LogLinTransObjLayerClassPart	lltrans_class;
}LogLinTransObjLayerClassRec;

extern LogLinTransObjLayerClassRec logLinTransObjLayerClassRec;

#endif /* _NLogLinTransObjP_h */

/*
 *      $Id: LogLinTransObjP.h,v 1.3 1994-01-27 21:24:32 boote Exp $
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

typedef struct _NhlLogLinTransObjLayerPart {
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
} NhlLogLinTransObjLayerPart;

typedef struct _NhlLogLinTransObjLayerRec {
	NhlObjLayerPart			base;
	NhlTransObjLayerPart		trobj;
	NhlLogLinTransObjLayerPart	lltrans;
} NhlLogLinTransObjLayerRec;

typedef struct _NhlLogLinTransObjLayerClassPart {
	char *foo;
}NhlLogLinTransObjLayerClassPart;

typedef struct _NhlLogLinTransObjLayerClassRec {
	NhlObjLayerClassPart		base_class;
	NhlTransObjLayerClassPart	trobj_class;
	NhlLogLinTransObjLayerClassPart	lltrans_class;
}NhlLogLinTransObjLayerClassRec;

typedef struct _NhlLogLinTransObjLayerClassRec	*NhlLogLinTransObjLayerClass;
typedef struct _NhlLogLinTransObjLayerRec	*NhlLogLinTransObjLayer;

extern NhlLogLinTransObjLayerClassRec NhllogLinTransObjLayerClassRec;

#endif /* _NLogLinTransObjP_h */

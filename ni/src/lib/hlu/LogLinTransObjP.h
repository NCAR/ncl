/*
 *      $Id: LogLinTransObjP.h,v 1.4 1995-04-07 10:42:49 boote Exp $
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

typedef struct _NhlLogLinTransObjClassPart {
	char *foo;
}NhlLogLinTransObjClassPart;

typedef struct _NhlLogLinTransObjClassRec {
	NhlObjClassPart		base_class;
	NhlTransObjClassPart	trobj_class;
	NhlLogLinTransObjClassPart	lltrans_class;
}NhlLogLinTransObjClassRec;

typedef struct _NhlLogLinTransObjClassRec	*NhlLogLinTransObjClass;
typedef struct _NhlLogLinTransObjLayerRec	*NhlLogLinTransObjLayer;

extern NhlLogLinTransObjClassRec NhllogLinTransObjClassRec;

#endif /* _NLogLinTransObjP_h */

/*
 *      $Id: LogLinTransObjP.h,v 1.6 1997-09-23 00:02:55 dbrown Exp $
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
#include <ncarg/hlu/hluutil.h>
#include <ncarg/hlu/TransObjP.h>
#include <ncarg/hlu/LogLinTransObj.h>

typedef struct _NhlLogLinTransObjLayerPart {

        NhlBoolean foo;
        
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
	NhlCompareDat	*xmin_dat;
	NhlCompareDat	*xmax_dat;
	NhlCompareDat	*ymin_dat;
	NhlCompareDat	*ymax_dat;
	NhlCompareDat   *xmin_ndc_dat;
	NhlCompareDat   *xmax_ndc_dat;
	NhlCompareDat   *ymin_ndc_dat;
	NhlCompareDat   *ymax_ndc_dat;
	NhlCompareDat   *log_xmin_dat;
	NhlCompareDat   *log_xmax_dat;
	NhlCompareDat   *log_ymin_dat;
	NhlCompareDat   *log_ymax_dat;
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

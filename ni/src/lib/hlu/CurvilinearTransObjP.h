/*
 *      $Id: CurvilinearTransObjP.h,v 1.3 2004-12-23 22:45:16 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  2002			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		CurvilinearTransObjP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Feb 11 15:14:27 MST 2002
 *
 *	Description:	Private header file for curvilinear type 2 grid 
 *			transformations.
 */
#ifndef _NCurvilinearTransObjP_h
#define _NCurvilinearTransObjP_h

#include <ncarg/hlu/TransObjP.h>
#include <ncarg/hlu/CurvilinearTransObj.h>

typedef struct _NhlCurvilinearTransObjLayerPart {
	/* Publically settable resources */

	NhlGenArray	x_coord_points_ga;
	float		x_min;
	float		x_max;
	NhlBoolean	x_reverse;
	NhlGenArray	y_coord_points_ga;
	float		y_min;
	float		y_max;
	NhlBoolean	y_reverse;
	
	/* Private fields */
	NhlGenArray	x_crv_ga;
	NhlGenArray	y_crv_ga;
	int		*ixmin;
	int		*ixmax;
	int		*iymin;
	int		*iymax;

	int		xaxis_size;
	int		yaxis_size;
	int		msize;
	int		nsize;

        double		x_crv_min;
        double		x_crv_max;
	double          x_crv_ext;
        double		y_crv_min;
        double		y_crv_max;
	double          y_crv_ext;

	float		ur;
	float 		ul;
	float		ut;
	float		ub;
	float 		compc_x_min;
	float		compc_x_max;
	float		compc_y_min;
	float 		compc_y_max;
	int 		log_lin_value;

	int		ixb,iyb,ixe,iye;
	int             handedness_sign;

	NhlCompareDat	*xmin_dat;
	NhlCompareDat	*xmax_dat;
	NhlCompareDat	*ymin_dat;
	NhlCompareDat	*ymax_dat;
	NhlCompareDat	*compc_xmin_dat;
	NhlCompareDat	*compc_xmax_dat;
	NhlCompareDat	*compc_ymin_dat;
	NhlCompareDat	*compc_ymax_dat;
	NhlCompareDat	*save_compc_xmin_dat;
	NhlCompareDat	*save_compc_xmax_dat;
	NhlCompareDat	*save_compc_ymin_dat;
	NhlCompareDat	*save_compc_ymax_dat;

} NhlCurvilinearTransObjLayerPart;

typedef struct _NhlCurvilinearTransObjLayerRec {
	NhlObjLayerPart			base;
	NhlTransObjLayerPart		trobj;
	NhlCurvilinearTransObjLayerPart	crtrans;
} NhlCurvilinearTransObjLayerRec;

typedef struct _NhlCurvilinearTransObjClassPart {
	char *foo;
}NhlCurvilinearTransObjClassPart;

typedef struct _NhlCurvilinearTransObjClassRec {
	NhlObjClassPart			base_class;
	NhlTransObjClassPart		trobj_class;
	NhlCurvilinearTransObjClassPart	crtrans_class;
}NhlCurvilinearTransObjClassRec;

typedef struct _NhlCurvilinearTransObjClassRec	*NhlCurvilinearTransObjClass;
typedef struct _NhlCurvilinearTransObjLayerRec	*NhlCurvilinearTransObjLayer;

extern NhlCurvilinearTransObjClassRec		NhlcurvilinearTransObjClassRec;

#endif /* _NCurvilinearTransObjP_h */

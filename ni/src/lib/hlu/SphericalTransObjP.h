/*
 *      $Id: SphericalTransObjP.h,v 1.1 2002-07-02 01:26:41 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  2002			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		SphericalTransObjP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jun 12 12:35:56 MDT 2002
 *
 *	Description:	Private header file for spherical grid
 *			transformations.
 */
#ifndef _NSphericalTransObjP_h
#define _NSphericalTransObjP_h

#include <ncarg/hlu/TransObjP.h>
#include <ncarg/hlu/SphericalTransObj.h>

typedef struct _LLCosSin {
	double latcos;
	double latsin;
	double loncos;
	double lonsin;
} LLCosSin;

typedef struct _NhlSphericalTransObjLayerPart {
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
	NhlGenArray	x_sph_ga;
	NhlGenArray	y_sph_ga;
	LLCosSin	*llcs;
	int		*ixmin;
	int		*ixmax;
	int		*iymin;
	int		*iymax;

	int		xaxis_size;
	int		yaxis_size;
	int		msize;
	int		nsize;

        float		x_sph_min;
        float		x_sph_max;
        float		y_sph_min;
        float		y_sph_max;

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

} NhlSphericalTransObjLayerPart;

typedef struct _NhlSphericalTransObjLayerRec {
	NhlObjLayerPart			base;
	NhlTransObjLayerPart		trobj;
	NhlSphericalTransObjLayerPart	sptrans;
} NhlSphericalTransObjLayerRec;

typedef struct _NhlSphericalTransObjClassPart {
	char *foo;
}NhlSphericalTransObjClassPart;

typedef struct _NhlSphericalTransObjClassRec {
	NhlObjClassPart			base_class;
	NhlTransObjClassPart		trobj_class;
	NhlSphericalTransObjClassPart	sptrans_class;
}NhlSphericalTransObjClassRec;

typedef struct _NhlSphericalTransObjClassRec	*NhlSphericalTransObjClass;
typedef struct _NhlSphericalTransObjLayerRec	*NhlSphericalTransObjLayer;

extern NhlSphericalTransObjClassRec		NhlsphericalTransObjClassRec;

#endif /* _NSphericalTransObjP_h */

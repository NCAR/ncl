/*
 *      $Id: TriMeshTransObjP.h,v 1.1 2004-03-11 02:00:34 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  2002			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		TriMeshTransObjP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Oct 16 17:41:11 MDT 2003
 *
 *	Description:	Private header file for triangular mesh grid
 *			transformation class.
 */
#ifndef _NTriMeshTransObjP_h
#define _NTriMeshTransObjP_h

#include <ncarg/hlu/TransObjP.h>
#include <ncarg/hlu/TriMeshTransObj.h>

typedef struct _LLCosSin {
	double latcos;
	double latsin;
	double loncos;
	double lonsin;
} LLCosSin;

typedef struct _NhlTriMeshTransObjLayerPart {
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
	NhlGenArray	x_trm_ga;
	NhlGenArray	y_trm_ga;
	LLCosSin	*llcs;
	int		*ixmin;
	int		*ixmax;
	int		*iymin;
	int		*iymax;

	int		xaxis_size;
	int		yaxis_size;
	int		msize;
	int		nsize;

        float		x_trm_min;
        float		x_trm_max;
        float		y_trm_min;
        float		y_trm_max;

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

} NhlTriMeshTransObjLayerPart;

typedef struct _NhlTriMeshTransObjLayerRec {
	NhlObjLayerPart			base;
	NhlTransObjLayerPart		trobj;
	NhlTriMeshTransObjLayerPart	tmtrans;
} NhlTriMeshTransObjLayerRec;

typedef struct _NhlTriMeshTransObjClassPart {
	char *foo;
}NhlTriMeshTransObjClassPart;

typedef struct _NhlTriMeshTransObjClassRec {
	NhlObjClassPart			base_class;
	NhlTransObjClassPart		trobj_class;
	NhlTriMeshTransObjClassPart	tmtrans_class;
}NhlTriMeshTransObjClassRec;

typedef struct _NhlTriMeshTransObjClassRec	*NhlTriMeshTransObjClass;
typedef struct _NhlTriMeshTransObjLayerRec	*NhlTriMeshTransObjLayer;

extern NhlTriMeshTransObjClassRec		NhltriMeshTransObjClassRec;

#endif /* _NTriMeshTransObjP_h */

/*
 *      $Id: shaper.h,v 1.2 1997-06-06 03:14:55 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1995			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		shaper.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Aug  5 17:25:12 MDT 1996
 *
 *	Description:	
 */
#ifndef	_SHAPER_H_
#define	_SHAPER_H_

#ifndef _NCL_H_
#include <ncarg/ncl/defs.h>
#include <ncarg/ncl/NclDataDefs.h>
#include <ncarg/ncl/ApiRecords.h>
#include <ncarg/ncl/NclApi.h>
#endif

#include <ncarg/ngo/datagrid.h>

typedef void (*NgShapeApply)(
#if     NhlNeedProto
        void *data
#endif
);
typedef void (*shGeoNotifyFunc) (
        NhlPointer data
        );

typedef struct _NgShaper {           /* shaper interface struct */
	NgGO		go;
	Widget		parent;
        Widget		frame;
        NgDataGrid	*datagrid;
	void		*shaper;
	void		*pdata;
	NrmQuark	qfile;
	NclApiVarInfoRec  *vinfo;
	long		*start;
	long		*finish;
	long		*stride;
	int		eff_dim_count;
	NhlBoolean	new_shape;
	NhlBoolean	new_data;
	NhlBoolean	restore;
	NgShapeApply	apply;
        shGeoNotifyFunc	geo_notify;
        NhlPointer	geo_data;
        Dimension	sub_width;
} NgShaper;


void NgShaperOn(
#if	NhlNeedProto
	NgShaper	*si,
	NhlBoolean	on
#endif
);

void NgDoShaper(
#if	NhlNeedProto
	NgShaper	*si
#endif
);

void NgDeactivateShaper(
#if	NhlNeedProto
	NgShaper	*si
#endif
);

void NgDestroyShaper(
#if	NhlNeedProto
	NgShaper	*si
#endif
);

void NgUpdateShaperCoordDataGrid(
#if	NhlNeedProto
	NgShaper	*si
#endif
);

#endif	/* _SHAPER_H_ */

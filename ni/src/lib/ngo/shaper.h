/*
 *      $Id: shaper.h,v 1.11 2000-03-21 02:35:51 dbrown Exp $
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

#include <ncarg/ngo/browse.h>
#include <ncarg/ngo/datagrid.h>
#include <ncarg/ngo/shapeinfogrid.h>
#include <ncarg/ngo/varpage.h>

typedef struct _NgShaper {           /* shaper interface struct */
        Widget		frame;	     /* top-level widget for the shaper */
	Widget		reverse;
	NrmQuark	qfile;
	NrmQuark	qvar;
	NclApiDataList	*dl;	
	NclApiVarInfoRec  *vinfo;
	long		*start;
	long		*finish;
	long		*stride;
	NgShapeNotify	shape_notify;
        AdjustPageGeoFunc geo_notify;
        NhlPointer	pdata;	      /* data for the funcptr functions */
        Dimension	sub_width;
        NgDataGrid	*datagrid;
        NgShapeInfoGrid	*shapeinfogrid;
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

NgShaper *NgCreateShaper(
	int		go_id,
	Widget		parent
);

NhlErrorTypes NgUpdateShaper(
	NgShaper	*si,
	NrmQuark	qfile,
	NrmQuark	qvar,
	long		*start,
	long		*finish,
	long		*stride
);

NgShaper *NgDupShaper(
	int		go_id,
	Widget		parent,
	NgShaper	*si,
	NgShaper	*old_si,
	NrmQuark	qfile,
	long		*start,
	long		*finish,
	long		*stride,
	NclApiDataList  *dl
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

void NgUpdateShaperCoords(
#if	NhlNeedProto
	NgShaper	*si
#endif
);

#endif	/* _SHAPER_H_ */

/*
 *      $Id: plotpage.h,v 1.2 1999-10-18 22:12:36 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		plotpage.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Apr 28 17:20:23 MDT 1999
 *
 *	Description:	
 */
#ifndef	_NG_PLOTPAGE_H
#define	_NG_PLOTPAGE_H

#include <ncarg/ngo/go.h>
#include <ncarg/ngo/datasourcegrid.h>

#ifndef _NCL_H_
#include <ncarg/ncl/defs.h>
#include <ncarg/ncl/NclDataDefs.h>
#include <ncarg/ncl/ApiRecords.h>
#include <ncarg/ncl/NclApi.h>
#define _NCL_H_
#endif

#include <ncarg/ngo/dataprofile.h>

typedef enum __plotState
{
        _plotNOTCREATED, _plotPREVIEW, _plotCREATING, _plotCREATED
} _plotState;

/* 
 * NgPageMessageType:  _NgPLOTCREATE 
 * this message is sent when a page associated with a newly-created or
 * a potential PLOT object needs information about plot styles and/or
 * data profiles.
 */

typedef struct _brPlotObjCreateRec    /* message type _NgPLOTOBJCREATE */
{
	int		obj_count;
	int		*obj_ids;
	NhlString	class_name;
	NhlString	plot_style;
	NhlString	plot_style_dir;
	NhlString	plot_style_name;
	NhlBoolean	has_input_data;
	_plotState	state;
	NgVarData	*vdata;
	int		vdata_count;
	int		app_id;
/* deprecated */
	NgDataProfile	dprof;
} brPlotObjCreateRec, *brPlotObjCreate;

typedef struct _NgPlotPage
{
	NhlString	class_name;
	NhlString	plot_style;
	NhlString	plot_style_dir;
	NhlString	plot_style_name;
	NhlBoolean	config_required;
} NgPlotPage;

extern brPlotObjCreate NgNewPlotObjCreateRec
(
        NhlString	class_name,
	NgDataProfile	data_profile,
	NhlString	plot_style,
	NhlString	plot_style_dir,
	int		obj_count,
	int		*obj_id,
	int		app_id,
	NhlBoolean	has_input_data,
	_plotState	state
);

extern void NgFreePlotObjCreateRec
(
	brPlotObjCreate	obj_create
);

      
extern void NgPlotObjCreateUpdate
(
	int go_id,
	int page_id
);

extern int *NgPlotObjGetHluIds
(
	int go_id,
	int page_id,
	int *count
);

#endif	/* _NG_PLOTPAGE_H */

/*
 *      $Id: hlupage.h,v 1.7 1999-02-23 03:56:49 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		hlupage.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jun 18 17:11:05 MDT 1997
 *
 *	Description:	
 */
#ifndef	_NG_HLUPAGE_H
#define	_NG_HLUPAGE_H

#include <ncarg/ngo/go.h>
#include <ncarg/ngo/datasourcegrid.h>
#include <ncarg/ngo/restree.h>

#ifndef _NCL_H_
#include <ncarg/ncl/defs.h>
#include <ncarg/ncl/NclDataDefs.h>
#include <ncarg/ncl/ApiRecords.h>
#include <ncarg/ncl/NclApi.h>
#define _NCL_H_
#endif

#include <ncarg/ngo/dataprofile.h>

typedef enum __hluState
{
        _hluNOTCREATED, _hluPREVIEW, _hluCREATED
} _hluState;

/* 
 * NgPageMessageType:  _NgHLUOBJCREATE 
 * this message is sent when a page associated with a newly-created or
 * a potential HLU object needs information about plot styles and/or
 * data profiles.
 */

typedef struct _brHluObjCreateRec    /* message type _NgHLUOBJCREATE */
{
	int		obj_id;
	NhlString	class_name;
	NhlString	plot_style;
	NhlString	plot_style_dir;
	NhlBoolean	has_input_data;
	_hluState	state;
	NgDataProfile	dprof;
} brHluObjCreateRec, *brHluObjCreate;

typedef struct _NgHluPage
{
        NhlString	class_name;
	NgDataProfile	data_profile;
	NhlString	plot_style;
	NhlString	plot_style_dir;
} NgHluPage;

extern brHluObjCreate NgNewHluObjCreateRec
(
        NhlString	class_name,
	NgDataProfile	data_profile,
	NhlString	plot_style,
	NhlString	plot_style_dir,
	int		obj_id,
	NhlBoolean	has_input_data,
	_hluState	state
);

extern void NgFreeHluObjCreateRec
(
	brHluObjCreate	obj_create
);

      
extern void NgHluObjCreateUpdate
(
	int go_id,
	int page_id
);

#endif	/* _NG_HLUPAGE_H */

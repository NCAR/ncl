/*
 *      $Id: shaperP.h,v 1.2 1997-06-06 03:14:55 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1995			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		shaperP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Sep 24 18:06:42 MDT 1996
 *
 *	Description:	
 */
#ifndef	_NX_SHAPERP_H
#define	_NX_SHAPERP_H

#include <ncarg/ngo/goP.h>
#include <ncarg/ngo/shaper.h>
#include <ncarg/ngo/shapeinfogrid.h>
#include <ncarg/ngo/vcrcontrol.h>

#define DEBUG_SHAPER 0

typedef struct _NgShaperRec {
	Widget		form;
	int		tgl_coord;
        NclApiDataList  *tgl_coord_dlist;
	NhlBoolean	new_coord;
	int		coords_alloced;
	Boolean		*coord_indexes_set;
        Widget		datagrid_toggle;
        NgDataGrid	*datagrid;
        NgShapeInfoGrid	*shapeinfogrid;
        NgVcrControl	vcr;
	Widget		all_selected_tgl;
	NhlBoolean	selected_only_set;
	Boolean		*coords_selected_only_set;
	Widget		indexes_tgl;
	NhlBoolean	indexes_set;
	Widget		synchro_step_tgl;
	NhlBoolean	synchro_step_set;
	Widget		reverse_tgl;
	NhlBoolean	new_rev_val;
	NhlBoolean	reverse_set;
	NhlBoolean	new_data;
        long		start;
        long		finish;
        long		stride;
        NhlByte		*selected;
        NhlBoolean	edit_timer_set;
        XtIntervalId	edit_timer_id;
        int		edit_timeout_value;
        unsigned char   edit_how;
} NgShaperRec;

#endif	/* _NX_SHAPERP_H */


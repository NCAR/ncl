/*
 *      $Id: shaperP.h,v 1.4 1999-08-28 00:18:45 dbrown Exp $
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
#include <ncarg/ngo/vcrcontrol.h>

#define DEBUG_SHAPER 0

typedef struct _NgShaperRec {
	/* public */
	NgShaper	si;

	/* private */

 	NgGO		go;
	Widget		parent;
	Widget		form;
        NclApiDataList  *tgl_coord_dlist;
	NhlBoolean	new_coord;
	int		coords_alloced;
        NgShapeInfoGrid	*shapeinfogrid;
        NgVcrControl	vcr;
	NhlBoolean	selected_only_set;
	Boolean		*coords_selected_only_set;
	NhlBoolean	indexes_set;
	NhlBoolean	synchro_step_set;
	Boolean		*coords_synchro_step_set;
	Widget		reverse_tgl;
	NhlBoolean	new_rev_val;
	NhlBoolean	reverse_set;
        long		start;
        long		finish;
        long		stride;
        NhlByte		*selected;
        NhlBoolean	edit_timer_set;
        XtIntervalId	edit_timer_id;
        int		edit_timeout_value;
        unsigned char   edit_how;
	NhlBoolean	restore;
	NhlBoolean	new_data;
        int		tgl_coord;
        Widget		datagrid_tgl;
	Widget		all_selected_tgl;
	Widget		indexes_tgl;
	Widget		synchro_step_tgl;
	NhlBoolean	ignore_synchro_step_cb;
} NgShaperRec;

#endif	/* _NX_SHAPERP_H */


/*
 *      $Id: hlupageP.h,v 1.11 1999-02-23 03:56:49 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		hlupageP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Jun  9 21:02:27 MDT 1997
 *
 *	Description:	
 */
#ifndef	_NG_HLUPAGEP_H_
#define	_NG_HLUPAGEP_H_

#include <ncarg/ngo/browseP.h>
#include <ncarg/ngo/hlupage.h>
#include <ncarg/ngo/datasourcegrid.h>

#define DEBUG_HLUPAGE 0

/*
 * this struct keeps track of auxilliary data objects if the hlupage
 * references a plot object with data
 */

typedef struct _brDataObjInfoRec {
        NrmQuark	qname;
	int		id;
} brDataObjInfoRec, *brDataObjInfo;

/*
 * This struct contains state information used to recreate an hlupage when
 * it is brought up again after being 'hidden' by the user.
 * Like page messages these structs are saved in the NgBrowseClassPart
 * using an XmLArray -- hidden_page_state.
 */

typedef struct _brHluSaveStateRec {
        NhlString	class_name;
	NgDataProfile	data_profile;
	NhlString	plot_style;
	NhlString	plot_style_dir;
	int		hlu_id;
	_hluState	state;
        NhlBoolean	do_auto_update;
	int		data_object_count;
	brDataObjInfo   *data_objects;
	int		notify_page_count;
	int		*notify_page_ids;
	NhlBoolean	has_input_data;
	XmLArray	datalinks;
} brHluSaveStateRec, *brHluSaveState;
	
typedef struct _brSetValCBInfo
{
	NgPageId pid;
	int	 goid;
} brSetValCBInfo;

typedef struct _brHluPageRec 
{
        NgHluPage	public;
        NgGO		go;
        int		nclstate;
        NhlBoolean	activated;
        NhlClass	class;
        NgDataSourceGrid	*data_source_grid;
	NhlBoolean	has_input_data;
        NhlBoolean	new_data;
        NgResTree	*res_tree;
        Widget		create_update;
        _hluState	state;
        int		hlu_id;
	int		app_id;
        Widget		auto_update;
        NhlBoolean	do_auto_update;
	NgDataProfile	data_profile;
	int		data_object_count;
	brDataObjInfo   *data_objects;
        NhlBoolean	do_setval_cb;
        NgCBWP		destroy_cb;
        _NhlCB		setval_cb;
	brSetValCBInfo	setval_info;
	XmLArray	datalinks;
	NhlBoolean	preview_destroy; 
} brHluPageRec;

extern brPageData *
NgGetHluPage(
	NgGO		go,
        brPane		*pane,
	brPage		*page,
        brPage		*copy_page,
	NgPageSaveState save_state
        );


#endif	/* _NG_HLUPAGEP_H_ */





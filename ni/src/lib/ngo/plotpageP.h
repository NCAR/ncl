/*
 *      $Id: plotpageP.h,v 1.1 1999-08-11 23:41:58 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		plotpageP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Apr 28 17:20:23 MDT 1999
 *
 *	Description:	
 */
#ifndef	_NG_PLOTPAGEP_H_
#define	_NG_PLOTPAGEP_H_

#include <ncarg/ngo/browseP.h>
#include <ncarg/ngo/plotpage.h>
#include <ncarg/ngo/datasourcegrid.h>
#include <ncarg/ngo/datavargrid.h>

#define DEBUG_PLOTPAGE 0

/*
 * This struct contains state information used to recreate an plotpage when
 * it is brought up again after being 'hidden' by the user.
 * Like page messages these structs are saved in the NgBrowseClassPart
 * using an XmLArray -- hidden_page_state.
 */

typedef struct _brPlotSaveStateRec {
        NhlString	class_name;
	NgDataProfile	data_profile;
	NhlString	plot_style;
	NhlString	plot_style_dir;
	int		hlu_count;
	int		*hlu_ids;
	_plotState	state;
	int		app_id;
        _NhlCB		plot_delete_cb;
        NhlBoolean	do_auto_update;
	int		notify_page_count;
	int		*notify_page_ids;
	NhlBoolean	has_input_data;
	XmLArray	datalinks;
} brPlotSaveStateRec, *brPlotSaveState;
	

typedef struct _brPlotPageRec 
{
        NgPlotPage	public;
        NgGO		go;
        int		nclstate;
        NhlBoolean	activated;
	NrmQuark	qclass;
        NgDataVarGrid	*data_var_grid;
        NgDataSourceGrid	*data_source_grid;
	NhlBoolean	has_input_data;
        NhlBoolean	new_data;
        Widget		create_update;
        _plotState	state;
	int		hlu_count;
        int		*hlu_ids;
	int		app_id;
        Widget		auto_update;
        NhlBoolean	do_auto_update;
	NgDataProfile	data_profile;
        NhlBoolean	do_setval_cb;
        _NhlCB		plot_delete_cb;
        _NhlCB		setval_cb;
	brSetValCBInfo	setval_info;
	XmLArray	datalinks;
	NhlBoolean	preview_destroy; 
	int		max_seq_num;
	NgVarData	*vdata;
	int 		vdata_count;
} brPlotPageRec;


extern brPageData *
_NgGetPlotPage(
	NgGO		go,
        brPane		*pane,
	brPage		*page,
        brPage		*copy_page,
	NgPageSaveState save_state,
	NhlPointer	init_data
        );

#endif	/* _NG_PLOTPAGEP_H_ */

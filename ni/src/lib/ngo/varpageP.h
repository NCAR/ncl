/*
 *      $Id: varpageP.h,v 1.9 1999-03-12 19:13:49 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		varpageP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon May  5 17:41:15 MDT 1997
 *
 *	Description:	
 */
#ifndef	_NG_VARPAGEP_H_
#define	_NG_VARPAGEP_H_

#include <ncarg/ngo/browseP.h>
#include <ncarg/ngo/varpage.h>
#include <ncarg/ngo/diminfogrid.h>
#include <ncarg/ngo/vartree.h>
#include <ncarg/ngo/shaper.h>
#include <ncarg/ngo/datagrid.h>
#include <ncarg/ngo/plotspecmenu.h>

#define DEBUG_VARPAGE 0

typedef struct _brVarSaveStateRec {
	NgVarData	vdata;
        long		*start;
        long		*finish;
        long		*stride;
	XmLArray	datalinks;
	NhlBoolean	shaper_managed;
} brVarSaveStateRec, *brVarSaveState;

typedef struct _brVarPageRec 
{
        NgGO		go;
        NgPageId	page_id;
        NgDimInfoGrid	*diminfogrid;
        NgVarTree	*vartree;
        NgShaper	*shaper;
        NhlBoolean	new_shape;
        Widget		shaper_toggle;
        NhlBoolean	shaper_managed;
        NgDataGrid	*datagrid;
        NhlBoolean	new_data;
        Widget		data_ctrl_form;
        Widget		datagrid_toggle;
        NgPlotSpecMenu  *plotspecmenu;
        NhlBoolean	datagrid_managed;
	NgVarData	vdata;
        long		*start;
        long		*finish;
        long		*stride;
	XmLArray	datalinks;
} brVarPageRec;

extern brPageData *
NgGetVarPage(
	NgGO		go,
        brPane		*pane,
	brPage		*page,
        brPage		*copy_page,
	NgPageSaveState save_state
        );


#endif	/* _NG_VARPAGEP_H_ */





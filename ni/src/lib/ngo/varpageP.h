/*
 *      $Id: varpageP.h,v 1.5 1997-10-03 20:08:33 dbrown Exp $
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

typedef struct _vpReceiverPage
{
        NgPageId	page_id;
        NhlPointer	page_data;
} vpReceiverPage;

typedef struct _brVarPageRec 
{
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
        long		*start;
        long		*finish;
        long		*stride;
        int		receiver_count;
        vpReceiverPage  *receiver_pages;
        NgVarPageOutput *output;
} brVarPageRec;

extern brPageData *
NgGetVarPage(
	NgGO		go,
        brPane		*pane,
	brPage		*page,
        brPage		*copy_page
        );


#endif	/* _NG_VARPAGEP_H_ */





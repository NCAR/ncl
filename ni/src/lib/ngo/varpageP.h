/*
 *      $Id: varpageP.h,v 1.2 1997-06-06 03:14:57 dbrown Exp $
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
#include <ncarg/ngo/diminfogrid.h>
#include <ncarg/ngo/vartree.h>
#include <ncarg/ngo/shaper.h>
#include <ncarg/ngo/datagrid.h>

#define DEBUG_VARPAGE 0

typedef struct _brVarPageRec 
{
        NgDimInfoGrid	*diminfogrid;
        NgVarTree	*vartree;
        NgDataGrid	*datagrid;
        NhlBoolean	new_data;
        Widget		datagrid_toggle;
	Widget		update_data_pb;
        NhlBoolean	datagrid_managed;
        NgShaper	*shaper;
        NhlBoolean	new_shape;
        Widget		shaper_toggle;
        NhlBoolean	shaper_managed;
        long		*start;
        long		*finish;
        long		*stride;
} brVarPageRec;

extern brPageData *
NgGetVarPage(
	NgGO		go,
        brPane		*pane,
	brPage		*page,
        brPage		*copy_page
        );


#endif	/* _NG_VARPAGEP_H_ */





/*
 *      $Id: hlupageP.h,v 1.7 1997-10-23 00:27:05 dbrown Exp $
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
#include <ncarg/ngo/varpage.h>
#include <ncarg/ngo/datasinkgrid.h>

#define DEBUG_HLUPAGE 0

typedef struct _hluData 
{
        NhlString	name;
        Widget		frame;
        Widget		form;
        int		n_items;
        Widget		*labels;
        Widget		*textfields;
} hluData;

typedef enum __hluState
{
        _hluNOTCREATED, _hluPREVIEW, _hluCREATED
} _hluState;

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
        NgDataSinkGrid	*data_sink_grid;
        NhlBoolean	new_data;
        NgResTree	*res_tree;
        Widget		create_update;
        _hluState	state;
        int		hlu_id;
        Widget		auto_update;
        NhlBoolean	do_auto_update;
        hluData		*data;
        int		var_data_count;
        NgVarPageOutput	**var_data;
        NrmQuark	data_objects[8];
        NhlBoolean	do_setval_cb;
        _NhlCB		setval_cb;
	brSetValCBInfo	setval_info;
} brHluPageRec;

extern brPageData *
NgGetHluPage(
	NgGO		go,
        brPane		*pane,
	brPage		*page,
        brPage		*copy_page
        );


#endif	/* _NG_HLUPAGEP_H_ */





/*
 *      $Id: plotstylemenuP.h,v 1.2 1999-09-29 02:06:01 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		plotstylemenuP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Mar 17 20:52:04 MST 1997
 *
 *	Description:	
 */
#ifndef	_NG_PLOTSTYLEMENUP_H_
#define	_NG_PLOTSTYLEMENUP_H_

#include <ncarg/ngo/goP.h>

#include <ncarg/ngo/browse.h>
#include <ncarg/ngo/plotstylemenu.h>
#include <ncarg/ngo/datasourcegrid.h>

#define DEBUG_ENTRY 1
#define DEBUG_MENUS 1 << 1
#define DEBUG_PLOTSTYLEMENU 0

typedef struct _NgMenuRec 
{
        Widget		menu;
        Widget		*buttons;
} NgMenuRec;

typedef struct _PlotStyleMenuRec
{
	NgPlotStyleMenu  public;
        NgGO		go;
        int		nsid;
        NgMenuRec	plot;
        NgMenuRec	var;
        NgMenuRec	data;
        Widget		create_dialog;
        Widget		dialog_text;
	Widget		exist_tgl;
	Widget		new_tgl;
	Widget		xwk_optmenu;
	Widget		xwk_menu;
	Widget		new_xwk_text;
	Widget		config_pb;
	NhlBoolean	new_xwk;
	NhlString	new_xwk_name;
} PlotStyleMenuRec;

#endif	/* _NG_PLOTSTYLEMENUP_H_ */


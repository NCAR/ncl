/*
 *      $Id: plotspecmenuP.h,v 1.4 1999-02-23 03:56:52 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		plotspecmenuP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Mar 17 20:52:04 MST 1997
 *
 *	Description:	
 */
#ifndef	_NG_PLOTSPECMENUP_H_
#define	_NG_PLOTSPECMENUP_H_

#include <ncarg/ngo/goP.h>

#include <ncarg/ngo/browse.h>
#include <ncarg/ngo/plotspecmenu.h>
#include <ncarg/ngo/datasourcegrid.h>

#define DEBUG_ENTRY 1
#define DEBUG_MENUS 1 << 1
#define DEBUG_PLOTSPECMENU 0

typedef struct _NgMenuRec 
{
        Widget		menu;
        int		count;
        Widget		*buttons;
        int		alloced;
        int		in_use;
	NhlBoolean	modified;
} NgMenuRec;

typedef struct _PlotSpecMenuRec
{
	NgPlotSpecMenu  public;
        NgGO		go;
        int		nsid;
        Widget		create_dialog;
        Widget		dialog_text;
        NgMenuRec	plot;
        NgMenuRec	var;
        NgMenuRec	data;
} PlotSpecMenuRec;

#endif	/* _NG_PLOTSPECMENUP_H_ */


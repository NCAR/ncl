/*
 *      $Id: plotspecmenu.h,v 1.2 1998-12-16 23:51:39 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		plotspecmenu.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Mar 17 20:52:04 MST 1997
 *
 *	Description:	
 */
#ifndef	_NG_PLOTSPECMENU_H
#define	_NG_PLOTSPECMENU_H

#include <ncarg/ngo/go.h>
#include <ncarg/ngo/browse.h>

#ifndef _NCL_H_
#include <ncarg/ncl/defs.h>
#include <ncarg/ncl/NclDataDefs.h>
#include <ncarg/ncl/ApiRecords.h>
#include <ncarg/ncl/NclApi.h>
#define _NCL_H_
#endif

/*
 * Public api
 */
        
typedef struct _NgPlotSpecMenu
{
        Widget		menubar;
        Widget		plot_mbutton;
        Widget		var_mbutton;
        Widget		data_mbutton;
        NrmQuark		qsymbol;
        NclApiVarInfoRec	*vinfo;
	long			*start;
	long			*finish;
	long			*stride;
        PageOutputNotify	output_notify;
        NhlPointer		pdata;
} NgPlotSpecMenu;

NgPlotSpecMenu *
NgCreatePlotSpecMenu(
        NgGO		go,
        Widget		parent
        );

NhlErrorTypes
NgUpdatePlotSpecMenu
(
        NgPlotSpecMenu		*plot_spec_menu
        );
        
void
NgDestroyPlotSpecMenu
(
        NgPlotSpecMenu		*plot_spec_menu
        );
        

#endif	/* _NG_PLOTSPECMENU_H */

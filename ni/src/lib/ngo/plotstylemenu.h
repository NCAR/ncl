/*
 *      $Id: plotstylemenu.h,v 1.1 1999-09-20 23:59:32 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		plotstylemenu.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Mar 17 20:52:04 MST 1997
 *
 *	Description:	
 */
#ifndef	_NG_PLOTSTYLEMENU_H
#define	_NG_PLOTSTYLEMENU_H

#include <ncarg/ngo/go.h>
#include <ncarg/ngo/browse.h>
#include <ncarg/ngo/dataprofile.h>

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
        
typedef struct _NgPlotStyleMenu
{
        Widget		menubar;
        Widget		plot_mbutton;
        Widget		var_mbutton;
        Widget		data_mbutton;
	int		page_id;
	NgVarData	vdata;
} NgPlotStyleMenu;

NgPlotStyleMenu *
NgCreatePlotStyleMenu(
        NgGO		go,
        Widget		parent
        );

NhlErrorTypes
NgUpdatePlotStyleMenu
(
        NgPlotStyleMenu		*plot_spec_menu
        );
        
void
NgDestroyPlotStyleMenu
(
        NgPlotStyleMenu		*plot_spec_menu
        );
        

#endif	/* _NG_PLOTSTYLEMENU_H */

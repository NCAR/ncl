/*
 *      $Id: plotstylemenu.h,v 1.2 2000-03-21 02:35:47 dbrown Exp $
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
	int		go_id,
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

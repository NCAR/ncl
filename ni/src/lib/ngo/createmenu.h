/*
 *      $Id: createmenu.h,v 1.1 1997-10-03 20:07:55 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		createmenu.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Sep 15 13:49:25 MDT 1997
 *
 *	Description:	
 */
#ifndef	_NG_CREATEMENU_H
#define	_NG_CREATEMENU_H

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
        
typedef struct _NgCreateMenuRec
{
        Widget		wks_mbutton;
        Widget		plot_mbutton;
        Widget		anno_mbutton;
        Widget		other_mbutton;
        Widget		var_mbutton;
        Widget		file_mbutton;
} NgCreateMenu;

NgCreateMenu *
NgCreateCreateMenu(
        int		goid,
        Widget		parent
        );

NhlErrorTypes
NgUpdateCreateMenu
(
        NgCreateMenu		*create_menu
        );
        
void
NgDestroyCreateMenu
(
        NgCreateMenu		*create_menu
        );
        

#endif	/* _NG_CREATEMENU_H */

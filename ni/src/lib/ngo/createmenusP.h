/*
 *      $Id: createmenusP.h,v 1.1 1997-06-20 16:38:15 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		createmenusP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Mar 17 20:52:04 MST 1997
 *
 *	Description:	
 */
#ifndef	_NG_CREATEMENUSP_H_
#define	_NG_CREATEMENUSP_H_

#include <ncarg/ngo/goP.h>

#include <ncarg/ngo/browse.h>
#include <ncarg/ngo/varpage.h>
#include <ncarg/ngo/createmenus.h>

#define DEBUG_ENTRY 1
#define DEBUG_MENUS 1 << 1
#define DEBUG_CREATEMENUS 0

typedef struct __cmDataReceptor {
	NhlString name;
	int	  type;
        NhlString def_name;
	NhlString class_name;
} _cmDataReceptor;

typedef struct _NgMenuRec 
{
        Widget		menu;
        int		count;
        Widget		*buttons;
        int		alloced;
        int		in_use;
	NhlBoolean	modified;
} NgMenuRec;

typedef struct _CreateMenusRec
{
	NgCreateMenus   public;
        NgGO		go;
        int		nsid;
        Widget		create_dialog;
        Widget		dialog_text;
        NgMenuRec	plot;
        NgMenuRec	var;
        NgMenuRec	data;
        int		pagecount;
        NgPageId	*page_ids;
        NgVarPageOutput *output;
} CreateMenusRec;

#endif	/* _NG_CREATEMENUSP_H_ */


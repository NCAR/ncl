/*
 *      $Id: createmenusP.h,v 1.2 1997-06-23 21:06:23 dbrown Exp $
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
#include <ncarg/ngo/hlupage.h>
#include <ncarg/ngo/createmenus.h>

#define DEBUG_ENTRY 1
#define DEBUG_MENUS 1 << 1
#define DEBUG_CREATEMENUS 0

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
} CreateMenusRec;

#endif	/* _NG_CREATEMENUSP_H_ */


/*
 *      $Id: createmenuP.h,v 1.1 1997-10-03 20:07:56 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		createmenuP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Jun  6 17:26:33 MDT 1997
 *
 *	Description:	
 */
#ifndef	_NG_CREATEMENUP_H_
#define	_NG_CREATEMENUP_H_

#include <ncarg/ngo/goP.h>
#include <ncarg/ngo/hlupage.h>
#include <ncarg/ngo/createmenu.h>

#define DEBUG_ENTRY 1
#define DEBUG_MENU 1 << 1
#define DEBUG_CREATEMENU 0

typedef struct _NgMenuRec 
{
        Widget		menu;
        int		count;
        Widget		*buttons;
        int		alloced;
        int		in_use;
	NhlBoolean	modified;
} NgMenuRec;

typedef struct _CreateMenuRec
{
	NgCreateMenu   	public;
        NgGO		go;
        Widget		parent;
        Widget		create_dialog;
        Widget		dialog_text;
        NhlClass	*classlist;
        int		classcount;
        NhlClass	create_class;
        NgMenuRec	wks;
        NgMenuRec	plot;
        NgMenuRec	anno;
        NgMenuRec	other;
        NgMenuRec	var;
        NgMenuRec	file;
        NgMenuRec	data;
} CreateMenuRec;

#endif	/* _NG_CREATEMENUP_H_ */


/*
 *      $Id: varmenusP.h,v 1.1 1997-06-04 18:08:35 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		varmenusP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Feb 10 14:22:58 MST 1997
 *
 *	Description:	
 */
#ifndef	_NG_VARMENUSP_H_
#define	_NG_VARMENUSP_H_

#include <ncarg/ngo/goP.h>

#include <ncarg/ngo/varmenus.h>

#define DEBUG_ENTRY 1
#define DEBUG_MENUS 1 << 1
#define DEBUG_VAR_MENUS 0

typedef enum _NgbrVarType
{
	_brREGULAR, _brFILEREF, _brFILEVAR
} NgbrVarType;

typedef struct _NgFileVarRec 
{
        struct _NgFileVarRec *next;
        Widget		override_sh;
        Widget		submenu;
        NrmQuark	qfile;
        Widget		*vbuttons;
        NclApiDataList	*dl;
        int		alloced;
        int		in_use;
} NgFileVarRec;

typedef struct _NgVarRec 
{
	NgbrVarType	vtype;
        Widget		menu;
        Widget		mbutton;
        int		varcount;
        Widget		*vbuttons;
        NrmQuark	*qvars;
        int		alloced;
        int		in_use;
	NhlBoolean	modified;
        NhlPointer	priv;
} NgVarRec;

typedef struct _VarMenusRec
{
        int		appmgr;
        int		nsid;
        NgVarRec	regvars;
        NgVarRec	filerefs;
        NgVarRec	filevars;
        XtCallbackProc	regvar_cb;
        XtCallbackProc	fileref_cb;
        XtCallbackProc	filevar_cb;
        XtPointer	udata;
} VarMenusRec;

#endif	/* _NG_VARMENUSP_H_ */


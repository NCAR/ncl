/*
 *      $Id: shellP.h,v 1.1 1999-09-11 01:07:00 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1998			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		shellP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, ColorMapado
 *
 *	Date:		Tue Aug 31 14:25:53 MDT 1999
 *
 *	Description:	
 */
#ifndef	_NG_SHELLP_H_
#define	_NG_SHELLP_H_

#include <ncarg/ngo/goP.h>
#include <ncarg/ngo/shell.h>

typedef struct _NgShellClassRec *NgShellClass;
typedef struct _NgShellRec *NgShell;

typedef struct _NgShellPart {
	NhlBoolean		focus;
	NgShellContentFunc	cfunc;
	NhlPointer		cfunc_data;
	NhlBoolean		do_focus_grab;
} NgShellPart;

typedef struct _NgShellRec {
	NhlObjLayerPart	base;
	NgGOPart	go;
	NgShellPart	shell;
} NgShellRec;

typedef struct _NgShellClassPart {
	int		foo;
} NgShellClassPart;

typedef struct _NgShellClassRec {
	NhlObjClassPart		base_class;
	NgGOClassPart		go_class;
	NgShellClassPart	shell_class;
} NgShellClassRec;

extern NgShellClassRec	NgshellClassRec;

#endif	/* _NG_SHELLP_H_ */

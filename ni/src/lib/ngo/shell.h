/*
 *      $Id: shell.h,v 1.2 2000-03-21 02:35:52 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1998			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		shell.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Aug 31 14:25:53 MDT 1999
 *
 *	Description:	
 */
#ifndef	_NG_SHELL_H
#define	_NG_SHELL_H

#include <ncarg/ngo/go.h>

typedef void (*NgShellContentFunc) (
	int		go_id,
	NhlPointer	data
);

#define NgNshContentFunc	"ngshContentFunc"
#define NgCshContentFunc	"ngShContentFunc"

#define NgNshContentFuncData	"ngshContentFuncData"
#define NgCshContentFuncData	"ngShContentFuncData"

#define NgNshDoFocusGrab	"ngshDoFocusGrab"
#define NgCshDoFocusGrab	"ngShDoFocusGrab"


extern NhlClass NgshellClass;

/*
 * Public api
 */

#endif	/* _NG_SHELL_H */

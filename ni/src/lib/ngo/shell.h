/*
 *      $Id: shell.h,v 1.1 1999-09-11 01:06:59 dbrown Exp $
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

#include <ncarg/ngo/shaper.h>

typedef void (*NgShellContentFunc) (
	NgGO		go,
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

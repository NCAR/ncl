/*
 *      $Id: xinteractP.h,v 1.1 1998-11-18 19:45:23 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		xinteractP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Sep 28 17:50:54 MDT 1998
 *
 *	Description:	This file has all the declarations for the xinteract.c
 *			source file.
 */

#ifndef	_XINTERACTP_H
#define	_XINTERACTP_H

#include <ncarg/ngo/xinteract.h>

#define DEBUG_XINTERACT 0

extern void _NgSelectionEH(
#if	NhlNeedProto
	Widget		w,		/* widget that called		*/
	XtPointer	data,		/* registered w/callback	*/
	XEvent		*calld,		/* event that triggered proc	*/
	Boolean		*cont		/* cont w/ further eventhandlers*/
#endif
);

extern void
_NgClearAllViewsCB(
	Widget		w,
	XtPointer	udata,
	XtPointer	cbdata
);


extern void
_NgDrawAllViewsCB(
	Widget		w,
	XtPointer	udata,
	XtPointer	cbdata
);

#endif	/* _XINTERACTP_H	*/

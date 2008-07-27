/*
 *      $Id: w_display.h,v 1.4 2008-07-27 03:22:37 haley Exp $
 */
/************************************************************************
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/************************************************************************
*									*
*			     Copyright (C)  1994			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		w_display.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Apr 19 15:28:28 MDT 1994
 *
 *	Description:	Declaration of functions in w_display.c
 */

#ifndef	_W_DISPLAY_H_
#define	_W_DISPLAY_H_

extern void CreateDisplayPopup(
#ifdef	NeedFuncProto
	Widget	button,
	char	*metafile
#endif
);

#endif	/* _W_DISPLAY_H_ */

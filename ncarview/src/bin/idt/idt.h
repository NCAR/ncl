/*
 *	$Id: idt.h,v 1.11 2000-07-12 18:13:27 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU Lesser General Public License as        *
* published by the Free Software Foundation; either version 2.1 of the  *
* License, or (at your option) any later version.                       *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* Lesser General Public License for more details.                       *
*                                                                       *
* You should have received a copy of the GNU Lesser General Public      *
* License along with this software; if not, write to the Free Software  *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

#ifndef	_idt_
#define	_idt_

#include <X11/Xlib.h>
#include <X11/Intrinsic.h>

typedef	struct	{
	XFontStruct	*x_font;
	char		*select_action;
	/*
	 * the rest are translator args
	 */
	char		*font;
	char		*device;
	Boolean		history;
	Boolean		soft;
	char		*lmin,
			*lmax,
			*lscale;
	char		*foreground,
			*background;
	Boolean		reverse;
	char		*pal;
	int		message_height;
	Boolean		version;
	Boolean		oldidt;
	Boolean		debug;
	} AppData, *AppDataPtr;

extern	AppData	App_Data;

typedef	struct FuncPtrPasser_ {
	void	(*func)();
	} FuncPtrPasser;

#define	TRANS_ARG_COUNT	11	/* number of translator args	*/

/*
 * names of options recognized by translator
 */
#define	TR_FONT		"-font"
#define	TR_DEVICE	"-device"
#define	TR_SOFT		"-soft"
#define	TR_LMIN		"-lmin"
#define	TR_LMAX		"-lmax"
#define	TR_LSCALE	"-lscale"
#define	TR_FOREGROUND	"-foreground"
#define	TR_BACKGROUND	"-background"
#define	TR_REVERSE	"-reverse"
#define	TR_PAL		"-pal"

#define	MAX_TEXT_LINES	20


#ifndef	MAX
#define	MAX(A,B)	((A) > (B) ? (A) : (B))
#endif


#endif

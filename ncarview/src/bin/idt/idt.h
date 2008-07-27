/*
 *	$Id: idt.h,v 1.13 2008-07-27 03:22:37 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
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

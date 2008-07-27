/*
 * $Id: w_dialog.h,v 1.5 2008-07-27 03:22:37 haley Exp $
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

#ifndef	_w_dialog_
#define	_w_dialog_

#include <X11/Intrinsic.h>

extern	void	OkSDTranslation(
#ifdef	NeedFuncProto
	Widget widget,
	XEvent *event,
	String *params,
	Cardinal *num_params
#endif
);

typedef void (*SelectFunc)(
#ifdef	NeedFuncProto
	Voidptr,
	char*
#endif
);

extern	void	CreateSimpleDialogPopup(
#ifdef	NeedFuncProto
	Widget		button,
	char		*label,
	SelectFunc	select,
	Voidptr		data,
	char		*default_value
#endif
);

#endif	/*	_w_dialog_	*/

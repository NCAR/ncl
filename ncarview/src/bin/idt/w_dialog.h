/*
 * $Id: w_dialog.h,v 1.4 2000-08-22 03:30:16 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU General Public License as published     *
* by the Free Software Foundation; either version 2 of the License, or  *
* (at your option) any later version.                                   *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* General Public License for more details.                              *
*                                                                       *
* You should have received a copy of the GNU General Public License     *
* along with this software; if not, write to the Free Software          *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
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

/* 
 * $Id: w_file.h,v 1.4 2008-07-27 03:22:37 haley Exp $
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

#ifndef	_w_file_
#define	_w_file_

#include <X11/Intrinsic.h>
#include "idt.h"

extern	void	CreateFileSelectPopup(
#ifdef	NeedFuncProto
	Widget	button,
	FuncPtrPasser	*select_action
#endif
);

extern	void	FinderTranslation(
#ifdef	NeedFuncProto
	Widget widget,
	XEvent *event,
	String *params,
	Cardinal *num_params
#endif
);

extern	void	OkFileTranslation(
#ifdef	NeedFuncProto
	Widget widget,
	XEvent *event,
	String *params,
	Cardinal *num_params
#endif
);



extern	void SelectFileTranslation(
#ifdef	NeedFuncProto
	Widget widget,
	XEvent *event,
	String *params,
	Cardinal *num_params
#endif
);

#endif	/* _w_file.h_	*/

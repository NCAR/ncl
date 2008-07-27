/*
 * $Id: text.h,v 1.4 2008-07-27 03:22:37 haley Exp $
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

#ifndef	_w_text_
#define	_w_text_

#include <X11/Intrinsic.h>

extern	Widget	InitText(
#ifdef	NeedFuncProto
	Widget	parent
#endif
);

extern	void	AppendText(
#ifdef	NeedFuncProto
	char	*t
#endif
);

#endif	/* _w_text_	*/

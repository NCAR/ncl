/* 
 * $Id: scroll.h,v 1.6 2008-07-27 03:22:37 haley Exp $
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

#ifndef	_scroll_
#define	_scroll_

#include "display.h"

extern	void	ScrollTo(
#ifdef	NeedFuncProto
	WidgetData	*wd,
	float		percent
#endif
);

#endif	/* _scroll_	*/

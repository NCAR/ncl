/*
 * $Id: xrubber.h,v 1.5 2008-07-27 03:22:37 haley Exp $
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

#ifndef	_xrubber_
#define	_xrubber_

#include <X11/Xlib.h>

extern	char	*ZoomCoords(
#ifdef	NeedFuncProto
	Display	*dpy,
	Window	root,
	float	ar,
	float	*llx, 
	float	*lly, 
	float	*urx, 
	float	*ury
#endif
);

#endif	/* _xrubber_	*/

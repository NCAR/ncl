/*
 *      $Id: xresources.h,v 1.5 2008-07-27 03:22:40 haley Exp $
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


#include <X11/Xlib.h>
#include <X11/Xresource.h>

#ifndef	_xresources_
#define	_xresources_



extern	XrmDatabase	_CtOpenResources(
#ifdef	NeedFuncProto
	Display			*dpy,
	char			*prog_name,
	char			*class_name,
	int			*argc,
	char			**argv,
	XrmOptionDescRec	*xodr,
	int			xodr_size
#endif
);

extern	void	_CtCloseResources(
#ifdef	NeedFuncProto
	XrmDatabase	xrd
#endif
);


extern	char	*_CtGetResource(
#ifdef	NeedFuncProto
	XrmDatabase	database,
	char		*parentname,
	char		*parentclass,
	char		*name,
	char		*cgmclass,
	char		*def
#endif
);

#endif	/*	_xresources	*/

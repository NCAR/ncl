/*
 *      $Id: xresources.h,v 1.2 1995-03-16 22:11:55 haley Exp $
 */

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

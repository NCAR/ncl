/*
 *      $Id: xresources.h,v 1.1 1993-04-01 23:04:57 clyne Exp $
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
	char		*class,
	char		*def
#endif
);

#endif	/*	_xresources	*/

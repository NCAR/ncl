/*
 *      $Id: xresources.h,v 1.3 2000-07-12 18:00:53 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU Lesser General Public License as        *
* published by the Free Software Foundation; either version 2.1 of the  *
* License, or (at your option) any later version.                       *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* Lesser General Public License for more details.                       *
*                                                                       *
* You should have received a copy of the GNU Lesser General Public      *
* License along with this software; if not, write to the Free Software  *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
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

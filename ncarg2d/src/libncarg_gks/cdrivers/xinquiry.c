/*
 *	$Id: xinquiry.c,v 1.4 2000-08-22 15:09:54 haley Exp $
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
* along with this software; if not, write to the Free Software         *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

/*
 *      File:		xinquiry.c
 *
 *      Author:		John Clyne
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *      Date:		Thu May 16 15:49:12 MDT 1991
 *
 *      Description:	This file contains routines for handling gks inquiry
 *			functions for the x device driver
 */
#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "common.h"
#include "gksc.h"
#include "gks.h"
#include "x.h"
#include "x_device.h"
#include "xddi.h"


/*ARGSUSED*/
X11_GetColorRepresentation(gksc)
	GKSC	*gksc;
{
	Xddp    	*xi = (Xddp *) gksc->ddp;
	Display 	*dpy = xi->dpy;
	Colormap	cmap = xi->cmap;

	int		*xptr = (int *) gksc->x.list;
	XColor          *rgbptr = (XColor *) gksc->rgb.list;

        unsigned        index   = (unsigned) xptr[0];
        Pixeltype       *color_pal = xi->color_pal;

	rgbptr->pixel = color_pal[index];

	XQueryColor(dpy, cmap, rgbptr);

	return(0);
}

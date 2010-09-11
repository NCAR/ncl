/*
 *      $Id: xinquiry.c,v 1.6 2008-07-23 17:28:02 haley Exp $
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

/*
 *      File:           xinquiry.c
 *
 *      Author:         John Clyne
 *                      National Center for Atmospheric Research
 *                      PO 3000, Boulder, Colorado
 *
 *      Date:           Thu May 16 15:49:12 MDT 1991
 *
 *      Description:    This file contains routines for handling gks inquiry
 *                      functions for the x device driver
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
int X11_GetColorRepresentation(gksc)
        GKSC    *gksc;
{
        Xddp            *xi = (Xddp *) gksc->ddp;
        Display         *dpy = xi->dpy;
        Colormap        cmap = xi->cmap;

        int             *xptr = (int *) gksc->x.list;
        XColor          *rgbptr = (XColor *) gksc->rgb.list;

        unsigned        index   = (unsigned) xptr[0];
        Pixeltype       *color_pal = xi->color_pal;

        rgbptr->pixel = color_pal[index];

        XQueryColor(dpy, cmap, rgbptr);

        return(0);
}

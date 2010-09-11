/*
 *      $Id: pixinquiry.c,v 1.2 2008-07-23 17:28:01 haley Exp $
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
 *      File:           pixinquiry.c
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
#include "pix_device.h"
#include "pixddi.h"


/*ARGSUSED*/
int PIX_GetColorRepresentation(gksc)
        GKSC    *gksc;
{
        PIXddp            *xi = (PIXddp *) gksc->ddp;
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

/*
 *      $Id: pixwrite_png.c,v 1.1 2004-03-20 00:06:55 dbrown Exp $
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
 *      File:           pixwrite_png.c
 *
 *      Author:         David Brown
 *                      National Center for Atmospheric Research
 *                      PO 3000, Boulder, Colorado
 *
 *      Date:           Thu Mar 18 17:55:40 MST 2004
 *
 *      Description:    This file writes an png file for the PIX workstation.
 *                      
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <ncarg/c.h>
#include "gks_device.h"
#include "common.h"
#include "gksc.h"
#include "gks.h"
#include <ncarg/gksP.h>
#include "x.h"
#include "pix_device.h"
#include "pixddi.h"

int PIX_Write_PNG
#ifdef  NeedFuncProto
(
        PIXddp    *xi

)
#else
(xi)
        PIXddp    *xi;
#endif

{
	return 0;
}

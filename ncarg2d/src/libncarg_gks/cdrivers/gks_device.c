/*
 *      $Id: gks_device.c,v 1.8 2004-03-16 18:50:40 fred Exp $
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
 *      File:           gks_device.c
 *
 *      Author:         John Clyne
 *                      National Center for Atmospheric Research
 *                      PO 3000, Boulder, Colorado
 *
 *      Date:           Wed May  1 17:49:30 MDT 1991
 *
 *      Description:    This file contains the devices for the GKS driver.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ncarg/c.h>
#include "gksc.h"
#include "gks_device.h"

int     Verbose = 0;

/*
 *      GKS_GetDevByName
 *      [exported]
 *
 *      return a device named by 'name'
 * on entry
 *      *name           : name of gks output device
 * on exit
 *      return          : the appropriate GKSdev structure or NULL if device
 *                        could not be found.
 */
GKSdev  *GKS_GetDevByName(name)
        char    *name;
{
        GKSdev  *ptr;
        int     i;


        static  int     first = 1;

        if (first) {
                if (getenv("GKS_VERBOSE")) {
                        Verbose = 1;
                }
                first = 0;
        }

        /*
         * look for device 'name'
         */
        if(!strcmp(name,"ctxt"))
                return GKS_GetCTXTdev();
        else if(!strcmp(name,"X11"))
                return GKS_GetX11dev();
        else if(!strcmp(name,"ps"))
                return GKS_GetPSdev();
        else if(!strcmp(name,"pdf"))
                return GKS_GetPDFdev();
        else if(!strcmp(name,"PIX"))
                return GKS_GetPIXdev();

        /*
         * device not found
         */
        ESprintf(ERR_OPN_DEV, "device not found (%s)", name);
        return ((GKSdev *) NULL);
}

/*
 *      $Id: gks_device.c,v 1.12 2009-04-19 22:45:56 haley Exp $
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
        else if(!strcmp(name,"cro"))
                return GKS_GetCROdev();

        /*
         * device not found
         */
        ESprintf(ERR_OPN_DEV, "device not found (%s)", name);
        return ((GKSdev *) NULL);
}

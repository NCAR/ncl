/***********************************************************************
*                                                                      *
*                Copyright (C)  1995                                   *
*        University Corporation for Atmospheric Research               *
*                All Rights Reserved                                   *
*                                                                      *
***********************************************************************/
/*
 *      File:           lg01c.c
 *
 *      Author:         Bob Lackman
 *          National Center for Atmospheric Research
 *          PO 3000, Boulder, Colorado
 *
 *      Date:           Fri Jan 13 18:31:18 MDT 1995
 *
 *      Description:    Demonstrates the Legend Object defaults.
 */
#include <stdio.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/ResList.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/Legend.h>
#include <ncarg/hlu/XWorkstation.h>
#include <ncarg/hlu/NcgmWorkstation.h>
        
main()
{
    int appid, wid, pid;
    int rlist;
    int NCGM=0;

/*
 * Initialize the high level utility library
 */
    NhlInitialize();

/*
 * Create an application context. Set the app dir to the current
 * directory so the application looks for a resource file in the
 * working directory. In this example the resource file supplies the
 * plot title only.
 */
    rlist = NhlRLCreate(NhlSETRL);
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNappDefaultParent,"True");
    NhlRLSetString(rlist,NhlNappUsrDir,"./");
    NhlCreate(&appid,"lg01",NhlappLayerClass,NhlDEFAULT_APP,rlist);

    if (NCGM) {
/*
 * Create a meta file workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./lg01c.ncgm");
        NhlCreate(&wid,"lg01Work",NhlncgmWorkstationLayerClass,
                  NhlDEFAULT_APP,rlist);
    }
    else {
/*
 * Create an X Workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlCreate(&wid,"lg01Work",NhlxWorkstationLayerClass,
                  NhlDEFAULT_APP,rlist);
    }
/*
 * Specify the viewport extent of the object.
 */
    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNvpXF,0.);
    NhlRLSetFloat(rlist,NhlNvpYF,1.);
    NhlRLSetFloat(rlist,NhlNvpWidthF,1.);
    NhlRLSetFloat(rlist,NhlNvpHeightF,1.);
    NhlCreate(&pid,"Legend",NhllegendLayerClass,wid,rlist);

    NhlDraw(pid);
    NhlFrame(wid);
    NhlDestroy(pid);
    NhlDestroy(wid);
    NhlDestroy(appid);
    NhlClose();
    exit(0);
}

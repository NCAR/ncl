/***********************************************************************
*                                                                      *
*                Copyright (C)  1995                                   *
*        University Corporation for Atmospheric Research               *
*                All Rights Reserved                                   *
*                                                                      *
***********************************************************************/
/*
 *      File:           tx03c.c
 *
 *      Author:         Bob Lackman
 *          National Center for Atmospheric Research
 *          PO 3000, Boulder, Colorado
 *
 *      Date:           Fri Jan 06 18:31:18 MDT 1995
 *
 *      Description:    Demonstrates the TextItem Object
 *                      Writes "NCAR Graphics" in a series of
 *                      114 different colors. (The default colormap.)
 */
#include <stdio.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/ResList.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/TextItem.h>
#include <ncarg/hlu/XWorkstation.h>
#include <ncarg/hlu/NcgmWorkstation.h>

main()
{
    int appid, wid, pid;
    int rlist;
    int M = 114;
    int i;
    int NCGM=1;
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
    NhlCreate(&appid,"tx03",NhlappClass,NhlDEFAULT_APP,rlist);

    if (NCGM) {
/*
 * Create a meta file workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./tx03c.ncgm");
        NhlCreate(&wid,"tx03Work",NhlncgmWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
    else {
/*
 * Create an XWorkstation object.
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlCreate(&wid,"tx03Work",NhlxWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
/*
 * Create 114 plots varying the fill color of the text bounding box
 * to all entries of the default workstation color map.
 */
    for( i = 1; i <= M; i++ ) {
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNtxBackgroundFillColor,i);
        NhlCreate(&pid,"TextItems",NhltextItemClass,wid,rlist);
        NhlDraw(pid);
        NhlFrame(wid);
    }
    NhlDestroy(pid);
    NhlDestroy(wid);
    NhlDestroy(appid);
    NhlClose();
    exit(0);
}

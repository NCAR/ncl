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
 *                      different colors (using the default colormap.)
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
    int srlist, grlist;
    int i, num_colors;
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
    srlist = NhlRLCreate(NhlSETRL);
    NhlRLClear(srlist);
    NhlRLSetString(srlist,NhlNappDefaultParent,"True");
    NhlRLSetString(srlist,NhlNappUsrDir,"./");
    NhlCreate(&appid,"tx03",NhlappClass,NhlDEFAULT_APP,srlist);

    if (NCGM) {
/*
 * Create a meta file workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkMetaName,"./tx03c.ncgm");
        NhlCreate(&wid,"tx03Work",NhlncgmWorkstationClass,
                  NhlDEFAULT_APP,srlist);
    }
    else {
/*
 * Create an XWorkstation object.
 */
        NhlRLClear(srlist);
        NhlRLSetInteger(srlist,NhlNwkPause,True);
        NhlCreate(&wid,"tx03Work",NhlxWorkstationClass,
                  NhlDEFAULT_APP,srlist);
    }
/*
 * Get the number of colors in the default color table.
 */
    grlist = NhlRLCreate(NhlGETRL);
    NhlRLClear(grlist);
    NhlRLGetInteger(grlist,NhlNwkColorMapLen,&num_colors);
    NhlGetValues(wid,grlist);
/*
 * Create multiple plots varying the fill color of the text bounding box
 * to all entries of the default workstation color map.
 */
    for( i = 1; i <= num_colors; i++ ) {
        NhlRLClear(srlist);
        NhlRLSetInteger(srlist,NhlNtxBackgroundFillColor,i);
        NhlCreate(&pid,"TextItems",NhltextItemClass,wid,srlist);
        NhlDraw(pid);
        NhlFrame(wid);
    }
    NhlDestroy(pid);
    NhlDestroy(wid);
    NhlDestroy(appid);
    NhlClose();
    exit(0);
}

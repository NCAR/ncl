/***********************************************************************
*                                                                      *
*                Copyright (C)  1995                                   *
*        University Corporation for Atmospheric Research               *
*                All Rights Reserved                                   *
*                                                                      *
***********************************************************************/
/*
 *      File:           ti01c.c
 *
 *      Author:         Bob Lackman
 *          National Center for Atmospheric Research
 *          PO 3000, Boulder, Colorado
 *
 *      Date:           Fri Jan 06 18:31:18 MDT 1995
 *
 *      Description:    Demonstrates the Title Object resource defaults.
 *                      Since using all the defaults would produce a
 *                      blank plot, an exception is made in this case and
 *                      the main title string is set in the resource file.
 */
#include <stdio.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/ResList.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/Title.h>
#include <ncarg/hlu/XWorkstation.h>
        
main()
{
    int appid, wid, pid;
    int rlist;

/*
 * Initialize the high level utility library
 */

    NhlInitialize();

/*
 * Create an application context. Set the app dir to the current directory
 * so the application looks for a resource file in the working directory.
 * In this example the resource file supplies the plot title only.
 */
    rlist = NhlRLCreate(NhlSETRL);
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNappDefaultParent,"True");
    NhlRLSetString(rlist,NhlNappUsrDir,"./");
    NhlCreate(&appid,"ti01",NhlappLayerClass,NhlDEFAULT_APP,rlist);

/*
 * Create an XWorkstation object.
 */
    NhlRLClear(rlist);
    NhlRLSetInteger(rlist,NhlNwkPause,True);
    NhlCreate(&wid,"ti01Work",NhlxWorkstationLayerClass,NhlDEFAULT_APP,
                                    rlist);
/*
 * Specify the viewport extent of the object.
 */

    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNvpXF,.2);
    NhlRLSetFloat(rlist,NhlNvpYF,.8);
    NhlRLSetFloat(rlist,NhlNvpWidthF,.6);
    NhlRLSetFloat(rlist,NhlNvpHeightF,.6);

    NhlCreate(&pid,"Titles",NhltitleLayerClass,wid,rlist);

    NhlDraw(pid);
    NhlFrame(wid);
    NhlDestroy(pid);
    NhlDestroy(wid);
    NhlDestroy(appid);
    NhlClose();
    exit(0);
}

/***********************************************************************
*                                                                      *
*                Copyright (C)  1995                                   *
*        University Corporation for Atmospheric Research               *
*                All Rights Reserved                                   *
*                                                                      *
***********************************************************************/
/*
 *      File:           lb02c.c
 *
 *      Author:         Bob Lackman
 *          National Center for Atmospheric Research
 *          PO 3000, Boulder, Colorado
 *
 *      Date:           Fri Jan 13 18:31:18 MDT 1995
 *
 *      Description:    Demonstrates the LabelBar Object
 *                      Creates color bars with every 6th index of the
 *                      32 different colors in the default colormap.
 */
#include <stdio.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/ResList.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/LabelBar.h>
#include <ncarg/hlu/XWorkstation.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/PSWorkstation.h>
        
main()
{
    int appid, wid, pid, rlist;
    int i, colors[16];
    char *line_labels[16];
    int NCGM=0, X11=1, PS=0;
/*
 * Initialize data values
 */
    for( i = 1; i <= 31; i+=2 ) colors[(i-1)/2] = i;
    line_labels[0] = "Color Index 1 ";
    line_labels[1] = "Color Index 3 ";
    line_labels[2] = "Color Index 5 ";
    line_labels[3] = "Color Index 7 ";
    line_labels[4] = "Color Index 9 ";
    line_labels[5] = "Color Index 11";
    line_labels[6] = "Color Index 13";
    line_labels[7] = "Color Index 15";
    line_labels[8] = "Color Index 17";
    line_labels[9] = "Color Index 19";
    line_labels[10] = "Color Index 21";
    line_labels[11] = "Color Index 23";
    line_labels[12] = "Color Index 25";
    line_labels[13] = "Color Index 27";
    line_labels[14] = "Color Index 29";
    line_labels[15] = "Color Index 31";
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
    NhlRLSetString(rlist,NhlNappUsrDir,"./");
    NhlRLSetString(rlist,NhlNappDefaultParent,"True");
    NhlCreate(&appid,"lb02",NhlappClass,NhlDEFAULT_APP,rlist);

    if (NCGM) {
/*
 * Create a meta file workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./lb02c.ncgm");
        NhlCreate(&wid,"lb02Work",NhlncgmWorkstationClass,NhlDEFAULT_APP,
                  rlist);
    }
    else if (X11) {
/*
 * Create an X Workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlCreate(&wid,"lb02Work",NhlxWorkstationClass,NhlDEFAULT_APP,
                  rlist);
    }
    else if (PS) {
/*
 * Create a PS workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPSFileName,"./lb02c.ps");
        NhlCreate(&wid,"lb02Work",NhlpsWorkstationClass,NhlDEFAULT_APP,
                  rlist);
    }
/*
 * Create a plot with 16 color indices (Every 6th one of the default
 * workstation colormap.
 */
    NhlRLClear(rlist);
    NhlRLSetIntegerArray(rlist,NhlNlbFillColors,colors,16);
    NhlRLSetStringArray(rlist,NhlNlbLabelStrings,line_labels,16);
    NhlRLSetFloat(rlist,NhlNvpXF,0.);
    NhlRLSetFloat(rlist,NhlNvpYF,1.);
    NhlRLSetFloat(rlist,NhlNvpWidthF,1.);
    NhlRLSetFloat(rlist,NhlNvpHeightF,1.);
    NhlCreate(&pid,"LabelBar",NhllabelBarClass,wid,rlist);

    NhlDraw(pid);
    NhlFrame(wid);
    NhlDestroy(pid);
    NhlDestroy(wid);
    NhlDestroy(appid);
    NhlClose();
    exit(0);
}

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
 *                      131 different colors in the default colormap.
 */
#include <stdio.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/ResList.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/LabelBar.h>
#include <ncarg/hlu/XWorkstation.h>
#include <ncarg/hlu/NcgmWorkstation.h>
        
main()
{
    int appid, wid, pid;
    int rlist;
    int colors[22];
    char *line_labels[22];
    int NCGM=0;
/*
 * Initialize data values
 */
    colors[0] = 1;
    colors[1] = 7;
    colors[2] = 13;
    colors[3] = 19;
    colors[4] = 25;
    colors[5] = 31;
    colors[6] = 37;
    colors[7] = 43;
    colors[8] = 49;
    colors[9] = 55;
    colors[10] =61;
    colors[11] = 67;
    colors[12] = 73;
    colors[13] = 79;
    colors[14] = 85;
    colors[15] = 91;
    colors[16] = 97;
    colors[17] = 103;
    colors[18] = 109;
    colors[19] = 115;
    colors[20] = 121;
    colors[21] = 127;
    line_labels[0] = "Color Index 1 ";
    line_labels[1] = "Color Index 7 ";
    line_labels[2] = "Color Index 13";
    line_labels[3] = "Color Index 19";
    line_labels[4] = "Color Index 25";
    line_labels[5] = "Color Index 31";
    line_labels[6] = "Color Index 37";
    line_labels[7] = "Color Index 43";
    line_labels[8] = "Color Index 49";
    line_labels[9] = "Color Index 55";
    line_labels[10] = "Color Index 61";
    line_labels[11] = "Color Index 67";
    line_labels[12] = "Color Index 73";
    line_labels[13] = "Color Index 79";
    line_labels[14] = "Color Index 85";
    line_labels[15] = "Color Index 91";
    line_labels[16] = "Color Index 97";
    line_labels[17] = "Color Index 103";
    line_labels[18] = "Color Index 109";
    line_labels[19] = "Color Index 115";
    line_labels[20] = "Color Index 121";
    line_labels[21] = "Color Index 127";
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
    else {
/*
 * Create an X Workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlCreate(&wid,"lb02Work",NhlxWorkstationClass,NhlDEFAULT_APP,
                  rlist);
    }
/*
 * Create a plot with 22 color indices (Every 6th one of the default
 * workstation colormap.
 */
    NhlRLClear(rlist);
    NhlRLSetIntegerArray(rlist,NhlNlbFillColors,colors,22);
    NhlRLSetStringArray(rlist,NhlNlbLabelStrings,line_labels,22);
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

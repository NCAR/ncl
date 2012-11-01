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
 *                      Creates color bars with every 5th index of the
 *                      255 different colors in the default colormap.
 */
#include <stdio.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/ResList.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/LabelBar.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/hlu/PDFWorkstation.h>
#include <ncarg/hlu/CairoWorkstation.h>
        
int main()
{
    int appid, wid, pid, rlist;
    int i, ii, colors[51];
    char *line_labels[51] = { "Color Index 1", 
                              "Color Index 6",
                              "Color Index 11",
                              "Color Index 16",
                              "Color Index 21",
                              "Color Index 26",
                              "Color Index 31",
                              "Color Index 36",
                              "Color Index 41",
                              "Color Index 46",
                              "Color Index 51",
                              "Color Index 56",
                              "Color Index 61",
                              "Color Index 66",
                              "Color Index 71",
                              "Color Index 76",
                              "Color Index 81",
                              "Color Index 86",
                              "Color Index 91",
                              "Color Index 96",
                              "Color Index 101",
                              "Color Index 106",
                              "Color Index 111",
                              "Color Index 116",
                              "Color Index 121",
                              "Color Index 126",
                              "Color Index 131",
                              "Color Index 136",
                              "Color Index 141",
                              "Color Index 146",
                              "Color Index 151",
                              "Color Index 156",
                              "Color Index 161",
                              "Color Index 166",
                              "Color Index 171",
                              "Color Index 176",
                              "Color Index 181",
                              "Color Index 186",
                              "Color Index 191",
                              "Color Index 196",
                              "Color Index 201",
                              "Color Index 206",
                              "Color Index 211",
                              "Color Index 216",
                              "Color Index 221",
                              "Color Index 226",
                              "Color Index 231",
                              "Color Index 236",
                              "Color Index 241",
                              "Color Index 246",
                              "Color Index 251"};


    char const *wks_type = "x11";
/*
 * Initialize data values
 */
    for( i = 0; i < 51; i++ ) {
      ii = i*5;
      colors[i] = ii;
    }
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

    if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
/*
 * Create a meta file workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./lb02c.ncgm");
        NhlCreate(&wid,"lb02Work",NhlncgmWorkstationClass,NhlDEFAULT_APP,
                  rlist);
    }
    else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 * Create an X Workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlCreate(&wid,"lb02Work",NhlcairoWindowWorkstationClass,NhlDEFAULT_APP,
                  rlist);
    }
    else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 * Create an older-style PostScript workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPSFileName,"./lb02c.ps");
        NhlCreate(&wid,"lb02Work",NhlpsWorkstationClass,NhlDEFAULT_APP,
                  rlist);
    }
    else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 * Create an older-style PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPDFFileName,"./lb02c.pdf");
        NhlCreate(&wid,"lb02Work",NhlpdfWorkstationClass,NhlDEFAULT_APP,
                  rlist);
    }
    else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
             !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 * Create a cairo PS/PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"./lb02c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"lb02Work",NhlcairoDocumentWorkstationClass,NhlDEFAULT_APP,
                  rlist);
    }
    else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 * Create a cairo PNG workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"./lb02c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"lb02Work",NhlcairoImageWorkstationClass,NhlDEFAULT_APP,
                  rlist);
    }
/*
 * Create a plot with 16 color indices (Every 6th one of the default
 * workstation colormap.
 */
    NhlRLClear(rlist);
    NhlRLSetInteger(rlist,NhlNlbBoxCount,51);
    NhlRLSetIntegerArray(rlist,NhlNlbFillColors,colors,51);
    NhlRLSetStringArray(rlist,NhlNlbLabelStrings,line_labels,51);
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

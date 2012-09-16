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
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/hlu/PDFWorkstation.h>
#include <ncarg/hlu/CairoWorkstation.h>

int main()
{
    int appid, wid, pid;
    int srlist, grlist;
    int i, num_colors;
    const char *wks_type = "x11";
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

    if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
/*
 * Create a meta file workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkMetaName,"./tx03c.ncgm");
        NhlCreate(&wid,"tx03Work",NhlncgmWorkstationClass,
                  NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 * Create an X11 workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetInteger(srlist,NhlNwkPause,True);
        NhlCreate(&wid,"tx03Work",NhlcairoWindowWorkstationClass,
                  NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 * Create an older-style PostScript workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkPSFileName,"./tx03c.ps");
        NhlCreate(&wid,"tx03Work",NhlpsWorkstationClass,
                  NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 * Create an older-style PDF workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkPDFFileName,"./tx03c.pdf");
        NhlCreate(&wid,"tx03Work",NhlpdfWorkstationClass,
                  NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
             !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 * Create a cairo PS/PDF workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkFileName,"./tx03c");
        NhlRLSetString(srlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"tx03Work",NhlcairoDocumentWorkstationClass,
                  NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 * Create a cairo PNG workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkFileName,"./tx03c");
        NhlRLSetString(srlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"tx03Work",NhlcairoImageWorkstationClass,
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
 * Create a TextItem and then draw multiple frames varying the fill 
 * color of the text bounding box to all entries of the default 
 * workstation color map.
 */
    NhlRLClear(srlist);
    NhlCreate(&pid,"TextItems",NhltextItemClass,wid,srlist);
    for( i = 1; i <= num_colors; i+=10 ) {
        NhlRLClear(srlist);
        NhlRLSetInteger(srlist,NhlNtxBackgroundFillColor,i);
        NhlSetValues(pid,srlist);
        NhlDraw(pid);
        NhlFrame(wid);
    }
    NhlDestroy(pid);
    NhlDestroy(wid);
    NhlDestroy(appid);
    NhlClose();
    exit(0);
}

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
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/hlu/PDFWorkstation.h>
#include <ncarg/hlu/CairoWorkstation.h>
        
int main()
{
    int appid, wid, pid;
    int rlist;
    char const *wks_type = "x11";

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
    NhlCreate(&appid,"lg01",NhlappClass,NhlDEFAULT_APP,rlist);

    if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
/*
 * Create a meta file workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./lg01c.ncgm");
        NhlCreate(&wid,"lg01Work",NhlncgmWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 * Create an X Workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlCreate(&wid,"lg01Work",NhlcairoWindowWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 * Create an older-style PostScript workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPSFileName,"./lg01c.ps");
        NhlCreate(&wid,"lg01Work",NhlpsWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 * Create an older-style PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPDFFileName,"./lg01c.pdf");
        NhlCreate(&wid,"lg01Work",NhlpdfWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
             !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 * Create a cairo PS/PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"./lg01c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"lg01Work",NhlcairoDocumentWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 * Create a cairo PNG workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"./lg01c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"lg01Work",NhlcairoImageWorkstationClass,
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
    NhlCreate(&pid,"Legend",NhllegendClass,wid,rlist);

    NhlDraw(pid);
    NhlFrame(wid);
    NhlDestroy(pid);
    NhlDestroy(wid);
    NhlDestroy(appid);
    NhlClose();
    exit(0);
}

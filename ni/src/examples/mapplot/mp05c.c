/*
 *      $Id: mp05c.c,v 1.4 2010-03-15 22:49:24 haley Exp $
 */
/***********************************************************************
*                                                                      *
*                Copyright (C)  1995                                   *
*        University Corporation for Atmospheric Research               *
*                All Rights Reserved                                   *
*                                                                      *
***********************************************************************/
/*
 *  File:       mp05c.c
 *
 *  Author:     Mary Haley
 *          National Center for Atmospheric Research
 *          PO 3000, Boulder, Colorado
 *
 *  Date:       Mon Oct 16 15:44:50 MDT 1995
 *
 *   Description:  Draws each of the ten map projections, with and 
 *                 without fills.
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/gks.h>
#include <ncarg/ncargC.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/hlu/PDFWorkstation.h>
#include <ncarg/hlu/CairoWorkstation.h>
#include <ncarg/hlu/MapPlot.h>

#define NMAPS  10

int main(int argc, char *argv[])
{
    int appid,wid,txid,mapid[NMAPS];
    int i, rlist;
    char const *wks_type = "x11";
    char mapstr[20];
/*
 * Initialize the high level utility library
 */
    NhlInitialize();
/*
 * Create an application object.
 */
    rlist = NhlRLCreate(NhlSETRL);
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNappUsrDir,"./");
    NhlCreate(&appid,"mp05",NhlappClass,NhlDEFAULT_APP,rlist);

    if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
/*
 * Create a meta file workstation
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./mp05c.ncgm");
        NhlCreate(&wid,"mp05Work",
                  NhlncgmWorkstationClass,NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 * Create an X workstation
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlCreate(&wid,"mp05Work",NhlcairoWindowWorkstationClass,appid,rlist);
    }
    else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 * Create an older-style PostScript workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPSFileName,"./mp05c.ps");
        NhlCreate(&wid,"mp05Work",
                  NhlpsWorkstationClass,NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 * Create an older-style PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPDFFileName,"./mp05c.pdf");
        NhlCreate(&wid,"mp05Work",
                  NhlpdfWorkstationClass,NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
             !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 * Create a cairo PS/PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"./mp05c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"mp05Work",
                  NhlcairoDocumentWorkstationClass,NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 * Create a cairo PNG workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"./mp05c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"mp05Work",
                  NhlcairoImageWorkstationClass,NhlDEFAULT_APP,rlist);
    }
/*
 * Create a TextItem object.
 */
    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNvpXF,0.2);
    NhlRLSetFloat(rlist,NhlNvpYF,0.95);
    NhlRLSetInteger(rlist,NhlNtxFont,26);
    NhlRLSetString(rlist,NhlNtxString,"Maximal-area projections of all types");
    NhlCreate(&txid,"TextItem",NhltextItemClass,wid,rlist);
/*
 * Create and draw ten different map objects and display them in the
 * same frame.
 */
    for( i = 0; i < NMAPS; i++ ) {
        sprintf( mapstr, "map%d", i );
        NhlCreate(&mapid[i],mapstr,NhlmapPlotClass,wid,0);
        NhlDraw(mapid[i]);
    }
    NhlDraw(txid);
    NhlFrame(wid);
/*
 * Draw each projection individually and fill the countries.
 */
    for( i = 0; i < NMAPS; i++ ) {
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNmpEllipticalBoundary,"True");
        NhlRLSetString(rlist,NhlNmpFillOn,"True");
        NhlRLSetString(rlist,NhlNmpLabelsOn,"False");
        NhlRLSetString(rlist,NhlNmpGridMaskMode,"MaskNotOcean");
        NhlRLSetFloat(rlist,NhlNmpGridLineThicknessF,1.1);
        NhlRLSetFloat(rlist,NhlNvpXF,0.1);
        NhlRLSetFloat(rlist,NhlNvpYF,0.9);
        NhlRLSetFloat(rlist,NhlNvpWidthF,0.8);
        NhlRLSetFloat(rlist,NhlNvpHeightF,0.8);
        NhlSetValues(mapid[i],rlist);
        NhlDraw(mapid[i]);
        NhlFrame(wid);
    }
/*
 * Destroy the objects created, close the HLU library and exit.
 */
    NhlDestroy(wid);
    NhlClose();
    exit(0);
}

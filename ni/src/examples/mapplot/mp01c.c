/*
 *      $Id: mp01c.c,v 1.12 2010-03-15 22:49:24 haley Exp $
 */
/***********************************************************************
*                                                                      *
*                Copyright (C)  1993                                   *
*        University Corporation for Atmospheric Research               *
*                All Rights Reserved                                   *
*                                                                      *
***********************************************************************/
/*
 *  File:       mp01c.c
 *
 *  Author:     David Brown
 *          National Center for Atmospheric Research
 *          PO 3000, Boulder, Colorado
 *
 *  Date:       Fri Oct 14 11:42:41 MDT 1994
 *
 *  Description:    Demonstrates basic MapPlot capabilities
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

int main(int argc, char *argv[])
{
    int appid,wid,mapid;
    int rlist;
    char const *wks_type = "x11";
/*
 * Initialize the high level utility library
 */
    NhlInitialize();
/*
 * Create an application context. Set the app dir to the current
 * directory so the application looks for a resource file in the
 * working directory. The resource file sets most of the Contour
 * resources that remain fixed throughout the life of the Contour
 * object.
 */
    rlist = NhlRLCreate(NhlSETRL);
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNappUsrDir,"./");
    NhlCreate(&appid,"mp01",NhlappClass,NhlDEFAULT_APP,rlist);

    if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
/*
 * Create a meta file workstation
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./mp01c.ncgm");
        NhlCreate(&wid,"mp01Work",
                  NhlncgmWorkstationClass,NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 * Create an X workstation
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlCreate(&wid,"mp01Work",NhlcairoWindowWorkstationClass,appid,rlist);
    }
    else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 * Create an older-style PostScript workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPSFileName,"./mp01c.ps");
        NhlCreate(&wid,"mp01Work",
                  NhlpsWorkstationClass,NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 * Create an older-style PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPDFFileName,"./mp01c.pdf");
        NhlCreate(&wid,"mp01Work",
                  NhlpdfWorkstationClass,NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
             !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 * Create a cairo PS/PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"./mp01c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"mp01Work",
                  NhlcairoDocumentWorkstationClass,NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 * Create a cairo PNG workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"./mp01c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"mp01Work",
                  NhlcairoImageWorkstationClass,NhlDEFAULT_APP,rlist);
    }
/*
 * Draw the default MapPlot object
 */
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNpmTitleDisplayMode,"always");
    NhlRLSetString(rlist,NhlNtiMainString,"mp01c - Frame 1");
    NhlCreate(&mapid,"Map0",NhlmapPlotClass,wid,rlist);
    NhlDraw(mapid);
    NhlFrame(wid);

/*
 * Change some projection resources, add color fill, and
 * all the outlines (Geophysical, National, and US States).
 */
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNtiMainString,"mp01c - Frame 2");
    NhlRLSetString(rlist,NhlNmpFillOn,"true");
    NhlRLSetString(rlist,NhlNmpOutlineBoundarySets,"allBoundaries"); 
    NhlRLSetString(rlist,NhlNmpProjection,"orthographic");
    NhlRLSetString(rlist,NhlNmpPerimOn,"true");
    NhlRLSetFloat(rlist,NhlNvpYF,0.9);
    NhlRLSetFloat(rlist,NhlNvpHeightF,0.8);
    NhlRLSetFloat(rlist,NhlNmpCenterLatF,10.0);
    NhlRLSetFloat(rlist,NhlNmpCenterLonF,-90.0);
    NhlRLSetFloat(rlist,NhlNmpCenterRotF,45.0);
    NhlSetValues(mapid,rlist);

    NhlDraw(mapid);
    NhlFrame(wid);
/*
 * Use the national color set and limit the projection, 
 * using lat/lon boundaries.
 */

    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNtiMainString,"mp01c - Frame 3");
    NhlRLSetString(rlist,NhlNmpFillBoundarySets,"national");
    NhlRLSetString(rlist,NhlNmpLimitMode,"latlon");
    NhlRLSetFloat(rlist,NhlNmpMinLatF,-60.0);
    NhlRLSetFloat(rlist,NhlNmpMaxLatF,60.0);
    NhlRLSetFloat(rlist,NhlNmpMinLonF,-135.0);
    NhlRLSetFloat(rlist,NhlNmpMaxLonF,-45.0);
    NhlSetValues(mapid,rlist);

    NhlDraw(mapid);
    NhlFrame(wid);
/*
 * Polar stereographic projection, change the grid spacing to 10 degrees
 */

    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNtiMainString,"mp01c - Frame 4");
    NhlRLSetString(rlist,NhlNmpProjection,"stereographic");
    NhlRLSetFloat(rlist,NhlNmpGridSpacingF,10.);
    NhlRLSetFloat(rlist,NhlNmpMinLatF,20.0);
    NhlRLSetFloat(rlist,NhlNmpMaxLatF,90.0);
    NhlRLSetFloat(rlist,NhlNmpMinLonF,0.0);
    NhlRLSetFloat(rlist,NhlNmpMaxLonF,360.0);
    NhlRLSetFloat(rlist,NhlNmpCenterLatF,90.0);
    NhlSetValues(mapid,rlist);

    NhlDraw(mapid);
    NhlFrame(wid);

/*
 * Satellite projection using the angle limit method;
 * color US States only individually.
 */
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNtiMainString,"mp01c - Frame 5");
    NhlRLSetString(rlist,NhlNmpFillBoundarySets,"geophysicalAndUSStates");
    NhlRLSetString(rlist,NhlNmpProjection,"satellite");
    NhlRLSetString(rlist,NhlNmpLimitMode,"angles");
    NhlRLSetFloat(rlist,NhlNmpLeftAngleF,45.0);
    NhlRLSetFloat(rlist,NhlNmpRightAngleF,45.0);
    NhlRLSetFloat(rlist,NhlNmpBottomAngleF,45.0);
    NhlRLSetFloat(rlist,NhlNmpTopAngleF,45.0);
    NhlRLSetFloat(rlist,NhlNmpCenterLatF,20.0);
    NhlRLSetFloat(rlist,NhlNmpSatelliteDistF,1.75);
    NhlSetValues(mapid,rlist);

    NhlDraw(mapid);
    NhlFrame(wid);

/*
 * Destroy the objects created, close the HLU library and exit.
 */

    NhlDestroy(mapid);
    NhlDestroy(wid);
    NhlDestroy(appid);
    NhlClose();
    exit(0);
}

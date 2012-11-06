/***********************************************************************
*                                                                      *
*                Copyright (C)  1993                                   *
*        University Corporation for Atmospheric Research               *
*                All Rights Reserved                                   *
*                                                                      *
***********************************************************************/
/*
 *  File:       mp04c.c
 *
 *  Author:     David Brown
 *          National Center for Atmospheric Research
 *          PO 3000, Boulder, Colorado
 *
 *  Date:       Fri Oct 14 11:42:41 MDT 1994
 *
 *  Description:    Illustrates use of Annotation objects.
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

/*
 * Define enough frames for a fairly smooth animation.
 */

#define FRAME_COUNT 36

typedef struct _Anno_List {
    NhlString   name;
    float       lat;
    float       lon;
} Anno_List;


int main(int argc, char *argv[])
{
    int appid,wid,mapid;
    int rlist,grlist;
    int i;
    ng_size_t   num_am_ids;
    int *am_ids;

    Anno_List anno_list[] = {
    {"Los Angeles",34.0,-118.28},
    {"Seattle",47.6,-122.33},
    {"Toronto",43.7,-79.4167},
    {"New York",40.67,-73.83},
    {"Miami",25.75,-80.25},
    {"Mexico City",19.417,-99.167},
    {"London",51.32,-0.1},
    {"Jakarta",-6.13,106.75},
    {"Moscow",55.75,37.7},
    {"New Delhi",28.37,77.217},
    {"Rio de Janeiro",-22.883,-43.283},
    {"Cairo",30.05,31.25},
    {"Buenos Aires", -34.67,-58.4167},
    {"Beijing",39.917,116.4167},
    {"Tokyo",35.67,139.67},
    {"Lagos",6.45,3.28},
    {"Nairobi",-1.283,36.833},
    {"Sydney",-33.9167,151.167},
    {"Bogota",4.633,-74.083},
    {"Lima",-12.1,-77.05},
    {"Cape Town",-33.933,18.4667},
    {"Calcutta",22.583,88.35},
    {"Shanghai",31.217,121.4167},
    {"Bombay",18.93,72.85},
    {"Denver",39.716,-105.017}
    };

    int text_ids[NhlNumber(anno_list)];
    char const *wks_type = "ncgm";
    
/*
 * Initialize the high level utility library
 */

    NhlInitialize();
/*
 * Create an application context. Set the app dir to the current
 * directory so the application looks for a resource file in the
 * working directory. Most resources that remain fixed are set
 * in the resource file.
 */
    rlist = NhlRLCreate(NhlSETRL);
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNappUsrDir,"./");
    NhlRLSetString(rlist,NhlNappDefaultParent,"True");
    NhlCreate(&appid,"mp04",NhlappClass,NhlDEFAULT_APP,rlist);

    if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
/*
 * Create a meta file workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./mp04c.ncgm");
        NhlCreate(&wid,"mp04Work",
                  NhlncgmWorkstationClass,NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 * Create an X workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlCreate(&wid,"mp04Work",
                  NhlcairoWindowWorkstationClass,NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 * Create an older-style PostScript workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPSFileName,"mp04c.ps");
        NhlCreate(&wid,"mp04Work",
                  NhlpsWorkstationClass,NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 * Create an older-style PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPDFFileName,"mp04c.pdf");
        NhlCreate(&wid,"mp04Work",
                  NhlpdfWorkstationClass,NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
             !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 * Create a cairo PS/PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"mp04c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"mp04Work",
                  NhlcairoDocumentWorkstationClass,NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 * Create a cairo PNG workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"mp04c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"mp04Work",
                  NhlcairoImageWorkstationClass,NhlDEFAULT_APP,rlist);
    }
/*
 * AnnoManager objects allow the PlotManager to manipulate any View class
 * object as an annotation a uniform fashion. They allow
 * the user to set the View object's size and location relative to
 * the viewport of a Plot. They may be located relative to one
 * of the viewport sides, or, as in this example, aligned with the plot's 
 * data space (amTrackData is set True in the resource file).
 *
 * Create a TextItem for each place name to be included on the map.
 * Collect the object ids into an array.
 */
    for (i = 0; i < NhlNumber(anno_list); i++) {

        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNtxString,anno_list[i].name);
        NhlCreate(&text_ids[i],anno_list[i].name,
              NhltextItemClass,wid,rlist);
    }

/* 
 * Since the MapPlot object is by default a PlotManager, you can
 * make each TextItem View object into an annotation simply by setting the 
 * pmAnnoViews resource with the array of TextItem ids. 
 */
    NhlRLClear(rlist);
    NhlRLSetIntegerArray(rlist,NhlNpmAnnoViews,text_ids,NhlNumber(text_ids));
    NhlCreate(&mapid,"Map0",NhlmapPlotClass,wid,rlist);

/*
 * Retrieve the ids of the AnnoManager objects created by the PlotManager and
 * then set their location in data coordinate space. The AnnoManager objects
 * are arranged in the same order as the TextItems in the pmAnnoViews
 * resource.
 */
    grlist = NhlRLCreate(NhlGETRL);
    NhlRLClear(grlist);
    NhlRLGetIntegerArray(grlist,NhlNpmAnnoManagers,&am_ids,&num_am_ids);
    NhlGetValues(mapid,grlist);

    for (i = 0; i < num_am_ids; i++) {
        NhlRLClear(rlist);
        NhlRLSetFloat(rlist,NhlNamDataXF,anno_list[i].lon);
        NhlRLSetFloat(rlist,NhlNamDataYF,anno_list[i].lat);
        NhlSetValues(am_ids[i],rlist);
    }

/*
 * Create FRAME_COUNT plots, varying the center longitude by an equal
 * amount each time.
 */
    for (i = FRAME_COUNT; i > 0; i--) {
        NhlRLClear(rlist);
        NhlRLSetFloat(rlist,NhlNmpCenterLonF,i * 360.0 / FRAME_COUNT);
        NhlSetValues(mapid,rlist);
        NhlDraw(mapid);
        NhlFrame(wid);
    }

/*
 * Destroy the objects created, close the HLU library and exit.
 */

    NhlDestroy(mapid);
    for (i = 0; i < NhlNumber(anno_list); i++) {
        NhlDestroy(text_ids[i]);
    }
    NhlDestroy(wid);
    NhlDestroy(appid);
    NhlClose();
    exit(0);
}

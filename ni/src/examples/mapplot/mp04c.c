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
 *  Description:    Illustrates use of AnnoManager and MapPlot objects.
 */

#include <stdio.h>
#include <math.h>
#include <ncarg/gks.h>
#include <ncarg/ncargC.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/XWorkstation.h>
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


main(int argc, char *argv[])
{
    int appid,wid,mapid;
    int rlist,grlist;
    int i, num_anno_ids;
    int *anno_ids;
    int NCGM=1;

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
    NhlCreate(&appid,"mp04",NhlappLayerClass,NhlDEFAULT_APP,rlist);

    if (NCGM) {
/*
 * Create a meta file workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./mp04c.ncgm");
        NhlCreate(&wid,"mp04Work",
                  NhlncgmWorkstationLayerClass,NhlDEFAULT_APP,rlist);
    }
    else {
/*
 * Create an X workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlCreate(&wid,"mp04Work",
                  NhlxWorkstationLayerClass,NhlDEFAULT_APP,rlist);
    }

/*
 * Annotation objects are generic object containers that the Overlay
 * object creates as wrappers for arbitrary view objects. They allow
 * the user to set the view object's size and location relative to
 * the viewport of the Overlay plot. They may be located relative to one
 * of the viewport sides, or, as in this example, aligned with the plot's 
 * data space (anTrackData is set True in the resource file).
 *
 * Create a TextItem for each place name to be included on the map.
 * Collect the object ids into an array.
 */
    for (i = 0; i < NhlNumber(anno_list); i++) {

        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNtxString,anno_list[i].name);
        NhlCreate(&text_ids[i],anno_list[i].name,
              NhltextItemLayerClass,wid,rlist);
    }

/* 
 * Since the MapPlot object is by default an Overlay plot, you can
 * make each TextItem View object into an Annotation simply by setting the 
 * ovAnnoViews resource with the array of TextItem ids. 
 */
    NhlRLClear(rlist);
    NhlRLSetIntegerArray(rlist,NhlNovAnnoViews,text_ids,NhlNumber(text_ids));
    NhlCreate(&mapid,"Map0",NhlmapPlotLayerClass,wid,rlist);

/*
 * Retrieve the ids of the Annotation objects created by the Overlay and
 * then set their location in data coordinate space. The Annotation objects
 * are arranged in the same order as the TextItems in the ovAnnoViews
 * resource.
 */
    grlist = NhlRLCreate(NhlGETRL);
    NhlRLClear(grlist);
    NhlRLGetIntegerArray(grlist,NhlNovAnnotations,&anno_ids,&num_anno_ids);
    NhlGetValues(mapid,grlist);

    for (i = 0; i < num_anno_ids; i++) {
        NhlRLClear(rlist);
        NhlRLSetFloat(rlist,NhlNanDataXF,anno_list[i].lon);
        NhlRLSetFloat(rlist,NhlNanDataYF,anno_list[i].lat);
        NhlSetValues(anno_ids[i],rlist);
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

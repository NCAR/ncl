/***********************************************************************
*                                                                      *
*                Copyright (C)  1993                                   *
*        University Corporation for Atmospheric Research               *
*                All Rights Reserved                                   *
*                                                                      *
***********************************************************************/
/*
 *  File:       an01c.c
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

/* 
 * Note that Annotation objects are available to any plot object that 
 * has created an Overlay. The MapPlot object creates an Overlay object
 * by default. Hence, Annotations are available. There is no need to 
 * include the Annotation header file, although you could.
 */
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
    int     anno_id;
    int     obj_id;
} Anno_List;


main(int argc, char *argv[])
{
    NhlErrorTypes ret = NhlNOERROR;
    int appid,wid,mapid;
    int rlist,grlist;
    int i;
    char buf[256];

    Anno_List anno_list[] = {
    {"Los Angeles",34.0,-118.28,-1,-1},
    {"Seattle",47.6,-122.33,-1,-1},
    {"Toronto",43.7,-79.4167},
    {"New York",40.67,-73.83,-1,-1},
    {"Miami",25.75,-80.25,-1,-1},
    {"Mexico City",19.417,-99.167,-1,-1},
    {"London",51.32,-0.1,-1,-1},
    {"Jakarta",-6.13,106.75,-1,-1},
    {"Moscow",55.75,37.7,-1,-1},
    {"New Delhi",28.37,77.217,-1,-1},
    {"Rio de Janeiro",-22.883,-43.283,-1,-1},
    {"Cairo",30.05,31.25,-1,-1},
    {"Buenos Aires", -34.67,-58.4167,-1,-1},
    {"Beijing",39.917,116.4167,-1,-1},
    {"Tokyo",35.67,139.67,-1,-1},
    {"Lagos",6.45,3.28,-1,-1},
    {"Nairobi",-1.283,36.833,-1,-1},
    {"Sydney",-33.9167,151.167,-1,-1},
    {"Bogota",4.633,-74.083,-1,-1},
    {"Lima",-12.1,-77.05,-1,-1},
    {"Cape Town",-33.933,18.4667,-1,-1},
    {"Calcutta",22.583,88.35,-1,-1},
    {"Shanghai",31.217,121.4167,-1,-1},
    {"Bombay",18.93,72.85,-1,-1},
    {"Denver",39.716,-105.017,-1,-1}
    };
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
    NhlRLSetString(rlist,NhlNappDefaultParent,"True");
    NhlCreate(&appid,"an01",NhlappLayerClass,NhlDEFAULT_APP,rlist);

#if XWORK
/*
 * Create an X workstation
 */
    NhlCreate(&wid,"an01Work",NhlxWorkstationLayerClass,NhlDEFAULT_APP,0);
#else
/*
 * Create a meta file workstation
 */
        rlist = NhlRLCreate(NhlSETRL);
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./an01c.ncgm");
        NhlCreate(&wid,"an01Work",
                  NhlncgmWorkstationLayerClass,NhlDEFAULT_APP,rlist);
#endif

/* 
 * Create a Map Plot object
 */
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNovTitleDisplayMode,"always");
    NhlRLSetString(rlist,NhlNtiMainString,"an01");
    NhlRLSetString(rlist,NhlNmpFillOn,"true");
    NhlRLSetString(rlist,NhlNmpProjection,"orthographic");
    NhlCreate(&mapid,"Map0",NhlmapPlotLayerClass,wid,rlist);
/*
 * Annotation objects are generic object containers that the Overlay
 * object knows how to manipulate in a uniform fashion. They may be 
 * manipulated in NDC space like the Title or LabelBar objects, or, as
 * in this example, aligned with with the plot object's data space.
 *
 * Create a TextItem for each place name to be included on the map.
 * Then create an Annotation object for each TextItem. Register each
 * Annotation with the MapPlot object, the creator of the Overlay.
 */
    for (i = 0; i < NhlNumber(anno_list); i++) {

        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNtxString,anno_list[i].name);
        NhlRLSetFloat(rlist,NhlNtxFontHeightF,0.01);
        NhlRLSetInteger(rlist,NhlNtxFontColor,18);
        NhlCreate(&anno_list[i].obj_id,anno_list[i].name,
              NhltextItemLayerClass,wid,rlist);
        

        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNanResizeNotify,"true");
        NhlRLSetString(rlist,NhlNanTrackData,"true");
        NhlRLSetString(rlist,NhlNanJust,"centerleft");
        NhlRLSetFloat(rlist,NhlNanDataXF,anno_list[i].lon);
        NhlRLSetFloat(rlist,NhlNanDataYF,anno_list[i].lat);
        NhlRLSetInteger(rlist,NhlNanPlotId,anno_list[i].obj_id);
        NhlCreate(&anno_list[i].anno_id,anno_list[i].name,
              NhlannotationLayerClass,wid,rlist);

        NhlRegisterAnnotation(mapid,anno_list[i].anno_id);
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

    for (i = 0; i < NhlNumber(anno_list); i++) {
        NhlDestroy(anno_list[i].obj_id);
        NhlDestroy(anno_list[i].anno_id);
    }
    NhlDestroy(mapid);
    NhlDestroy(wid);
    NhlDestroy(appid);
    NhlClose();
    exit(0);
}

/*
**      $Id: xy06c.c,v 1.2 1995-02-22 16:35:47 haley Exp $
*/
/************************************************************************
*                                                                       *
*                Copyright (C)  1995                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
************************************************************************/
/*
**  File:       xy06c.c
**
**  Author:     Mary Haley (converted partly from example "agex04")
**          National Center for Atmospheric Research
**          PO 3000, Boulder, Colorado
**
**  Date:       Thu Jan 26 13:44:45 MST 1995
**
**  Description:    This example shows one way on how to create an XyPlot
**                  object with multiple lines using the CoordArrTable 
**                  object.  Some of the XyPlot line resources are tweaked to
**                  show how to change the appearances of the markers and/or
**                  lines.
**
**                  Resources other than XyPlot resources are modified in
**                  this example to show how to customize tick marks, change
**                  titles, and view port size.
**
**                  The "CoordArrTable" object is used to set up the data,
**                  and the "DataDep" object is used to describe attributes
**                  of the data being plotted.
**
**                  This example is similar to the ncargex Autograph example
**                  "agex04".
*/

#include <stdio.h>
#include <math.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/ResList.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/XWorkstation.h>
#include <ncarg/hlu/XyPlot.h>
#include <ncarg/hlu/CoordArrTable.h>

#define NPTS  201
#define NCURVE  10

/*
 * Create data arrays for XyPlot object.
 */
float *ydra[NCURVE];
float *xdra[NCURVE];
int len[NCURVE] = {NPTS,NPTS,NPTS,NPTS,NPTS,NPTS,NPTS,NPTS,NPTS,NPTS};

/*
 * Create arrays for customizing tickmarks.
 */
char *explicit_labels[] ={"-1.0","-.8","-.6","-.4","-.2","0.",".2",".4",".6",
                          ".8","1.0"};
float explicit_values[] ={-1.,-.8,-.6,-.4,-.2,0.,.2,.4,.6,.8,1.};
/*
 * Create arrays for customizing curves.
 */
int colors[NCURVE] = {20,60,30,10,50,90,5,70,40,80};
int markcolors[NCURVE] = {85,45,75,5,95,55,15,35,65,25};
int markers[NCURVE] = {1,2,3,4,5,1,2,3,4,5};
int markmodes[NCURVE] = {NhlMARKERSONLY,NhlMARKLINES,NhlMARKERSONLY,
                         NhlMARKLINES,NhlMARKERSONLY,NhlMARKLINES,
                         NhlMARKERSONLY,NhlMARKLINES,NhlMARKERSONLY,
                         NhlMARKLINES};
float marksizes[NCURVE] = {.010,.012,.014,.016,.018,.018,.016,.014,.012,.010};

main()
{
    int     appid,xworkid,plotid,dataid,datadepid;
    int     rlist;
    int     i, j;
/*
 * Initialize the HLU library and set up resource template
 */
    NhlInitialize();
    rlist = NhlRLCreate(NhlSETRL);
/*
 * Create Application and XWorkstation objects.  The Application
 * object name is used to determine the name of the resource file,
 * which is "xy06.res" in this case.
 */
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNappDefaultParent,"True");
    NhlRLSetString(rlist,NhlNappUsrDir,"./");
    NhlCreate(&appid,"xy06",NhlappLayerClass,NhlDEFAULT_APP,rlist);

    NhlCreate(&xworkid,"xy06Work",NhlxWorkstationLayerClass,NhlDEFAULT_APP,0);
/*
 * Initialize data.
 */
    for( j = 0; j < NCURVE; j++ ) {
        ydra[j] = (float *)malloc(sizeof(float)*NPTS);
        xdra[j] = (float *)malloc(sizeof(float)*NPTS);
        if (xdra[j] == NULL || ydra[j] == NULL) {
            NhlPError(NhlFATAL,NhlEUNKNOWN,
                      "Unable to malloc space for xdra/ydra array");
            exit(3);
        }
        for( i = 0; i < NPTS; i++ ) {
            xdra[j][i] =  -1.+.02*(float)(i);
            if( i > 100 ) xdra[j][i] = 2.-xdra[j][i];
            ydra[j][i] = (float)(j+1)*sqrt(1.000000000001-pow(xdra[j][i],2.))/10.;
            if(i > 100) ydra[j][i] = -ydra[j][i];
        }
    }
/*
 * Create the CoordArrTable object which defines the data for the XyPlot
 * object. The id from this object will become the value for the XyDataDep
 * resource, "dsDataItem".
 */
    NhlRLClear(rlist);
    NhlRLSetIntegerArray(rlist,NhlNctYTableLengths,len,NCURVE);
    NhlRLSetIntegerArray(rlist,NhlNctXTableLengths,len,NCURVE);
    NhlRLSetArray(rlist,NhlNctYTable,ydra,NhlTPointer,sizeof(NhlPointer),
                  NCURVE);
    NhlRLSetArray(rlist,NhlNctXTable,xdra,NhlTPointer,sizeof(NhlPointer),
                  NCURVE);
    NhlCreate(&dataid,"xyData",NhlcoordArrTableLayerClass,NhlDEFAULT_APP,
              rlist);
/*
 * Create a DataDep object (as a child of the XWorkstation object) which
 * allows you to change attributes of the data like line and marker color,
 * marker size, and marker type. The id from this object will become the
 * value of the XyPlot resource "xyCurveData".
 */
    NhlRLClear(rlist);
    NhlRLSetInteger(rlist,NhlNdsDataItem,dataid);
    NhlRLSetIntegerArray(rlist,NhlNxyColors,colors,NCURVE);
    NhlRLSetIntegerArray(rlist,NhlNxyMarkerModes,markmodes,NCURVE);
    NhlRLSetIntegerArray(rlist,NhlNxyMarkerColors,markcolors,NCURVE);
    NhlRLSetFloatArray(rlist,NhlNxyMarkerSizes,marksizes,NCURVE);
    NhlRLSetIntegerArray(rlist,NhlNxyMarkers,markers,NCURVE);
    NhlCreate(&datadepid,"xyDataDep",NhlxyDataDepLayerClass,xworkid,rlist);
/*
 * Create the XyPlot object and tweak some of the tickmark, title and
 * view port resources (some in the "xy06.res" resource file).
 */
    NhlRLClear(rlist);
    NhlRLSetInteger(rlist,NhlNxyCurveData,datadepid);
/*
 * Modify TickMark resources.
 */
    NhlRLSetStringArray(rlist,NhlNtmXBLabels,explicit_labels,
                    NhlNumber(explicit_labels));
    NhlRLSetFloatArray(rlist,NhlNtmXBValues,explicit_values,
                    NhlNumber(explicit_values));
    NhlRLSetStringArray(rlist,NhlNtmYLLabels,explicit_labels,
                    NhlNumber(explicit_labels));
    NhlRLSetFloatArray(rlist,NhlNtmYLValues,explicit_values,
                    NhlNumber(explicit_values));
    NhlCreate(&plotid,"xyPlot",NhlxyPlotLayerClass,xworkid,rlist);
/*
 * Draw the plot (to its parent XWorkstation).
 */
    NhlDraw(plotid);
    NhlFrame(xworkid);
/*
 * NhlDestroy destroys the given id and all of its children
 * so destroying "xworkid" will also destroy "plotid".
 */
    NhlRLDestroy(rlist);
    NhlDestroy(xworkid);
    NhlDestroy(appid);
/*
 * Restores state.
 */
    NhlClose();

    exit(0);
}

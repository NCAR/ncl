/*
**      $Id: xy04c.c,v 1.3 1995-02-16 14:53:30 haley Exp $
*/
/***********************************************************************
*                                                                      *
*                Copyright (C)  1995                                   *
*        University Corporation for Atmospheric Research               *
*                All Rights Reserved                                   *
*                                                                      *
***********************************************************************/
/*
**  File:       xy04c.c
**
**  Author:     Mary Haley
**          National Center for Atmospheric Research
**          PO 3000, Boulder, Colorado
**
**  Date:       Fri Jan 27 08:24:42 MST 1995
**
** Description:    This program shows one way on how to create an XyPlot
**                 object with multiple lines in the plot using
**                 the CoordArrays object.  Some of the XyPlot line
**                 resources are tweaked in the resource file to
**                 show how to change the appearance of these multiple
**                 lines.
**
**                 The "CoordArrays" object is used to set up the data,
**                 and the "DataDep" object is used to describe
**                 attributes of the data being plotted, like the
**                 line color and the dash patterns.
**
*/

#include <stdio.h>
#include <math.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/ResList.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/XWorkstation.h>
#include <ncarg/hlu/XyPlot.h>
#include <ncarg/hlu/CoordArrays.h>

#define NPTS  500
#define NCURVE  4
#define PI100 0.031415926535898

float ydra[NCURVE][NPTS];
int len[2] = {NCURVE,NPTS};

/*
 * Set up arrays of labels, colors, and dash patterns for each curve.
 */
int colors[NCURVE] = {50, 75, 100, 40};
int xydash[NCURVE] = {0, 5, 10, 15};
char *explabs[NCURVE] ={"Curve 1","Curve 2","Curve 3", "Curve 4"};

main()
{
    int     appid,xworkid,plotid,dataid,datadepid;
    int     rlist;
    int     i, j;
    float   theta;
/*
 * Initialize data for the XyPlot object.
 */
    for( j = 0; j < NCURVE; j++ ) {
        for( i = 0; i < NPTS; i++ ) {
            theta = PI100*(float)(i);
            ydra[j][i] = (j+1)*100.+.9*(float)(i)*sin(theta);
        }
    }
/*
 * Initialize the HLU library and set up resource template.
 */
    NhlInitialize();
    rlist = NhlRLCreate(NhlSETRL);
/*
 * Create Application and XWorkstation objects.  The Application
 * object name is used to determine the name of the resource file,
 * which is "xy04.res" in this case.
 */
    NhlCreate(&appid,"xy04",NhlappLayerClass,NhlDEFAULT_APP,0);
    NhlCreate(&xworkid,"xy04Work",NhlxWorkstationLayerClass,
              NhlDEFAULT_APP,0);
/*
 * Define the "CoordArrays" object.  Since only the Y values are
 * specified here, each Y value will be paired with its integer
 * array index.  The id for this object will then later be used as
 * the value for the Data Dep resource, "dsDataItem".
 */
    NhlRLClear(rlist);
    NhlRLSetMDFloatArray(rlist,NhlNcaYArray,&ydra[0][0],2,len);
    NhlCreate(&dataid,"xyData",NhlcoordArraysLayerClass,NhlDEFAULT_APP,
              rlist);
/*
 * Create a DataDep object as a child of the XWorkstation object.
 * DataDep resources are used for defining things like the color, label,
 * and dash pattern of each line.  The id from this object will become
 * the value for the XyPlot resource "xyCurveData".
 */
    NhlRLClear(rlist);
    NhlRLSetInteger(rlist,NhlNdsDataItem,dataid);
    NhlRLSetIntegerArray(rlist,NhlNxyColors,colors,NhlNumber(colors));
    NhlRLSetIntegerArray(rlist,NhlNxyDashPatterns,xydash,
                         NhlNumber(xydash));
    NhlRLSetStringArray(rlist,NhlNxyExplicitLabels,explabs,
                        NhlNumber(explabs));
    NhlCreate(&datadepid,"xyDataDep",NhlxyDataDepLayerClass,xworkid,
              rlist);
/*
 * Create the XyPlot object which is created as a child of the
 * XWorkstation object.  The resources that are being changed are done
 * in the "xy04.res" file, and they will affect this XyPlot object.
 */
    NhlRLClear(rlist);
    NhlRLSetInteger(rlist,NhlNxyCurveData,datadepid);
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

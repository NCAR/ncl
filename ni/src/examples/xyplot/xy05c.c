/*
**      $Id: xy05c.c,v 1.3 1995-02-10 15:18:47 haley Exp $
*/
/************************************************************************
*                                                                       *
*                Copyright (C)  1995                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
************************************************************************/
/*
**  File:       xy05c.c
**
**  Author:     Mary Haley
**          National Center for Atmospheric Research
**          PO 3000, Boulder, Colorado
**
**  Date:       Fri Jan 27 08:24:42 MST 1995
**
**  Description:    This example shows how to create an XY plot object with
**                  multiple lines using the CoordArrays and multiple Data
**                  objects.  Using multiple Data objects allows you 
**                  to have a different number of points in each line.
**
**                  Some of the XY marker resources are tweaked in the
**                  resource file to show how to change the appearance of
**                  these multiple lines.  This example also shows you how to
**                  use the xyYIrregularPoints resource to define your own Y
**                  axis values.
**
**                  The "CoordArrays" object is used to set up the data,
**                  and the "DataDep" object is used to describe attributes
**                  of the data being plotted, like the marker styles and
**                  sizes.
*/

#include <stdio.h>
#include <math.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/ResList.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/XWorkstation.h>
#include <ncarg/hlu/XyPlot.h>
#include <ncarg/hlu/CoordArrays.h>

#define NCURVE  4
#define PI100 .031415926535898
float *ydra[NCURVE];

int len[NCURVE] = {500,200,400,300};

main()
{
    int     appid,xworkid,plotid,dataid[NCURVE],datadepid[NCURVE];
    int     rlist;
    int     i, j;
    float   theta;
    float   explicit_values[10];
    char    datastr[10];
/*
 * Initialize XY data
 */
    for( j = 0; j < NCURVE; j++ ) {
        ydra[j] = (float *)malloc(sizeof(float)*len[j]);
        if (ydra[j] == NULL) {
            NhlPError(NhlFATAL,NhlEUNKNOWN,"Unable to malloc space for ydra array");
            exit(3);
        }
        for( i = 0; i < len[j]; i++ ) {
            theta = PI100*(float)(i);
            ydra[j][i] = (j+1)*100.+.9*(float)(i)*sin(theta);
        }
    }
/*
 * Set up the array of points we want to use for the Y axis.
 */
    for( i = 0; i < 10; i++ ) {
        explicit_values[i] = pow(2.,(float)(i+4));
    }
/*
 * Initialize the HLU library and set up resource template
 */
    NhlInitialize();
    rlist = NhlRLCreate(NhlSETRL);
/*
 * Create application and X workstation object.  The application name
 * is used to determine the name of the resource file, which will be
 * 'xy05.res' in this case.
 */
    NhlCreate(&appid,"xy05",NhlappLayerClass,NhlDEFAULT_APP,0);
    NhlCreate(&xworkid,"xy05Work",NhlxWorkstationLayerClass,appid,0);
/*
 * Define the data objects.  Since only the Y values are specified here,
 * each Y value will be paired with its integer array index.  The id for
 * each object will then later be used as a value for a Data Dep
 * resource, "dsDataItem".
 */
    for( i = 0; i < NCURVE; i++ ) {
        NhlRLClear(rlist);
        NhlRLSetFloatArray(rlist,NhlNcaYArray,ydra[i],len[i]);
        sprintf(datastr,"xyCoord%1d",i+1);
        NhlCreate(&dataid[i],datastr,NhlcoordArraysLayerClass,NhlDEFAULT_APP,
              rlist);
    }
/*
 * Define Data Dependent resources and tweak some resources in the
 * resource file.
 */
    for( i = 0; i < NCURVE; i++ ) {
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNdsDataItem,dataid[i]);
        sprintf(datastr,"xyDep%1d",i+1);
        NhlCreate(&datadepid[i],datastr,NhlxyDataDepLayerClass,NhlDEFAULT_APP,
             rlist);
    }
/*
 * This array of Data Dependent objects is now the resource value for
 * xyCurveData.  Tweak some more XYPlot resources in the resource file.
 */
    NhlRLClear(rlist);
    NhlRLSetIntegerArray(rlist,NhlNxyCurveData,datadepid,NhlNumber(datadepid));
    NhlRLSetFloatArray(rlist,NhlNxyYIrregularPoints,explicit_values,
                    NhlNumber(explicit_values));
    NhlCreate(&plotid,"XYPlot",NhlxyPlotLayerClass,xworkid,rlist);
/*
 * Draw the plot (to its parent X Workstation)
 */
    NhlDraw(plotid);
    NhlFrame(xworkid);
/*
 * NhlDestroy destroys the given id and all of its children
 * so destroying "xworkid" will also destroy plotid.
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

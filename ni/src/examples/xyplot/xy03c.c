/*
**      $Id: xy03c.c,v 1.6 1995-02-16 14:53:27 haley Exp $
*/
/***********************************************************************
*                                                                      *
*                Copyright (C)  1995                                   *
*        University Corporation for Atmospheric Research               *
*                All Rights Reserved                                   *
*                                                                      *
***********************************************************************/
/*
**  File:       xy03c.c
**
**  Author:     Mary Haley
**          National Center for Atmospheric Research
**          PO 3000, Boulder, Colorado
**
**  Date:       Fri Jan 27 08:24:42 MST 1995
**
** Description:    This program shows how to create an XyPlot object
**                 with some of the XyPlot line resources tweaked.  A
**                 resource file is used to changed the resources 
**                 except in those cases where a resource is an array
**                 and can only be changed programmatically.
**                 This program uses the same Y-axis dataset as the
**                 example "xy02", but this time values for the X
**                 axis are specified, changing the look of the plot.
**
**                 The "CoordArrays" object is used to set up the data.
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
#define PI100 .031415926535898

main()
{
    int     appid,xworkid,plotid,dataid;
    int     rlist;
    int     i;
    float   xdra[NPTS],ydra[NPTS], theta;
/*
 * Initialize some data for the XyPlot object.
 */
    for( i = 0; i < NPTS; i++ ) {
        theta = PI100*(float)(i);
        xdra[i] = 500.+.9*(float)(i)*cos(theta);
        ydra[i] = 500.+.9*(float)(i)*sin(theta);
    }
/*
 * Initialize the HLU library and set up resource template.
 */
    NhlInitialize();
    rlist = NhlRLCreate(NhlSETRL);
/*
 * Create Application and XWorkstation objects.  The Application object
 * name is used to determine the name of the resource file, which is
 * "xy03.res" in this case.
 */
    NhlCreate(&appid,"xy03",NhlappLayerClass,NhlDEFAULT_APP,0);
    NhlCreate(&xworkid,"xy03Work",NhlxWorkstationLayerClass,
               NhlDEFAULT_APP,0);
/*
 * Define the data object.  The id for this object will later be used
 * as the value for the XyPlot data resource, "xyCurveData".
 */
    NhlRLClear(rlist);
    NhlRLSetFloatArray(rlist,NhlNcaXArray,xdra,NhlNumber(xdra));
    NhlRLSetFloatArray(rlist,NhlNcaYArray,ydra,NhlNumber(ydra));
    NhlCreate(&dataid,"xyData",NhlcoordArraysLayerClass,
              NhlDEFAULT_APP,rlist);
/*
 * Create the XyPlot object which is created as a child of the
 * Xworkstation object.  The resources that are being changed are done
 * in the "xy03.res" file, and they will affect this XyPlot object.
 */
    NhlRLClear(rlist);
    NhlRLSetInteger(rlist,NhlNxyCurveData,dataid);
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

/*
**      $Id: xy01c.c,v 1.10 1995-03-17 20:56:31 haley Exp $
*/
/***********************************************************************
*                                                                      *
*                Copyright (C)  1995                                   *
*        University Corporation for Atmospheric Research               *
*                All Rights Reserved                                   *
*                                                                      *
***********************************************************************/
/*
**  File:       xy01c.c
**
**  Author:     Mary Haley
**          National Center for Atmospheric Research
**          PO 3000, Boulder, Colorado
**
**  Date:       Fri Jan 27 08:24:42 MST 1995
**
** Description:    This program shows how to create an XyPlot object
**                 using all the default resources except for the data
**                 resource; since there's no "default data", we need
**                 to create some.  A resource file is included with
**                 this example, but only to show what all the XyPlot
**                 resources are and what their defaults are set to.
**                 The whole resource file is commented out.
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

/*
 * Define the number of points in the curve.
 */
#define NPTS  500
#define PI100 .031415926535898

main()
{
    int     appid,xworkid,plotid,dataid;
    int     rlist;
    int     i;
    float   ydra[NPTS], theta;
/*
 * Initialize some data for the XY plot.
 */
    for( i = 0; i < NPTS; i++ ) {
        theta = PI100*(float)(i);
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
 * "xy01.res" in this case.
 */
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNappDefaultParent,"True");
    NhlRLSetString(rlist,NhlNappUsrDir,"./");
    NhlCreate(&appid,"xy01",NhlappLayerClass,NhlDEFAULT_APP,rlist);

    NhlCreate(&xworkid,"xy01Work",NhlxWorkstationLayerClass,
              NhlDEFAULT_APP,0);
/*
 * Define the data object.  Since only the Y values are specified here,
 * each Y value will be paired with its integer array index.  The id
 * for this object will later be used as the value for the XyPlot data
 * resource, "xyCoordData".
 */
    NhlRLClear(rlist);
    NhlRLSetFloatArray(rlist,NhlNcaYArray,ydra,NhlNumber(ydra));
    NhlCreate(&dataid,"xyData",NhlcoordArraysLayerClass,NhlDEFAULT_APP,
              rlist);
/*
 * Create the XyPlot object which is created as a child of the
 * XWorkstation object.
 */
    NhlRLClear(rlist);
    NhlRLSetInteger(rlist,NhlNxyCoordData,dataid);
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

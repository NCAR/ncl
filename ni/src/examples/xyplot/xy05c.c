/*
**      $Id: xy05c.c,v 1.11 1995-04-07 10:55:13 boote Exp $
*/
/**********************************************************************
*                                                                     *
*                Copyright (C)  1995                                  *
*        University Corporation for Atmospheric Research              *
*                All Rights Reserved                                  *
*                                                                     *
**********************************************************************/
/*
**  File:       xy05c.c
**
**  Author:     Mary Haley
**          National Center for Atmospheric Research
**          PO 3000, Boulder, Colorado
**
**  Date:       Thu Jan 26 13:44:45 MST 1995
**
**  Description:    This example shows one way on how to create an
**                  XyPlot object with multiple lines using the
**                  CoordArrTable  object.  Some of the XyPlot line
**                  resources are tweaked to show how to change the
**                  appearances of the lines.
**
**                  The "CoordArrTable" object is used to set up the
**                  data. (The Fortran and NCL version of this example
**                  uses "CoordArrays" since "CoordArrTable" is not
**                  available).
**
*/

#include <stdio.h>
#include <math.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/ResList.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/XWorkstation.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/XyPlot.h>
#include <ncarg/hlu/CoordArrTable.h>

#define NCURVE  10
#define NPTS    100
#define PI      3.14159

/*
 * Create data arrays for XyPlot object.
 */
float *y[NCURVE];
int length[NCURVE];

main()
{
    int     appid,xworkid,plotid,dataid;
    int     rlist;
    int     i, j;
    int NCGM=0;
/*
 * Initialize the HLU library and set up resource template
 */
    NhlInitialize();
    rlist = NhlRLCreate(NhlSETRL);
/*
 * Create Application object.  The Application object name is used to
 * determine the name of the resource file, which is "xy05.res" in this
 * case.
 */
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNappDefaultParent,"True");
    NhlRLSetString(rlist,NhlNappUsrDir,"./");
    NhlCreate(&appid,"xy05",NhlappClass,NhlDEFAULT_APP,rlist);

    if (NCGM) {
/*
 * Create a meta file object.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./xy05c.ncgm");
        NhlCreate(&xworkid,"xy05Work",NhlncgmWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
    else {
/*
 * Create an XWorkstation object.
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlCreate(&xworkid,"xy05Work",NhlxWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
/*
 * Initialize data.
 */
    for( j = 0; j < NCURVE; j++ ) {
        length[j] = NPTS - j*10;
        y[j] = (float *)malloc(sizeof(float)*length[j]);
        if (y[j] == NULL) {
            NhlPError(NhlFATAL,NhlEUNKNOWN,
                      "Unable to malloc space for y array");
            exit(3);
        }
		for( i = 0; i < length[j]; i++ ) {
            y[j][i] = (float)(j+1)*sin((float)(2.*i*PI)/(float)(length[j]-1));
        }
    }
/*
 * Create the CoordArrTable object which defines the data for the
 * XyPlot object. The id from this object will become the value for
 * the XyPlot resource, "xyCoordData".
 */
    NhlRLClear(rlist);
    NhlRLSetIntegerArray(rlist,NhlNctYTableLengths,length,NCURVE);
    NhlRLSetArray(rlist,NhlNctYTable,y,NhlTPointer,sizeof(NhlPointer),
                  NCURVE);
    NhlCreate(&dataid,"xyData",NhlcoordArrTableClass,
              NhlDEFAULT_APP,rlist);
/*
 * Create the XyPlot object and tweak some of the tickmark, title and
 * view port resources (some in the "xy05.res" resource file).
 */
    NhlRLClear(rlist);
    NhlRLSetInteger(rlist,NhlNxyCoordData,dataid);
    NhlCreate(&plotid,"xyPlot",NhlxyPlotClass,xworkid,rlist);
/*
 * Draw the plot.
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

/*
**      $Id: xy05c.c,v 1.12 1995-04-27 17:34:40 haley Exp $
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
#define NCOLORS 12
#define PI      3.14159

/*
 * Create data arrays for XyPlot object.
 */
float *y[NCURVE];
int length[NCURVE];

main()
{
    int     appid,xworkid,plotid,dataid;
    int     rlist, i, j, len[2];
    float   cmap[NCOLORS][3];
    int NCGM=0;
/*
 * Initialize the HLU library and set up resource template
 */
    NhlInitialize();
    rlist = NhlRLCreate(NhlSETRL);
/*
 * Modify the color map.  Color indices '0' and '1' are the background
 * and foreground colors respectively.
 */
    cmap[0][0] = 0.00; cmap[0][1] = 0.00; cmap[0][2] = 0.00;
    cmap[1][0] = 1.00; cmap[1][1] = 1.00; cmap[1][2] = 1.00;
    cmap[2][0] = 0.00; cmap[2][1] = 0.00; cmap[2][2] = 1.00;
    cmap[3][0] = 0.00; cmap[3][1] = 1.00; cmap[3][2] = 0.00;
    cmap[4][0] = 0.00; cmap[4][1] = 1.00; cmap[4][2] = 0.75;
    cmap[5][0] = 0.50; cmap[5][1] = 0.50; cmap[5][2] = 0.63;
    cmap[6][0] = 1.00; cmap[6][1] = 0.00; cmap[6][2] = 0.00;
    cmap[7][0] = 0.75; cmap[7][1] = 0.38; cmap[7][2] = 0.25;
    cmap[8][0] = 0.75; cmap[8][1] = 0.00; cmap[8][2] = 0.75;
    cmap[9][0] = 1.00; cmap[9][1] = 0.38; cmap[9][2] = 0.38;
    cmap[10][0] = 1.00; cmap[10][1] = 0.83; cmap[10][2] = 0.00;
    cmap[11][0] = 1.00; cmap[11][1] = 1.00; cmap[11][2] = 0.00;

    len[0] = NCOLORS;  len[1] = 3;
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
        NhlRLSetMDFloatArray(rlist,NhlNwkColorMap,&cmap[0][0],2,len);
        NhlCreate(&xworkid,"xy05Work",NhlncgmWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
    else {
/*
 * Create an XWorkstation object.
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlRLSetMDFloatArray(rlist,NhlNwkColorMap,&cmap[0][0],2,len);
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

/*
**      $Id: xy09c.c,v 1.2 1995-06-22 21:09:45 haley Exp $
*/
/************************************************************************
*                                                                       *
*                Copyright (C)  1995                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
************************************************************************/
/*
**  File:       xy09c.c
**
**  Author:     Mary Haley (copied from example "agex06")
**          National Center for Atmospheric Research
**          PO 3000, Boulder, Colorado
**
**  Date:       Thu Jan 26 13:54:19 MST 1995
**
**  Description:    This example is similar to the ncargex Autograph
**                  example "agex06".  It shows how to create different
**                  kinds of axes using a combination of TickMark and
**                  XyPlot resources.
*/


#include <stdio.h>
#include <math.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/ResList.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/XWorkstation.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/hlu/XyPlot.h>
#include <ncarg/hlu/CoordArrays.h>

/*
 * Define the number of points in each curve and the number of colors.
 */
#define NPTS  501
#define NCOLORS 6
#define PI100  .031415926535898

/*
 * Create data arrays for XyPlot.
 */
float ydra[NPTS],xdra[NPTS];

main()
{
    int     appid,xworkid,plotid,dataid;
    int     rlist, i, len[2];
    float   theta;
    float   cmap[NCOLORS][3];
    char plot_name[8];
    int NCGM=0, X11=1, PS=0;
/*
 * Initialize the HLU library and set up resource template
 */
    NhlInitialize();

    rlist = NhlRLCreate(NhlSETRL);
/*
 * Modify the color map.  Color indices '0' and '1' are the background
 * and foreground colors respectively.
 */
    cmap[0][0] = cmap[0][1] = cmap[0][2] = 0.;
    cmap[1][0] = cmap[1][1] = cmap[1][2] = 1.;
    cmap[2][0] = 1.0; cmap[2][1] = 0.5; cmap[2][2] = 0.0;
    cmap[3][0] = 0.0; cmap[3][1] = 1.0; cmap[3][2] = 0.5;
    cmap[4][0] = 0.5; cmap[4][1] = 0.0; cmap[4][2] = 1.0;
    cmap[5][0] = 0.6; cmap[5][1] = 0.2; cmap[5][2] = 0.2;
    len[0] = NCOLORS;  len[1] = 3;
/*
 * Create Application object.  The Application object name is used to
 * determine the name of the resource file, which is "xy08.res" in this
 * case.
 */
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNappUsrDir,"./");
    NhlRLSetString(rlist,NhlNappDefaultParent,"True");
    NhlCreate(&appid,"xy09",NhlappClass,NhlDEFAULT_APP,rlist);
    if (NCGM) {
/*
 * Create an NCGMWorkstation object.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./xy09c.ncgm");
        NhlRLSetMDFloatArray(rlist,NhlNwkColorMap,&cmap[0][0],2,len);
        NhlCreate(&xworkid,"xy09Work",NhlncgmWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
    else if (X11) {
/*
 * Create an XWorkstation object.
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlRLSetMDFloatArray(rlist,NhlNwkColorMap,&cmap[0][0],2,len);
        NhlCreate(&xworkid,"xy09Work",NhlxWorkstationClass,NhlDEFAULT_APP,
                  rlist);
    }
    else if (PS) {
/*
 * Create a PS workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPSFileName,"./xy09c.ps");
        NhlRLSetMDFloatArray(rlist,NhlNwkColorMap,&cmap[0][0],2,len);
        NhlCreate(&xworkid,"xy09Work",NhlpsWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
/*
 * Initialize data for the XyPlot object.
 */
    for( i = 0; i < NPTS; i++ ) {
        theta = PI100*(float)(i);
        xdra[i]=500.+.9*(float)(i)*cos(theta);
        ydra[i]=500.+.9*(float)(i)*sin(theta);
    }
/*
 * Define data object.
 */
    NhlRLClear(rlist);
    NhlRLSetFloatArray(rlist,NhlNcaYArray,ydra,NhlNumber(ydra));
    NhlRLSetFloatArray(rlist,NhlNcaXArray,xdra,NhlNumber(xdra));
    NhlCreate(&dataid,"xyData",NhlcoordArraysClass,NhlDEFAULT_APP,rlist);
/*
 * Create and draw four XyPlot objects.
 */
    for( i = 1; i <= 4; i++ ) {
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNxyCoordData,dataid);
        sprintf( plot_name, "xyPlot%1d", i );
        NhlCreate(&plotid,plot_name,NhlxyPlotClass,xworkid,rlist);
        NhlDraw(plotid);
    }
    NhlFrame(xworkid);
/*
 * NhlDestroy destroys the given id and all of its children
 * so destroying xworkwid will also destroys plotid.
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


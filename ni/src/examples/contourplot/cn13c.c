/*
**      $Id: cn13c.c,v 1.4 1996-01-04 16:46:34 haley Exp $
*/
/***********************************************************************
*                                                                      *
*                Copyright (C)  1995                                   *
*        University Corporation for Atmospheric Research               *
*     The use of this Software is governed by a License Agreement      *
*                                                                      *
***********************************************************************/
/*
**  File:       cn13c.c
**
**  Author:     Mary Haley
**          National Center for Atmospheric Research
**          PO 3000, Boulder, Colorado
**
**  Date:       Fri Oct 20 13:28:23 MDT 1995
**
**  Description:  This example emulates LLU example "mpex10".  It shows
**                how to do inverse map tranformations and raster contour
**                plots.
*/


#include <stdio.h>
#include <math.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/ResList.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/XWorkstation.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/hlu/ContourPlot.h>
#include <ncarg/hlu/MapPlot.h>
#include <ncarg/hlu/ScalarField.h>
#include <math.h>

#define NCLS 300
#define DTOR .017453292519943
#define NCOLORS 66
#define max(x,y)   ((x) > (y) ? (x) : (y))
#define min(x,y)   ((x) < (y) ? (x) : (y))

main()
{
    float x[NCLS*NCLS], y[NCLS*NCLS],rlat[NCLS*NCLS], rlon[NCLS*NCLS];
    float dval, oor,  icra[NCLS][NCLS], miss_val = 1.e12;
    int appid, workid, dataid, cnid, mpid;
    int srlist, i, j, l, status, count[2];
    NhlErrorTypes ierr;
/*
 * Declare variables for defining color map.
 */
    int     length[2];
    float   cmap[NCOLORS][3];
/*
 * Default is to display to an X11 window.
 */
    int NCGM=0, X11=1, PS=0;
/*
 * Initialize the HLU library and set up resource template.
 */
    NhlInitialize();
    srlist = NhlRLCreate(NhlSETRL);
/*
 * Create Application object.
 */
    NhlRLClear(srlist);
    NhlRLSetString(srlist,NhlNappDefaultParent,"True");
    NhlRLSetString(srlist,NhlNappUsrDir,"./");
    NhlCreate(&appid,"cn13",NhlappClass,NhlDEFAULT_APP,srlist);
/*
 * Modify the color map. Colors for contour fill areas varying from
 * blue to red.
 */
    cmap[ 0][0] = 0.00; cmap[ 0][1] = 0.00; cmap[ 0][2] = 0.00;
    cmap[ 1][0] = 1.00; cmap[ 1][1] = 1.00; cmap[ 1][2] = 1.00;
    for( i = 1; i <= NCOLORS-2; i++ ) {
        cmap[i+1][0] = (float)(i-1)/(float)(NCOLORS-3);
        cmap[i+1][1] = 0.;
        cmap[i+1][2] = (float)((NCOLORS-2)-i)/(float)(NCOLORS-3);
    }
                                   
    length[0] = NCOLORS;  length[1] = 3;

    if (NCGM) {
/*
 * Create a meta file object.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkMetaName,"./cn13c.ncgm");
        NhlRLSetMDFloatArray(srlist,NhlNwkColorMap,&cmap[0][0],2,length);
        NhlCreate(&workid,"cn13Work",NhlncgmWorkstationClass,
                  NhlDEFAULT_APP,srlist);
    }
    else if (X11) {
/*
 * Create an XWorkstation object.
 */
        NhlRLClear(srlist);
        NhlRLSetInteger(srlist,NhlNwkPause,True);
        NhlRLSetMDFloatArray(srlist,NhlNwkColorMap,&cmap[0][0],2,length);
        NhlCreate(&workid,"cn13Work",NhlxWorkstationClass,
              NhlDEFAULT_APP,srlist);
    }
    else if (PS) {
/*
 * Create a PS workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkPSFileName,"./cn13c.ps");
        NhlRLSetMDFloatArray(srlist,NhlNwkColorMap,&cmap[0][0],2,length);
        NhlCreate(&workid,"cn13Work",NhlpsWorkstationClass,
                  NhlDEFAULT_APP,srlist);
    }
/*
 * Create a MapPlot object.
 */
    NhlRLClear(srlist);
    NhlRLSetString(srlist,NhlNmpProjection,"Orthographic");
    NhlRLSetFloat(srlist,NhlNmpCenterLatF,40.);
    NhlRLSetFloat(srlist,NhlNmpCenterLonF,-105.);
    NhlRLSetFloat(srlist,NhlNmpCenterRotF,0.);
    NhlCreate(&mpid,"MapPlot",NhlmapPlotClass,workid,srlist);
/*
 * Calculate nice range of x,y values, and then get their
 * corresponding lon,lat values.
 */
    l = 0;
    for( i = 0; i < NCLS; i++ ) {
        for( j = 0; j < NCLS; j++ ) {
            x[l] = .05+.90*((float)i+.5)/(float)NCLS;
            y[l] = .05+.90*((float)j+.5)/(float)NCLS;
            l++;
        }
    }
    ierr = NhlNDCToData(mpid,x,y,NCLS*NCLS,rlon,rlat,NULL,NULL,&status,&oor);
/*
 * Now create a cell array.
 */
    l = 0;
    for( i = 0; i < NCLS; i++ ) {
        for( j = 0; j < NCLS; j++ ) {
            if( rlat[l] == oor ) {
                icra[j][i] = miss_val;
            }
            else {
                dval=.25*(1.+cos(DTOR*10.*rlat[l]))+
                  .25*(1.+sin(DTOR*10.*rlon[l]))*cos(DTOR*rlat[l]);
                icra[j][i] = 2.+dval*(float)(NCOLORS-2);
                if( icra[j][i] != miss_val) icra[j][i] = min((float)(NCOLORS-1),icra[j][i] );
            }
            l++;
        }
    }
/*
 * Create a scalar field object.
 */
    count[0] = count[1] = NCLS;
    NhlRLClear(srlist);
    NhlRLSetMDFloatArray(srlist,NhlNsfDataArray,&icra[0][0],2,(int *)count);
    NhlRLSetFloat(srlist,NhlNsfMissingValueV,miss_val);
    NhlCreate(&dataid,"DataItem",NhlscalarFieldClass,appid,srlist);
/*
 * Create contour object.
 */
    NhlRLClear(srlist);
    NhlRLSetInteger(srlist,NhlNcnScalarFieldData,dataid);
    NhlCreate(&cnid,"ContourPlot",NhlcontourPlotClass,workid,srlist);
/*
 * Draw MapPlot on ContourPlot.
 */
    NhlDraw(cnid);
    NhlDraw(mpid);
    NhlFrame(workid);
/*
 * NhlDestroy destroys the given id and all of its children.
 */
    NhlRLDestroy(srlist);
    NhlDestroy(appid);
/*
 * Restores state.
 */
    NhlClose();
    exit(0);
}

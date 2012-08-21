/*
**      $Id: cn13c.c,v 1.8 2010-03-15 22:49:23 haley Exp $
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
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/hlu/PDFWorkstation.h>
#include <ncarg/hlu/CairoWorkstation.h>
#include <ncarg/hlu/ContourPlot.h>
#include <ncarg/hlu/MapPlot.h>
#include <ncarg/hlu/ScalarField.h>
#include <math.h>

#define NCLS 300
#define DTOR .017453292519943
#define NCOLORS 66
#define max(x,y)   ((x) > (y) ? (x) : (y))
#define min(x,y)   ((x) < (y) ? (x) : (y))

float x[NCLS*NCLS], y[NCLS*NCLS],rlat[NCLS*NCLS], rlon[NCLS*NCLS];

int main()
{
    float dval, oor,  icra[NCLS][NCLS], miss_val = 1.e12;
    int appid, workid, dataid, cnid, mpid;
    int srlist, i, j, l, status;
    ng_size_t count[2];
    NhlErrorTypes ierr;
/*
 * Declare variables for defining color map.
 */
    ng_size_t   length[2];
    float   cmap[NCOLORS][3];
/*
 * Default is to display to an X11 window.
 */
    char const *wks_type = "x11";
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

    if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
/*
 * Create a meta file object.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkMetaName,"./cn13c.ncgm");
        NhlRLSetMDFloatArray(srlist,NhlNwkColorMap,&cmap[0][0],2,length);
        NhlCreate(&workid,"cn13Work",NhlncgmWorkstationClass,
                  NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 * Create an X11 workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetInteger(srlist,NhlNwkPause,True);
        NhlRLSetMDFloatArray(srlist,NhlNwkColorMap,&cmap[0][0],2,length);
        NhlCreate(&workid,"cn13Work",NhlcairoWindowWorkstationClass,
              NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 * Create an older-style PostScript workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkPSFileName,"./cn13c.ps");
        NhlRLSetMDFloatArray(srlist,NhlNwkColorMap,&cmap[0][0],2,length);
        NhlCreate(&workid,"cn13Work",NhlpsWorkstationClass,
                  NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 * Create an older-style PDF workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkPDFFileName,"./cn13c.pdf");
        NhlRLSetMDFloatArray(srlist,NhlNwkColorMap,&cmap[0][0],2,length);
        NhlCreate(&workid,"cn13Work",NhlpdfWorkstationClass,
                  NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
             !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 * Create a cairo PS/PDF workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkFileName,"./cn13c");
        NhlRLSetString(srlist,NhlNwkFormat,(char*)wks_type);
        NhlRLSetMDFloatArray(srlist,NhlNwkColorMap,&cmap[0][0],2,length);
        NhlCreate(&workid,"cn13Work",NhlcairoDocumentWorkstationClass,
                  NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 * Create a cairo PNG workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkFileName,"./cn13c");
        NhlRLSetString(srlist,NhlNwkFormat,(char*)wks_type);
        NhlRLSetMDFloatArray(srlist,NhlNwkColorMap,&cmap[0][0],2,length);
        NhlCreate(&workid,"cn13Work",NhlcairoImageWorkstationClass,
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
    NhlRLSetMDFloatArray(srlist,NhlNsfDataArray,&icra[0][0],2,count);
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

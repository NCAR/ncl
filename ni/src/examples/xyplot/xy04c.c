/*
**      $Id: xy04c.c,v 1.16 2010-03-15 22:49:25 haley Exp $
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
**                 and the resource file is used to set up attributes
**                 of the data being plotted, like the line color, the 
**                 dash patterns, and line label colors.
**
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
#include <ncarg/hlu/XyPlot.h>
#include <ncarg/hlu/CoordArrays.h>

/*
 * Define the number of curves and points in each curve.
 */
#define NPTS  500
#define NCURVE  4
#define NCOLORS 6
#define PI100 0.031415926535898

float ydra[NCURVE][NPTS];
char   *cmap[NCOLORS] = {"black","white","red","green","blue","yellow"};

int main()
{
    int     appid,xworkid,plotid,dataid;
    int     rlist, i, j; 
    ng_size_t len[2];
    float   theta;
    char const *wks_type = "x11";
/*
 * Initialize data for the XyPlot object.
 */
    for( j = 0; j < NCURVE; j++ ) {
        for( i = 0; i < NPTS; i++ ) {
            theta = PI100*(float)(i);
            ydra[j][i] = (j*200.)+(i*.9)*sin(theta);
        }
    }
/*
 * Initialize the HLU library and set up resource template.
 */
    NhlInitialize();
    rlist = NhlRLCreate(NhlSETRL);
/*
 * Create Application object.  The Application object name is used to
 * determine the name of the resource file, which is "xy04.res" in
 * this case.
 */
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNappDefaultParent,"True");
    NhlRLSetString(rlist,NhlNappUsrDir,"./");
    NhlCreate(&appid,"xy04",NhlappClass,NhlDEFAULT_APP,rlist);

    if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
/*
 * Create a meta file object.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./xy04c.ncgm");
        NhlRLSetStringArray(rlist,NhlNwkColorMap,cmap,NCOLORS);
        NhlCreate(&xworkid,"xy04Work",NhlncgmWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 * Create an X11 workstation
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlRLSetStringArray(rlist,NhlNwkColorMap,cmap,NCOLORS);
        NhlCreate(&xworkid,"xy04Work",NhlcairoWindowWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 * Create an older-style PostScript workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPSFileName,"./xy04c.ps");
        NhlRLSetStringArray(rlist,NhlNwkColorMap,cmap,NCOLORS);
        NhlCreate(&xworkid,"xy04Work",NhlpsWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 * Create an older-style PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPDFFileName,"./xy04c.pdf");
        NhlRLSetStringArray(rlist,NhlNwkColorMap,cmap,NCOLORS);
        NhlCreate(&xworkid,"xy04Work",NhlpdfWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
             !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 * Create a cairo PS/PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"./xy04c");
        NhlRLSetString(rlist,NhlNwkFormat, (char*)wks_type);
        NhlRLSetStringArray(rlist,NhlNwkColorMap,cmap,NCOLORS);
        NhlCreate(&xworkid,"xy04Work",NhlcairoDocumentWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 * Create a cairo PNG workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"./xy04c");
        NhlRLSetString(rlist,NhlNwkFormat, (char*)wks_type);
        NhlRLSetStringArray(rlist,NhlNwkColorMap,cmap,NCOLORS);
        NhlCreate(&xworkid,"xy04Work",NhlcairoImageWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
/*
 * Define the "CoordArrays" object.  Since only the Y values are
 * specified here, each Y value will be paired with its integer
 * array index.  The id for this object will then later be used as
 * the value for the Data Dep resource, "dsDataItem".
 */
    len[0] = NCURVE; len[1] = NPTS;
    NhlRLClear(rlist);
    NhlRLSetMDFloatArray(rlist,NhlNcaYArray,&ydra[0][0],2,len);
    NhlCreate(&dataid,"xyData",NhlcoordArraysClass,NhlDEFAULT_APP,
              rlist);
/*
 * This new DataItem object is now the resource value for xyCoordData.
 * Tweak some XyPlot resources as well (in the resource file).
 * Also tweak some XyDataSpec resources in the resource file.
 * When Data is added to an XyPlot object, an XyDataSpec object is
 * created internally to the XyPlot.  It has the same name as the Data
 * object that is added, and so you can set XyDataSpec (XyPlot Data
 * Specific) resources for each piece of data added to the xyCoordData
 * resource.
 *
 * Create the XyPlot object which is created as a child of the
 * workstation object.  The resources that are being
 * changed are done in the "xy04.res" file.
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

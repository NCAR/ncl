/*
 *      $Id: xy15c.c,v 1.6 2010-03-15 22:49:25 haley Exp $
 */
/************************************************************************
 *                                                                      *
 *                Copyright (C)  1995                                   *
 *        University Corporation for Atmospheric Research               *
 *                All Rights Reserved                                   *
 *                                                                      *
 ***********************************************************************/
/*
 *  File:       xy15c.c
 *
 *  Author:     David Brown
 *              National Center for Atmospheric Research
 *              PO 3000, Boulder, Colorado
 *
 *  Date:       Fri Jun 30 14:11:32 MDT 1995
 *
 * Description:  
 *              This example illustrates the creation of a set of 4
 *              of 'stacked' XyPlots. Each plot has the same X axis.
 *              By making the top 3 plots into annotations of the 
 *              bottom plot, all four plots can be manipulated as
 *              a unit. To demonstrate this concept the second frame sets
 *              the viewport of the base plot. Because all the annotations
 *              have their "amResizeNotify" resource set to true (in the
 *              resource file), all the annotation plots resize themselves
 *              proportionally to the change in the size of the base plot.
 *              Each plot draws a variation of sinusoidal curve.
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
 * Define the number of points in each curve.
 */
#define NPTS  500
#define PI100 0.031415926535898

float ydra1[NPTS];
float ydra2[NPTS];
float ydra3[NPTS];
float ydra4[NPTS];

int main()
{
    int     appid,xworkid;
    int     dataid1,dataid2,dataid3,dataid4;
    int     xy1, xy2, xy3, xy4;
    int     am2, am3, am4;
    int     rlist, i;
    float   theta;
    char const *wks_type = "x11";

/*
 * Initialize data for the XyPlot object.
 */
    for( i = 0; i < NPTS; i++ ) {
        theta = PI100*(float)(i);
        ydra1[i] = sin(theta);
        ydra2[i] = sin(theta * theta);
        ydra3[i] = sin(exp(theta));
        ydra4[i] = sin(3*sqrt(fabs(theta)));
    }
/*
 * Initialize the HLU library and set up resource template.
 */
    NhlInitialize();
    rlist = NhlRLCreate(NhlSETRL);
/*
 * Create Application object.  The Application object name is used to
 * determine the name of the resource file, which is "xy15.res" in
 * this case.
 */
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNappDefaultParent,"True");
    NhlRLSetString(rlist,NhlNappUsrDir,"./");
    NhlCreate(&appid,"xy15",NhlappClass,NhlDEFAULT_APP,rlist);

    if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
/*
 * Create a meta file object.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./xy15c.ncgm");
        NhlCreate(&xworkid,"xy15Work",NhlncgmWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 * Create an X11 workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlCreate(&xworkid,"xy15Work",NhlcairoWindowWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 * Create an older-style PostScript workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPSFileName,"./xy15c.ps");
        NhlCreate(&xworkid,"xy15Work",NhlpsWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 * Create an older-style PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPDFFileName,"./xy15c.pdf");
        NhlCreate(&xworkid,"xy15Work",NhlpdfWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
             !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 * Create a cairo PS/PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"./xy15c");
        NhlRLSetString(rlist,NhlNwkFormat, (char*)wks_type);
        NhlCreate(&xworkid,"xy15Work",NhlcairoDocumentWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 * Create a cairo PNG workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"./xy15c");
        NhlRLSetString(rlist,NhlNwkFormat, (char*)wks_type);
        NhlCreate(&xworkid,"xy15Work",NhlcairoImageWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
/*
 * Define 4 separate CoordArrays objects - one for each XYPlot. 
 */
    NhlRLClear(rlist);
    NhlRLSetFloatArray(rlist,NhlNcaYArray,&ydra1[0],NPTS);
    NhlCreate(&dataid1,"xyData1",NhlcoordArraysClass,NhlDEFAULT_APP,
              rlist);
    NhlRLClear(rlist);
    NhlRLSetFloatArray(rlist,NhlNcaYArray,&ydra2[0],NPTS);
    NhlCreate(&dataid2,"xyData2",NhlcoordArraysClass,NhlDEFAULT_APP,
              rlist);
    NhlRLClear(rlist);
    NhlRLSetFloatArray(rlist,NhlNcaYArray,&ydra3[0],NPTS);
    NhlCreate(&dataid3,"xyData3",NhlcoordArraysClass,NhlDEFAULT_APP,
              rlist);
    NhlRLClear(rlist);
    NhlRLSetFloatArray(rlist,NhlNcaYArray,&ydra4[0],NPTS);
    NhlCreate(&dataid4,"xyData4",NhlcoordArraysClass,NhlDEFAULT_APP,
              rlist);
/*
 * Create 4 XyPlot objects. 
 */
    NhlRLClear(rlist);
    NhlRLSetInteger(rlist,NhlNxyCoordData,dataid1);
    NhlRLSetFloat(rlist,NhlNvpYF,0.3);
    NhlCreate(&xy1,"xy1",NhlxyPlotClass,xworkid,rlist);


    NhlRLClear(rlist);
    NhlRLSetInteger(rlist,NhlNxyCoordData,dataid2);
    NhlCreate(&xy2,"xy2",NhlxyPlotClass,xworkid,rlist);


    NhlRLClear(rlist);
    NhlRLSetInteger(rlist,NhlNxyCoordData,dataid3);
    NhlCreate(&xy3,"xy3",NhlxyPlotClass,xworkid,rlist);


    NhlRLClear(rlist);
    NhlRLSetInteger(rlist,NhlNxyCoordData,dataid4);
    NhlCreate(&xy4,"xy4",NhlxyPlotClass,xworkid,rlist);

    am2 = NhlAddAnnotation(xy1,xy2);
    am3 = NhlAddAnnotation(xy1,xy3);
    am4 = NhlAddAnnotation(xy1,xy4);
/*
 * Draw the plot.
 */
    NhlDraw(xy1);
    NhlFrame(xworkid);
/*
 * Now set the viewport of the base plot only; redrawing the base plot
 * causes all the other plots to be redrawn as well. Notice that they
 * are all resized and repositioned to match the new size and position
 * of the base plot. The whole assemblage functions as a single 
 * composite plot object.
 */
    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNvpWidthF,0.65);
    NhlRLSetFloat(rlist,NhlNvpXF,0.25);
    NhlRLSetFloat(rlist,NhlNvpYF,0.4);
    NhlRLSetFloat(rlist,NhlNvpHeightF,0.16);
    NhlSetValues(xy1,rlist);
    
    NhlDraw(xy1);
    NhlFrame(xworkid);
/*
 * Destroy the resource list.
 * The workstation, all the plots, and the data objects are all
 * descended from the App object, so it is only necessary to 
 * destroy the App object in order to destroy all the objects created
 * in this example.
 */
    NhlRLDestroy(rlist);
    NhlDestroy(appid);
/*
 * Restores state.
 */
    NhlClose();

    exit(0);
}

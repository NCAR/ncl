/*
 *      $Id: xy16c.c,v 1.6 2010-03-15 22:49:25 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  1996                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
************************************************************************/
/*
 *   File:       xy16c.c
 *
 *   Author:     Bob Lackman
 *           National Center for Atmospheric Research
 *           PO 3000, Boulder, Colorado
 *
 *   Date:       24 Jan 1996
 *
 *   Description:    Reads an ASCII file with 4 variables:
 *                   lon, u, v, and t.  u, v, and t are plotted
 *                   with 3 separate y axes.
 *
 *                   This example shows how the Data Spec object
 *                   can be retrieved so you can change certain
 *                   aspects of the plot, like the line color.
 *                   line color.
 *
 *                   In this example, each of the three variables
 *                   are assigned their own Y axis and scales by
 *                   creating three distinct XyPlot objects. The
 *                   first object is given a full grid and has
 *                   its Y axis scale on the left. The second 
 *                   object has no grid and the Y axis scale on
 *                   the right of the first grid. The third
 *                   object's viewport is made wider than the
 *                   first two and its X axis is scaled so that
 *                   the data end at the right grid boundary of
 *                   the first object.  Only the Y axis on the
 *                   right is drawn for the third object. The
 *                   curves and the Y axis scales are color
 *                   coordinated so you can tell which curve
 *                   goes with which scale.
 *                   
 *                   There is no resource file for this example.
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
#include <ncarg/hlu/Title.h>
#include <ncarg/hlu/TextItem.h>
#include <ncarg/hlu/TickMark.h>

#define NCURVE 3
#define NPTS   129

/*
 * Define variables for tick mark labels
 */
char *yllabs[5] = {"-90.","-80.","-70.","-60.","-50."};
char *yrlabs1[6] = {"0.","10.","20.","30.","40.","50."};
char *yrlabs2[5] = {"-20.","-10.","0.","10.","20."};
float ylvals[5] =  {-90.,-80.,-70.,-60.,-50.};
float yrvals1[6] = {0.,10.,20.,30.,40.,50.};
float yrvals2[5] = {-20.,-10.,0.,10.,20.};

int main()
{
    FILE *fp;
    int i;
/*
 * Define variables for data
 */
    float lon[NPTS],u[NPTS],v[NPTS],t[NPTS];
/*
 * Define HLU variables for creating objects.
 */
    int appid,field1,field2,field3,xy1,xy2,xy3,xworkid ;
    int grlist, srlist, spec1, spec2, spec3;
/*
 * Define variable for data file
 */
    char filename[10];
/*
 * Indicate whether we want output to go to NCGM, X11 window or
 * PS file.
 */
    char const *wks_type = "x11";

    strcpy(filename,"xy.asc");
/*
 * Create Application object.
 */
    NhlInitialize();
    srlist = NhlRLCreate(NhlSETRL);
    grlist = NhlRLCreate(NhlGETRL);
    NhlRLClear(srlist);
    NhlRLSetString(srlist,NhlNappDefaultParent,"True");
    NhlRLSetString(srlist,NhlNappUsrDir,"./");
    NhlCreate(&appid,"xy16",NhlappClass,NhlDEFAULT_APP,srlist);
/*
 * Read ASCII file "xy.asc".
 */
    fp = fopen(filename,"r");
/*
 * Read data.
 */
    for( i = 0; i < NPTS; i++ ) {
      fscanf(fp,"%g",&lon[i]);
      fscanf(fp,"%g",&u[i]);
      fscanf(fp,"%g",&v[i]);
      fscanf(fp,"%g",&t[i]);
    }
/*
 * Create an NCGM workstation.
 */
    if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
      NhlRLClear(srlist);
      NhlRLSetString(srlist,NhlNwkMetaName,"./xy16c.ncgm");
      NhlCreate(&xworkid,"xy16Work",NhlncgmWorkstationClass,NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 * Create an X11 workstation.
 */
      NhlRLClear(srlist);
      NhlRLSetString(srlist,NhlNwkPause,"True");
      NhlCreate(&xworkid,"xy16Work",NhlcairoWindowWorkstationClass,NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 * Create an older-style PostScript workstation.
 */
      NhlRLClear(srlist);
      NhlRLSetString(srlist,NhlNwkPSFileName,"./xy16c.ps");
      NhlCreate(&xworkid,"xy16Work",NhlpsWorkstationClass,NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 * Create an older-style PDF workstation.
 */
      NhlRLClear(srlist);
      NhlRLSetString(srlist,NhlNwkPDFFileName,"./xy16c.pdf");
      NhlCreate(&xworkid,"xy16Work",NhlpdfWorkstationClass,NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
             !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 * Create a cairo PS/PDF workstation.
 */
      NhlRLClear(srlist);
      NhlRLSetString(srlist,NhlNwkFileName,"./xy16c");
      NhlRLSetString(srlist,NhlNwkFormat, (char*)wks_type);
      NhlCreate(&xworkid,"xy16Work",NhlcairoDocumentWorkstationClass,NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 * Create a cairo PNG workstation.
 */
      NhlRLClear(srlist);
      NhlRLSetString(srlist,NhlNwkFileName,"./xy16c");
      NhlRLSetString(srlist,NhlNwkFormat, (char*)wks_type);
      NhlCreate(&xworkid,"xy16Work",NhlcairoImageWorkstationClass,NhlDEFAULT_APP,srlist);
    }
/*
 *  xy.asc has 4 vars of length 129 longitudes, lon, u, v, t
 *
 *     The data is taken at 43N latitude.  Longitude is an index
 *     1-129 standing for 0 deg - 360 deg in steps of 360/128?
 *     u and v are in m/s, and t is in deg K.
 *
 *     Convert from degrees K to degrees F and configure its extents
 *     missing values.
 */
    for( i = 0; i < NPTS; i++ ) {
      t[i] = (t[i] - 273.15) * 9 / 5 + 32.0;
      lon[i] = (lon[i] - 1.) * 360./128.;
    }
/*
 * Create the first Coord Arrays data object.
 */
      NhlRLClear(srlist);
      NhlRLSetFloatArray(srlist,NhlNcaYArray,t,NPTS);
      NhlRLSetFloatArray(srlist,NhlNcaXArray,lon,NPTS);
      NhlCreate(&field1,"field1",NhlcoordArraysClass,NhlDEFAULT_APP,srlist);
/*
 * Create the second Coord Arrays data object.
 */
      NhlRLClear(srlist);
      NhlRLSetFloatArray(srlist,NhlNcaYArray,u,NPTS);
      NhlRLSetFloatArray(srlist,NhlNcaXArray,lon,NPTS);
      NhlCreate(&field2,"field2",NhlcoordArraysClass,NhlDEFAULT_APP,srlist);
/*
 * Create the third Coord Arrays data object.
 */
      NhlRLClear(srlist);
      NhlRLSetFloatArray(srlist,NhlNcaYArray,v,NPTS);
      NhlRLSetFloatArray(srlist,NhlNcaXArray,lon,NPTS);
      NhlCreate(&field3,"field3",NhlcoordArraysClass,NhlDEFAULT_APP,srlist);
/*
 * Create XyPlot object for curve 1 and assign data to it.
 */
      NhlRLClear(srlist);
      NhlRLSetFloat(srlist,NhlNvpXF,.20);
      NhlRLSetFloat(srlist,NhlNvpYF,.80);
      NhlRLSetFloat(srlist,NhlNvpWidthF,.5);
      NhlRLSetFloat(srlist,NhlNvpHeightF,.6);
      NhlRLSetInteger(srlist,NhlNxyCoordData,field1);
      NhlRLSetString(srlist,NhlNtrYReverse,"False");
      NhlRLSetFloat(srlist,NhlNtrYMaxF,-50.);
      NhlRLSetFloat(srlist,NhlNtrYMinF,-90.);
      NhlRLSetFloat(srlist,NhlNtrXMaxF,360.);
      NhlRLSetFloat(srlist,NhlNtrXMinF,0.);
      NhlRLSetString(srlist,NhlNtmYROn,"False");
      NhlRLSetString(srlist,NhlNtmYUseLeft,"False");
      NhlRLSetString(srlist,NhlNtmYLLabelsOn,"True");
      NhlRLSetFloat(srlist,NhlNtmYLMajorLengthF,.01);
      NhlRLSetFloat(srlist,NhlNtmYLMajorOutwardLengthF,.01);
      NhlRLSetString(srlist,NhlNtmYLMode,"Explicit");
      NhlRLSetFloatArray(srlist,NhlNtmYLValues,ylvals,5);
      NhlRLSetStringArray(srlist,NhlNtmYLLabels,yllabs,5);
      NhlRLSetString(srlist,NhlNtmYLLabelsOn,"True");
      NhlRLSetString(srlist,NhlNtmYLLabelFontColor,"red");
      NhlRLSetString(srlist,NhlNtiXAxisString,"Longitude (Degs)");
      NhlRLSetString(srlist,NhlNtiYAxisString,"Temperature in Deg C");
      NhlRLSetFloat(srlist,NhlNtiXAxisFontHeightF,0.02);
      NhlRLSetFloat(srlist,NhlNtiYAxisFontHeightF,0.02);
      NhlRLSetString(srlist,NhlNtiXAxisFont,"helvetica-bold");
      NhlRLSetString(srlist,NhlNtiYAxisFont,"helvetica-bold");
      NhlRLSetString(srlist,NhlNtiYAxisFontColor,"red");
      NhlRLSetString(srlist,NhlNtmYRMinorOn,"False");
      NhlRLSetString(srlist,NhlNtmYLMinorOn,"False");
      NhlCreate(&xy1,"xy1",NhlxyPlotClass,xworkid,srlist);
/*
 * Create XyPlot object for curve 2 and assign data to it.
 */
      NhlRLClear(srlist);
      NhlRLSetFloat(srlist,NhlNvpXF,.20);
      NhlRLSetFloat(srlist,NhlNvpYF,.80);
      NhlRLSetFloat(srlist,NhlNvpWidthF,.5);
      NhlRLSetFloat(srlist,NhlNvpHeightF,.6);
      NhlRLSetInteger(srlist,NhlNxyCoordData,field2);
      NhlRLSetString(srlist,NhlNtrYReverse,"False");
      NhlRLSetFloat(srlist,NhlNtrYMaxF,50.);
      NhlRLSetFloat(srlist,NhlNtrYMinF,0.);
      NhlRLSetFloat(srlist,NhlNtrXMaxF,360.);
      NhlRLSetFloat(srlist,NhlNtrXMinF,0.);
      NhlRLSetString(srlist,NhlNtmYROn,"True");
      NhlRLSetString(srlist,NhlNtmYLOn,"False");
      NhlRLSetString(srlist,NhlNtmYUseLeft,"False");
      NhlRLSetString(srlist,NhlNtmYRLabelsOn,"True");
      NhlRLSetString(srlist,NhlNtmYLLabelsOn,"False");
      NhlRLSetFloat(srlist,NhlNtmYRMajorLengthF,.01);
      NhlRLSetFloat(srlist,NhlNtmYRMajorOutwardLengthF,.01);
      NhlRLSetString(srlist,NhlNtmYRMode,"Explicit");
      NhlRLSetFloatArray(srlist,NhlNtmYRValues,yrvals1,6);
      NhlRLSetStringArray(srlist,NhlNtmYRLabels,yrlabs1,6);
      NhlRLSetString(srlist,NhlNtmYRLabelFontColor,"cyan");
      NhlRLSetString(srlist,NhlNtiYAxisString,"U component of wind (m/s)");
      NhlRLSetString(srlist,NhlNtiYAxisSide,"Right");
      NhlRLSetFloat(srlist,NhlNtiXAxisFontHeightF,0.02);
      NhlRLSetFloat(srlist,NhlNtiYAxisFontHeightF,0.02);
      NhlRLSetString(srlist,NhlNtiMainFont,"helvetica-bold");
      NhlRLSetString(srlist,NhlNtiXAxisFont,"helvetica-bold");
      NhlRLSetString(srlist,NhlNtiYAxisFont,"helvetica-bold");
      NhlRLSetString(srlist,NhlNtiYAxisFontColor,"cyan");
      NhlRLSetString(srlist,NhlNtmYRMinorOn,"False");
      NhlRLSetString(srlist,NhlNtmYLMinorOn,"False");
      NhlCreate(&xy2,"xy2",NhlxyPlotClass,xworkid,srlist);
/*
 * Create XyPlot object for curve 3 and assign data to it.
 *
 * Increase the veiwport so the right scale will be about .15 NDC
 * right of the other grids.  Plot only the right vertical axis.
 * .5NDC = 360 deg lon, thus .65NDC = 360+108 deg lon.
 */
      NhlRLClear(srlist);
      NhlRLSetFloat(srlist,NhlNvpXF,.20);
      NhlRLSetFloat(srlist,NhlNvpYF,.80);
      NhlRLSetFloat(srlist,NhlNvpWidthF,.65);
      NhlRLSetFloat(srlist,NhlNvpHeightF,.6);
      NhlRLSetInteger(srlist,NhlNxyCoordData,field3);
      NhlRLSetString(srlist,NhlNtrYReverse,"False");
      NhlRLSetFloat(srlist,NhlNtrYMaxF,20.);
      NhlRLSetFloat(srlist,NhlNtrYMinF,-20.);
      NhlRLSetFloat(srlist,NhlNtrXMaxF,468.);
      NhlRLSetFloat(srlist,NhlNtrXMinF,0.);
      NhlRLSetString(srlist,NhlNtmYROn,"True");
      NhlRLSetString(srlist,NhlNtmYLOn,"False");
      NhlRLSetString(srlist,NhlNtmYUseLeft,"False");
      NhlRLSetString(srlist,NhlNtmYRLabelsOn,"True");
      NhlRLSetFloat(srlist,NhlNtmYRMajorLengthF,.01);
      NhlRLSetFloat(srlist,NhlNtmYRMajorOutwardLengthF,.01);
      NhlRLSetString(srlist,NhlNtmXBOn,"False");
      NhlRLSetString(srlist,NhlNtmXTOn,"False");
      NhlRLSetString(srlist,NhlNtmYLOn,"False");
      NhlRLSetString(srlist,NhlNtmYROn,"True");
      NhlRLSetString(srlist,NhlNtmYRMode,"Explicit");
      NhlRLSetFloatArray(srlist,NhlNtmYRValues,yrvals2,5);
      NhlRLSetStringArray(srlist,NhlNtmYRLabels,yrlabs2,5);
      NhlRLSetString(srlist,NhlNtmYRLabelFontColor,"green");
      NhlRLSetString(srlist,NhlNtiYAxisString,"V component of wind (m/s)");
      NhlRLSetString(srlist,NhlNtiYAxisSide,"Right");
      NhlRLSetFloat(srlist,NhlNtiXAxisFontHeightF,0.02);
      NhlRLSetFloat(srlist,NhlNtiYAxisFontHeightF,0.02);
      NhlRLSetString(srlist,NhlNtiMainString,"Three Variables with Individual Scales");
      NhlRLSetFloat(srlist,NhlNtiMainFontHeightF,0.030);
      NhlRLSetString(srlist,NhlNtiMainFont,"helvetica-bold");
      NhlRLSetString(srlist,NhlNtiYAxisFont,"helvetica-bold");
      NhlRLSetString(srlist,NhlNtiYAxisFontColor,"green");
      NhlRLSetString(srlist,NhlNtmYRMinorOn,"False");
      NhlRLSetString(srlist,NhlNtmYLMinorOn,"False");
      NhlRLSetString(srlist,NhlNtmXBMinorOn,"False");
      NhlRLSetString(srlist,NhlNtmXTBorderOn,"False");
      NhlRLSetString(srlist,NhlNtmXBBorderOn,"False");
      NhlRLSetString(srlist,NhlNtmYLBorderOn,"False");
      NhlCreate(&xy3,"xy3",NhlxyPlotClass,xworkid,srlist);
/*
 * Get the Data Spec Id of the first plot so we can then change
 * the line color.
 */
      NhlRLClear(grlist);
      NhlRLGetInteger(grlist,NhlNxyCoordDataSpec,&spec1);
      NhlGetValues(xy1,grlist);
      
      NhlRLClear(srlist);
      NhlRLSetString(srlist,NhlNxyMonoLineColor,"true");
      NhlRLSetString(srlist,NhlNxyLineColor,"red");
      NhlSetValues(spec1,srlist);
/*
 * Get the Data Spec id of the second plot so we can then change
 * the line color.
 */
      NhlRLClear(grlist);
      NhlRLGetInteger(grlist,NhlNxyCoordDataSpec,&spec2);
      NhlGetValues(xy2,grlist);

      NhlRLClear(srlist);
      NhlRLSetString(srlist,NhlNxyMonoLineColor,"true");
      NhlRLSetString(srlist,NhlNxyLineColor,"cyan");
      NhlSetValues(spec2,srlist);
/*
 * Get the Data Spec id of the third plot so we can then change
 * the line color.
 */
      NhlRLClear(grlist);
      NhlRLGetInteger(grlist,NhlNxyCoordDataSpec,&spec3);
      NhlGetValues(xy3,grlist);

      NhlRLClear(srlist);
      NhlRLSetString(srlist,NhlNxyMonoLineColor,"true");
      NhlRLSetString(srlist,NhlNxyLineColor,"green");
      NhlSetValues(spec3,srlist);
/*
 * Draw all three plots.
 */
      NhlDraw(xy1);
      NhlDraw(xy2);
      NhlDraw(xy3);
      NhlFrame(xworkid);
      NhlDestroy(appid);
      NhlClose();
      exit(0);
}

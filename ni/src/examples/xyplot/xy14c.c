/*
**      $Id: xy14c.c,v 1.7 2010-03-15 22:49:25 haley Exp $
*/
/***********************************************************************
*                                                                      *
*                Copyright (C)  1995                                   *
*        University Corporation for Atmospheric Research               *
*     The use of this Software is governed by a License Agreement      *
*                                                                      *
***********************************************************************/
/*
**  File:       xy14c.c
**
**  Author:     Fred Clare (converted to C by Mary Haley)
**          National Center for Atmospheric Research
**          PO 3000, Boulder, Colorado
**
**  Date:       Tue Sep 26 15:47:51 MDT 1995
**
** Description:  This example demonstrates the data manipulation
**               capabilities of the HLUS in conjunction with XyPlot.
**
**               This example is really meant to show the data manipulation
**               capabilities of NCL, since it does some array arithmetic
**               that you can't do in C or Fortran 77.        
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
 * Define temperature/rainfall data for a 25-hour period in
 * an NCL variable.  In a real-world NCL script these values
 * would probably be read in, either directly or as part of
 * a data file like a netCDF or HDF file.
 *
 * The temperature data is hourly data in degrees Celsius and
 * the rainfall data is hourly data in millimeters.
 */

float tr_data[2][25] = { { 9.00,  8.30,  7.40,  6.20,  5.60,  3.40,  2.70,  1.20,
			   3.70,  5.80,  5.90,  4.50,  7.40, 11.60, 10.40,  9.20,
			   7.80, 10.20, 12.10, 11.40,  9.00,  8.90,  8.50,  7.40,
			   6.60},
			 {0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
			  0.00,  0.00,  0.00,  0.25,  0.63,  0.10,  0.00,  0.53,
			  3.75,  3.10,  0.52,  0.00,  0.00,  0.00,  0.00,  0.00,
			  0.00, 0.00}};

float yvalues[5] = {2.,1.,0.,-1.,-2.};
char *ylabels[5] = {"2~F33~s","~F33~s","~F22~MEAN","-~F33~s","-2~F33~s"};

#define NHOURS   25

int main()
{
    int appid, work_id, plotid, dataid, dataspec;
    int srlist, i, j;
    float x_array[NHOURS], y_array[NHOURS];
    float x_array2[5], y_array2[5];
    float t_mean, std_dev, mm2inch = 1./25.4;
/*
 * Default is to display output to an X workstation
 */
    char const *wks_type = "x11";
/*
 * Initialize the HLU library.
 */
    NhlInitialize();
    srlist = NhlRLCreate(NhlSETRL);
/*
 * Create Application object.
 */
    NhlRLClear(srlist);
    NhlRLSetString(srlist,NhlNappDefaultParent,"True");
    NhlRLSetString(srlist,NhlNappUsrDir,"./");
    NhlCreate(&appid,"xy14",NhlappClass,NhlDEFAULT_APP,srlist);

    if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
/*
 * Create a meta file object.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkMetaName,"./xy14c.ncgm");
        NhlCreate(&work_id,"xy14Work",NhlncgmWorkstationClass,
                  NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 * Create an X11 workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetInteger(srlist,NhlNwkPause,True);
        NhlCreate(&work_id,"xy14Work",NhlcairoWindowWorkstationClass,
              NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 * Create an older-style PostScript workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkPSFileName,"./xy14c.ps");
        NhlCreate(&work_id,"xy14Work",NhlpsWorkstationClass,
                  NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 * Create an older-style PDF workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkPDFFileName,"./xy14c.pdf");
        NhlCreate(&work_id,"xy14Work",NhlpdfWorkstationClass,
                  NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
             !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 * Create a cairo PS/PDF workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkFileName,"./xy14c");
        NhlRLSetString(srlist,NhlNwkFormat, (char*)wks_type);
        NhlCreate(&work_id,"xy14Work",NhlcairoDocumentWorkstationClass,
                  NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 * Create a cairo PNG workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkFileName,"./xy14c");
        NhlRLSetString(srlist,NhlNwkFormat, (char*)wks_type);
        NhlCreate(&work_id,"xy14Work",NhlcairoImageWorkstationClass,
                  NhlDEFAULT_APP,srlist);
    }
/*
 * Create X-axis values and convert data to Fahrenheit.
 */
    for( i = 0; i < NHOURS; i++ ) {
        x_array[i] = (float)i;
        y_array[i] = (9./5.)*tr_data[0][i]+32.;
    }
/*
 * Picture 1:
 *
 * Convert the temperatures to degrees Fahrenheit and plot as an XyPlot
 * object in the X11 window.  Note the usage of NCL algebraic operators
 * and array selection capabilities.
 */
    NhlRLClear(srlist);
    NhlRLSetFloatArray(srlist,NhlNcaXArray,x_array,NHOURS);
    NhlRLSetFloatArray(srlist,NhlNcaYArray,y_array,NHOURS);
    NhlCreate(&dataid,"xyData",NhlcoordArraysClass,appid,srlist);

    NhlRLClear(srlist);
    NhlRLSetInteger(srlist,NhlNxyCoordData, dataid);
/*
 * X-axis resources
 */
    NhlRLSetString(srlist,NhlNtmXBMode, "MANUAL");
    NhlRLSetFloat(srlist,NhlNtmXBTickStartF, 0.);
    NhlRLSetFloat(srlist,NhlNtmXBTickEndF, 24.);
    NhlRLSetFloat(srlist,NhlNtmXBTickSpacingF, 4.);
    NhlRLSetInteger(srlist,NhlNtmXBLabelFont, 22);
    NhlRLSetFloat(srlist,NhlNtmXBLabelFontHeightF, 0.024);
/*
 * Y-axis resources.  The title precision is changed to "2" so that integer
 * values will be used (the temperatures are in a range requiring only
 * two digits).
 */
    NhlRLSetString(srlist,NhlNtmYLMode, "MANUAL");
    NhlRLSetFloat(srlist,NhlNtmYLTickStartF, 30.);
    NhlRLSetFloat(srlist,NhlNtmYLTickEndF, 60.);
    NhlRLSetFloat(srlist,NhlNtmYLTickSpacingF, 5.);
    NhlRLSetString(srlist,NhlNtmYLAutoPrecision, "False");
    NhlRLSetInteger(srlist,NhlNtmYLPrecision, 2);
    NhlRLSetInteger(srlist,NhlNtmYLLabelFont, 22);
    NhlRLSetFloat(srlist,NhlNtmYLLabelFontHeightF, 0.024);
    NhlRLSetInteger(srlist,NhlNtmYLMinorPerMajor, 4);
/*
 * Specify the Y-axis range precisely.
 */
    NhlRLSetFloat(srlist,NhlNtrYMinF, 30.);
    NhlRLSetFloat(srlist,NhlNtrYMaxF, 60.);
/*
 * Supply titles
 */
    NhlRLSetInteger(srlist,NhlNtiMainFont, 26);
    NhlRLSetFloat(srlist,NhlNtiMainFontHeightF, .03);
    NhlRLSetString(srlist,NhlNtiMainString, "Hourly Temperatures");
    NhlRLSetInteger(srlist,NhlNtiXAxisFont, 22);
    NhlRLSetString(srlist,NhlNtiXAxisString, "Hours Since Midnight");
    NhlRLSetInteger(srlist,NhlNtiYAxisFont, 22);
    NhlRLSetString(srlist,NhlNtiYAxisString, "Temperature (degrees F)");
    NhlCreate(&plotid,"XyPlot",NhlxyPlotClass,work_id,srlist);

    NhlDraw(plotid);
    NhlFrame(work_id);
/*
 * Picture 2:
 *
 * Convert the rainfall data to inches and plot in the X11 window. 
 * (In NCL, note how easy it is to select the rainfall data from the
 * original two-dimensional data array.
 */
    for( i = 0; i < NHOURS; i++ ) y_array[i] = mm2inch*tr_data[1][i];
    NhlRLClear(srlist);
    NhlRLSetFloatArray(srlist,NhlNcaXArray,x_array,NHOURS);
    NhlRLSetFloatArray(srlist,NhlNcaYArray,y_array,NHOURS);
    NhlCreate(&dataid,"xyData",NhlcoordArraysClass,appid,srlist);

    NhlRLClear(srlist);
    NhlRLSetInteger(srlist,NhlNxyCoordData, dataid);
/*
 * X-axis resources
 */
    NhlRLSetString(srlist,NhlNtmXBMode, "MANUAL");
    NhlRLSetFloat(srlist,NhlNtmXBTickStartF, 0.);
    NhlRLSetFloat(srlist,NhlNtmXBTickEndF, 24.);
    NhlRLSetFloat(srlist,NhlNtmXBTickSpacingF, 4.);
    NhlRLSetInteger(srlist,NhlNtmXBLabelFont, 22);
    NhlRLSetFloat(srlist,NhlNtmXBLabelFontHeightF, 0.024);
/*
 * Y-axis resources.
 */
    NhlRLSetString(srlist,NhlNtmYLMode, "MANUAL");
    NhlRLSetFloat(srlist,NhlNtmYLTickStartF, 0.);
    NhlRLSetFloat(srlist,NhlNtmYLTickEndF, 0.15);
    NhlRLSetFloat(srlist,NhlNtmYLTickSpacingF, 0.03);
    NhlRLSetString(srlist,NhlNtmYLAutoPrecision, "False");
    NhlRLSetInteger(srlist,NhlNtmYLPrecision, 2);
    NhlRLSetInteger(srlist,NhlNtmYLLabelFont, 22);
    NhlRLSetFloat(srlist,NhlNtmYLLabelFontHeightF, 0.024);
    NhlRLSetInteger(srlist,NhlNtmYLMinorPerMajor, 4);
/*
 * Specify the Y-axis range precisely.
 */
    NhlRLSetFloat(srlist,NhlNtrYMinF, 0.);
    NhlRLSetFloat(srlist,NhlNtrYMaxF, 0.15);
/*
 * Supply titles
 */
    NhlRLSetInteger(srlist,NhlNtiMainFont, 26);
    NhlRLSetFloat(srlist,NhlNtiMainFontHeightF, .03);
    NhlRLSetString(srlist,NhlNtiMainString, "Hourly Rainfall");
    NhlRLSetInteger(srlist,NhlNtiXAxisFont, 22);
    NhlRLSetString(srlist,NhlNtiXAxisString, "Hours Since Midnight");
    NhlRLSetInteger(srlist,NhlNtiYAxisFont, 22);
    NhlRLSetString(srlist,NhlNtiYAxisString, "Rainfall in Inches");
    NhlCreate(&plotid,"XyPlot",NhlxyPlotClass,work_id,srlist);
    NhlDraw(plotid);
    NhlFrame(work_id);
/*
 * Picture 3:
 *
 * For each hourly temperature reading, plot the number of standard
 * deviations from the mean the reading represents.  (In NCL, note the
 * use of the NCL exponentiation operator (^) and the sqrt function.)
 */
    t_mean = 0.;
    for( i = 0; i < NHOURS; i++ )  t_mean = t_mean+(9./5.)*tr_data[0][i]+32.;
    t_mean = t_mean/NHOURS;
    std_dev = 0.;
    for( i = 0; i < NHOURS; i++ ) {
         std_dev = std_dev+pow( ((9./5.)*tr_data[0][i]+32.) - t_mean,2.);
     }
    std_dev = sqrt(std_dev/(NHOURS-1));
    for( i = 0; i < NHOURS; i++ ) {
        y_array[i] = ((9./5.)*tr_data[0][i]+32. - t_mean)/std_dev;
    }
    NhlRLClear(srlist);
    NhlRLSetFloatArray(srlist,NhlNcaXArray,x_array,NHOURS);
    NhlRLSetFloatArray(srlist,NhlNcaYArray,y_array,NHOURS);
    NhlCreate(&dataid,"xyData",NhlcoordArraysClass,appid,srlist);

    NhlRLClear(srlist);
/*
 * X-axis resources
 */
    NhlRLSetString(srlist,NhlNtmXBMode, "MANUAL");
    NhlRLSetFloat(srlist,NhlNtmXBTickStartF, 0.);
    NhlRLSetFloat(srlist,NhlNtmXBTickEndF, 24.);
    NhlRLSetFloat(srlist,NhlNtmXBTickSpacingF, 4.);
    NhlRLSetInteger(srlist,NhlNtmXBLabelFont, 22);
    NhlRLSetFloat(srlist,NhlNtmXBLabelFontHeightF, 0.024);
/*
 * Y-axis resources.
 */
    NhlRLSetString(srlist,NhlNtmYLMode, "EXPLICIT");
    NhlRLSetFloatArray(srlist,NhlNtmYLValues, yvalues,5);
    NhlRLSetStringArray(srlist,NhlNtmYLLabels, ylabels,5);
    NhlRLSetFloat(srlist,NhlNtmYLTickSpacingF, 1.);
    NhlRLSetInteger(srlist,NhlNtmYLLabelFont, 22);
    NhlRLSetFloat(srlist,NhlNtmYLLabelFontHeightF, 0.024);
    NhlRLSetString(srlist,NhlNtmYMajorGrid, "True");
    NhlRLSetString(srlist,NhlNtmYLMinorOn, "False");
    NhlRLSetString(srlist,NhlNtmYRMinorOn, "False");
/*
 * Specify the Y-axis range precisely.
 */
    NhlRLSetFloat(srlist,NhlNtrYMinF, -2.5);
    NhlRLSetFloat(srlist,NhlNtrYMaxF, 2.5);
/*
 * Supply titles
 */
    NhlRLSetInteger(srlist,NhlNtiMainFont, 26);
    NhlRLSetFloat(srlist,NhlNtiMainFontHeightF, .03);
    NhlRLSetString(srlist,NhlNtiMainString, "Temperature Deviations From Mean");
    NhlRLSetInteger(srlist,NhlNtiXAxisFont, 22);
    NhlRLSetString(srlist,NhlNtiXAxisString, "Hours Since Midnight");
    NhlRLSetString(srlist,NhlNtiYAxisString, "Deviations");
    NhlRLSetInteger(srlist,NhlNtiYAxisFont, 22);
    NhlCreate(&plotid,"XyPlot",NhlxyPlotClass,work_id,srlist);
/*
 * Specify the use of filled circle markers.
 */
    dataspec = NhlAddData(plotid,"xyCoordData",dataid);

    NhlRLClear(srlist);
    NhlRLSetString(srlist,NhlNxyMarkLineMode, "MARKERS");
    NhlRLSetString(srlist,NhlNxyMonoMarkLineMode, "True");
    NhlRLSetInteger(srlist,NhlNxyMarker, 16);
    NhlRLSetString(srlist,NhlNxyMonoMarkerSize, "True");
    NhlRLSetFloat(srlist,NhlNxyMarkerSizeF, .02);
    NhlSetValues(dataspec,srlist);
    NhlDraw(plotid);
    NhlFrame(work_id);
/*
 * Picture 4:
 *
 * (In Ncl, this section illustrate the use of being able to easily pick
 * out subsections of data arrays.
 */
    j = 0;
    for( i = 2; i < NHOURS; i+=6 ) {
        x_array2[j] = x_array[i];
        y_array2[j++] = (9./5.)*tr_data[0][i]+32.;
    }
    NhlRLClear(srlist);
    NhlRLSetFloatArray(srlist,NhlNcaXArray,x_array2,j);
    NhlRLSetFloatArray(srlist,NhlNcaYArray,y_array2,j);
    NhlCreate(&dataid,"xyData",NhlcoordArraysClass,appid,srlist);

    NhlRLClear(srlist);
    NhlRLSetInteger(srlist,NhlNxyCoordData,dataid);
/*
 * X-axis resources 
 */
    NhlRLSetString(srlist,NhlNtmXBMode, "MANUAL");
    NhlRLSetFloat(srlist,NhlNtmXBTickStartF, 2.);
    NhlRLSetFloat(srlist,NhlNtmXBTickEndF, 22.);
    NhlRLSetFloat(srlist,NhlNtmXBTickSpacingF, 6.);
    NhlRLSetInteger(srlist,NhlNtmXBLabelFont, 22);
    NhlRLSetFloat(srlist,NhlNtmXBLabelFontHeightF, 0.024);
    NhlRLSetInteger(srlist,NhlNtmXBMinorPerMajor, 0);
/*
 * Y-axis resources.
 */
    NhlRLSetString(srlist,NhlNtmYLMode, "MANUAL");
    NhlRLSetFloat(srlist,NhlNtmYLTickStartF, 30.);
    NhlRLSetFloat(srlist,NhlNtmYLTickEndF, 60.);
    NhlRLSetFloat(srlist,NhlNtmYLTickSpacingF, 5.);
    NhlRLSetString(srlist,NhlNtmYLAutoPrecision, "False");
    NhlRLSetInteger(srlist,NhlNtmYLPrecision, 2);
    NhlRLSetInteger(srlist,NhlNtmYLLabelFont, 22);
    NhlRLSetFloat(srlist,NhlNtmYLLabelFontHeightF, 0.024);
    NhlRLSetInteger(srlist,NhlNtmYLMinorPerMajor, 4);
/*
 * Specify the Y-axis range precisely.
 */
    NhlRLSetFloat(srlist,NhlNtrYMinF, 30.);
    NhlRLSetFloat(srlist,NhlNtrYMaxF, 60.);
/*
 * Supply titles
 */
    NhlRLSetInteger(srlist,NhlNtiMainFont, 26);
    NhlRLSetString(srlist,NhlNtiMainString, "Temperatures at Six Hour Intervals");
    NhlRLSetFloat(srlist,NhlNtiMainFontHeightF, .03);
    NhlRLSetInteger(srlist,NhlNtiXAxisFont, 22);
    NhlRLSetString(srlist,NhlNtiXAxisString, "Hours Since Midnight");
    NhlRLSetInteger(srlist,NhlNtiYAxisFont, 22);
    NhlRLSetString(srlist,NhlNtiYAxisString, "Temperature (degrees F)");
    NhlCreate(&plotid,"XyPlot",NhlxyPlotClass,work_id,srlist);

    NhlDraw(plotid);
    NhlFrame(work_id);
/*
 * NhlDestroy destroys the given id and all of its children.
 */
    NhlDestroy(appid);
    NhlRLDestroy(srlist);
/*
 * Restores state.
 */
    NhlClose();
    exit(0);
}

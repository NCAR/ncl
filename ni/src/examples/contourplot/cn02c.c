/*
 *      $Id: cn02c.c,v 1.9 2010-03-15 22:49:23 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  1993                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
************************************************************************/
/*
 *  File:       cn02c.c
 *
 *  Author:     David Brown
 *          National Center for Atmospheric Research
 *          PO 3000, Boulder, Colorado
 *
 *  Date:       Tue Oct  4 18:48:08 MDT 1994
 *
 *  Description:    Demonstrates basic features of the ContourPlot object
 */

#include <math.h>
#include <stdio.h>
#include <ncarg/gks.h>
#include <ncarg/ncargC.h>
#include <ncarg/hlu/hlu.h>

/*
 * Include a header file for each object created
 */

#include <ncarg/hlu/App.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/hlu/PDFWorkstation.h>
#include <ncarg/hlu/CairoWorkstation.h>
#include <ncarg/hlu/ScalarField.h>
#include <ncarg/hlu/ContourPlot.h>

static const int M=25, N=25;

int main(int argc, char *argv[])
{
    float   T[25 * 25];
    ng_size_t len_dims[2];
    int appid,wid,dataid,cnid;
    int srlist,grlist;
    float   x,y;
    int i,j;
    float   *fscales;
    int *colors;
    ng_size_t count;
    int itmp;
    char const *wks_type = "x11";

/* create a simple bull's eye pattern test data set */

#define PI  3.14159
    for (i=-N/2;i<=N/2;i++) {
        for (j=-M/2;j<=M/2;j++) {
            x = 8.0 * i;
            y = 8.0 * j;
            *(T+(M*(i+N/2)+j+M/2)) = 
                100.0 - sqrt((double)(x*x + y*y));
        }
    }
/*
 * Initialize the high level utility library
 */
    NhlInitialize();
/*
 * Create an application context. Set the app dir to the current directory
 * so the application looks for a resource file in the working directory.
 * In this example the resource file supplies the plot title only.
 */
    srlist = NhlRLCreate(NhlSETRL);
    NhlRLClear(srlist);
    NhlRLSetString(srlist,NhlNappUsrDir,"./");
    NhlCreate(&appid,"cn02",NhlappClass,NhlDEFAULT_APP,srlist);

    if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
/*
 * Create a meta file workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkMetaName,"./cn02c.ncgm");
        NhlCreate(&wid,"cn02Work",
                  NhlncgmWorkstationClass,appid,srlist);
    }
    else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 * Create an X workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetInteger(srlist,NhlNwkPause,True);
        NhlCreate(&wid,"cn02Work",NhlcairoWindowWorkstationClass,appid,srlist);
    }
    else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 * Create an older-style PostScript workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkPSFileName,"./cn02c.ps");
        NhlCreate(&wid,"cn02Work",NhlpsWorkstationClass,appid,srlist);
    }
    else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 * Create an older-style PDF workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkPDFFileName,"./cn02c.pdf");
        NhlCreate(&wid,"cn02Work",NhlpdfWorkstationClass,appid,srlist);
    }
    else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
             !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 * Create a cairo PS/PDF workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkFileName,"./cn02c");
        NhlRLSetString(srlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"cn02Work",NhlcairoDocumentWorkstationClass,appid,srlist);
    }
    else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 * Create a cairo PNG workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkFileName,"./cn02c");
        NhlRLSetString(srlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"cn02Work",NhlcairoImageWorkstationClass,appid,srlist);
    }
/*
 * Create a ScalarField data object using the data set defined above.
 * By default the array bounds will define the data boundaries (zero-based,
 * as in C language conventions)
 */

    NhlRLClear(srlist);
    len_dims[0] = N;
    len_dims[1] = M;
    NhlRLSetMDFloatArray(srlist,NhlNsfDataArray,T,2,len_dims);
    NhlCreate(&dataid,"bullseye",NhlscalarFieldClass,appid,
              srlist);
/*
 * Create a ContourPlot object, supplying the ScalarField object as data
 */
    NhlRLClear(srlist);
    NhlRLSetInteger(srlist,NhlNcnScalarFieldData,dataid);
    NhlCreate(&cnid,"ContourPlot1",NhlcontourPlotClass,wid,srlist);
/*
 * In the ContourPlot object, many resources that apply to the lines 
 * representing the contour levels and the fill areas between the levels
 * have both a scalar and an array form. You control which applies by 
 * setting an associated boolean flag, identified by the prefix "Mono".
 * As an illustration, set NhlNcnMonoLineDashPattern and NhlNcnMonoLineColor
 * false to use a different line dash pattern and line color at each level.
 * At the same time set the line thickness of all lines to twice the
 * default thickness.
 */
    NhlRLClear(srlist);
    NhlRLSetFloat(srlist,NhlNcnLineThicknessF,2.0);
    NhlRLSetString(srlist,NhlNcnMonoLineDashPattern,"false");
    NhlRLSetString(srlist,NhlNcnMonoLineColor,"false");
    NhlSetValues(cnid,srlist);
    NhlDraw(cnid);
    NhlFrame(wid);
/*
 * Change back to a single solid line color and use pattern fill
 */
    NhlRLClear(srlist);
    NhlRLSetFloat(srlist,NhlNcnLineThicknessF,1.0);
    NhlRLSetString(srlist,NhlNcnMonoLineDashPattern,"true");
    NhlRLSetString(srlist,NhlNcnMonoLineColor,"true");
    NhlRLSetString(srlist,NhlNcnFillOn,"true");
    NhlRLSetString(srlist,NhlNcnMonoFillColor,"true");
    NhlRLSetString(srlist,NhlNcnMonoFillPattern,"false");
    NhlSetValues(cnid,srlist);
    NhlDraw(cnid);
    NhlFrame(wid);
/*
 * Get the fill scale array to illustrate how you would modify the
 * values of an array resource. By default all elements of this array
 * are set to 1.0, resulting in each fill pattern appearing at its
 * 'standard' size.
 * The user is responsible for freeing the memory allocated for this array.
 * Modify the array to range from sparse (2.5) at the low data values 
 * to dense (0.5) at high values, and set the new values, turning off
 * the "mono" flag resource at the same time. 
 */
    grlist = NhlRLCreate(NhlGETRL);
    NhlRLClear(grlist);
    NhlRLGetFloatArray(grlist,NhlNcnFillScales,&fscales,&count);
    NhlGetValues(cnid,grlist);

    for (i = 0; i < count; i++) {
        fscales[i] = 2.5 - 2.0 * i / (float) (count - 1);
    }
    NhlRLClear(srlist);
    NhlRLSetString(srlist,NhlNcnMonoFillScale,"false");
    NhlRLSetFloatArray(srlist,NhlNcnFillScales,fscales,count);
    NhlSetValues(cnid,srlist);
    NhlDraw(cnid);
    NhlFrame(wid);
/*
 * Use solid multi-colored fill instead of single-colored pattern fill.
 * Using the scalar form of the line color resource, change the contour 
 * lines to use the background color.
 */
    NhlRLClear(srlist);
    NhlRLSetInteger(srlist,NhlNcnLineColor,NhlBACKGROUND);
    NhlRLSetString(srlist,NhlNcnMonoFillColor,"false");
    NhlRLSetString(srlist,NhlNcnMonoFillPattern,"true");
    NhlSetValues(cnid,srlist);
    NhlDraw(cnid);
    NhlFrame(wid);

/*
 * Invert the fill colors.
 * First get the current array contents, reverse their order, 
 * then re-set the resource using the modified array. Note that the user 
 * is responsible for freeing the memory allocated for the array. 
 * Turn lines off altogether and also turn off the line and high/low labels.
 */
    NhlRLClear(grlist);
    NhlRLGetIntegerArray(grlist,NhlNcnFillColors,&colors,&count);
    NhlGetValues(cnid,grlist);

    for (i = 0; i < count / 2; i++) {
        itmp = colors[i];
        colors[i] = colors[count - 1 - i];
        colors[count - 1 - i] = itmp;
    }

    NhlRLClear(srlist);
    NhlRLSetIntegerArray(srlist,NhlNcnFillColors,colors,count);
    NhlRLSetString(srlist,NhlNcnLinesOn,"false");
    NhlRLSetString(srlist,NhlNcnLineLabelsOn,"false");
    NhlRLSetString(srlist,NhlNcnHighLabelsOn,"false");
    NhlRLSetString(srlist,NhlNcnLowLabelsOn,"false");
    NhlSetValues(cnid,srlist);
    NhlDraw(cnid);
    NhlFrame(wid);
    
/*
 * Destroy the objects created, close the HLU library and exit.
 */
    NhlFree(fscales);
    NhlFree(colors);
    NhlDestroy(dataid);
    NhlDestroy(cnid);
    NhlDestroy(wid);
    NhlDestroy(appid);

    NhlClose();
    exit(0);
}

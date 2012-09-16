/*
 *      $Id: cn03c.c,v 1.8 2010-03-15 22:49:23 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  1993                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
************************************************************************/
/*
 *  File:       cn03c.c
 *
 *  Author:     David Brown
 *          National Center for Atmospheric Research
 *          PO 3000, Boulder, Colorado
 *
 *  Date:       Mon Oct  3 11:35:03 MDT 1994
 *
 *  Description:    Demonstrates basic features of the ContourPlot object.
 *          The first frame emulates the contour plot drawn 
 *          in cn01c using low-level NCARG calls. 
 *          
 */

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
#include <ncarg/hlu/LogLinPlot.h>

/*
 * This header file contains the data used by this example
 */

#include "cn03c.h"

/*
 * The coordinates of the irregular Y dimension
 */

float level[] = { 1000, 850, 700, 500, 400, 300, 250, 200, 150, 100} ;

/*
 * Explicit labels and label locations for the X Axis tickmarks
 */
char *labels[] = { "90~S~o~N~S", "60~S~o~N~S", "30~S~o~N~S", "EQ", 
            "30~S~o~N~N", "60~S~o~N~N", "90~S~o~N~N" };

float labellocs[] = { -90.0, -60.0, -30.0, 0.0, 30.0, 60.0, 90.0 };

/*
 * The data dimensions
 */

int M=73, N=10;
        
int main()
{
    int appid,wid,cnid,dataid,llid;
    int rlist, grlist;
    ng_size_t len_dims[2];
    float xvp,yvp,heightvp,widthvp;
    char const *wks_type = "x11";
/*
 * Initialize the high level utility library
 */

    NhlInitialize();
/*
 * Create an application context. Set the app dir to the current directory
 * so the application looks for the resource file the directory it executes
 * from. 
 */
    rlist = NhlRLCreate(NhlSETRL);
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNappUsrDir,"./");
    NhlCreate(&appid,"cn03",NhlappClass,NhlDEFAULT_APP,rlist);
    
    if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
/*
 * Create a meta file workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./cn03c.ncgm");
        NhlCreate(&wid,"cn03Work",
                  NhlncgmWorkstationClass,NhlDEFAULT_APP,rlist); 
    }
    else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 * Create an X workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlCreate(&wid,"cn03Work",
                  NhlcairoWindowWorkstationClass,NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 * Create an older-style PostScript workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPSFileName,"./cn03c.ps");
        NhlCreate(&wid,"cn03Work",
                  NhlpsWorkstationClass,NhlDEFAULT_APP,rlist); 
    }
    else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 * Create an older-style PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPDFFileName,"./cn03c.pdf");
        NhlCreate(&wid,"cn03Work",
                  NhlpdfWorkstationClass,NhlDEFAULT_APP,rlist); 
    }
    else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
             !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 * Create a cairo PS/PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"./cn03c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"cn03Work",
                  NhlcairoDocumentWorkstationClass,NhlDEFAULT_APP,rlist); 
    }
    else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 * Create a cairo PNG workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"./cn03c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"cn03Work",
                  NhlcairoImageWorkstationClass,NhlDEFAULT_APP,rlist); 
    }
/*
 * Create a scalar field data object with a linear X dimension representing
 * latitude and an irregular Y dimension representing geopotential height.
 * Define the start and end points of the data, based on the dataset.
 */

    NhlRLClear(rlist);
    len_dims[0] = N, len_dims[1] = M;
    NhlRLSetMDFloatArray(rlist,NhlNsfDataArray,T,2,len_dims);
    NhlRLSetFloatArray(rlist,NhlNsfYArray,level,NhlNumber(level));
    NhlRLSetFloat(rlist,NhlNsfXCStartV,-90.0);
    NhlRLSetFloat(rlist,NhlNsfXCEndV,90.0);
    NhlRLSetFloat(rlist,NhlNsfYCStartV,1000.0);
    NhlRLSetFloat(rlist,NhlNsfYCEndV,100.0);
    NhlCreate(&dataid,"mydata",NhlscalarFieldClass,NhlDEFAULT_APP,
              rlist);
/*
 * Create a ContourPlot object. Since ContourPlot contains a TickMark object by
 * default, the non-default TickMark resources can be set in the ContourPlot
 * object.
 */
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNtiMainString,
                   "Profile @ 105~S~o~N~W - Frame 1");
    NhlRLSetInteger(rlist,NhlNcnScalarFieldData,dataid);
    NhlRLSetFloat(rlist,NhlNvpXF,0.125);
    NhlRLSetFloat(rlist,NhlNvpYF,0.85);
    NhlRLSetFloat(rlist,NhlNvpWidthF,0.6);
    NhlRLSetFloat(rlist,NhlNvpHeightF,0.6);
    NhlRLSetFloat(rlist,NhlNcnLevelSpacingF,5.0);
    NhlRLSetInteger(rlist,NhlNtmXBMode,NhlEXPLICIT);
    NhlRLSetInteger(rlist,NhlNtmXBMinorOn,False);
    NhlRLSetFloatArray(rlist,
                       NhlNtmXBValues,labellocs,NhlNumber(labellocs));
    NhlRLSetStringArray(rlist,
                        NhlNtmXBLabels,labels,NhlNumber(labels));
    NhlCreate(&cnid,"ContourPlot1",NhlcontourPlotClass,wid,rlist);

    NhlDraw(cnid);
    NhlFrame(wid);

/*
 * Color and add dash patterns to the lines, then display a legend
 * listing the line types. The position of the Legend is controlled by
 * resources set in the resource file. Thicken lines.
 * Note that the Legend and LabelBar are provided to the ContourPlot object
 * by its PlotManager (created by default when the ContourPlot object
 * is initialized). Therefore the resources to control them have the 
 * prefix 'pm' rather than 'cn'. 
 */
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNtiMainString,
                   "Profile @ 105~S~o~N~W - Frame 2");
    NhlRLSetInteger(rlist,NhlNcnMonoLineColor,False);
    NhlRLSetInteger(rlist,NhlNcnMonoLineDashPattern,False);
    NhlRLSetString(rlist,NhlNpmLegendDisplayMode,"always");
    NhlSetValues(cnid,rlist);

    NhlDraw(cnid);
    NhlFrame(wid);
/*
 * Turn lines off, and use solid color fill instead.
 * Remove the Legend and display a LabelBar.
 * Turn off line and high/low labels.
 */
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNtiMainString,
                   "Profile @ 105~S~o~N~W - Frame 3");
    NhlRLSetString(rlist,NhlNcnLinesOn,"false");
    NhlRLSetString(rlist,NhlNcnFillOn,"true");
    NhlRLSetString(rlist,NhlNpmLegendDisplayMode,"never");
    NhlRLSetString(rlist,NhlNpmLabelBarDisplayMode,"always");
    NhlRLSetString(rlist,NhlNcnLineLabelsOn,"false");
    NhlRLSetString(rlist,NhlNcnHighLabelsOn,"false");
    NhlRLSetString(rlist,NhlNcnLowLabelsOn,"false");
    NhlSetValues(cnid,rlist);

    NhlDraw(cnid);
    NhlFrame(wid);

/*
 * Now show the plot with the Y-Axis linearized, by overlaying the
 * plot on a LogLinPlot object. Retrieve the current view coordinates
 * of the ContourPlot object and pass them on to the LogLinPlot object.
 * Note the LogLinPlot needs to be told the data boundaries. 
 */

    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNtiMainString,
                   "Profile @ 105~S~o~N~W - Frame 4");
    NhlSetValues(cnid,rlist);

    grlist = NhlRLCreate(NhlGETRL);
    NhlRLClear(grlist);
    NhlRLGetFloat(grlist,NhlNvpXF,&xvp);
    NhlRLGetFloat(grlist,NhlNvpYF,&yvp);
    NhlRLGetFloat(grlist,NhlNvpWidthF,&widthvp);
    NhlRLGetFloat(grlist,NhlNvpHeightF,&heightvp);
    NhlGetValues(cnid,grlist);

    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNvpXF,xvp);
    NhlRLSetFloat(rlist,NhlNvpYF,yvp);
    NhlRLSetFloat(rlist,NhlNvpWidthF,widthvp);
    NhlRLSetFloat(rlist,NhlNvpHeightF,heightvp);
    NhlRLSetFloat(rlist,NhlNtrXMinF,-90.0);
    NhlRLSetFloat(rlist,NhlNtrXMaxF,90.0);
    NhlRLSetFloat(rlist,NhlNtrYMaxF,1000.0);
    NhlRLSetFloat(rlist,NhlNtrYMinF,100.0);
    NhlRLSetString(rlist,NhlNtrYReverse,"True");
    NhlCreate(&llid,"LogLin1",NhllogLinPlotClass,wid,rlist);

/*
 * The LogLinPlot becomes the Base Plot, since it controls the coordinate
 * system that we are mapping to. Overlay the ContourPlot object on the base,
 * then plot the LogLinPlot object. Note that you cannot draw the ContourPlot 
 * object directly, once it becomes an overlay Plot.
 */
    NhlAddOverlay(llid,cnid,-1);
    NhlDraw(llid);
    NhlFrame(wid);

    NhlDestroy(llid);
    NhlDestroy(dataid);
    NhlDestroy(cnid);
    NhlDestroy(wid);
    NhlDestroy(appid);
    NhlClose();
    exit(0);
}

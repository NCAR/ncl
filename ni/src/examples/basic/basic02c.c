/*
 * $Id: basic02c.c,v 1.13 2010-03-15 22:49:23 haley Exp $
 */
/***********************************************************************
*                                                                      *
*                            Copyright (C)  1995                       *
*                 University Corporation for Atmospheric Research      *
*                            All Rights Reserved                       *
*                                                                      *
************************************************************************
*
*      File:            basic02c.c
*
*      Author:          Tim Scheitlin (converted by Ed Stautler)
*                       National Center for Atmospheric Research
*                       PO 3000, Boulder, Colorado
*
*      Date:            Mon Mar 20 10:43:42 MST 1995
*
*      Description:     The first frame in this example demonstrates how
*                       to set the view port for a contour plot.
*                       Note: no data is used in this example, so the
*                       output appears only as a bounding box with
*                       tickmarks.
*
*                       The second frame in this example demonstrates how
*                       to produce multiple plots on a single frame.
*/

#include <ncarg/hlu/App.h>
#include <ncarg/hlu/ResList.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/hlu/PDFWorkstation.h>
#include <ncarg/hlu/CairoWorkstation.h>
#include <ncarg/hlu/ContourPlot.h>
#include <ncarg/hlu/hlu.h>

int main()
{
    int appid,wks,con1,rlist;

    char const *wks_type = "x11";

/*
 * Initialize the graphics libraries and create a resource list that
 * is normally used to assign name/value pairs within objects.  Then
 * clear (empty) this list, and create an application object.  This
 * object manages multiple resource databases used by separate objects.
 */
    NhlInitialize();
    rlist = NhlRLCreate(NhlSETRL);

    NhlRLClear(rlist);
    NhlCreate(&appid,"basic02",NhlappClass,NhlDEFAULT_APP,rlist);
/*
 * ###########
 * # FRAME 1 #
 * ###########
 * Choose the type of output you want to create.  You may write your
 * output to an NCGM file, X workstation window, or a PostScript file. 
 */
    if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
/*
 * Create a meta file workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./basic02c.ncgm");
        NhlCreate(&wks,"wks",NhlncgmWorkstationClass,NhlDEFAULT_APP,
                  rlist);
    }
    else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 * Create an X workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlCreate(&wks,"wks",NhlcairoWindowWorkstationClass,NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 * Create an older-style PostScript workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPSFileName,"./basic02c.ps");
        NhlCreate(&wks,"wks",NhlpsWorkstationClass,NhlDEFAULT_APP,
                  rlist);
    }
    else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 * Create an older-style PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPDFFileName,"./basic02c.pdf");
        NhlCreate(&wks,"wks",NhlpdfWorkstationClass,NhlDEFAULT_APP,
                  rlist);
    }
    else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
             !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 * Create a cairo PS/PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"./basic02c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wks,"wks",NhlcairoDocumentWorkstationClass,NhlDEFAULT_APP,
                  rlist);
    }
    else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 * Create a cairo PNG workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"./basic02c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wks,"wks",NhlcairoImageWorkstationClass,NhlDEFAULT_APP,
                  rlist);
    }

/*
 * Create a plot object.  In this example, we will create a contour plot.
 *
 * Four view class resources, vpXF, vpYF, vpWidthF, and vpHeightF, are
 * assigned values in the following create call.  The combination of
 * these four resources determines where the plot will display in the
 * output window.  The values of these resources are specified in 
 * Normalized Device Coordinates (NDCs).  In this two-dimensional coordinate 
 * system (0,0) specifies the lower-left corner and (1,1) specifies the 
 * upper-right corner of a plot.
 */
    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,"vpXF",0.05); 
    NhlRLSetFloat(rlist,"vpYF",0.95); 
    NhlRLSetFloat(rlist,"vpWidthF",0.4); 
    NhlRLSetFloat(rlist,"vpHeightF",0.4); 
    NhlCreate(&con1,"con1",NhlcontourPlotClass,wks,rlist);
/*
 * Draw the plot. 
 */
    NhlDraw(con1);
/*
 * The frame call updates and then clears the workstation.
 * Anything written to the workstation after a frame call is made will be
 * drawn in a subsequent frame. 
 */
    NhlFrame(wks);
/*
 * ###########
 * # FRAME 2 #
 * ###########
 *
 * This example demonstrates drawing multiple plots in a single frame.
 *
 * Calling draw again will produce the identical plot that was drawn in the
 * first frame.
 */
    NhlDraw(con1);
/*
 * To add another plot to the same frame, we first need to reset the 
 * viewport resources so that the next plot does not overwrite the first
 * one.  The setvalues expression is used to set resources after an object
 * has already been created.  The first argument, "con1", in the setvalues
 * expression specifies an object id of a plot that was generated earlier
 * with the create call.  This is then followed by a list of resource value
 * pairs that apply to the object.
 */
    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,"vpXF",0.55); 
    NhlRLSetFloat(rlist,"vpYF",0.45); 
    NhlRLSetFloat(rlist,"vpWidthF",0.2); 
    NhlRLSetFloat(rlist,"vpHeightF",0.2); 
        NhlSetValues(con1,rlist);
/*
 * Because of the new viewport resource settings, calling draw produces 
 * a plot in the lower-right quadrant of the frame.
 */
    NhlDraw(con1);
/*
 * Updates and clear the workstation.
 */
    NhlFrame(wks);
/*
 * Clean up (destroying the parent object recursively destroys all of its 
 * children).
 */
    NhlDestroy(con1);
    NhlClose();
    exit (0);
}

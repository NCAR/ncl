/*
 * $Id: basic01c.c,v 1.13 2010-03-15 22:49:23 haley Exp $
 */
/***********************************************************************
*                                                                      *
*                            Copyright (C)  1995                       *
*                 University Corporation for Atmospheric Research      *
*                            All Rights Reserved                       *
*                                                                      *
************************************************************************
*
*      File:            basic01c.c
*
*      Author:          Tim Scheitlin (converted by Ed Stautler)
*                       National Center for Atmospheric Research
*                       PO 3000, Boulder, Colorado
*
*      Date:            Mon Mar 20 10:43:42 MST 1995
*
*      Description:     This example demonstrates how to draw a contour
*                       plot using mostly defaults.  Note: no data is
*                       used in this example, so the output appears
*                       only as a bounding box with tickmarks.
* 
*                       The minimum set of steps needed for creating
*                       any plot involve the following:
*
*                       1. Initialize the graphics libraries
*                       2. Choose the type of output 
*                       3. Create a plot object
*                       4. Draw the plot
*                       5. Call frame
*                       6. Clean up memory
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
 * ##########
 * # STEP 1 #
 * ##########
 * Initialize the graphics libraries and create a resource list that
 * is normally used to assign name/value pairs within objects.  Then
 * clear (empty) this list, and create an application object.  This
 * object manages multiple resource databases used by separate objects.  
 *
 * The first argument, "&appid", is a variable that identifies the object.
 * The second argument, "basic01", sets the name of the object being created.
 * The third argument, "NhlappClass", identifies the type or class 
 * of the created object. 
 * The fourth argument, "NhlDEFAULT_APP", specifies the id of the objects 
 * parent.  In this case, the object has no parent, so the constant 
 * "NhlDEFAULT_APP" is used. 
 * The fifth argument, "rlist", is the resource list modifiers to be used
 * when creating the object.  In this example, no modifications are made to
 * default values.
 *
 */
    NhlInitialize();
    rlist = NhlRLCreate(NhlSETRL);

    NhlRLClear(rlist);
    NhlCreate(&appid,"basic01",NhlappClass,NhlDEFAULT_APP,rlist);
/*
 * ##########
 * # STEP 2 #
 * ##########
 * Choose the type of output you want to create.  You may write your
 * output to an NCAR Computer Graphics Metafile (NCGM) and view it later using
 * the NCAR Graphics utilities ctrans or idt.  You may also write your
 * output directly into a window of a workstation running the X Window system.
 * Another option is to write your ouput into a PostScript file.
 *
 * The first argument, "&wks", is a variable that identifies the object.
 * The second argument, '"wks"', sets the name of the object being created.
 * The third argument, "xWorkstationClass", identifies the type or class 
 * of the object to create.  In this case an X workstation.
 * The fourth argument, "NhlDEFAULT_APP", specifies the id of the objects 
 * parent.  In this case, "NhlDEFAULT_APP" is used to specify the default,
 * which is the App object that has just been created in the previous step.
 * The fifth argument, "rlist", is the resource list modifiers to be used
 * when creating the object.  In this example, no modifications are made to
 * default values.
 */
    if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
/*
 * Create a meta file workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./basic01c.ncgm");
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
 * Create a PS file workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPSFileName,"./basic01c.ps");
        NhlCreate(&wks,"wks",NhlpsWorkstationClass,NhlDEFAULT_APP,
                  rlist);
    }
    else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 *  Create a PDF file workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPDFFileName,"./basic01c.pdf");
        NhlCreate(&wks,"wks",NhlpdfWorkstationClass,NhlDEFAULT_APP,
                  rlist);
   }
    else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
             !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 *  Create a cairo PS/PDF file workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"./basic01c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wks,"wks",NhlcairoDocumentWorkstationClass,NhlDEFAULT_APP,
                  rlist);
   }
    else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 *  Create a cairo PNG file workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"./basic01c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wks,"wks",NhlcairoImageWorkstationClass,NhlDEFAULT_APP,
                  rlist);
   }

/*
 * ##########
 * # STEP 3 #
 * ##########
 * Create a plot object.  In this example, we will create a contour plot,
 * but we could have just as easily created any other type of plot such as
 * an Xy plot, or a Map plot.
 *
 * The first argument, "&con1", is a variable that identifies the object.
 * The second create call argument, '"con1"', sets the name of the object.
 * This is an arbitrary name and does not have to match the variable object
 * identifier used in the first parameter.
 * The third argument, "contourPlotClass", identifies the type or class
 * of the object to create.  In this case, the type is a contour plot. 
 * The third argument, "wks", specifies the id of the object's parent.  By 
 * specifying the id of the X workstation created earlier, the plot will
 * be drawn into an X window.
 * The fifth argument, "rlist", is the resource list modifiers to be used
 * when creating the object.  In this example, no modifications are made to
 * default values.
 */

    NhlRLClear(rlist);
    NhlCreate(&con1,"con1",NhlcontourPlotClass,wks,rlist);
/*
 * ##########
 * # STEP 4 #
 * ##########
 * This step draws the plot into the X workstation window.  The argument to
 * the draw function is the variable name of the object that you want to
 * draw.
 */
    NhlDraw(con1);
/*
 * ##########
 * # STEP 5 #
 * ##########
 * The frame call updates and then clears the workstation.
 */
    NhlFrame(wks);
/*
 * ##########
 * # STEP 6 #
 * ##########
 * This is the final step used for cleanup.  The NhlDestroy
 * function detroys objects and frees up memory.
 * Destroying a parent object
 * automatically destroys all of its children.  The NhlClose function
 * is used to tell the HLU library that the programmer is done 
 * using it, and to free up any memory that it can. 
 */
    NhlDestroy(con1);
    NhlClose();
    exit(0);
}

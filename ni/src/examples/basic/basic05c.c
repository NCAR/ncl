/*
 * $Id: basic05c.c,v 1.12 2010-03-15 22:49:23 haley Exp $
 */
/***********************************************************************
*                                                                      *
*                            Copyright (C)  1995                       *
*                 University Corporation for Atmospheric Research      *
*                            All Rights Reserved                       *
*                                                                      *
************************************************************************
*
*      File:            basic05c.c
*
*      Author:          Tim Scheitlin (converted by Ed Stautler)
*                       National Center for Atmospheric Research
*                       PO 3000, Boulder, Colorado
*
*      Date:            Fri Apr 28 12:26:32 MDT 1995
*
*      Description:     This example demonstrates how to read, display, and
*                       manipulate colormaps.
*
*             The NG 4.x HLU software supports several different predefined
*             colormaps of various sizes.  This example demonstrates how to
*             display each of those colormaps using the labelbar utility.
*             This example also shows how to change entries in a colormap
*             and create a completely new colormap.
*
*/

#include <ncarg/hlu/App.h>
#include <ncarg/hlu/ResList.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/hlu/PDFWorkstation.h>
#include <ncarg/hlu/CairoWorkstation.h>
#include <ncarg/hlu/Workstation.h>
#include <ncarg/hlu/LabelBar.h>
#include <ncarg/hlu/hlu.h>


int main()
{
    int i;
    int appid,wks,lbar,rlist,glist,text;
    char *colorindices[255];
    int num_dims;
    ng_size_t *len_dims;
    float *cmap;
    float newcmap[100*3];
    char const *wks_type = "x11";
/*
 * Display the default colormap.
 *
 * Initialize labels for the color map entries
 */
    for(i=1; i<=255; i++) {
        colorindices[i-1] = (char*)malloc(4*sizeof(char));
        sprintf(colorindices[i-1],"%d",i);
    }
/*
 * Initialize libraries and create a resource list.
 */
    NhlInitialize();
    rlist = NhlRLCreate(NhlSETRL);

    NhlRLClear(rlist);
    NhlCreate(&appid,"appid",NhlappClass,NhlDEFAULT_APP,rlist);

    if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
/*
 * Create a meta file workstation.
 */
        NhlRLClear(rlist);
/*
 * Set Colormap to default. Note, this assignment is redundant
 */
        NhlRLSetString(rlist,NhlNwkMetaName,"./basic05c.ncgm");
        NhlCreate(&wks,"wks",NhlncgmWorkstationClass,NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 * Create an X workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,"wkPause","True");
/*
 * Set the colormode to private so there is no color contention
 */
/*      NhlRLSetString(rlist,"wkXColorMode","private"); */
        NhlCreate(&wks,"wks",NhlcairoWindowWorkstationClass,NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 * Create an older-style PostScript workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPSFileName,"./basic05c.ps");
        NhlCreate(&wks,"wks",NhlpsWorkstationClass,NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 * Create an older-style PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPDFFileName,"./basic05c.pdf");
        NhlCreate(&wks,"wks",NhlpdfWorkstationClass,NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
             !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 * Create a cairo PS/PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"./basic05c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wks,"wks",NhlcairoDocumentWorkstationClass,NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 * Create a cairo PNG workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"./basic05c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wks,"wks",NhlcairoImageWorkstationClass,NhlDEFAULT_APP,rlist);
    }

/*
 * Create a labelbar object. 
 */
    NhlRLClear(rlist);
/*
 * Assign the labels
 */
    NhlRLSetStringArray(rlist,"lbLabelStrings",colorindices,255);
/*
 * Label every 5th entry
 */
    NhlRLSetInteger(rlist,"lbLabelStride",5);
/*
 * Single pattern used for fill
 */
    NhlRLSetString(rlist,"lbMonoFillPattern","True");
/*
 * Set fill pattern to solid
 */
        NhlRLSetString(rlist,"lbFillPattern","SolidFill");
/*
 * No lines between colors
 */
        NhlRLSetString(rlist,"lbBoxLinesOn","False");
/*
 * Display 255 entries
 */
        NhlRLSetInteger(rlist,"lbBoxCount",255);
/*
 * Turn off labelbar perimeter
 */
        NhlRLSetString(rlist,"lbPerimOn","False");
/*
 * Plot title
 */
        NhlRLSetString(rlist,"lbTitleString","(New) Default Colormap");
/*
 * Title font
 */
        NhlRLSetString(rlist,"lbTitleFont","Helvetica-bold");
/*
 * Label font
 */
        NhlRLSetString(rlist,"lbLabelFont","Helvetica");
/*
 * Set viewport to max size
 */
        NhlRLSetFloat(rlist,"vpXF",0.0);
        NhlRLSetFloat(rlist,"vpYF",1.0);
        NhlRLSetFloat(rlist,"vpHeightF",1.0);
        NhlRLSetFloat(rlist,"vpWidthF",1.0);

        NhlCreate(&lbar,"lbar",NhllabelBarClass,wks,rlist);
/*
 *  Create a text label
 */
        NhlRLClear(rlist);
/*
 * Set font
 */
        NhlRLSetString(rlist,"txFont","Helvetica-bold");
/*
 * Set position and height
 */
        NhlRLSetFloat(rlist,"txPosXF",.5);
        NhlRLSetFloat(rlist,"txPosYF",.03);
        NhlRLSetFloat(rlist,"txFontHeightF",.035);
/*
 * Set the function code to the "*" character so that the 
 * default function code character, the colon, can be used
 * in the "txString" resource.
 */
        NhlRLSetString(rlist,"txFuncCode","*"); 
/*
 * Set the text value
 */
        NhlRLSetString(rlist,"txString","Note: Entry 0 is the background color");

        NhlCreate(&text,"text",NhltextItemClass,wks,rlist);
/*
 * Draw the labelbar displaying the default colormap and textual annotation
 */
    NhlDraw(lbar);
    NhlDraw(text);
    NhlFrame(wks);
/*
 *  Change the colormap to the old (pre V6.1.0) default color map
 */
    NhlRLClear(rlist);
    NhlRLSetString(rlist,"wkColorMap","default");
    NhlSetValues(wks,rlist);

    NhlRLClear(rlist);
    NhlRLSetString(rlist,"lbTitleString","(Old) Default Colormap");
    NhlRLSetInteger(rlist,"lbBoxCount",31);
    NhlSetValues(lbar,rlist);
/*
 * Draw the labelbar displaying the default colormap and textual annotation
 */
    NhlDraw(lbar);
    NhlDraw(text);
    NhlFrame(wks);

/*
 *  Change the colormap to one of the predefined colormaps
 */
    NhlRLClear(rlist);
    NhlRLSetString(rlist,"wkColorMap","cyclic");
    NhlSetValues(wks,rlist);
/*
 *  Change the labelbar title, annotation, and number of entries.
 */
    NhlRLClear(rlist);
/*
 * Labelbar title
 */
    NhlRLSetString(rlist,"lbTitleString","Cyclic Colormap");
/*
 * Label every entry
 */
    NhlRLSetInteger(rlist, "lbLabelStride", 1);                  
/*
 * Number of entries to display
 */
    NhlRLSetInteger(rlist, "lbBoxCount", 7);
    NhlSetValues(lbar,rlist);
/*
 *
 * Draw the labelbar displaying the default colormap and textual annotation
 */
    NhlDraw(lbar);
    NhlDraw(text);
    NhlFrame(wks);
/*
 *  Change the colormap to one of the predefined colormaps
 */
    NhlRLClear(rlist);
    NhlRLSetString(rlist,"wkColorMap","gscyclic");
    NhlSetValues(wks,rlist);
/*
 *  Change the labelbar title, annotation, and number of entries.
 */
    NhlRLClear(rlist);
/*
 * Labelbar title
 */
    NhlRLSetString(rlist,"lbTitleString","Gscyclic Colormap");
/*
 * Label every entry
 */
    NhlRLSetInteger(rlist, "lbLabelStride", 1);                  
/*
 * Number of entries to display
 */
    NhlRLSetInteger(rlist, "lbBoxCount", 7);

    NhlSetValues(lbar,rlist);
/*
 * Draw the labelbar displaying the default colormap and textual annotation
 */
    NhlDraw(lbar);
    NhlDraw(text);
    NhlFrame(wks);
/*
 *  Change the colormap to one of the predefined colormaps
 */
    NhlRLClear(rlist);
    NhlRLSetString(rlist,"wkColorMap","gsltod");
    NhlSetValues(wks,rlist);
/*
 *  Change the labelbar title, annotation, and number of entries.
 */
    NhlRLClear(rlist);
/*
 * Labelbar title
 */
    NhlRLSetString(rlist,"lbTitleString","Gsltod Colormap");
/*
 * Label every other entry
 */
    NhlRLSetInteger(rlist, "lbLabelStride", 2);                  
/*
 * Number of entries to display
 */
    NhlRLSetInteger(rlist, "lbBoxCount", 32);

    NhlSetValues(lbar,rlist);
/*
 * Draw the labelbar displaying the default colormap and textual annotation
 */
    NhlDraw(lbar);
    NhlDraw(text);
    NhlFrame(wks);
/*
 *  Change the colormap to one of the predefined colormaps
 */
    NhlRLClear(rlist);
    NhlRLSetString(rlist,"wkColorMap","gsdtol");
    NhlSetValues(wks,rlist);
/*
 *  Change the labelbar title, annotation, and number of entries.
 */
    NhlRLClear(rlist);
/*  
 * Labelbar title
 */
    NhlRLSetString(rlist,"lbTitleString","Gsdtol Colormap");
/*
 * Label every other entry
 */
    NhlRLSetInteger(rlist, "lbLabelStride", 2);                  
/*
 * Number of entries to display
 */
    NhlRLSetInteger(rlist, "lbBoxCount", 32);

    NhlSetValues(lbar,rlist);
/*
 * Draw the labelbar displaying the default colormap and textual annotation
 */
    NhlDraw(lbar);
    NhlDraw(text);
    NhlFrame(wks);
/*
 *  Change the colormap to one of the predefined colormaps
 */
    NhlRLClear(rlist);
    NhlRLSetString(rlist,"wkColorMap","uniform");
    NhlSetValues(wks,rlist);
/*
 *  Change the labelbar title, annotation, and number of entries.
 */
    NhlRLClear(rlist);
/*
 * Labelbar title
 */
    NhlRLSetString(rlist,"lbTitleString","Uniform Colormap");
/*
 * Label every 10th entry
 */
    NhlRLSetInteger(rlist, "lbLabelStride", 10);                  
/*
 * Number of entries to display
 */
    NhlRLSetInteger(rlist, "lbBoxCount", 112);

    NhlSetValues(lbar,rlist);
/*
 * Draw the labelbar displaying the default colormap and textual annotation
 */
    NhlDraw(lbar);
    NhlDraw(text);
    NhlFrame(wks);
/*
 *  Change the colormap to one of the predefined colormaps
 */
    NhlRLClear(rlist);
    NhlRLSetString(rlist,"wkColorMap","temp1");
    NhlSetValues(wks,rlist);
/*
 *  Change the labelbar title, annotation, and number of entries.
 */
    NhlRLClear(rlist);
/*
 * Labelbar title
 */
    NhlRLSetString(rlist,"lbTitleString","Temp1 Colormap");
/*
 * Label every 5th entry
 */
    NhlRLSetInteger(rlist, "lbLabelStride", 5);                  
/*
 * Number of entries to display
 */
    NhlRLSetInteger(rlist, "lbBoxCount", 62);

    NhlSetValues(lbar,rlist);
/*
 * Draw the labelbar displaying the default colormap and textual annotation
 */
    NhlDraw(lbar);
    NhlDraw(text);
    NhlFrame(wks);
/*
 *  Change the colormap to one of the predefined colormaps
 */
    NhlRLClear(rlist);
    NhlRLSetString(rlist,"wkColorMap","psgcap");
    NhlSetValues(wks,rlist);
/*
 *  Change the labelbar title, annotation, and number of entries.
 */
    NhlRLClear(rlist);
/*
 * Labelbar title
 */
    NhlRLSetString(rlist,"lbTitleString","Psgcap Colormap");
/*
 * Label every 15th entry
 */
    NhlRLSetInteger(rlist, "lbLabelStride", 15);                  
/*
 * Number of entries to display
 */
    NhlRLSetInteger(rlist, "lbBoxCount", 230);

    NhlSetValues(lbar,rlist);
/*
 * Draw the labelbar displaying the default colormap and textual annotation
 */
    NhlDraw(lbar);
    NhlDraw(text);
    NhlFrame(wks);
/*
 *  Change the colormap to one of the predefined colormaps
 */
    NhlRLClear(rlist);
    NhlRLSetString(rlist,"wkColorMap","example");
    NhlSetValues(wks,rlist);
/*
 *  Change the labelbar title, annotation, and number of entries.
 */
    NhlRLClear(rlist);
/*
 * Labelbar title
 */
    NhlRLSetString(rlist,"lbTitleString","Example Colormap");
/*
 * Label every 15th entry
 */
    NhlRLSetInteger(rlist, "lbLabelStride", 10);                  
/*
 * Number of entries to display
 */
    NhlRLSetInteger(rlist, "lbBoxCount", 114);

    NhlSetValues(lbar,rlist);
/*
 * Draw the labelbar displaying the default colormap and textual annotation
 */
    NhlDraw(lbar);
    NhlDraw(text);
    NhlFrame(wks);
/*
 * This next example changes three entries in the colormap.  Changing the
 * first entry (colormap index 0) in the colormap, sets the background
 * color for a plot. The second entry (color index 1) sets the foreground
 * color for a plot.
 *
 * The colormap is stored in a 3xN variable where N is the length of
 * the colormap.  Each entry in the color map consists of a vector
 * of 3 normalized red-green-blue color values.
 *
 * Assign gray scale colormap
 */
    NhlRLClear(rlist);
    NhlRLSetString(rlist,"wkColorMap","gscyclic");
    NhlSetValues(wks,rlist);
/*
 * Copy the colormap rgb values into the variable cmap
 */
    glist = NhlRLCreate(NhlGETRL);
    NhlRLClear(glist);
    NhlRLGetMDFloatArray(glist,"wkColorMap",&cmap,&num_dims,&len_dims);
    NhlGetValues(wks,glist);
/*
 * Change the first entry in the colormap array to blue, the
 * second to green, and the fourth to red..
 *
 * Background color
 */
    cmap[0*3 + 0] = 0.0;
    cmap[0*3 + 1] = 0.0;
    cmap[0*3 + 2] = 1.0;
/*
 * Foreground color
 */
    cmap[1*3 + 0] = 0.0;
    cmap[1*3 + 1] = 1.0;
    cmap[1*3 + 2] = 0.0;
/*
 * Colormap entry 3
 */
    cmap[3*3 + 0] = 1.0;
    cmap[3*3 + 1] = 0.0;
    cmap[3*3 + 2] = 0.0;
/*
 * Assign the new color map to the workstation object.
 */
    NhlRLClear(rlist);
    NhlRLSetMDFloatArray(rlist,"wkColorMap",cmap,num_dims,len_dims);
    NhlSetValues(wks,rlist);
/*
 * Add a different title.
 */
    NhlRLClear(rlist);
/*
 * Set the title for the labelbar
 */
    NhlRLSetString(rlist,"lbTitleString","Changing colormap entries");
/*
 * Label every entry
 */
    NhlRLSetInteger(rlist, "lbLabelStride", 1);                  
/*
 * Number of entries to display
 */
    NhlRLSetInteger(rlist, "lbBoxCount", 7);

    NhlSetValues(lbar,rlist);
/*
 * Change the textual annotation at bottom of frame
 */
    NhlRLClear(rlist);
/*
 * Set the title for the labelbar
 */
    NhlRLSetString(rlist,"txString","Entry 0 (background) set to Blue");

    NhlSetValues(text,rlist);
/*
 * Draw and frame the labelbar.
 */
    NhlDraw(lbar);
    NhlDraw(text);
    NhlFrame(wks);
/*
 * This next example demonstrates how to create and assign a new colormap.
 *
 * Create an array that will contain the new colormap.  For this example,
 * we are creating a colormap with 100 (arbitrary size) rgb entries.
 *
 * Assign new RGB values to each entry of the colormap.
 * The first entry (background color) is black.  The rest of
 * the colormap is a smooth table that ranges from red to blue.
 */
    newcmap[0*3 + 0] = 0.; /* red component */
    newcmap[0*3 + 1] = 0.; /* green component */
    newcmap[0*3 + 2] = 0.; /* blue component */

    for(i=1; i<100; i++) {
        newcmap[i*3 + 0] = 1.0-(i/99.0);
        newcmap[i*3 + 1] = i/99.0;
        newcmap[i*3 + 2] = i/99.0;
    }
/*
 * Assign the new color map to the workstation object.
 */
    NhlRLClear(rlist);
    num_dims=2;
    len_dims[0]=100;
    len_dims[1]=3;
    NhlRLSetMDFloatArray(rlist,"wkColorMap",newcmap,num_dims,len_dims);
    NhlSetValues(wks,rlist);
/*
 * Assign a new title.
 */
    NhlRLClear(rlist);
/*
 * Set the title for the labelbar
 */
    NhlRLSetString(rlist,"lbTitleString","New colormap");
/*
 * Label every entry
 */
    NhlRLSetInteger(rlist, "lbLabelStride", 10);                  
/*
 * Number of entries to display
 */
    NhlRLSetInteger(rlist, "lbBoxCount", 99);

    NhlSetValues(lbar,rlist);
/*
 * Draw and frame the labelbar
 */
    NhlDraw(lbar);
    NhlFrame(wks);
/* 
 * Clean up
 */
    NhlDestroy(lbar);
    NhlDestroy(text);
    NhlDestroy(wks);

    NhlClose();
    exit (0);
}

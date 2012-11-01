/*************************************************************************
 *                                                                       *
 *                Copyright (C)  1995                                    *
 *        University Corporation for Atmospheric Research                *
 *                All Rights Reserved                                    *
 *                                                                       *
 *************************************************************************
 *
 *   File:         basic06c.c
 *
 *   Author:       Fred Clare
 *                 National Center for Atmospheric Research
 *                 PO 3000, Boulder, Colorado
 *
 *
 *   Date:         Wed May 24 12:54:47 MDT 1995
 *
 *   Description:  This C program demonstrates how to position 
 *                 objects on an output device and how to change 
 *                 their sizes.  A simple color table is also defined 
 *                 and used for changing the color of a curve in an 
 *                 XyPlot.  The script begins with two procedures - 
 *                 one for drawing plot objects and one for drawing 
 *                 text objects.
 */

#include <stdio.h>
#include <math.h>

#include <ncarg/hlu/App.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/hlu/PDFWorkstation.h>
#include <ncarg/hlu/CairoWorkstation.h>
#include <ncarg/hlu/XyPlot.h>
#include <ncarg/hlu/CoordArrays.h>
#include <ncarg/hlu/TickMark.h>
#include <ncarg/hlu/TextItem.h>
#include <ncarg/hlu/DataComm.h>


void draw_plot();
void draw_text();

/*
 * Main program.
 */
int main()
{

    int appid,rlist;
    int xwork_id,text_id,box_id,data_id;
    int dataspec;
    int i;

    char text[6];

    float xdra[] = {0.0, 0.1, 0.5, 0.9, 1.0, 0.9, 0.5, 0.1, 0.0};
    float ydra[] = {0.5, 0.9, 1.0, 0.9, 0.5, 0.1, 0.0, 0.1, 0.5};
    float xpos,ypos;

/*
 * Define a simple color map (index 0 defines the background color).
 */
    float cmap[4][3] = { { 1.0, 1.0, 1.0 },
                 { 0.0, 0.0, 1.0 },
                 { 0.0, 1.0, 0.0 },
                 { 1.0, 0.0, 0.0 } };
    ng_size_t dims[] = {4,3};
/*
 * Set the display. Default is to display output to an X workstation.
 */
    char const *wks_type = "x11";

/*
 * Initialize the high level utility library and create application.
 */
    NhlInitialize();

        rlist = NhlRLCreate(NhlSETRL);
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNappUsrDir,"./");
        NhlCreate(&appid,"basic06",NhlappClass,NhlDEFAULT_APP,rlist);


    if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
    /*
     * Create a meta file workstation.
     */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./basic06c.ncgm");
        NhlRLSetMDFloatArray(rlist,NhlNwkColorMap,&cmap[0][0],2,dims);
        NhlCreate(&xwork_id,"simple",NhlncgmWorkstationClass,
              NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
    /*
     * Create an X workstation.
     */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlRLSetMDFloatArray(rlist,NhlNwkColorMap,&cmap[0][0],2,dims);
        NhlCreate(&xwork_id,"simple",NhlcairoWindowWorkstationClass,
                          NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
    /*
     * Create a PS file workstation.
     */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPSFileName,"./basic06c.ps");
        NhlRLSetMDFloatArray(rlist,NhlNwkColorMap,&cmap[0][0],2,dims);
        NhlCreate(&xwork_id,"simple",NhlpsWorkstationClass,
              NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
    /*
     * Create a PS file workstation.
     */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPDFFileName,"./basic06c.pdf");
        NhlRLSetMDFloatArray(rlist,NhlNwkColorMap,&cmap[0][0],2,dims);
        NhlCreate(&xwork_id,"simple",NhlpdfWorkstationClass,
              NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
             !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
    /*
     * Create a cairo PS/PDF workstation.
     */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"./basic06c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlRLSetMDFloatArray(rlist,NhlNwkColorMap,&cmap[0][0],2,dims);
        NhlCreate(&xwork_id,"simple",NhlcairoDocumentWorkstationClass,
              NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
    /*
     * Create a cairo PNG workstation.
     */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"./basic06c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlRLSetMDFloatArray(rlist,NhlNwkColorMap,&cmap[0][0],2,dims);
        NhlCreate(&xwork_id,"simple",NhlcairoImageWorkstationClass,
              NhlDEFAULT_APP,rlist);
    }

/*
 * Create data object for an XyPlot
 */
    NhlRLClear(rlist);
    NhlRLSetFloatArray(rlist,NhlNcaXArray,xdra,NhlNumber(xdra));
    NhlRLSetFloatArray(rlist,NhlNcaYArray,ydra,NhlNumber(ydra));
    NhlCreate(&data_id,"xyData",NhlcoordArraysClass,NhlDEFAULT_APP,rlist);

/*
 * Create a simple XyPlot object with no labels or borders.  The
 * parent for this object is xwork_id, hence it will be sent to
 * the workstation identified by xwork_id when the draw procedure
 * is invoked on it.
 */
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNtmXBBorderOn,"False");
    NhlRLSetString(rlist,NhlNtmXTBorderOn,"False");
    NhlRLSetString(rlist,NhlNtmYLBorderOn,"False");
    NhlRLSetString(rlist,NhlNtmYRBorderOn,"False");
    NhlRLSetString(rlist,NhlNtmXBOn,"False");
    NhlRLSetString(rlist,NhlNtmXTOn,"False");
    NhlRLSetString(rlist,NhlNtmYLOn,"False");
    NhlRLSetString(rlist,NhlNtmYROn,"False");
    NhlRLSetFloat(rlist,NhlNvpXF,0.0);
    NhlRLSetFloat(rlist,NhlNvpYF,1.0);
    NhlRLSetFloat(rlist,NhlNvpWidthF,1.0);
    NhlRLSetFloat(rlist,NhlNvpHeightF,1.0);
    NhlCreate(&box_id,"Box",NhlxyPlotClass,xwork_id,rlist);

/*
 * Create a TextItem object.
 */
    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNtxPosXF,0.5);
    NhlRLSetFloat(rlist,NhlNtxPosYF,0.5);
    NhlRLSetInteger(rlist,NhlNtxFont,26);
        NhlCreate(&text_id,"Text",NhltextItemClass,xwork_id,rlist);

/*
 * Add the data identified by data_id to the XyPlot.
 */
    dataspec = NhlAddData(box_id,"xyCoordData",data_id);

/*
 * Draw three labeled boxes at different sizes and in different positions
 * and with different colors.
 */
    for(i=1;i<=3;++i)
    {
        xpos = -0.05*i*i + 0.5*i - 0.20;
        ypos = 1.0-xpos;
        sprintf(text,"%s %d","Box",i);  

    /*
     * Specify a text string and its color.
     */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNtxString,text);
        NhlRLSetInteger(rlist,NhlNtxFontColor,4-i);
        NhlSetValues(text_id,rlist);

    /*
     * Set the XyPlot curve color.
     */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNxyMonoLineColor,"True");
        NhlRLSetInteger(rlist,NhlNxyLineColor,i);
        NhlSetValues(dataspec,rlist);

    /*
     * Draw box and text.
     */
        draw_plot(box_id, xpos, ypos, 0.36-0.09*(i-1));
        draw_text(text_id, xpos, ypos, 0.08-0.02*(i-1));
    }

    NhlFrame(xwork_id);

    NhlDestroy(xwork_id);
        NhlClose();
    exit (0);
}


void draw_plot(id,x,y,scale)
int id;
float x,y,scale;
{
/*
 * This procedure takes the plot object with identifier "id" and 
 * draws it centered at coordinate (x,y) and scaled by "scale".  
 * The original plot object is returned unchanged.
 */
    int rlist = NhlRLCreate(NhlSETRL);
    int grlist = NhlRLCreate(NhlGETRL);

    float x_ref,y_ref,width_ref,height_ref;

    NhlRLClear(grlist);
    NhlRLGetFloat(grlist,NhlNvpXF,&x_ref);
    NhlRLGetFloat(grlist,NhlNvpYF,&y_ref);
    NhlRLGetFloat(grlist,NhlNvpWidthF,&width_ref);
    NhlRLGetFloat(grlist,NhlNvpHeightF,&height_ref);
        NhlGetValues(id,grlist);

    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNvpXF,x - 0.5*width_ref*scale);
    NhlRLSetFloat(rlist,NhlNvpYF,y + 0.5*height_ref*scale);
    NhlRLSetFloat(rlist,NhlNvpWidthF,width_ref*scale);
    NhlRLSetFloat(rlist,NhlNvpHeightF,height_ref*scale);
        NhlSetValues(id,rlist);

        NhlDraw(id);

    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNvpXF,x_ref);
    NhlRLSetFloat(rlist,NhlNvpYF,y_ref);
    NhlRLSetFloat(rlist,NhlNvpWidthF,width_ref);
    NhlRLSetFloat(rlist,NhlNvpHeightF,height_ref);
        NhlSetValues(id,rlist);
}

void draw_text(id,x,y,height)
int id;
float x,y,height;
{
/*
 * This procedure takes the text string in the object identified by "id"
 * and draws it centered at coordinate (x,y) with a height of "height".
 */
    int rlist = NhlRLCreate(NhlSETRL);
    int grlist = NhlRLCreate(NhlGETRL);

    float xpos,ypos,fheight;

    NhlRLClear(grlist);
    NhlRLGetFloat(grlist,NhlNtxPosXF,&xpos);
    NhlRLGetFloat(grlist,NhlNtxPosYF,&ypos);
    NhlRLGetFloat(grlist,NhlNtxFontHeightF,&fheight);
        NhlGetValues(id,grlist);

    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNtxPosXF,x);
    NhlRLSetFloat(rlist,NhlNtxPosYF,y);
    NhlRLSetFloat(rlist,NhlNtxFontHeightF,height);
        NhlSetValues(id,rlist);

        NhlDraw(id);

    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNtxPosXF,xpos);
    NhlRLSetFloat(rlist,NhlNtxPosYF,ypos);
    NhlRLSetFloat(rlist,NhlNtxFontHeightF,fheight);
        NhlSetValues(id,rlist);
}

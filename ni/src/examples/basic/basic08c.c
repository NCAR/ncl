/******************************************************************************
 *                                                                            *
 *                             Copyright (C)  1995                            *
 *               University Corporation for Atmospheric Research              *
 *                             All Rights Reserved                            *
 *                                                                            *
 ******************************************************************************
 *    
 *   File:         basic08c.c
 *   Author:       David Brown (translated to C by David Younghans)
 *                 National Center for Atmospheric Research
 *                 PO 3000, Boulder, Colorado  80303
 *
 *   Date:         Wed September 20, 1995
 *
 *   Description:  This C program demonstrates how to draw
 *                 a plot object including any annotations
 *                 outside its viewport within a predefined
 *                 bounded area of NDC space. The procedure
 *                 draw_bounded_plot is intended to be useful in any
 *                 context where it is desired to keep an entire plot
 *                 within predetermined boundaries.
 *
 */

#include <stdio.h>
#include <math.h>

#include <ncarg/hlu/App.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/hlu/PDFWorkstation.h>
#include <ncarg/hlu/CairoWorkstation.h>
#include <ncarg/hlu/LogLinPlot.h>
#include <ncarg/hlu/ContourPlot.h>


void draw_bounded_plot();

/*
 * Main program.
 */
int main()
{

    int appid,rlist,grlist;
    int wid,gid,ll_id,cn_id;

    float x[5];
    float y[5];

/*
 * Set the display. Default is to display output to an X workstation.
 */
    char const *wks_type = "x11";

/*
 * Initialize the high level utility library and create application.
 */
    NhlInitialize();

    rlist = NhlRLCreate(NhlSETRL);
    grlist = NhlRLCreate(NhlGETRL);
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNappUsrDir,"./");
    NhlCreate(&appid,"basic08",NhlappClass,NhlDEFAULT_APP,rlist);


    if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
      /*
       * Create a metafile workstation with the default colormap.
       */
          NhlRLClear(rlist);
          NhlRLSetString(rlist,NhlNwkMetaName,"./basic08c.ncgm");
          NhlCreate(&wid,"simple_ncgm",NhlncgmWorkstationClass,
                    NhlDEFAULT_APP,rlist);
      }
    
    if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
      /*
       * Create an X workstation.
       */
         NhlRLClear(rlist);
         NhlRLSetInteger(rlist,NhlNwkPause,True);
         NhlCreate(&wid,"simple_x11",NhlcairoWindowWorkstationClass,
                   NhlDEFAULT_APP,rlist);
     }

    if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
      /*
       * Create an older-style PostScript workstation.
       */
          NhlRLClear(rlist);
          NhlRLSetString(rlist,NhlNwkPSFileName,"./basic08c.ps");
          NhlCreate(&wid,"simple_ps",NhlpsWorkstationClass,
                    NhlDEFAULT_APP,rlist);
      }
    if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
      /*
       * Create an older-style PDF workstation.
       */
          NhlRLClear(rlist);
          NhlRLSetString(rlist,NhlNwkPDFFileName,"./basic08c.pdf");
          NhlCreate(&wid,"simple_pdf",NhlpdfWorkstationClass,
                    NhlDEFAULT_APP,rlist);
      }
    if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
        !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
      /*
       * Create a cairo PS/PDF workstation.
       */
          NhlRLClear(rlist);
          NhlRLSetString(rlist,NhlNwkFileName,"./basic08c");
          NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
          NhlCreate(&wid,"simple_cairo",NhlcairoDocumentWorkstationClass,
                    NhlDEFAULT_APP,rlist);
      }
    if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
      /*
       * Create a cairo PNG workstation.
       */
          NhlRLClear(rlist);
          NhlRLSetString(rlist,NhlNwkFileName,"./basic08c");
          NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
          NhlCreate(&wid,"simple_cairo",NhlcairoImageWorkstationClass,
                    NhlDEFAULT_APP,rlist);
      }

/* 
 * Create a LogLinPlot object with a viewport that fills the viewspace. This
 * will be used for drawing immediate mode polylines indicating the intended
 * boundary of each plot object.
 */
    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNvpXF,0.0);
    NhlRLSetFloat(rlist,NhlNvpYF,1.0);
    NhlRLSetFloat(rlist,NhlNvpWidthF,1.0);
    NhlRLSetFloat(rlist,NhlNvpHeightF,1.0);
    NhlCreate(&ll_id,"loglin",NhllogLinPlotClass,wid,rlist);

/* 
 * Set GraphicStyle resources to modify the immediate mode line attributes.
 */
    NhlRLClear(grlist);
    NhlRLGetInteger(grlist,NhlNwkDefGraphicStyleId,&gid);
    NhlGetValues(wid,grlist);

    NhlRLClear(rlist);
    NhlRLSetInteger(rlist,NhlNgsLineColor,2);
    NhlRLSetInteger(rlist,NhlNgsLineDashPattern,1);
    NhlSetValues(gid,rlist);
    
/*
 * Create an empty ContourPlot object with a Title, a LabelBar, and a Legend.
 * Note that the viewport is square and covers the whole NDC space,
 * meaning that if the plot were drawn as created, all annotations exterior
 * to the viewport would be outside the viewspace and therefore clipped.
 */
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNpmLabelBarDisplayMode,"always");
    NhlRLSetString(rlist,NhlNpmLegendDisplayMode,"always");
    NhlRLSetString(rlist,NhlNtiMainString,"bounded contour plot");
    NhlRLSetFloat(rlist,NhlNvpXF,0.0);
    NhlRLSetFloat(rlist,NhlNvpYF,1.0);
    NhlRLSetFloat(rlist,NhlNvpHeightF,1.0);
    NhlRLSetFloat(rlist,NhlNvpWidthF,1.0);
    NhlCreate(&cn_id,"contour",NhlcontourPlotClass,wid,rlist);

/*
 * The first frame illustrates drawing the plot with a 5% margin around
 * the viewable area. Draw an immediate mode line indicating the boundary
 * that defines the margin.
 */
    x[0] = 0.05;
    x[1] = 0.95;
    x[2] = 0.95;
    x[3] = 0.05;
    x[4] = 0.05;

    y[0] = 0.05;
    y[1] = 0.05;
    y[2] = 0.95;
    y[3] = 0.95;
    y[4] = 0.05;

    NhlNDCPolyline(ll_id,gid,x,y,5);

/*
 * Draw the plot with the desired boundary parameters.
 */
    draw_bounded_plot(cn_id,1,0.05,0.95,0.05,0.95,rlist);
    NhlFrame(wid);

/*
 * The second frame illustrates use of the draw_bounded_plot procedure
 * to place several plots with varying aspect ratios in a single frame.
 */
    x[0] = 0.025;
    x[1] = 0.475;
    x[2] = 0.475;
    x[3] = 0.025;
    x[4] = 0.025;

    y[0] = 0.525;
    y[1] = 0.525;
    y[2] = 0.975;
    y[3] = 0.975;
    y[4] = 0.525;

    NhlNDCPolyline(ll_id,gid,x,y,5);

/*
 * Set the ContourPlot viewport so that the width is twice the height.
 * (The absolute numbers are not important here, only the ratio matters.)
 */
    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNvpWidthF,0.6);
    NhlRLSetFloat(rlist,NhlNvpHeightF,0.3);
    NhlRLSetString(rlist,NhlNtiMainString,"width is limiting dimension");
    NhlSetValues(cn_id,rlist);

/*
 * Draw the plot with the desired boundary parameters.
 */
    draw_bounded_plot(cn_id,1,0.025,0.475,0.525,0.975,rlist);

/*
 * Draw an immediate mode line indicating the desired boundary of the
 * second plot.
 */  
    x[0] = 0.525;
    x[1] = 0.975;
    x[2] = 0.975;
    x[3] = 0.525;
    x[4] = 0.525;

    y[0] = 0.525;
    y[1] = 0.525;
    y[2] = 0.975;
    y[3] = 0.975;
    y[4] = 0.525;

    NhlNDCPolyline(ll_id,gid,x,y,5);

/*
 * Set the ContourPlot viewport so that the height is twice the width.
 */
    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNvpWidthF,0.3);
    NhlRLSetFloat(rlist,NhlNvpHeightF,0.6);
    NhlRLSetString(rlist,NhlNtiMainString,"height is limiting dimension");
    NhlSetValues(cn_id,rlist);

/*
 * Draw the plot with the desired boundary parameters.
 */
    draw_bounded_plot(cn_id,1,0.525,0.975,0.525,0.975,rlist);

/*
 * Draw an immediate mode line indicating the desired boundary of the
 * third plot.
 */
    x[0] = 0.125;
    x[1] = 0.875;
    x[2] = 0.875;
    x[3] = 0.125;
    x[4] = 0.125;

    y[0] = 0.1;
    y[1] = 0.1;
    y[2] = 0.4;
    y[3] = 0.4;
    y[4] = 0.1;

    NhlNDCPolyline(ll_id,gid,x,y,5);

/*
 * For this plot the aspect ratio is distored in order to fill as much as
 * possible of the desired area. Note that the space is not completely filled.
 * This is because a number of factors affecting the final aspect raio,
 * such as the text size used for titles, are determined based on only
 * one of the viewport's dimensions.
 */
    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNvpHeightF,1.0);
    NhlRLSetFloat(rlist,NhlNvpWidthF,1.0);
    NhlRLSetString(rlist,NhlNtiMainString,"distort aspect ratio to fill area");
    NhlSetValues(cn_id,rlist);

/*
 * Draw the plot with the desired boundary parameters.
 */
    draw_bounded_plot(cn_id,0,0.125,0.875,0.1,0.4,rlist);
    NhlFrame(wid);

/*
 * Clean up.
 */
    NhlDestroy(wid);
    NhlClose();
    exit(0);
}


void draw_bounded_plot(id,keep_aspect,left,right,bottom,top,rlist)
int id,keep_aspect,rlist;
float left,right,bottom,top;

/*
 * This procedure taked the plot object with identifier "id" and
 * draws it within the NDC boundaries represented by left, right,
 * top, and bottom. If keep_aspect is True, the aspect ratio of the plot
 * is preserved: the plot fills the extent of the limiting dimension and
 * is centered within the extent of the other dimension. If keep_aspect is
 * False, the aspect ratio is distorted in order to fill as much of the
 * space as possible, given certain limitations in the ability of some
 * HLU objects to distort themselves to any arbitrary aspect ratio.
 */

{
    int grlist;
    float x_save,y_save,width_save,height_save,
    bb_top,bb_bottom,bb_left,bb_right,bb_height,bb_width,
    frame_height,frame_width,
    factor,x_off,y_off,x,y,width,height;

    struct _NhlBoundingBox box;

    grlist = NhlRLCreate(NhlGETRL);

    NhlRLClear(grlist);
    NhlRLGetFloat(grlist,NhlNvpXF,&x_save);
    NhlRLGetFloat(grlist,NhlNvpYF,&y_save);
    NhlRLGetFloat(grlist,NhlNvpWidthF,&width_save);
    NhlRLGetFloat(grlist,NhlNvpHeightF,&height_save);
    NhlGetValues(id,grlist);

    NhlGetBB(id,&box);

    bb_top = box.t;
    bb_bottom = box.b;
    bb_left = box.l;
    bb_right = box.r;

    bb_height = bb_top - bb_bottom;
    bb_width = bb_right - bb_left;
    frame_height = top - bottom;
    frame_width = right - left;
    x = x_save;
    y = y_save;
    height = height_save;
    width = width_save;

    if (!keep_aspect)
      {
          factor = frame_width / bb_width;
          width = width * factor;
          x_off = (x - bb_left) * factor;
          x = left + x_off;
          factor = frame_height / bb_height;
          height = height * factor;
          y_off = (y - bb_top) * factor;
          y = top + y_off;

          NhlRLClear(rlist);
          NhlRLSetFloat(rlist,NhlNvpXF,x);
          NhlRLSetFloat(rlist,NhlNvpYF,y);
          NhlRLSetFloat(rlist,NhlNvpWidthF,width);
          NhlRLSetFloat(rlist,NhlNvpHeightF,height);
          NhlSetValues(id,rlist);

          NhlRLClear(grlist);
          NhlRLGetFloat(grlist,NhlNvpXF,&x);
          NhlRLGetFloat(grlist,NhlNvpYF,&y);
          NhlRLGetFloat(grlist,NhlNvpWidthF,&width);
          NhlRLGetFloat(grlist,NhlNvpHeightF,&height);
          NhlGetValues(id,grlist);

          NhlGetBB(id,&box);

          bb_top = box.t;
          bb_bottom = box.b;
          bb_right = box.r;
          bb_left = box.l;

          bb_height = bb_top - bb_bottom;
          bb_width = bb_right - bb_left;
      }

    if ((bb_height / bb_width) < (frame_height / frame_width))
      /*
       * Width is the limiting dimension.
       */
      {
         factor =  frame_width / bb_width;
         width = width * factor;
         height = height * factor;
         x_off = (x - bb_left) * factor;
         y_off = (y - bb_top) * factor;
         x = left + x_off;
         bb_height = bb_height * factor;
         y = top + y_off - 0.5 * (frame_height - bb_height);
     }

    else
      /*
       * Height is the limiting dimension.
       */
      {
        factor = frame_height / bb_height;
        height = height * factor;
        width = width * factor;
        x_off = (x - bb_left) * factor;
        y_off = (y - bb_top) * factor;
        bb_width = bb_width * factor;
        x = left + x_off + 0.5 * (frame_width - bb_width);
        y = top + y_off;
       }

    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNvpXF,x);
    NhlRLSetFloat(rlist,NhlNvpYF,y);
    NhlRLSetFloat(rlist,NhlNvpWidthF,width);
    NhlRLSetFloat(rlist,NhlNvpHeightF,height);
    NhlSetValues(id,rlist);

    NhlDraw(id);

    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNvpXF,x_save);
    NhlRLSetFloat(rlist,NhlNvpYF,y_save);
    NhlRLSetFloat(rlist,NhlNvpWidthF,width_save);
    NhlRLSetFloat(rlist,NhlNvpHeightF,height_save);
    NhlSetValues(id,rlist);
}


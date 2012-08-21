/*
 *      $Id: pr03c.c,v 1.9 2010-03-15 22:49:24 haley Exp $
 */
/***********************************************************************
*                                                                      *
*                Copyright (C)  1996                                   *
*        University Corporation for Atmospheric Research               *
*                All Rights Reserved                                   *
*                                                                      *
***********************************************************************/
/*
 *  File:       pr03c.c
 *
 *  Author:     David Brown
 *          	National Center for Atmospheric Research
 *          	PO 3000, Boulder, Colorado
 *
 *  Date:       Mon May 13 11:25:15 MDT 1996
 *
 *  Description:    Demonstrates graphics primitives drawn in NDC and
 *                  data space into a LogLinPlot (and therefore using
 *                  the LogLinTransformation).
 *                  A number of primitives are clipped in order to test
 *                  the way clipping works.
 */

#include <stdio.h>
#include <math.h>
#include <ncarg/gks.h>
#include <ncarg/ncargC.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/hlu/PDFWorkstation.h>
#include <ncarg/hlu/CairoWorkstation.h>
#include <ncarg/hlu/LogLinPlot.h>

int main(int argc, char *argv[])

{
    char const *wks_type = "x11";
    int rlist,grlist;
    int appid,wid,canvas,gsid,id;
    int i;
    float plx[] = { 0.1,0.9,0.5,0.1 };
    float ply[] = { 0.1,0.1,0.9,0.1 };
    float pmx[] = { 0.05,0.95,0.5,0.5 };
    float pmy[] = { 0.05,0.05,1.05,0.5 };
    float pgx[] = { 0.2,0.8,0.5,0.2 };
    float pgy[] = { 0.25,0.25,0.85,0.25 };
    float dpgx[] = { 10.0,110.0,110.0,5.0,110.0,10.0,10.0 };
    float dpgy[] = { 1.,1.,20.0,20.,110.,110,1.0 };
/*
 * Initialize the high level utility library
 */
    NhlInitialize();
/*
 * Create an application context. Set the app dir to the current
 * directory so the application looks for a resource file in the working
 * directory. 
 */
    rlist = NhlRLCreate(NhlSETRL);
    grlist = NhlRLCreate(NhlGETRL);
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNappUsrDir,"./");
    NhlCreate(&appid,"pr03",NhlappClass,NhlDEFAULT_APP,rlist);

    if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
/*
 * Create a meta file workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./pr03c.ncgm");
        NhlCreate(&wid,"pr03cWork",
                  NhlncgmWorkstationClass,NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 * Create an X workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlCreate(&wid,"pr03cWork",NhlcairoWindowWorkstationClass,appid,rlist);
    }

    else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 * Create an older-style PostScript workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPSFileName,"pr03c.ps");
        NhlCreate(&wid,"pr03cWork",NhlpsWorkstationClass,appid,rlist);
    }
    else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 * Create an older-style PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPDFFileName,"pr03c.pdf");
        NhlCreate(&wid,"pr03cWork",NhlpdfWorkstationClass,appid,rlist);
    }
    else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
             !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 * Create a cairo PS/PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"pr03c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"pr03cWork",NhlcairoDocumentWorkstationClass,appid,rlist);
    }
    else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 * Create a cairo PNG workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"pr03c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"pr03cWork",NhlcairoImageWorkstationClass,appid,rlist);
    }

/*
 * Get the workstation default GraphicStyle and change the value of
 * one of its resources
 */
    NhlRLGetInteger(grlist,NhlNwkDefGraphicStyleId,&id);
    NhlGetValues(wid,grlist);
    NhlRLClear(rlist);
    NhlRLSetInteger(rlist,NhlNgsLineColor,2);
    NhlSetValues(id,rlist);
/*
 * Create a LogLinPlot that covers the entire NDC space 
 * to use as a drawing canvas
 */
    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNvpXF,0.0);
    NhlRLSetFloat(rlist,NhlNvpYF,1.0);
    NhlRLSetFloat(rlist,NhlNvpWidthF,1.0);
    NhlRLSetFloat(rlist,NhlNvpHeightF,1.0);
    NhlCreate(&canvas,"canvas",NhllogLinPlotClass,wid,rlist);
/*
 * Explicitly create a GraphicStyle
 */
    NhlRLClear(rlist);
    NhlCreate(&gsid,"style",NhlgraphicStyleClass,wid,rlist);
/*
 * Set workstation line attributes and draw a triangle
 */
    NhlRLClear(rlist);
    NhlRLSetInteger(rlist,NhlNgsLineDashPattern,2);
    NhlRLSetFloat(rlist,NhlNgsLineThicknessF,3.0);
    NhlSetValues(gsid,rlist);

    NhlDraw(canvas);
/*
 * The polyline call uses the default GraphicStyle; Polygon and
 * Polymarker use the explicitly created GraphicStyle.
 */
    NhlNDCPolyline(canvas,gsid,plx,ply,4);
    NhlNDCPolygon(canvas,gsid,pgx,pgy,4);
    NhlNDCPolymarker(canvas,gsid,pmx,pmy,4);
    NhlFrame(wid);

    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNvpXF,0.2);
    NhlRLSetFloat(rlist,NhlNvpYF,0.8);
    NhlRLSetFloat(rlist,NhlNvpWidthF,0.6);
    NhlRLSetFloat(rlist,NhlNvpHeightF,0.6);
    NhlSetValues(canvas,rlist);
       
    NhlDraw(canvas);
    NhlNDCPolyline(canvas,gsid,plx,ply,4);
    NhlNDCPolygon(canvas,gsid,pgx,pgy,4);
    NhlNDCPolymarker(canvas,gsid,pmx,pmy,4);
    NhlFrame(wid);

    for (i=0; i<4; i++) {
        plx[i] += 0.4;
        ply[i] += 0.4;
        pgx[i] += 0.4;
        pgy[i] += 0.4;
        pmx[i] += 0.4;
        pmy[i] += 0.4;
    }
       
    NhlDraw(canvas);
    NhlNDCPolyline(canvas,gsid,plx,ply,4);
    NhlNDCPolygon(canvas,gsid,pgx,pgy,4);
    NhlNDCPolymarker(canvas,gsid,pmx,pmy,4);
    NhlFrame(wid);

    for (i=0; i<4; i++) {
        plx[i] -= 0.4;
        ply[i] -= 0.4;
        pgx[i] -= 0.4;
        pgy[i] -= 0.4;
        pmx[i] -= 0.4;
        pmy[i] -= 0.4;
    }
/* 
 * Modify the data values to be in a meaningful range relative to the
 * data coordinate extent.
 */
    for (i=0; i<4; i++) {
        plx[i] = plx[i] * 100.0 + 50.;
        ply[i] = ply[i] * 100.0 + 50.;
        pgx[i] = pgx[i] * 100.0 + 50.;
        pgy[i] = pgy[i] * 100.0 + 50.;
        pmx[i] = pmx[i] * 100.0 + 50.;
        pmy[i] = pmy[i] * 100.0 + 50.;
    }

    NhlDraw(canvas);
    NhlDataPolyline(canvas,gsid,plx,ply,4);
    NhlDataPolymarker(canvas,gsid,pmx,pmy,4);
    NhlDataPolygon(canvas,gsid,pgx,pgy,4);
    NhlFrame(wid);

    for (i=0; i<4; i++) {
        plx[i] = plx[i] - 80.;
        ply[i] = ply[i] - 80.;
        pgx[i] = pgx[i] - 80.;
        pgy[i] = pgy[i] - 80.;
        pmx[i] = pmx[i] - 80.;
        pmy[i] = pmy[i] - 80.;
    }

    NhlDraw(canvas);
    NhlDataPolyline(canvas,gsid,plx,ply,4);
    NhlDataPolymarker(canvas,gsid,pmx,pmy,4);
    NhlDataPolygon(canvas,gsid,pgx,pgy,4);
    NhlFrame(wid);

    NhlDraw(canvas);
    NhlDataPolygon(canvas,gsid,dpgx,dpgy,7);
    NhlFrame(wid);

    NhlClose();
    exit(0);
}

/*
 *      $Id: pr04c.c,v 1.2 1996-06-28 20:55:38 haley Exp $
 */
/***********************************************************************
*                                                                      *
*                Copyright (C)  1993                                   *
*        University Corporation for Atmospheric Research               *
*                All Rights Reserved                                   *
*                                                                      *
***********************************************************************/
/*
 *  File:       pr04c.c
 *
 *  Author:     David Brown
 *          	National Center for Atmospheric Research
 *          	PO 3000, Boulder, Colorado
 *
 *  Date:       Wed May 22 19:16:58 MDT 1996
 *
 *  Description:    Demonstrates graphics primitives drawn in NDC and 
 *                  and data space into an IrregularPlot (and therefore 
 *                  using the IrregularTransObj).
 */

#include <stdio.h>
#include <math.h>
#include <ncarg/gks.h>
#include <ncarg/ncargC.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/hlu/XWorkstation.h>
#include <ncarg/hlu/IrregularPlot.h>


main(int argc, char *argv[])

{
    int NCGM=0, X11=1, PS=0;
    int rlist,grlist;
    int appid,wid,canvas;
    int i;
    float plx[5] = { 0.1,0.9,0.5,0.1 };
    float ply[5] = { 0.1,0.1,0.9,0.1 };
    float pmx[4] = { 0.05,0.95,0.5,0.5 };
    float pmy[4] = { 0.05,0.05,1.05,0.5 };
    float pgx[4] = { 0.2,0.8,0.5,0.2 };
    float pgy[4] = { 0.25,0.25,0.85,0.25 };
    float dpgx[7] = { 5.0,110.0,110.0,0.0,110.0,5.0,5.0 };
    float dpgy[7] = { 10.,10.,20.0,20.,110.,110,10.0 };

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
    NhlCreate(&appid,"pr04",NhlappClass,NhlDEFAULT_APP,rlist);

    if (NCGM) {
/*
 * Create a meta file workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./pr04c.ncgm");
        NhlCreate(&wid,"pr04Work",
                  NhlncgmWorkstationClass,NhlDEFAULT_APP,rlist);
    }
    else if (X11) {
/*
 * Create an X workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlCreate(&wid,"pr04Work",NhlxWorkstationClass,appid,rlist);
    }

    else if (PS) {
/*
 * Create a PS workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPSFileName,"pr04c.ps");
        NhlCreate(&wid,"pr04Work",NhlpsWorkstationClass,appid,rlist);
    }
/*
 * Create an IrregularPlot that covers the entire NDC space 
 * to use as a drawing canvas
 */
    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNvpXF,0.1);
    NhlRLSetFloat(rlist,NhlNvpYF,0.9);
    NhlRLSetFloat(rlist,NhlNvpWidthF,0.8);
    NhlRLSetFloat(rlist,NhlNvpHeightF,0.8);
    NhlRLSetString(rlist,NhlNpmTitleDisplayMode,"always");
    NhlRLSetString(rlist,NhlNtiMainString,
		   "Irregular Plot with NDC Primitives");
    NhlCreate(&canvas,"canvas",NhlirregularPlotClass,wid,rlist);

    NhlDraw(canvas);
    NhlNDCPolyline(canvas,0,plx,ply,4);
    NhlNDCPolygon(canvas,0,pgx,pgy,4);
    NhlNDCPolymarker(canvas,0,pmx,pmy,4);
    NhlFrame(wid);

    for (i=0; i<4; i++) {
	    plx[i] = plx[i] * 100.0 + 50.;
	    ply[i] = ply[i] * 100.0 + 50.;
	    pgx[i] = pgx[i] * 100.0 + 50.;
	    pgy[i] = pgy[i] * 100.0 + 50.;
	    pmx[i] = pmx[i] * 100.0 + 50.;
	    pmy[i] = pmy[i] * 100.0 + 50.;
    }

    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNtiMainString,
		   "Clipped Data Space Primitives");
    NhlSetValues(canvas,rlist);

    NhlDraw(canvas);
    NhlDataPolyline(canvas,0,plx,ply,4);
    NhlDataPolymarker(canvas,0,pmx,pmy,4);
    NhlDataPolygon(canvas,0,pgx,pgy,4);
    NhlFrame(wid);

    for (i=0; i<4; i++) {
	    plx[i] = plx[i] - 40.;
	    ply[i] = ply[i] - 40.;
	    pgx[i] = pgx[i] - 40.;
	    pgy[i] = pgy[i] - 40.;
	    pmx[i] = pmx[i] - 40.;
	    pmy[i] = pmy[i] - 40.;
    }

    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNtiMainString,
		   "Data Space Primitives Repositioned");
    NhlSetValues(canvas,rlist);

    NhlDraw(canvas);
    NhlDataPolyline(canvas,0,plx,ply,4);
    NhlDataPolymarker(canvas,0,pmx,pmy,4);
    NhlDataPolygon(canvas,0,pgx,pgy,4);
    NhlFrame(wid);


    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNtiMainString,
		   "A Diamond in Data Space");
    NhlSetValues(canvas,rlist);
    NhlDraw(canvas);
    plx[0] = 10.0;
    plx[1] = 50.0;
    plx[2] = 90.0;
    plx[3] = 50.0;
    plx[4] = 10.0;
    ply[0] = 50.0;
    ply[1] = 10.0;
    ply[2] = 50.0;
    ply[3] = 90.0;
    ply[4] = 50.0;
    NhlDataPolygon(canvas,0,plx,ply,4);
    plx[0] = 5.0;
    plx[2] = 95.0;
    plx[4] = 5.0;
    ply[1] = 5.0;
    ply[3] = 95.0;
    NhlDataPolyline(canvas,0,plx,ply,5);
    NhlFrame(wid);

    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNtiMainString,
		   "A Self-Intersecting Data Polygon");
    NhlSetValues(canvas,rlist);
    NhlDraw(canvas);
    NhlDataPolygon(canvas,0,dpgx,dpgy,7);
    NhlFrame(wid);

    NhlClose();
    exit(0);
}

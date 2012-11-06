/*
 *      $Id: st03c.c,v 1.7 2010-03-15 22:49:24 haley Exp $
 */
/***********************************************************************
*                                                                      *
*                Copyright (C)  1996                                   *
*        University Corporation for Atmospheric Research               *
*                All Rights Reserved                                   *
*                                                                      *
***********************************************************************/
/*
 *  File:       st03c.c
 *
 *  Author:     David Brown (converted by Mary Haley)
 *              National Center for Atmospheric Research
 *              PO 3000, Boulder, Colorado
 *
 *  Date:       Thu June 27 8:46:55 MST 1996
 *
 *  Description: 
 *               This plot shows a StreamlinePlot overlaid on a polar
 *               stereographic map projection. It illustrates some of
 *               the problems with streamlines when the transformation to
 *               NDC results in grid cells that vary widely in size.
 *               Adjustment of certain parameters may improve the
 *               appearance somewhat, but not as much as might be
 *               desired. For this and other reasons, StreamlinePlot is
 *               still undergoing development and its output may be
 *               expected to change in the next release.
 *               The data is extracted from an NMC forecast dataset for 
 *               11/10/1994.
 */

#include <math.h>
#include <ncarg/gks.h>
#include <ncarg/ncargC.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/hlu/PDFWorkstation.h>
#include <ncarg/hlu/CairoWorkstation.h>
#include <ncarg/hlu/StreamlinePlot.h>
#include <ncarg/hlu/MapPlot.h>
#include <netcdf.h>

int main(int argc, char *argv[])
{
    char const *wks_type = "x11";
    int appid, wid, dataid, stid, mpid;
    int rlist, grlist;
    ng_size_t len_dims[2];
    long  start[2], count[2], lonlen, latlen;
    int ncid, uid, vid, latid, lonid;
    float *U, *V, stepsize, spacing;
    char  filename[256];
    const char *dir = _NGGetNCARGEnv("data");
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
    NhlCreate(&appid,"st03",NhlappClass,NhlDEFAULT_APP,rlist);

    if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
/*
 * Create a meta file workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./st03c.ncgm");
        NhlCreate(&wid,"st03Work",
                  NhlncgmWorkstationClass,NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 * Create an X workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlCreate(&wid,"st03Work",NhlcairoWindowWorkstationClass,appid,rlist);
    }

    else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 * Create an older-style PostScript workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPSFileName,"st03c.ps");
        NhlCreate(&wid,"st03Work",NhlpsWorkstationClass,appid,rlist);
    }
    else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 * Create an older-style PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPDFFileName,"st03c.pdf");
        NhlCreate(&wid,"st03Work",NhlpdfWorkstationClass,appid,rlist);
    }
    else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
             !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 * Create a cairo PS/PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"st03c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"st03Work",NhlcairoDocumentWorkstationClass,appid,rlist);
    }
    else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 * Create a cairo PNG workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"st03c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"st03Work",NhlcairoImageWorkstationClass,appid,rlist);
    }
/*
 * Open the netCDF file.
 */
    sprintf( filename, "%s/cdf/941110_UV.cdf", dir );
    ncid = ncopen(filename,NC_NOWRITE);
/*
 * Get the data.
 */
    latid = ncdimid(ncid,"lat");
    lonid = ncdimid(ncid,"lon");
    ncdiminq(ncid,latid,(char *)0,&latlen);
    ncdiminq(ncid,lonid,(char *)0,&lonlen);
    uid = ncvarid(ncid,"u");
    vid = ncvarid(ncid,"v");

    start[0] = start[1] = 0;
    count[0] = latlen; count[1] = lonlen;
    U = (float *)malloc(sizeof(float)*latlen*lonlen);
    V = (float *)malloc(sizeof(float)*latlen*lonlen);
    ncvarget(ncid,uid,(long const *)start,(long const *)count,U);
    ncvarget(ncid,vid,(long const *)start,(long const *)count,V);
/*
 * Create a VectorField data object using the data set defined above.
 * By default the array bounds will define the data boundaries (zero-based,
 * as in C language conventions)
 */
    len_dims[0] = latlen;
    len_dims[1] = lonlen;
    NhlRLClear(rlist);
    NhlRLSetMDFloatArray(rlist,NhlNvfUDataArray,U,2,len_dims);
    NhlRLSetMDFloatArray(rlist,NhlNvfVDataArray,V,2,len_dims);
    NhlRLSetFloat(rlist,NhlNvfXCStartV, -180.0);
    NhlRLSetFloat(rlist,NhlNvfXCEndV, 180.0);
    NhlRLSetFloat(rlist,NhlNvfYCStartV, -90.0);
    NhlRLSetFloat(rlist,NhlNvfYCEndV, 90.0);
    NhlRLSetFloat(rlist,NhlNvfYCStartSubsetV, 0.0);
    NhlRLSetFloat(rlist,NhlNvfYCEndSubsetV, 87.5);
    NhlCreate(&dataid,"vfield",NhlvectorFieldClass,appid,rlist);
/*
 * Create a StreamlinePlot object, supplying the VectorField object as data
 */
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNtiMainString,
           "StreamlinePlot Overlaying MapPlot");
    NhlRLSetInteger(rlist,NhlNstVectorFieldData,dataid);
    NhlCreate(&stid,"streamlineplot",NhlstreamlinePlotClass,wid,rlist);

    NhlCreate(&mpid,"mapplot",NhlmapPlotClass,wid,0);

    NhlAddOverlay(mpid,stid,-1);
    NhlDraw(mpid);
    NhlFrame(wid);
      
    NhlRLClear(grlist);
    NhlRLGetFloat(grlist,NhlNstStepSizeF,&stepsize);
    NhlRLGetFloat(grlist,NhlNstMinLineSpacingF,&spacing);
    NhlGetValues(stid,grlist);
/* 
 * Set the minimum arrow spacing
 */
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNtiMainString,"Setting the Minimum Arrow Spacing");
    NhlRLSetFloat(rlist,NhlNstMinArrowSpacingF,0.025);
    NhlSetValues(stid,rlist);

    NhlDraw(mpid);
    NhlFrame(wid);
/* 
 * Set the minimum line spacing
 */
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNtiMainString,"Smaller Line Spacing");
    NhlRLSetFloat(rlist,NhlNstMinLineSpacingF,spacing * 0.5);
    NhlSetValues(stid,rlist);

    NhlDraw(mpid);
    NhlFrame(wid);
/* 
 * Set the step size
 */
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNtiMainString,"Smaller Step Size");
    NhlRLSetFloat(rlist,NhlNstStepSizeF,stepsize * 0.5);
    NhlSetValues(stid,rlist);

    NhlDraw(mpid);
    NhlFrame(wid);
/* 
 * Clean up
 */
    NhlDestroy(appid);
    NhlClose();
    exit(0);
}

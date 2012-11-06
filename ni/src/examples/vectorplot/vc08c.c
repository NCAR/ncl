/*
 *      $Id: vc08c.c,v 1.7 2010-03-15 22:49:25 haley Exp $
 */
/***********************************************************************
*                                                                      *
*                Copyright (C)  1996                                   *
*        University Corporation for Atmospheric Research               *
*                All Rights Reserved                                   *
*                                                                      *
***********************************************************************/
/*
 *  File:       vc08c.c
 *
 *   Author:     Bob Lackman
 *               National Center for Atmospheric Research
 *               PO 3000, Boulder, Colorado
 *
 *   Date:       Wed Jan 24, 1996
 *
 *   Description:    Plots wind vectors at a grid stride of 3.
 *                   Vectors are colored by wind speed.
 */

#include <math.h>
#include <stdio.h>
#include <ncarg/gks.h>
#include <ncarg/ncargC.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/hlu/PDFWorkstation.h>
#include <ncarg/hlu/CairoWorkstation.h>
#include <ncarg/hlu/VectorPlot.h>
#include <ncarg/hlu/VectorField.h>
#include <ncarg/hlu/TextItem.h>
#include <ncarg/hlu/MapPlot.h>
#include <netcdf.h>

#define NLON  129
#define NLAT  64
      
int vccolors[14] = {26,28,30,33,36,39,42,45,48,51,54,56,58,60};

int main()
{
    char const *wks_type = "x11";
    int appid,wid,vcid,vfield,mapid,txid1,txid2;
    int rlist;
    float U[NLAT][NLON],V[NLAT][NLON];
/*
 * Declare variables for getting information from netCDF file.
 */
    int vf, u_id, v_id, lon_id, lat_id;
    int  i;
    long lonlen, latlen, start[4], count[4];
    ng_size_t icount[2];
    float lon[NLON], lat[NLAT];
    char filename[256];
    const char *dir = _NGGetNCARGEnv("data");
    char title[256], subtitle[256], txtstring[256];

/*
 * Initialize the char arrays.
 */
    for (i = 0; i < 256; i++) {
      title[i] = '\0';
      subtitle[i] = '\0';
      txtstring[i] = '\0';
    }
/*
 * Open the netCDF file.
 */
    sprintf( filename, "%s/cdf/ex01B1_uv300.hs.nc", dir );
    vf = ncopen(filename,NC_NOWRITE);
/*
 * Initialize the high level utility library
 *
 * Create an application context. Set the app dir to the current
 * directory so the application looks for a resource file in the working
 * directory. 
 */
    NhlInitialize();
    rlist = NhlRLCreate(NhlSETRL);
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNappDefaultParent,"True");
    NhlRLSetString(rlist,NhlNappUsrDir,"./");
    NhlCreate(&appid,"vc08",NhlappClass,0,rlist);

    if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
/*
 * Create a meta file workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkColorMap,"uniform");
        NhlRLSetString(rlist,NhlNwkMetaName,"./vc08c.ncgm");
        NhlCreate(&wid,"vc08Work",
                  NhlncgmWorkstationClass,NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 * Create an X workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkColorMap,"uniform");
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlCreate(&wid,"vc08Work",NhlcairoWindowWorkstationClass,appid,rlist);
    }

    else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 * Create an older-style PostScript workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkColorMap,"uniform");
        NhlRLSetString(rlist,NhlNwkPSFileName,"vc08c.ps");
        NhlCreate(&wid,"vc08Work",NhlpsWorkstationClass,appid,rlist);
    }
    else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 * Create an older-style PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkColorMap,"uniform");
        NhlRLSetString(rlist,NhlNwkPDFFileName,"vc08c.pdf");
        NhlCreate(&wid,"vc08Work",NhlpdfWorkstationClass,appid,rlist);
    }
    else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
             !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 * Create a cairo PS/PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkColorMap,"uniform");
        NhlRLSetString(rlist,NhlNwkFileName,"vc08c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"vc08Work",NhlcairoDocumentWorkstationClass,appid,rlist);
    }
    else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 * Create a cairo PNG workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkColorMap,"uniform");
        NhlRLSetString(rlist,NhlNwkFileName,"vc08c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"vc08Work",NhlcairoImageWorkstationClass,appid,rlist);
    }
/*
 * Get netCDF file information.
 */
    lat_id = ncdimid(vf,"latitude");
    lon_id = ncdimid(vf,"longitude");
    u_id = ncvarid(vf,"U");
    v_id = ncvarid(vf,"V");
    ncdiminq(vf,lat_id,(char *)0,&latlen);
    ncdiminq(vf,lon_id,(char *)0,&lonlen);
/*
 * Get longitude and latitude values.
 */
    lat_id = ncvarid(vf,"lat");
    lon_id = ncvarid(vf,"lon");
    start[0] = 0;
    count[0] = latlen;
    ncvarget(vf,lat_id,(long const *)start,(long const *)count,lat);
    count[0] = lonlen;
    ncvarget(vf,lon_id,(long const *)start,(long const *)count,lon);
/*
 * Get U and V data values.
 */
    start[0] = start[1] = start[2] = start[3] = 0;
    count[0] = count[1] = 1;
    count[2] = latlen; count[3] = lonlen;
    ncvarget(vf,u_id,(long const *)start,(long const *)count,U);
    ncvarget(vf,v_id,(long const *)start,(long const *)count,V);
/*
 * Get the titles (global attributes).
 */
    ncattget(vf,NC_GLOBAL,"title",title);
    ncattget(vf,NC_GLOBAL,"sub_title_rhs",subtitle);
/*
 * Close the netCDF file.
 */
    ncclose(vf);
/*
 * Create a VectorField data object, U and V are 4-D arrays which
 * are a function of time, level, latitude and longitude.  Use the
 * 1st time & level.
 */
    icount[0] = latlen;
    icount[1] = lonlen;

    NhlRLClear(rlist);
    NhlRLSetMDFloatArray(rlist,NhlNvfUDataArray,&U[0][0],2,icount);
    NhlRLSetMDFloatArray(rlist,NhlNvfVDataArray,&V[0][0],2,icount);
    NhlRLSetFloat(rlist,NhlNvfXCStartV,lon[0]);
    NhlRLSetFloat(rlist,NhlNvfXCEndV, lon[lonlen-1]);
    NhlRLSetFloat(rlist,NhlNvfYCStartV,lat[0]);
    NhlRLSetFloat(rlist,NhlNvfYCEndV,lat[latlen-1]);
/*
 * Specify a stride of 3 in both dimensions
 */
    NhlRLSetInteger(rlist,NhlNvfXCStride,3);
    NhlRLSetInteger(rlist,NhlNvfYCStride,3);
    NhlCreate(&vfield,"VectorField",NhlvectorFieldClass,appid,rlist);
/*
 * Create a VectorPlot object "vcid" and connect the data object "vfield".
 */
    sprintf(txtstring,"$VMG$ %s",subtitle);
    NhlRLClear(rlist);
    NhlRLSetInteger(rlist,NhlNvcVectorFieldData,vfield);
    NhlRLSetFloat(rlist,NhlNvcMinFracLengthF,0.33);
    NhlRLSetFloat(rlist,NhlNvcRefLengthF,0.025);
    NhlRLSetString(rlist,NhlNvcRefAnnoString1,txtstring);
    NhlRLSetString(rlist,NhlNvcMonoLineArrowColor,"false");
    NhlRLSetString(rlist,NhlNpmLabelBarDisplayMode,"always");
    NhlRLSetString(rlist,NhlNpmLabelBarSide,"bottom");
    NhlRLSetString(rlist,NhlNlbOrientation,"horizontal");
    NhlRLSetString(rlist,NhlNlbTitleString,subtitle);
    NhlRLSetIntegerArray(rlist,NhlNvcLevelColors,vccolors,14);
/*    NhlFRLSetFloat(rlist,NhlNvcLineArrowThicknessF,1.75);  */
    NhlCreate(&vcid,"vectorplot",NhlvectorPlotClass,wid,rlist);
/*
 * Create a map object.
 */
    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNvpXF,0.05);
    NhlRLSetFloat(rlist,NhlNvpWidthF,0.9);
    NhlRLSetFloat(rlist,NhlNvpYF,0.85);
    NhlRLSetString(rlist,NhlNmpGridAndLimbDrawOrder,"predraw");
    NhlCreate(&mapid,"map",NhlmapPlotClass,wid,rlist);

    NhlAddOverlay(mapid,vcid,-1);
    NhlDraw(mapid);
/*
 * Create a text item object as a main title.
 */         
    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNtxPosXF,0.5);
    NhlRLSetFloat(rlist,NhlNtxPosYF,0.85);
    NhlRLSetString(rlist,NhlNtxJust,"CENTERCENTER");
    NhlRLSetString(rlist,NhlNtxString,title);
    NhlRLSetFloat(rlist,NhlNtxFontHeightF,.030);
    NhlRLSetInteger(rlist,NhlNtxFont,25);
    NhlCreate(&txid1,"main",NhltextItemClass,wid,rlist);
/*
 * Create a subheader text item object.
 */
    sprintf(txtstring,"Wind                                                          300mb                                                       (m/s)");
    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNtxPosXF,0.5);
    NhlRLSetFloat(rlist,NhlNtxPosYF,0.80);
    NhlRLSetString(rlist,NhlNtxJust,"CENTERCENTER");
    NhlRLSetString(rlist,NhlNtxString,txtstring);
    NhlRLSetFloat(rlist,NhlNtxFontHeightF,.015);
    NhlRLSetInteger(rlist,NhlNtxFont,25);
    NhlCreate(&txid2,"text",NhltextItemClass,wid,rlist);
    NhlDraw(txid1);
    NhlDraw(txid2);
    NhlFrame(wid);
/*
 * Destroy the objects created, close the HLU library and exit.
 */
    NhlDestroy(wid);
    NhlDestroy(appid);
    NhlClose();
    exit(0);
}


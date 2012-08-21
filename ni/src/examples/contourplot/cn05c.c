/*
**      $Id: cn05c.c,v 1.9 2010-03-15 22:49:23 haley Exp $
*/
/***********************************************************************
*                                                                      *
*                Copyright (C)  1995                                   *
*        University Corporation for Atmospheric Research               *
*     The use of this Software is governed by a License Agreement      *
*                                                                      *
***********************************************************************/
/*
**  File:       cn05c.c
**
**  Author:     Tim Scheitln (converted to C by Mary Haley)
**          National Center for Atmospheric Research
**          PO 3000, Boulder, Colorado
**
**  Date:       Tue Sep 26 09:44:01 MDT 1995
**
**   Description: Demonstrates how to create a map plot animation with a 
**                contour overlay and labelbar annotation.
**
*/

#include <stdio.h>
#include <math.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/ResList.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/hlu/PDFWorkstation.h>
#include <ncarg/hlu/CairoWorkstation.h>
#include <ncarg/hlu/ContourPlot.h>
#include <ncarg/hlu/MapPlot.h>
#include <ncarg/hlu/ScalarField.h>
#include <netcdf.h>

/*
 * The PLOT_ALL_DATA flag controls whether or not all 31 days of data is 
 * read and plotted.  Setting the flag to 1 will cause this script to 
 * plot 31 frames.  Otherwise, the script will read only 3 days worth of 
 * data and plot 3 frames.
 */
#define PLOT_ALL_DATA  0

/*
 * Define the maximum number of colors.
 */
#define NCOLORS    64

int main()
{
/*
 * Declare variables for the HLU routine calls.
 */
    int     appid, workid, field1, con1;
    int     mapid, lb1id, lb2id;
    int     srlist, i, day;
    ng_size_t  icount[2];
/*
 * Declare variables for defining color map.
 */
    ng_size_t     length[2];
    float   cmap[NCOLORS][3];
    extern void gen_colormap();
/*
 * Create an array that will contain the indices into the 
 * colormap defined later.
 */
    int fillindices[NCOLORS-2];
/*
 * Declare variables for getting information from netCDF file.
 */
    int   ncid, t_id, lon_id, lat_id, time_id;
    float T[40][49];
    int   lon[49], lat[40];
    long  start[3], count[3], lonlen, latlen, nframes;
    char  filename[256], daystr[10];
    const char *dir = _NGGetNCARGEnv("data");
/*
 * Default is to create an NCGM file.
 */
    char const *wks_type = "ncgm";
/*
 * Open the netCDF file.
 */
    sprintf( filename, "%s/cdf/meccatemp.cdf", dir );
    ncid = ncopen(filename,NC_NOWRITE);
/*
 * Initialize the HLU library and set up resource template.
 */
    NhlInitialize();
    srlist = NhlRLCreate(NhlSETRL);
/*
 * Create Application object.
 */
    NhlRLClear(srlist);
    NhlRLSetString(srlist,NhlNappDefaultParent,"True");
    NhlRLSetString(srlist,NhlNappUsrDir,"./");
    NhlCreate(&appid,"cn05",NhlappClass,NhlDEFAULT_APP,srlist);

    if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
/*
 * Create a meta file object.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkMetaName,"./cn05c.ncgm");
        NhlCreate(&workid,"cn05Work",NhlncgmWorkstationClass,
                  NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 * Create an X11 workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetInteger(srlist,NhlNwkPause,True);
        NhlCreate(&workid,"cn05Work",NhlcairoWindowWorkstationClass,
              NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 * Create an older-style PostScript workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkPSFileName,"./cn05c.ps");
        NhlCreate(&workid,"cn05Work",NhlpsWorkstationClass,
                  NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 * Create an older-style PDF workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkPDFFileName,"./cn05c.pdf");
        NhlCreate(&workid,"cn05Work",NhlpdfWorkstationClass,
                  NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
             !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 * Create a cairo PS/PDF workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkFileName,"./cn05c");
        NhlRLSetString(srlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&workid,"cn05Work",NhlcairoDocumentWorkstationClass,
                  NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 * Create a cairo PNG workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkFileName,"./cn05c");
        NhlRLSetString(srlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&workid,"cn05Work",NhlcairoImageWorkstationClass,
                  NhlDEFAULT_APP,srlist);
    }
/*
 * Generate a colormap.
 */
    gen_colormap(cmap);
/*
 * Assign the colormap to the workstation.
 */
    length[0] = NCOLORS;  length[1] = 3;
    NhlRLClear(srlist);
    NhlRLSetMDFloatArray(srlist,NhlNwkColorMap,&cmap[0][0],2,length);
    NhlSetValues(workid,srlist);
/*
 * Get the temperature and lat/lon dimensions.
 */
    lat_id = ncdimid(ncid,"lat");
    lon_id = ncdimid(ncid,"lon");
    t_id = ncvarid(ncid,"t");
    ncdiminq(ncid,lat_id,(char *)0,&latlen);
    ncdiminq(ncid,lon_id,(char *)0,&lonlen);
    lat_id = ncvarid(ncid,"lat");
    lon_id = ncvarid(ncid,"lon");
/*
 * Get temperature and lat/lon values.
 */
    start[0] = start[1] = start[2] = 0;
    count[0] = 1; count[1] = latlen; count[2] = lonlen;
    ncvarget(ncid,t_id,(long const *)start,(long const *)count,T);
    count[0] = latlen;
    ncvarget(ncid,lat_id,(long const *)start,(long const *)count,lat);
    count[0] = lonlen;
    ncvarget(ncid,lon_id,(long const *)start,(long const *)count,lon);
/*
 * Create a scalar field object that will be used as the
 * dataset for the contour object.
 */
    icount[0] = latlen; icount[1] = lonlen;
    NhlRLClear(srlist);
    NhlRLSetMDFloatArray(srlist,NhlNsfDataArray,&T[0][0],2,icount);
    NhlRLSetInteger(srlist,NhlNsfXCStartV,lon[0]);
    NhlRLSetInteger(srlist,NhlNsfXCEndV,lon[lonlen-1]);
    NhlRLSetInteger(srlist,NhlNsfYCStartV,lat[0]);
    NhlRLSetInteger(srlist,NhlNsfYCEndV,lat[latlen-1]);
    NhlCreate(&field1,"field1",NhlscalarFieldClass,appid,srlist);
/*
 * Assign the indices, skipping the first two colormap entries
 * that contain the default background and foreground color. 
 */
    for( i = 0; i < NCOLORS-2; i++ ) fillindices[i] = i+2;
/*
 * Create a ContourPlot object.
 */
    NhlRLClear(srlist);
/*
 * Assign the data that was read earlier.
 */
    NhlRLSetInteger(srlist,NhlNcnScalarFieldData,field1);
/*
 * Assign the colormap fill indices
 */
    NhlRLSetIntegerArray(srlist,NhlNcnFillColors,fillindices,NCOLORS-2);
/*
 * Set the range and spacing of the contour levels.
 */
    NhlRLSetString(srlist,NhlNcnLevelSelectionMode, "ManualLevels");
    NhlRLSetFloat(srlist,NhlNcnMinLevelValF,195.0);
    NhlRLSetFloat(srlist,NhlNcnMaxLevelValF,328.0);
    NhlRLSetFloat(srlist,NhlNcnLevelSpacingF,2.25);
/*
 * Turn on contour fills.
 */
    NhlRLSetString(srlist,NhlNcnFillOn,"True");
/*
 * Turn off the contour lines and labels.
 */
    NhlRLSetString(srlist,NhlNcnLinesOn,"False");
    NhlRLSetString(srlist,NhlNcnLineLabelsOn,"False");
    NhlRLSetString(srlist,NhlNcnHighLabelsOn,"False");
    NhlRLSetString(srlist,NhlNcnLowLabelsOn,"False");
    NhlRLSetString(srlist,NhlNcnInfoLabelOn,"False");
/*
 * Turn on the overlay labelbar.
 */
    NhlRLSetString(srlist,NhlNpmLabelBarDisplayMode,"ALWAYS");
/*
 * Set the labelbar size
 */
    NhlRLSetFloat(srlist,NhlNpmLabelBarHeightF,0.15);
    NhlRLSetFloat(srlist,NhlNpmLabelBarWidthF,0.6);
/*
 * Set the location and orientation of the labelbar.
 */
    NhlRLSetString(srlist,NhlNpmLabelBarSide,"bottom");
    NhlRLSetString(srlist,NhlNlbOrientation,"horizontal");
/*
 * Set the lablebar title, font, and color.
 */
    NhlRLSetString(srlist,NhlNlbTitleString,"Day 1");
    NhlRLSetInteger(srlist,NhlNlbTitleFont,22);
    NhlRLSetString(srlist,NhlNlbTitleFontColor,"PaleGreen4");
/*
 * Turn off the labelbar perimeter box 
 */
    NhlRLSetString(srlist,NhlNlbPerimOn,"False");
/*
 * Turn off lines that separate each color in the labelbar.
 */
    NhlRLSetInteger(srlist,NhlNlbBoxLinesOn,0);
/*
 * Turn off labelbar labels
 */
    NhlRLSetString(srlist,NhlNlbLabelsOn,"False");
    NhlCreate(&con1,"con1",NhlcontourPlotClass,workid,srlist);
/*
 * Create map object.
 */
    NhlRLClear(srlist);
/*
 * Allow the map to be stretched along 
 * the horizontal and vertical view axes.
 */
    NhlRLSetString(srlist,NhlNmpShapeMode,"FreeAspect");
/*
 * Set the viewport position and size. This will
 * stretch the map along its axes.
 */
    NhlRLSetFloat(srlist,NhlNvpXF,0.03);
    NhlRLSetFloat(srlist,NhlNvpYF,0.90);
    NhlRLSetFloat(srlist,NhlNvpWidthF,0.94);
    NhlRLSetFloat(srlist,NhlNvpHeightF,0.68);
/*
 * Set the center of projection.
 */
    NhlRLSetFloat(srlist,NhlNmpCenterLatF,0.0);
    NhlRLSetFloat(srlist,NhlNmpCenterLonF,150.0);
/*
 * Set the projection type.
 */
    NhlRLSetString(srlist,NhlNmpProjection,"CYLINDRICALEQUIDISTANT");
/*
 * Turn off grid and limb lines, labels, and permimeter.
 */
    NhlRLSetString(srlist,NhlNmpGridAndLimbOn,"False");
    NhlRLSetString(srlist,NhlNmpLabelsOn,"False");
    NhlRLSetString(srlist,NhlNmpPerimOn,"False");
/*
 * Turn on main title and set its value, font, and color.
 */
    NhlRLSetString(srlist,NhlNpmTitleDisplayMode,"Always");
    NhlRLSetString(srlist,NhlNtiMainString,
                   "January Global Surface Temperature");
    NhlRLSetInteger(srlist,NhlNtiMainFont,22);
    NhlRLSetString(srlist,NhlNtiMainFontColor,"PaleGreen4");
    NhlCreate(&mapid,"map",NhlmapPlotClass,workid,srlist);
    NhlAddOverlay(mapid,con1,-1);
/*
 * Create two labels (high and low values) for labelbar.
 */
    NhlRLClear(srlist);
    NhlRLSetString(srlist,NhlNtxString,"195 K");
    NhlRLSetFloat(srlist,NhlNtxPosXF,0.05);
    NhlRLSetFloat(srlist,NhlNtxPosYF,0.03);
    NhlRLSetInteger(srlist,NhlNtxFont,22);
    NhlRLSetString(srlist,NhlNtxFontColor,"PaleGreen4");
    NhlRLSetFloat(srlist,NhlNtxFontHeightF,0.03);
    NhlRLSetString(srlist,NhlNtxJust,"CENTERLEFT");
    NhlCreate(&lb1id,"lbarlo",NhltextItemClass,workid,srlist);

    NhlRLClear(srlist);
    NhlRLSetString(srlist,NhlNtxString,"328 K");
    NhlRLSetFloat(srlist,NhlNtxPosXF,0.85);
    NhlRLSetFloat(srlist,NhlNtxPosYF,0.03);
    NhlRLSetInteger(srlist,NhlNtxFont,22);
    NhlRLSetString(srlist,NhlNtxFontColor,"PaleGreen4");
    NhlRLSetFloat(srlist,NhlNtxFontHeightF,0.03);
    NhlRLSetString(srlist,NhlNtxJust,"CENTERLEFT");
    NhlCreate(&lb2id,"lbarhi",NhltextItemClass,workid,srlist);
/*
 * Draw all objects.
 */
    printf("Plotting Day 1\n");

    NhlDraw(mapid);
    NhlDraw(lb1id);
    NhlDraw(lb2id);
    NhlFrame(workid);
/*
 * Loop on remaining time steps
 */
    if ( !PLOT_ALL_DATA ) {
        nframes=2;
    }
    else {
        time_id = ncdimid(ncid,"time");
        ncdiminq(ncid,time_id,(char *)0,&nframes);
        nframes--;
    }
    for( i = 1; i <= nframes; i++ ) {
        printf("Plotting Day %d\n", i+1);
/*
 * Read the next data field (next day).
 */
        start[0] = i; start[1] = start[2] = 0;
        count[0] = 1; count[1] = latlen; count[2] = lonlen;
        ncvarget(ncid,t_id,(long const *)start,(long const *)count,T);
        icount[0] = latlen; icount[1] = lonlen;
        NhlRLClear(srlist);
        NhlRLSetMDFloatArray(srlist,NhlNsfDataArray,&T[0][0],2,icount);
        NhlSetValues(field1,srlist);
/*
 * Increment day string.
 */
        day = i+1;
        sprintf( daystr, "Day %d", day );
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNlbTitleString,daystr);
        NhlSetValues(con1,srlist);
/*
 * Draw another plot.
 */
        NhlDraw(mapid);
        NhlDraw(lb1id);
        NhlDraw(lb2id);
        NhlFrame(workid);
    }

    if( !PLOT_ALL_DATA ) {
        printf("To plot all 31 days in this animation, edit the NCL\n");
        printf("script, and set the PLOT_ALL_DATA flag to TRUE.\n");
    }
/*
 * Close the netCDF file.
 */
    ncclose(ncid);
/*
 * NhlDestroy destroys the given id and all of its children.
 */
    NhlRLDestroy(srlist);
    NhlDestroy(appid);
/*
 * Restores state.
 */
    NhlClose();
    exit(0);
}

/*
 * This procedure explicitly sets the red, green, and blue
 * intensities for each element in a colormap array.
 */
void gen_colormap( float cmap[NCOLORS][3] )
{
/*
 * Set the color map.
 */
    cmap[63][0] = 1.00; cmap[63][1] = .000; cmap[63][2] = .000;
    cmap[62][0] = 1.00; cmap[62][1] = .000; cmap[62][2] = .000;
    cmap[61][0] = .990; cmap[61][1] = .000; cmap[61][2] = .000;
    cmap[60][0] = .950; cmap[60][1] = .010; cmap[60][2] = .000;
    cmap[59][0] = .900; cmap[59][1] = .030; cmap[59][2] = .000;
    cmap[58][0] = .870; cmap[58][1] = .050; cmap[58][2] = .000;
    cmap[57][0] = .830; cmap[57][1] = .070; cmap[57][2] = .000;
    cmap[56][0] = .800; cmap[56][1] = .090; cmap[56][2] = .000;
    cmap[55][0] = .750; cmap[55][1] = .090; cmap[55][2] = .000;
    cmap[54][0] = .700; cmap[54][1] = .090; cmap[54][2] = .000;
    cmap[53][0] = .700; cmap[53][1] = .100; cmap[53][2] = .000;
    cmap[52][0] = .700; cmap[52][1] = .120; cmap[52][2] = .000;
    cmap[51][0] = .700; cmap[51][1] = .130; cmap[51][2] = .000;
    cmap[50][0] = .700; cmap[50][1] = .180; cmap[50][2] = .000;
    cmap[49][0] = .700; cmap[49][1] = .240; cmap[49][2] = .000;
    cmap[48][0] = .700; cmap[48][1] = .260; cmap[48][2] = .000;
    cmap[47][0] = .700; cmap[47][1] = .270; cmap[47][2] = .000;
    cmap[46][0] = .700; cmap[46][1] = .285; cmap[46][2] = .000;
    cmap[45][0] = .690; cmap[45][1] = .300; cmap[45][2] = .000;
    cmap[44][0] = .680; cmap[44][1] = .330; cmap[44][2] = .000;
    cmap[43][0] = .675; cmap[43][1] = .375; cmap[43][2] = .000;
    cmap[42][0] = .570; cmap[42][1] = .420; cmap[42][2] = .000;
    cmap[41][0] = .565; cmap[41][1] = .485; cmap[41][2] = .000;
    cmap[40][0] = .560; cmap[40][1] = .530; cmap[40][2] = .000;
    cmap[39][0] = .555; cmap[39][1] = .545; cmap[39][2] = .000;
    cmap[38][0] = .550; cmap[38][1] = .550; cmap[38][2] = .000;
    cmap[37][0] = .160; cmap[37][1] = .565; cmap[37][2] = .000;
    cmap[36][0] = .130; cmap[36][1] = .570; cmap[36][2] = .000;
    cmap[35][0] = .100; cmap[35][1] = .575; cmap[35][2] = .000;
    cmap[34][0] = .060; cmap[34][1] = .680; cmap[34][2] = .000;
    cmap[33][0] = .030; cmap[33][1] = .685; cmap[33][2] = .000;
    cmap[32][0] = .000; cmap[32][1] = .690; cmap[32][2] = .000;
    cmap[31][0] = .000; cmap[31][1] = .725; cmap[31][2] = .000;
    cmap[30][0] = .000; cmap[30][1] = .700; cmap[30][2] = .100;
    cmap[29][0] = .000; cmap[29][1] = .650; cmap[29][2] = .200;
    cmap[28][0] = .000; cmap[28][1] = .600; cmap[28][2] = .300;
    cmap[27][0] = .000; cmap[27][1] = .550; cmap[27][2] = .400;
    cmap[26][0] = .000; cmap[26][1] = .500; cmap[26][2] = .500;
    cmap[25][0] = .000; cmap[25][1] = .450; cmap[25][2] = .600;
    cmap[24][0] = .000; cmap[24][1] = .400; cmap[24][2] = .700;
    cmap[23][0] = .000; cmap[23][1] = .350; cmap[23][2] = .700;
    cmap[22][0] = .000; cmap[22][1] = .300; cmap[22][2] = .700;
    cmap[21][0] = .000; cmap[21][1] = .250; cmap[21][2] = .700;
    cmap[20][0] = .000; cmap[20][1] = .200; cmap[20][2] = .700;
    cmap[19][0] = .000; cmap[19][1] = .150; cmap[19][2] = .700;
    cmap[18][0] = .000; cmap[18][1] = .100; cmap[18][2] = .700;
    cmap[17][0] = .000; cmap[17][1] = .050; cmap[17][2] = .700;
    cmap[16][0] = .000; cmap[16][1] = .000; cmap[16][2] = .700;
    cmap[15][0] = .050; cmap[15][1] = .050; cmap[15][2] = .700;
    cmap[14][0] = .100; cmap[14][1] = .100; cmap[14][2] = .700;
    cmap[13][0] = .150; cmap[13][1] = .150; cmap[13][2] = .700;
    cmap[12][0] = .200; cmap[12][1] = .200; cmap[12][2] = .700;
    cmap[11][0] = .250; cmap[11][1] = .250; cmap[11][2] = .700;
    cmap[10][0] = .300; cmap[10][1] = .300; cmap[10][2] = .700;
    cmap[ 9][0] = .350; cmap[ 9][1] = .350; cmap[ 9][2] = .700;
    cmap[ 8][0] = .420; cmap[ 8][1] = .400; cmap[ 8][2] = .700;
    cmap[ 7][0] = .450; cmap[ 7][1] = .450; cmap[ 7][2] = .700;
    cmap[ 6][0] = .560; cmap[ 6][1] = .500; cmap[ 6][2] = .700;
    cmap[ 5][0] = .550; cmap[ 5][1] = .550; cmap[ 5][2] = .700;
    cmap[ 4][0] = .610; cmap[ 4][1] = .600; cmap[ 4][2] = .700;
    cmap[ 3][0] = .650; cmap[ 3][1] = .650; cmap[ 3][2] = .700;
    cmap[ 2][0] = .700; cmap[ 2][1] = .700; cmap[ 2][2] = .700;
/*
 * Entry 1 is the foreground color.
 * The continental outlines in the map object use this color 
 * as a default.
 */
    cmap[ 1][0] = .000; cmap[ 1][1] = .000; cmap[ 1][2] = .000;
/*
 * Entry 0 is the background color.
 */
    cmap[ 0][0] = .000; cmap[ 0][1] = .000; cmap[ 0][2] = .000;
}


/*
 *      $Id: cn16c.c,v 1.7 2010-03-15 22:49:23 haley Exp $
 */
/***********************************************************************
*                                                                      *
*                Copyright (C)  1997                                   *
*        University Corporation for Atmospheric Research               *
*     The use of this Software is governed by a License Agreement      *
*                                                                      *
************************************************************************
 *
 *  File:       cn16c.c
 *
 *   Author:     Bob Lackman
 *               National Center for Atmospheric Research
 *               PO 3000, Boulder, Colorado
 *
 *   Date:       Wed Mar 19, 1996
 *
 *   Description:    Combines a vector fill contour plot, a raster
 *                   contour plot, and a map plot on a single frame.
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

#define NLAT 91
#define NLON 181

/*
 * Declare variables for Tick Mark values.
 */
float xbvalues[13] = {30.,60.,90.,120.,150.,180.,210.,240.,270.,300.,330.,360.,390.};
char *xblabels[13] = {"30E","60E","90E","120E","150E","180","150W","120W","90W","60W","30W","0","30E"};
float ylvalues[7] = {-90, -60,-30,0,30,60,90};
char *yllabels[7] = {"90S","60S","30S","0","30N","60N","90N"};

/*
 * Declare variables for contour plot fills.
 */
int fillcolors[17] = {2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1};

int main()
{
/*
 * Declare variables for the HLU routine calls.
 */
  int appid, wid, cn, ice, mp, srlist, sst;
  int i, j;
  float cellsize;
/*
 * Define data variables.
 */
  float sstjan[NLAT][NLON], sstshft[NLAT][NLON];
/*
 * Declare variables for getting information from netCDF file.
*/
  int  ncid, lon_id, lat_id, stid;
  float lat[NLAT];
  long start[3], count[3], lonlen, latlen;
  ng_size_t icount[2];
  char filename[256];
  const char *dir = _NGGetNCARGEnv("data");
/*
 * Default is to create an X11 window.
 */
  char const *wks_type = "x11";

/*
 * Initialize the HLU library and set up resource template.
 * A resource file is not used in this example, but if you did
 * want one, it would be called "cn16.res".
 */
  NhlInitialize();
  srlist = NhlRLCreate(NhlSETRL);
  NhlRLClear(srlist);
  NhlRLSetString(srlist,NhlNappDefaultParent,"True");
  NhlRLSetString(srlist,NhlNappUsrDir,"./");
  NhlCreate(&appid,"cn16",NhlappClass,0,srlist);

  if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
/*
 * Create an NCGM workstation.
 */
    NhlRLClear(srlist);
    NhlRLSetString(srlist,"wkMetaName","./cn16c.ncgm");
    NhlCreate(&wid,"cn16Work",NhlncgmWorkstationClass,0,srlist);
  }
  else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 * Create an X11 workstation.
 */
    NhlRLClear(srlist);
    NhlRLSetString(srlist,"wkPause","True");
    NhlCreate(&wid,"cn16Work",NhlcairoWindowWorkstationClass,0,srlist);
  }
  else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 * Create an older-style PostScript workstation.
 */
    NhlRLClear(srlist);
    NhlRLSetString(srlist,"wkPSFileName","./cn16c.ps");
    NhlCreate(&wid,"cn16Work",NhlpsWorkstationClass,0,srlist);
  }
  else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 * Create an older-style PDF workstation.
 */
    NhlRLClear(srlist);
    NhlRLSetString(srlist,"wkPDFFileName","./cn16c.pdf");
    NhlCreate(&wid,"cn16Work",NhlpdfWorkstationClass,0,srlist);
  }
  else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
           !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 * Create a cairo PS/PDF workstation.
 */
    NhlRLClear(srlist);
    NhlRLSetString(srlist,"wkFileName","./cn16c");
    NhlRLSetString(srlist,"wkFormat",(char*)wks_type);
    NhlCreate(&wid,"cn16Work",NhlcairoDocumentWorkstationClass,0,srlist);
  }
  else if (!strcmp(wks_type,"png")) {
/*
 * Create a cairo PNG workstation.
 */
    NhlRLClear(srlist);
    NhlRLSetString(srlist,"wkFileName","./cn16c");
    NhlRLSetString(srlist,"wkFormat",(char*)wks_type);
    NhlCreate(&wid,"cn16Work",NhlcairoImageWorkstationClass,0,srlist);
  }
/*
 * Read NCL created NetCDF file containing U and V wind components.
 */
  sprintf( filename, "%s/cdf/sstdata_netcdf.nc", dir );
  ncid = ncopen(filename,NC_NOWRITE);
/*
 * Get the lat/lon dimensions.
 */
  lat_id = ncdimid(ncid,"latitude");
  lon_id = ncdimid(ncid,"longitude");
  ncdiminq(ncid,lat_id,(char *)0,&latlen);
  ncdiminq(ncid,lon_id,(char *)0,&lonlen);
/*
 * Read in lat values from netCDF file.
 */
  lat_id = ncvarid(ncid,"lat");
  start[0] = 0;
  count[0] = latlen;
  ncvarget(ncid,lat_id,(long const *)start,(long const *)count,lat);
/*
 * Read in sstdata netCDF file.
 */
  stid = ncvarid(ncid,"sst");
  start[0] = start[1] = start[2] = 0;
  count[0] = 1;
  count[1] = latlen;
  count[2] = lonlen;
  ncvarget(ncid,stid,(long const *)start,(long const *)count,sstjan);
/*
 * Close the netCDF file.
 */
  ncclose(ncid);
/*
 * The input sea surface temperature array of 0 to 360 longitude is
 * shifted to 30E to 390 (30E) by array index manipulation.
 */
  for( i = 15; i <= 180; i++ ) {
    for( j = 0; j < NLAT; j++ ) {
      sstshft[j][i-15] = sstjan[j][i];
    }
  }
  for( i = 165; i <= 180; i++ ) {
    for( j = 0; j < NLAT; j++ ) {
      sstshft[j][i] = sstjan[j][i-165];
    }
  }
/*
 * Create an sst ScalarField data object.
 */
  icount[0] = NLAT;
  icount[1] = NLON;
  NhlRLClear(srlist);
  NhlRLSetMDFloatArray(srlist,NhlNsfDataArray,&sstshft[0][0],2,icount);
  NhlRLSetFloat(srlist,NhlNsfXCStartV,30.);
  NhlRLSetFloat(srlist,NhlNsfXCEndV,390.);
  NhlRLSetFloat(srlist,NhlNsfYCStartV,lat[0]);
  NhlRLSetFloat(srlist,NhlNsfYCEndV,lat[latlen-1]);
  NhlCreate(&sst,"sf",NhlscalarFieldClass,appid,srlist);
/*
 * Create a ContourPlot object.
 */
  NhlRLClear(srlist);
  NhlRLSetInteger(srlist,NhlNcnScalarFieldData,sst);
  NhlRLSetFloat(srlist,NhlNvpXF,.10);
  NhlRLSetFloat(srlist,NhlNvpYF,.80);
  NhlRLSetFloat(srlist,NhlNvpWidthF,.80);
  NhlRLSetFloat(srlist,NhlNvpHeightF,.40);
  NhlRLSetString(srlist,NhlNcnInfoLabelOn,"False");
  NhlRLSetString(srlist,NhlNcnHighLabelsOn,"False");
  NhlRLSetString(srlist,NhlNcnLowLabelsOn,"False");
  NhlRLSetString(srlist,NhlNcnMonoLineColor,"False");
  NhlRLSetString(srlist,NhlNcnLineDrawOrder,"predraw");
  NhlRLSetString(srlist,NhlNcnFillDrawOrder,"predraw");
  NhlRLSetString(srlist,NhlNcnLabelDrawOrder,"predraw");
  NhlRLSetInteger(srlist,NhlNcnLineLabelInterval,2);
  NhlRLSetString(srlist,NhlNcnLineLabelPlacementMode,"computed");
  NhlRLSetString(srlist,NhlNtiMainOn,"True");
  NhlRLSetString(srlist,NhlNtiMainString,"JANUARY SST CLIMATOLOGY");
  NhlRLSetFloat(srlist,NhlNtiMainFontHeightF,.020);
  NhlRLSetInteger(srlist,NhlNtiMainFont,25);
  NhlRLSetString(srlist,NhlNtmXBMode,"EXPLICIT");
  NhlRLSetString(srlist,NhlNtmYLMode,"EXPLICIT");
  NhlRLSetFloatArray(srlist,NhlNtmXBValues,xbvalues,13);
  NhlRLSetStringArray(srlist,NhlNtmXBLabels,xblabels,13);
  NhlRLSetFloatArray(srlist,NhlNtmYLValues,ylvalues,7);
  NhlRLSetStringArray(srlist,NhlNtmYLLabels,yllabels,7);
  NhlRLSetString(srlist,NhlNtmXTLabelsOn,"True");
  NhlRLSetString(srlist,NhlNtmYRLabelsOn,"True");
  NhlRLSetFloat(srlist,NhlNtmXBLabelFontHeightF,.015);
  NhlRLSetFloat(srlist,NhlNtmYLLabelFontHeightF,.015);
  NhlRLSetFloat(srlist,NhlNtmXBMajorOutwardLengthF,.006);
  NhlRLSetFloat(srlist,NhlNtmXBMajorLengthF,.006);
  NhlRLSetFloat(srlist,NhlNtmYLMajorOutwardLengthF,.006);
  NhlRLSetFloat(srlist,NhlNtmYLMajorLengthF,.006);
  NhlRLSetString(srlist,NhlNtmXBMinorOn,"False");
  NhlRLSetString(srlist,NhlNtmXTMinorOn,"False");
  NhlRLSetString(srlist,NhlNtmYLMinorOn,"False");
  NhlRLSetString(srlist,NhlNtmYRMinorOn,"False");
  NhlCreate(&cn,"cn",NhlcontourPlotClass,wid,srlist);
/*
 * The ice field is added as a raster contour.  Areas without ice
 * are colored transparent.
 */
  cellsize = .8/360.;
  NhlRLClear(srlist);
  NhlRLSetInteger(srlist,NhlNcnScalarFieldData,sst);
  NhlRLSetFloat(srlist,NhlNvpXF,.10);
  NhlRLSetFloat(srlist,NhlNvpYF,.80);
  NhlRLSetFloat(srlist,NhlNvpWidthF,.80);
  NhlRLSetFloat(srlist,NhlNvpHeightF,.40);
  NhlRLSetString(srlist,NhlNtmXBOn,"False");
  NhlRLSetString(srlist,NhlNtmYLOn,"False");
  NhlRLSetString(srlist,NhlNtmXBMinorOn,"False");
  NhlRLSetString(srlist,NhlNtmYLMinorOn,"False");
  NhlRLSetString(srlist,NhlNtmXBLabelsOn,"False");
  NhlRLSetString(srlist,NhlNtmYLLabelsOn,"False");
  NhlRLSetString(srlist,NhlNtiMainOn,"False");
  NhlRLSetFloat(srlist,NhlNtmXBMajorLengthF,0.);
  NhlRLSetFloat(srlist,NhlNtmYLMajorLengthF,0.);
  NhlRLSetString(srlist,NhlNcnFillOn,"True");
  NhlRLSetString(srlist,NhlNcnFillMode,"RasterFill");
  NhlRLSetFloat(srlist,NhlNcnRasterCellSizeF,cellsize);
  NhlRLSetFloat(srlist,NhlNcnMinLevelValF,-2.0);
  NhlRLSetIntegerArray(srlist,NhlNcnFillColors,fillcolors,17);
  NhlRLSetString(srlist,NhlNcnLineLabelsOn,"False");
  NhlRLSetString(srlist,NhlNcnLinesOn,"False");
  NhlRLSetString(srlist,NhlNcnMonoFillColor,"False");
  NhlRLSetString(srlist,NhlNcnInfoLabelOn,"False");
  NhlRLSetString(srlist,NhlNcnHighLabelsOn,"False");
  NhlCreate(&ice,"ice",NhlcontourPlotClass,wid,srlist);
/*
 * Create a MapPlot object.
 */
  NhlRLClear(srlist);
  NhlRLSetFloat(srlist,NhlNvpXF,.10);
  NhlRLSetFloat(srlist,NhlNvpYF,.80);
  NhlRLSetFloat(srlist,NhlNvpWidthF,.80);
  NhlRLSetFloat(srlist,NhlNvpHeightF,.40);
  NhlRLSetString(srlist,NhlNmpFillOn,"True");
  NhlRLSetString(srlist,NhlNmpLabelsOn,"False");
  NhlRLSetInteger(srlist,NhlNmpDefaultFillColor,11);
  NhlRLSetInteger(srlist,NhlNmpLandFillColor,11);
  NhlRLSetString(srlist,NhlNmpOutlineOn,"False");
  NhlRLSetString(srlist,NhlNmpAreaMaskingOn,"True");
  NhlRLSetString(srlist,NhlNmpMaskAreaSpecifiers,"Oceans");
  NhlRLSetString(srlist,NhlNmpGridAndLimbOn,"False");
  NhlRLSetString(srlist,NhlNmpLimitMode,"latlon");
  NhlRLSetFloat(srlist,NhlNmpMinLonF,30.);
  NhlRLSetFloat(srlist,NhlNmpMaxLonF,390.);
  NhlRLSetFloat(srlist,NhlNmpCenterLonF,210.);
  NhlCreate(&mp,"mp",NhlmapPlotClass,wid,srlist);
/*
 * Draw everything and clean up.
 */
  NhlDraw(ice);
  NhlDraw(cn);
  NhlDraw(mp);
  NhlFrame(wid);
  NhlDestroy(wid);
  NhlClose();
  return(0);
}


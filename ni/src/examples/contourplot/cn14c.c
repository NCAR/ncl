/*
 *      $Id: cn14c.c,v 1.8 2010-03-15 22:49:23 haley Exp $
 */
/***********************************************************************
*                                                                      *
*                Copyright (C)  1997                                   *
*        University Corporation for Atmospheric Research               *
*     The use of this Software is governed by a License Agreement      *
*                                                                      *
************************************************************************
 *
 *  File:       cn14c.c
 *
 *   Author:     Bob Lackman
 *               National Center for Atmospheric Research
 *               PO 3000, Boulder, Colorado
 *
 *   Date:       Fri Feb 16, 1996
 *
 *   Description:    Reads two netCDF variables, U and V winds, and produces
 *                   a contour plot of the wind speed.
 *
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


#define NPTS  73
#define NCOLORS 16

/*
 * Declare variables for Tick Mark values.
 */
float xbvalues[7] = {-180.,-120.,-60.,0.,60.,120.,180.};
float ylvalues[7] = {-90., -60.,-30.,0.,30.,60.,90.};
char *xblabels[7] = {"180W","120W","60W","0","60E","120E","180E"};
char *yllabels[7] = {"90S","60S","30S","EQ","30N","60N","90N"};

/*
 * Declare variables for contour plot fills.
 */
int fillcolors[14] = {15,14,2,3,4,7,6,8,9,10,11,13,12,0};

int main()
{
  int i,j;
/*
 * Declare variables for the HLU routine calls.
 */
  int appid, workid, cndata, cn_plot, map, srlist;
/*
 * Declare variables for color map
 */
  ng_size_t length[2];
  float cmap[NCOLORS][3];
/*
 * Declare variables to hold U, V, and wind speed data.
 */
  float u[NPTS][NPTS], v[NPTS][NPTS], spd[NPTS][NPTS];
/*
 * Declare variables for getting information from netCDF file.
 */
  int  ncid, lon_id, lat_id, uid, vid;
  float lon[NPTS], lat[NPTS];
  long start[2], count[2], lonlen, latlen;
  ng_size_t icount[2];
  char filename[256];
  const char *dir = _NGGetNCARGEnv("data");
/*
 * Default is to display to an X11 window.
 */
    char const *wks_type = "x11";
/*
 * Generate a color map.
 */
	cmap[0][0] = 1.00;  /* white */
	cmap[0][1] = 1.00;
	cmap[0][2] = 1.00;
	cmap[1][0] = 0.00;  /* black */
	cmap[1][1] = 0.00;
	cmap[1][2] = 0.00;
	cmap[2][0] = 0.00;  /* blue */
	cmap[2][1] = 0.00;
	cmap[2][2] = 1.00;
	cmap[3][0] = 0.20;  /* sky blue */
	cmap[3][1] = 0.56;
	cmap[3][2] = 0.80;
	cmap[4][0] = 0.00;  /* cyan */
	cmap[4][1] = 1.00;
	cmap[4][2] = 1.00;
	cmap[5][0] = 0.50;  /* blue magenta */
	cmap[5][1] = 0.00;
	cmap[5][2] = 1.00;
	cmap[6][0] = 0.00;  /* green */
	cmap[6][1] = 1.00;
	cmap[6][2] = 0.00;
	cmap[7][0] = 0.14;  /* forest green */
	cmap[7][1] = 0.56;
	cmap[7][2] = 0.14;
	cmap[8][0] = 1.00;  /* yellow */
	cmap[8][1] = 1.00;
	cmap[8][2] = 0.00;
	cmap[9][0] = 1.00;  /* orange */
	cmap[9][1] = 0.50;
	cmap[9][2] = 0.00;
	cmap[10][0] = 1.00; /* magenta */
	cmap[10][1] = 0.00;
	cmap[10][2] = 1.00;
	cmap[11][0] = 1.00; /* red */
	cmap[11][1] = 0.00;
	cmap[11][2] = 0.00;
	cmap[12][0] = 0.65; /* brown */
	cmap[12][1] = 0.16;
	cmap[12][2] = 0.16;
	cmap[13][0] = 0.86; /* tan */
	cmap[13][1] = 0.58;
	cmap[13][2] = 0.44;
	cmap[14][0] = 0.66; /* light gray */
	cmap[14][1] = 0.66;
	cmap[14][2] = 0.66;
	cmap[15][0] = 0.40; /* dark gray */
	cmap[15][1] = 0.40;
	cmap[15][2] = 0.40;
/*
 * Initialize the HLU library and set up resource template.
 * A resource file is not used in this example, but if you did
 * want one, it would be called "cn14.res".
 */
	NhlInitialize();
	srlist = NhlRLCreate(NhlSETRL);
	NhlRLClear(srlist);
	NhlRLSetString(srlist,"appDefaultParent","True");
	NhlRLSetString(srlist,"appUsrDir","./");
	NhlCreate(&appid,"cn14",NhlappClass,0,srlist);

	if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
/*
 * Create an NCGM workstation.
 */
	  NhlRLClear(srlist);
	  NhlRLSetString(srlist,"wkMetaName","./cn14c.ncgm");
	  NhlCreate(&workid,"cn14Work",NhlncgmWorkstationClass,0,srlist);
	}
	else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 * Create an X11 workstation.
 */
	  NhlRLClear(srlist);
	  NhlRLSetString(srlist,"wkPause","True");
	  NhlCreate(&workid,"cn14Work",NhlcairoWindowWorkstationClass,0,srlist);
	}
	else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 * Create an older-style PostScript workstation.
 */
	  NhlRLClear(srlist);
	  NhlRLSetString(srlist,"wkPSFileName","./cn14c.ps");
	  NhlCreate(&workid,"cn14Work",NhlpsWorkstationClass,0,srlist);
	}
	else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 * Create an older-style PDF workstation.
 */
	  NhlRLClear(srlist);
	  NhlRLSetString(srlist,"wkPDFFileName","./cn14c.pdf");
	  NhlCreate(&workid,"cn14Work",NhlpdfWorkstationClass,0,srlist);
	}
        else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
                 !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 * Create a cairo PS/PDF workstation.
 */
	  NhlRLClear(srlist);
	  NhlRLSetString(srlist,"wkFileName","./cn14c");
	  NhlRLSetString(srlist,"wkFormat",(char*)wks_type);
	  NhlCreate(&workid,"cn14Work",NhlcairoDocumentWorkstationClass,0,srlist);
	}
        else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 * Create a cairo PNG workstation.
 */
	  NhlRLClear(srlist);
	  NhlRLSetString(srlist,"wkFileName","./cn14c");
	  NhlRLSetString(srlist,"wkFormat",(char*)wks_type);
	  NhlCreate(&workid,"cn14Work",NhlcairoImageWorkstationClass,0,srlist);
	}
/*
 * Set color map resource.
 */
	length[0] = NCOLORS;
	length[1] = 3;
	NhlRLClear(srlist);
	NhlRLSetMDFloatArray(srlist,NhlNwkColorMap,&cmap[0][0],2,length);
	NhlSetValues(workid,srlist);
/*
 * Read NCL created NetCDF file containing U and V wind components.
 */
    sprintf( filename, "%s/cdf/941110_UV.cdf", dir );
    ncid = ncopen(filename,NC_NOWRITE);
/*
 * Get the lat/lon dimensions.
 */
	lat_id = ncdimid(ncid,"lat");
	lon_id = ncdimid(ncid,"lon");
    ncdiminq(ncid,lat_id,(char *)0,&latlen);
    ncdiminq(ncid,lon_id,(char *)0,&lonlen);
/*
 * Read in lat/lon values from netCDF file.
 */
    lat_id = ncvarid(ncid,"lat");
    count[0] = latlen;
    start[0] = 0;
    ncvarget(ncid,lat_id,(long const *)start,(long const *)count,lat);

    lon_id = ncvarid(ncid,"lon");
    count[0] = lonlen;
    ncvarget(ncid,lon_id,(long const *)start,(long const *)count,lon);
/*
 * Read in u and v from netCDF file.
 */
	uid = ncvarid(ncid,"u");
	vid = ncvarid(ncid,"v");
	start[0] = start[1] = 0;
	count[0] = latlen;
	count[1] = lonlen;
	ncvarget(ncid,uid,(long const *)start,(long const *)count,u);
	ncvarget(ncid,vid,(long const *)start,(long const *)count,v);
/*
 * Close the netCDF file.
 */
    ncclose(ncid);
/*
 * Compute the wind speed.
 */
	for( i=0; i < latlen; i++ ) {
	  for( j=0; j < lonlen; j++ ) {
		spd[i][j] = sqrt(pow(u[i][j],2.) + pow(v[i][j],2.));
	  }
	}
/*
 * Create the ScalarField object needed by ContourPlot.
 */
	icount[0] = icount[1] = NPTS;
	NhlRLClear(srlist);
	NhlRLSetMDFloatArray(srlist,NhlNsfDataArray,&spd[0][0],2,icount);
	NhlRLSetFloat(srlist,NhlNsfXCStartV,lon[0]);
	NhlRLSetFloat(srlist,NhlNsfXCEndV,lon[lonlen-1]);
	NhlRLSetFloat(srlist,NhlNsfYCStartV,lat[0]);
	NhlRLSetFloat(srlist,NhlNsfYCEndV,lat[latlen-1]);
	NhlCreate(&cndata,"cndata",NhlscalarFieldClass,appid,srlist);
/*
 * Create the ContourPlot object and assign data to it.
 */
	NhlRLClear(srlist);
	NhlRLSetFloat(srlist,NhlNvpXF,.10);
	NhlRLSetFloat(srlist,NhlNvpYF,.80);
	NhlRLSetFloat(srlist,NhlNvpWidthF,.80);
	NhlRLSetFloat(srlist,NhlNvpHeightF,.40);
	NhlRLSetInteger(srlist,NhlNcnScalarFieldData,cndata);
	NhlRLSetString(srlist,NhlNcnFillOn,"True");
	NhlRLSetIntegerArray(srlist,NhlNcnFillColors,fillcolors,14);
/*
 * Set the range and spacing of the contour levels.
 */
	NhlRLSetString(srlist,NhlNcnLevelSelectionMode,"ManualLevels");
	NhlRLSetFloat(srlist,NhlNcnMinLevelValF,2.0);
	NhlRLSetFloat(srlist,NhlNcnMaxLevelValF,28.0);
	NhlRLSetFloat(srlist,NhlNcnLevelSpacingF,3.0);
/*
 * Turn off the contour lines and labels.
 */
	NhlRLSetString(srlist,NhlNcnLinesOn,"False");
	NhlRLSetString(srlist,NhlNcnLineLabelsOn,"False");
	NhlRLSetString(srlist,NhlNcnHighLabelsOn,"False");
	NhlRLSetString(srlist,NhlNcnLowLabelsOn,"False");
	NhlRLSetString(srlist,NhlNcnInfoLabelOn,"False");
/*
 *  Set resources of the inherited Title objects.
 */
	NhlRLSetString(srlist,NhlNtiXAxisString,"Longitude");
	NhlRLSetString(srlist,NhlNtiXAxisFont,"helvetica");
	NhlRLSetFloat(srlist,NhlNtiXAxisFontHeightF,.02);
	NhlRLSetString(srlist,NhlNtiYAxisString,"Latitude");
	NhlRLSetString(srlist,NhlNtiYAxisFont,"helvetica");
	NhlRLSetFloat(srlist,NhlNtiYAxisFontHeightF,.02);
	NhlRLSetString(srlist,NhlNtiMainString,"Wind Speed");
	NhlRLSetString(srlist,NhlNtiMainFont,"helvetica");
	NhlRLSetFloat(srlist,NhlNtiMainFontHeightF,.03);
/*
 * Set resources of the inherited TickMark object.
 */
	NhlRLSetString(srlist,NhlNtmXBMode,"EXPLICIT");
	NhlRLSetFloatArray(srlist,NhlNtmXBValues,xbvalues,7);
	NhlRLSetStringArray(srlist,NhlNtmXBLabels,xblabels,7);
	NhlRLSetFloatArray(srlist,NhlNtmYLValues,ylvalues,7);
	NhlRLSetStringArray(srlist,NhlNtmYLLabels,yllabels,7);
	NhlRLSetString(srlist,NhlNtmYLMode,"EXPLICIT");
	NhlRLSetFloat(srlist,NhlNtmXBLabelFontHeightF,.016);
	NhlRLSetFloat(srlist,NhlNtmYLLabelFontHeightF,.016);
	NhlRLSetString(srlist,NhlNtmXBLabelFont,"times-roman");
	NhlRLSetString(srlist,NhlNtmYLLabelFont,"times-roman");
	NhlRLSetString(srlist,NhlNtmXBMinorOn,"False");
	NhlRLSetString(srlist,NhlNtmYLMinorOn,"False");
	NhlRLSetString(srlist,NhlNpmLabelBarDisplayMode,"ALWAYS");
	NhlRLSetFloat(srlist,NhlNpmLabelBarHeightF,.15);
	NhlRLSetFloat(srlist,NhlNpmLabelBarWidthF,.8);
	NhlRLSetString(srlist,NhlNpmLabelBarSide,"bottom");
	NhlRLSetString(srlist,NhlNlbOrientation,"horizontal");
	NhlRLSetInteger(srlist,NhlNlbBoxLinesOn,0);
	NhlRLSetString(srlist,NhlNlbLabelsOn,"True");
	NhlRLSetString(srlist,NhlNlbPerimOn,"False");
	NhlRLSetString(srlist,NhlNlbAutoManage,"False");
	NhlRLSetFloat(srlist,NhlNlbLabelFontHeightF,0.015);
	NhlRLSetString(srlist,NhlNlbLabelFont,"times-roman");
	NhlCreate(&cn_plot,"cn_plot",NhlcontourPlotClass,workid,srlist);
/*
 * Create a MapPlot object.
 */
	NhlRLClear(srlist);
	NhlRLSetFloat(srlist,NhlNvpXF,.10);
	NhlRLSetFloat(srlist,NhlNvpYF,.80);
	NhlRLSetFloat(srlist,NhlNvpWidthF,.80);
	NhlRLSetFloat(srlist,NhlNvpHeightF,.40);
	NhlRLSetString(srlist,NhlNmpFillOn,"True");
	NhlRLSetString(srlist,NhlNmpLandFillColor,"Black");
	NhlRLSetString(srlist,NhlNmpOceanFillColor,"Transparent");
	NhlRLSetString(srlist,NhlNmpInlandWaterFillColor,"Black");
	NhlRLSetString(srlist,NhlNmpGridLineColor,"LightGray");
	NhlRLSetString(srlist,NhlNmpGeophysicalLineColor,"Black");
	NhlRLSetFloat(srlist,NhlNmpGeophysicalLineThicknessF,1.);
	NhlCreate(&map,"map",NhlmapPlotClass,workid,srlist);
/*
 * Notice that we are not doing an overlay here! We are simply drawing
 * the ContourPlot first, and then the Map Plot.
 */
	NhlDraw(cn_plot);
	NhlDraw(map);
	NhlFrame(workid);
	NhlClose();
	return(0);
}

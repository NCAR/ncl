/*
 *      $Id: cn15c.c,v 1.12 2010-03-16 21:00:45 haley Exp $
 */
/***********************************************************************
*                                                                      *
*                Copyright (C)  1997                                   *
*        University Corporation for Atmospheric Research               *
*     The use of this Software is governed by a License Agreement      *
*                                                                      *
************************************************************************
 *
 *  File:       cn15c.c
 *
 *   Author:     Bob Lackman
 *               National Center for Atmospheric Research
 *               PO 3000, Boulder, Colorado
 *
 *   Date:       Fri Feb 16, 1996
 *
 *   Description:    Combines a contour plot and an xy plot on a
 *                   single frame.  Output goes to an X11 window,
 *                   an NCGM, PostScript file, a PDF file, a cairo
 *                   PS file, a cairo PDF file, and a cairo PNG file.
 */
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/hlu/PDFWorkstation.h>
#include <ncarg/hlu/CairoWorkstation.h>
#include <ncarg/hlu/ScalarField.h>
#include <ncarg/hlu/TextItem.h>
#include <ncarg/hlu/CoordArrays.h>
#include <ncarg/hlu/ContourPlot.h>
#include <ncarg/hlu/MapPlot.h>
#include <ncarg/hlu/XyPlot.h>
#include <netcdf.h>

#define NLAT 91
#define NLON 181
#define NCOLORS 16

/*
 * Declare variables for Tick Mark values.
 */
float xbvalues1[7] = {0.,60.,120.,180.,240.,300.,360.};
char *xblabels1[7] = {"0","60E","120E","180","120W","60W","0"};
float xbvalues2[11] = {0.,3.,6.,9.,12.,15.,18.,21.,24.,27.,30.};
float ylvalues[7] = {-90., -60.,-30.,0.,30.,60.,90.};
char *xblabels2[11] = {"0","3","6","9","12","15","18","21","24","27","30"};
char *yllabels[7] = {"90S","60S","30S","EQ","30N","60N","90N"};

/*
 * Declare variables for contour plot fills.
 */
int linecolors[15] = {12,13,14,7,2,0,8,11,6,9,4,3,5,7,10};
int fillcolors[16] = {0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2};

int main()
{
/*
 * Declare variables for the HLU routine calls.
 */
    int appid, jan, cn, ice, xy_plot, mp, tx, srlist;
/*
 * Declare variables for color map
 */
    ng_size_t length[2];
    float cmap[NCOLORS][3];
/*
 * Declare variables to hold contour and xy plot data.
 */
    float sstjan[NLAT][NLON], lat[NLAT], zoneave[NLAT];
    int ocean1;
    float cellsize;
/*
 * Declare variables for getting information from netCDF file.
 */
    int  ncid, lon_id, lat_id, stid;
    long start[3], count[3];
    ng_size_t icount[2];
    long lonlen, latlen;
    char filename[256];
    const char *dir = _NGGetNCARGEnv("data");
    FILE *fp;
/*
 * Output to all four workstations.
 */
    int ncgm1, x1, ps1, pdf1, ps2, pdf2, png1;
    int nlt, nln, zonal, ksst;
    float sstzon;
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
 * want one, it would be called "cn15.res".
 */
    NhlInitialize();
    srlist = NhlRLCreate(NhlSETRL);
    NhlRLClear(srlist);
    NhlRLSetString(srlist,"appDefaultParent","True");
    NhlRLSetString(srlist,"appUsrDir","./");
    NhlCreate(&appid,"cn15",NhlappClass,0,srlist);
/*
 * Create an NCGM workstation.
 */
    length[0] = NCOLORS;
    length[1] = 3;

    NhlRLClear(srlist);
    NhlRLSetString(srlist,NhlNwkMetaName,"./cn15c.ncgm");
    NhlRLSetMDFloatArray(srlist,NhlNwkColorMap,&cmap[0][0],2,length);
    NhlCreate(&ncgm1,"cn15Work",NhlncgmWorkstationClass,0,srlist);
/*
 * Create an X11 workstation.
 */
      NhlRLClear(srlist);
      NhlRLSetString(srlist,NhlNwkPause,"True");
      NhlRLSetMDFloatArray(srlist,NhlNwkColorMap,&cmap[0][0],2,length);
      NhlCreate(&x1,"cn15Work",NhlcairoWindowWorkstationClass,0,srlist);
/*
 * Create an older-style PostScript workstation.
 */
      NhlRLClear(srlist);
      NhlRLSetString(srlist,NhlNwkPSFileName,"./cn15c.ps");
      NhlRLSetString(srlist,NhlNwkVisualType,"color");
/*
 * Since the plots are beside each other, use landscape mode and the
 * PostScript resources for positioning the plot on the paper.
 */
      NhlRLSetString(srlist,NhlNwkOrientation,"landscape");
      NhlRLSetInteger(srlist,NhlNwkDeviceLowerX,0);
      NhlRLSetInteger(srlist,NhlNwkDeviceLowerY,60);
      NhlRLSetInteger(srlist,NhlNwkDeviceUpperX,600);
      NhlRLSetInteger(srlist,NhlNwkDeviceUpperY,700);
      NhlRLSetMDFloatArray(srlist,NhlNwkColorMap,&cmap[0][0],2,length);
      NhlCreate(&ps1,"cn15Work",NhlpsWorkstationClass,0,srlist);
/*
 * Create an older-style PDF workstation.
 */
      NhlRLClear(srlist);
      NhlRLSetString(srlist,NhlNwkPDFFileName,"./cn15c.pdf");
      NhlRLSetString(srlist,NhlNwkVisualType,"color");
/*
 * Since the plots are beside each other, use landscape mode and the
 * PostScript resources for positioning the plot on the paper.
 */
      NhlRLSetString(srlist,NhlNwkOrientation,"landscape");
      NhlRLSetInteger(srlist,NhlNwkDeviceLowerX,0);
      NhlRLSetInteger(srlist,NhlNwkDeviceLowerY,60);
      NhlRLSetInteger(srlist,NhlNwkDeviceUpperX,600);
      NhlRLSetInteger(srlist,NhlNwkDeviceUpperY,700);
      NhlRLSetMDFloatArray(srlist,NhlNwkColorMap,&cmap[0][0],2,length);
      NhlCreate(&pdf1,"cn15Work",NhlpdfWorkstationClass,0,srlist);
/*
 * Create a cairo PostScript workstation.
 */
      NhlRLClear(srlist);
      NhlRLSetString(srlist,NhlNwkFileName,"./cn15c.cairo");
      NhlRLSetString(srlist,NhlNwkFormat,"ps");
/*
 * Since the plots are beside each other, use landscape mode and the
 * PostScript resources for positioning the plot on the paper.
 */
      NhlRLSetString(srlist,NhlNwkOrientation,"landscape");
      NhlRLSetInteger(srlist,NhlNwkDeviceLowerX,0);
      NhlRLSetInteger(srlist,NhlNwkDeviceLowerY,60);
      NhlRLSetInteger(srlist,NhlNwkDeviceUpperX,600);
      NhlRLSetInteger(srlist,NhlNwkDeviceUpperY,700);
      NhlRLSetMDFloatArray(srlist,NhlNwkColorMap,&cmap[0][0],2,length);
      NhlCreate(&ps2,"cn15Work",NhlcairoDocumentWorkstationClass,0,srlist);
/*
 * Create a cairo PDF workstation.
 */
      NhlRLClear(srlist);
      NhlRLSetString(srlist,NhlNwkFileName,"./cn15c.cairo");
      NhlRLSetString(srlist,NhlNwkFormat,"pdf");
/*
 * Since the plots are beside each other, use landscape mode and the
 * PostScript resources for positioning the plot on the paper.
 */
      NhlRLSetString(srlist,NhlNwkOrientation,"landscape");
      NhlRLSetInteger(srlist,NhlNwkDeviceLowerX,0);
      NhlRLSetInteger(srlist,NhlNwkDeviceLowerY,60);
      NhlRLSetInteger(srlist,NhlNwkDeviceUpperX,600);
      NhlRLSetInteger(srlist,NhlNwkDeviceUpperY,700);
      NhlRLSetMDFloatArray(srlist,NhlNwkColorMap,&cmap[0][0],2,length);
      NhlCreate(&pdf2,"cn15Work",NhlcairoDocumentWorkstationClass,0,srlist);
/*
 * Create a cairo PNG workstation.
 */
      NhlRLClear(srlist);
      NhlRLSetString(srlist,NhlNwkFileName,"./cn15c.cairo");
      NhlRLSetString(srlist,NhlNwkFormat,"png");
      NhlRLSetMDFloatArray(srlist,NhlNwkColorMap,&cmap[0][0],2,length);
      NhlCreate(&png1,"cn15Work",NhlcairoImageWorkstationClass,0,srlist);
/*
 * Open and read NetCDF file.
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
 * Create an sst data object.
 */
    icount[1] = NLON;
    icount[0] = NLAT;
    NhlRLClear(srlist);
    NhlRLSetMDFloatArray(srlist,NhlNsfDataArray,&sstjan[0][0],2,icount);
    NhlRLSetFloat(srlist,NhlNsfXCStartV,0.);
    NhlRLSetFloat(srlist,NhlNsfXCEndV,360.);
    NhlRLSetFloat(srlist,NhlNsfYCStartV,lat[0]);
    NhlRLSetFloat(srlist,NhlNsfYCEndV,lat[latlen-1]);
    NhlCreate(&jan,"sf",NhlscalarFieldClass,appid,srlist);
/*
 * Create a ContourPlot object.
 */
      NhlRLClear(srlist);
      NhlRLSetInteger(srlist,NhlNcnScalarFieldData,jan);
      NhlRLSetFloat(srlist,NhlNvpXF,.06);
      NhlRLSetFloat(srlist,NhlNvpYF,.65);
      NhlRLSetFloat(srlist,NhlNvpWidthF,.60);
      NhlRLSetFloat(srlist,NhlNvpHeightF,.30);
      NhlRLSetString(srlist,NhlNcnInfoLabelOn,"False");
      NhlRLSetString(srlist,NhlNcnHighLabelsOn,"False");
      NhlRLSetString(srlist,NhlNcnLowLabelsOn,"False");
      NhlRLSetString(srlist,NhlNcnMonoLineColor,"False");
      NhlRLSetIntegerArray(srlist,NhlNcnLineColors,linecolors,15);
      NhlRLSetString(srlist,NhlNcnLineDrawOrder,"predraw");
      NhlRLSetString(srlist,NhlNcnFillDrawOrder,"predraw");
      NhlRLSetString(srlist,NhlNcnLabelDrawOrder,"predraw");
      NhlRLSetInteger(srlist,NhlNcnLineLabelInterval,2);
      NhlRLSetString(srlist,NhlNcnLineLabelPlacementMode,"computed");
      NhlRLSetString(srlist,NhlNtmXBMode,"EXPLICIT");
      NhlRLSetFloatArray(srlist,NhlNtmXBValues,xbvalues1,7);
      NhlRLSetStringArray(srlist,NhlNtmXBLabels,xblabels1,7);
      NhlRLSetString(srlist,NhlNtmYLMode,"EXPLICIT");
      NhlRLSetFloatArray(srlist,NhlNtmYLValues,ylvalues,7);
      NhlRLSetStringArray(srlist,NhlNtmYLLabels,yllabels,7);
      NhlRLSetString(srlist,NhlNtmXTLabelsOn,"False");
      NhlRLSetString(srlist,NhlNtmYRLabelsOn,"True");
      NhlRLSetFloat(srlist,NhlNtmXBLabelFontHeightF,.010);
      NhlRLSetFloat(srlist,NhlNtmYLLabelFontHeightF,.010);
      NhlRLSetFloat(srlist,NhlNtmXBMajorOutwardLengthF,.006);
      NhlRLSetFloat(srlist,NhlNtmXBMajorLengthF,.006);
      NhlRLSetFloat(srlist,NhlNtmXTMajorLengthF,0.);
      NhlRLSetFloat(srlist,NhlNtmXTMajorOutwardLengthF,0.);
      NhlRLSetFloat(srlist,NhlNtmYLMajorOutwardLengthF,.006);
      NhlRLSetFloat(srlist,NhlNtmYLMajorLengthF,.006);
      NhlRLSetString(srlist,NhlNtmXBMinorOn,"False");
      NhlRLSetString(srlist,NhlNtmXTMinorOn,"False");
      NhlRLSetString(srlist,NhlNtmYLMinorOn,"False");
      NhlRLSetString(srlist,NhlNtmYRMinorOn,"False");
      NhlCreate(&cn,"cn",NhlcontourPlotClass,x1,srlist);
/*
 * Create another ContourPlot object.
 */
      cellsize = .8/360.;
      NhlRLClear(srlist);
      NhlRLSetInteger(srlist,NhlNcnScalarFieldData,jan);
      NhlRLSetFloat(srlist,NhlNvpXF,.06);
      NhlRLSetFloat(srlist,NhlNvpYF,.65);
      NhlRLSetFloat(srlist,NhlNvpWidthF,.60);
      NhlRLSetFloat(srlist,NhlNvpHeightF,.30);
      NhlRLSetString(srlist,NhlNtmXBOn,"False");
      NhlRLSetString(srlist,NhlNtmYLOn,"False");
      NhlRLSetString(srlist,NhlNtmXBMinorOn,"False");
      NhlRLSetString(srlist,NhlNtmYLMinorOn,"False");
      NhlRLSetString(srlist,NhlNtmXBLabelsOn,"False");
      NhlRLSetString(srlist,NhlNtmYLLabelsOn,"False");
      NhlRLSetFloat(srlist,NhlNtmXBMajorLengthF,0.);
      NhlRLSetFloat(srlist,NhlNtmYLMajorLengthF,0.);
      NhlRLSetString(srlist,NhlNcnFillOn,"True");
      NhlRLSetString(srlist,NhlNcnFillMode,"RasterFill");
      NhlRLSetFloat(srlist,NhlNcnRasterCellSizeF,cellsize);
      NhlRLSetFloat(srlist,NhlNcnMinLevelValF,-2.0);
      NhlRLSetIntegerArray(srlist,NhlNcnFillColors,fillcolors,16);
      NhlRLSetString(srlist,NhlNcnLineLabelsOn,"False");
      NhlRLSetString(srlist,NhlNcnLinesOn,"False");
      NhlRLSetString(srlist,NhlNcnMonoFillColor,"False");
      NhlRLSetString(srlist,NhlNcnInfoLabelOn,"False");
      NhlRLSetString(srlist,NhlNcnHighLabelsOn,"False");
      NhlCreate(&ice,"ice",NhlcontourPlotClass,x1,srlist);
/*
 * Create a MapPlot object.
 */
      NhlRLClear(srlist);
      NhlRLSetFloat(srlist,NhlNvpXF,.06);
      NhlRLSetFloat(srlist,NhlNvpYF,.65);
      NhlRLSetFloat(srlist,NhlNvpWidthF,.60);
      NhlRLSetFloat(srlist,NhlNvpHeightF,.30);
      NhlRLSetString(srlist,NhlNmpFillOn,"True");
      NhlRLSetString(srlist,NhlNmpLabelsOn,"False");
      NhlRLSetString(srlist,NhlNmpDefaultFillColor,"DarkSalmon");
      NhlRLSetString(srlist,NhlNmpLandFillColor,"DarkSalmon");
      NhlRLSetString(srlist,NhlNmpOutlineOn,"False");
      NhlRLSetString(srlist,NhlNmpAreaMaskingOn,"True");
      NhlRLSetString(srlist,NhlNmpMaskAreaSpecifiers,"Oceans");
      NhlRLSetInteger(srlist,NhlNmpInlandWaterFillColor,2);
      NhlRLSetString(srlist,NhlNmpGridAndLimbOn,"False");
      NhlRLSetString(srlist,NhlNmpLimitMode,"latlon");
      NhlRLSetFloat(srlist,NhlNmpMinLonF,0.);
      NhlRLSetFloat(srlist,NhlNmpMaxLonF,360.);
      NhlRLSetFloat(srlist,NhlNmpCenterLonF,180.);
      NhlCreate(&mp,"mp",NhlmapPlotClass,x1,srlist);
/*
 * Create a TextItem object.
 */
      NhlRLClear(srlist);
      NhlRLSetFloat(srlist,NhlNtxPosXF,0.5);
      NhlRLSetFloat(srlist,NhlNtxPosYF,0.8);
      NhlRLSetString(srlist,NhlNtxJust,"CENTERCENTER");
      NhlRLSetString(srlist,NhlNtxString,"January Climatological Surface Temperature");
      NhlRLSetFloat(srlist,NhlNtxFontHeightF,.030);
      NhlRLSetInteger(srlist,NhlNtxFont,25);
      NhlCreate(&tx,"tx",NhltextItemClass,x1,srlist);
/*
 *  Read the ocean(1)/land(2) mask ascii dataset created by Areas/Ezmap.
 */
      sprintf( filename, "%s/asc/oceanland30e.asc", dir );
      fp = fopen(filename,"r");
      for(nlt=0; nlt < NLAT; nlt++) {
        ksst   = 0;     /* knt the number of grid pts at ocean locations */
        sstzon = 0.0;    /* save and plt later as [lat vs zoneave] */

/*
 * loop over all lon at a particular lat
 */
        for(nln=0; nln < NLON; nln++ ) {  
          fscanf(fp,"%d", &ocean1 );
          if (ocean1 == 1) {
            ksst++;
            sstzon = sstzon + sstjan[nlt][nln];
          }
        }
        if (ksst != 0) {
          zoneave[nlt] = sstzon/ksst;
        }
      }
/*
 * Create a coordarrays data object.
 */
      NhlRLClear(srlist);
      NhlRLSetFloatArray(srlist,NhlNcaYArray,lat,NLAT);
      NhlRLSetFloatArray(srlist,NhlNcaXArray,zoneave,NLAT);
      NhlRLSetFloat(srlist,NhlNcaXMissingV,1.e36);
      NhlCreate(&zonal,"zonal",NhlcoordArraysClass,appid,srlist);
/*
 * Create XyPlot object and assign data to it.
 */
      NhlRLClear(srlist);
      NhlRLSetFloat(srlist,NhlNvpXF, .73);
      NhlRLSetFloat(srlist,NhlNvpYF, .65);
      NhlRLSetFloat(srlist,NhlNvpWidthF, .25);
      NhlRLSetFloat(srlist,NhlNvpHeightF,.30);
      NhlRLSetInteger(srlist,NhlNxyCoordData, zonal);
      NhlRLSetFloat(srlist,NhlNtrYMaxF,90.);
      NhlRLSetFloat(srlist,NhlNtrYMinF,-90.);
      NhlRLSetFloat(srlist,NhlNtrXMaxF,30.);
      NhlRLSetFloat(srlist,NhlNtrXMinF,0.);
      NhlRLSetString(srlist,NhlNtmXTLabelsOn,"False");
      NhlRLSetString(srlist,NhlNtmYRLabelsOn,"False");
      NhlRLSetString(srlist,NhlNtmYLLabelsOn,"False");
      NhlRLSetFloat(srlist,NhlNtmXBMajorLengthF,.006);
      NhlRLSetFloat(srlist,NhlNtmXBMajorOutwardLengthF,.006);
      NhlRLSetFloat(srlist,NhlNtmXBLabelFontHeightF, .010);
      NhlRLSetFloat(srlist,NhlNtmYLMajorOutwardLengthF,.006);
      NhlRLSetFloat(srlist,NhlNtmYLMajorLengthF,.006);
      NhlRLSetFloat(srlist,NhlNtmXTMajorLengthF,0.);
      NhlRLSetFloat(srlist,NhlNtmXTMajorOutwardLengthF,0.);
      NhlRLSetFloat(srlist,NhlNtmYRMajorLengthF,0.);
      NhlRLSetFloat(srlist,NhlNtmYRMajorOutwardLengthF,0.);
      NhlRLSetString(srlist,NhlNtmXBMinorOn,"False");
      NhlRLSetString(srlist,NhlNtmYLMinorOn,"False");
      NhlRLSetString(srlist,NhlNtmXBMode,"EXPLICIT");
      NhlRLSetFloatArray(srlist,NhlNtmXBValues,xbvalues2,11);
      NhlRLSetStringArray(srlist,NhlNtmXBLabels,xblabels2,11);
      NhlCreate(&xy_plot,"xy_plot",NhlxyPlotClass,x1,srlist);
/*
 * Draw all objects to X11 window.
 */
      NhlDraw(ice);
      NhlDraw(cn);
      NhlDraw(mp);
      NhlDraw(xy_plot);
      NhlDraw(tx);
      NhlFrame(x1);
/*
 * Reassign the workstation to save an ncgm.
 */
      NhlChangeWorkstation (ice,ncgm1);
      NhlChangeWorkstation (cn,ncgm1);
      NhlChangeWorkstation (xy_plot,ncgm1);
      NhlChangeWorkstation (tx,ncgm1);
      NhlChangeWorkstation (mp,ncgm1);
/*
 * Draw all objects to the NCGM.
 */
      NhlDraw(ice);
      NhlDraw(cn);
      NhlDraw(mp);
      NhlDraw(xy_plot);
      NhlDraw(tx);
      NhlFrame(ncgm1);
/*
 * Reassign the workstation to save PS.
 */
      NhlChangeWorkstation (ice,ps1);
      NhlChangeWorkstation (cn,ps1);
      NhlChangeWorkstation (mp,ps1);
      NhlChangeWorkstation (xy_plot,ps1);
      NhlChangeWorkstation (tx,ps1);
/*
 * Draw all objects to PostScript.
 */
      NhlDraw(ice);
      NhlDraw(cn);
      NhlDraw(mp);
      NhlDraw(xy_plot);
      NhlDraw(tx);
      NhlFrame(ps1);
/*
 * Reassign the workstation to save PDF.
 */
      NhlChangeWorkstation (ice,pdf1);
      NhlChangeWorkstation (cn,pdf1);
      NhlChangeWorkstation (mp,pdf1);
      NhlChangeWorkstation (xy_plot,pdf1);
      NhlChangeWorkstation (tx,pdf1);
/*
 * Draw all objects to PDF.
 */
      NhlDraw(ice);
      NhlDraw(cn);
      NhlDraw(mp);
      NhlDraw(xy_plot);
      NhlDraw(tx);
      NhlFrame(pdf1);
/*
 * Reassign the workstation to save cairo PS.
 */
      NhlChangeWorkstation (ice,ps2);
      NhlChangeWorkstation (cn,ps2);
      NhlChangeWorkstation (mp,ps2);
      NhlChangeWorkstation (xy_plot,ps2);
      NhlChangeWorkstation (tx,ps2);
/*
 * Draw all objects to cairo PS.
 */
      NhlDraw(ice);
      NhlDraw(cn);
      NhlDraw(mp);
      NhlDraw(xy_plot);
      NhlDraw(tx);
      NhlFrame(ps2);
/*
 * Reassign the workstation to save cairo PDF.
 */
      NhlChangeWorkstation (ice,pdf2);
      NhlChangeWorkstation (cn,pdf2);
      NhlChangeWorkstation (mp,pdf2);
      NhlChangeWorkstation (xy_plot,pdf2);
      NhlChangeWorkstation (tx,pdf2);
/*
 * Draw all objects to cairo PDF.
 */
      NhlDraw(ice);
      NhlDraw(cn);
      NhlDraw(mp);
      NhlDraw(xy_plot);
      NhlDraw(tx);
      NhlFrame(pdf2);
/*
 * Reassign the workstation to save cairo PNG.
 */
      NhlChangeWorkstation (ice,png1);
      NhlChangeWorkstation (cn,png1);
      NhlChangeWorkstation (mp,png1);
      NhlChangeWorkstation (xy_plot,png1);
      NhlChangeWorkstation (tx,png1);
/*
 * Draw all objects to cairo PNG.
 */
      NhlDraw(ice);
      NhlDraw(cn);
      NhlDraw(mp);
      NhlDraw(xy_plot);
      NhlDraw(tx);
      NhlFrame(png1);
 /*
  * Remove resources
  */
      NhlDestroy(ncgm1);
      NhlDestroy(x1);
      NhlDestroy(ps1);
      NhlDestroy(pdf1);
      NhlDestroy(ps2);
      NhlDestroy(pdf2);
      NhlDestroy(png1);
      NhlDestroy(appid);
      return(0);
}



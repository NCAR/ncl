/*
**      $Id: cn10c.c,v 1.10 2010-03-15 22:49:23 haley Exp $
*/
/***********************************************************************
*                                                                      *
*                Copyright (C)  1995                                   *
*        University Corporation for Atmospheric Research               *
*     The use of this Software is governed by a License Agreement      *
*                                                                      *
***********************************************************************/
/*
**  File:       cn10c.c
**
**  Author:     Fred Clare (converted to C by Mary Haley)
**          National Center for Atmospheric Research
**          PO 3000, Boulder, Colorado
**
**  Date:       Mon Sep 25 15:51:33 MDT 1995
**
**  Description:    Reads a netCDF file and produces five plots:
**
**                     1.)  A quick-and-dirty contour plot.
**                     2.)  An improved contour plot.
**                     3.)  A contour plot of a sub-area of plot 2.)
**                     4.)  An XyPlot that is a slice through the
**                          the contoured area in 2.)
**                     5.)  An overlay of the contour plot in 2.) on
**                          an MapPlot object.
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
#include <ncarg/hlu/XyPlot.h>
#include <ncarg/hlu/ScalarField.h>
#include <ncarg/hlu/CoordArrays.h>
#include <netcdf.h>

/*
 * Define the maximum number of colors.
 */
#define NCOLORS    27

int main()
{
/*
 * Declare variables for the HLU routine calls.
 */
    int     appid, work_id, field1, field2, con1, con2;
    int     dataspec, mapid, y_dataid, xy_id;
    int     srlist, grlist, i;
    float   ymin, ymax, special_value = -9999.;
    ng_size_t   icount[2];
/*
 * Declare variables for defining color map.
 */
    ng_size_t   length[2];
    int       colors[13];
    float   cmap[NCOLORS][3];
/*
 * Declare variables for getting information from netCDF file.
 */
    int     ncid, mnd_id, xdim_id, ydim_id;
    float   mound[15][18], mound2[5][5], xdim[20], ydim[20];
    long  start[2], count[2], xlen, ylen;
    char    filename[256];
    const char *dir = _NGGetNCARGEnv("data");

    char const *wks_type = "x11";
/*
 * Initialize the HLU library and set up resource template.
 */
    NhlInitialize();
    srlist = NhlRLCreate(NhlSETRL);
/*
 * Modify the color map.  Color indices '0' and '1' are the background
 * and foreground colors respectively.
 */
    cmap[ 0][0] = 1.00; cmap[ 0][1] = 1.00; cmap[ 0][2] = 1.00;
    cmap[ 1][0] = 0.00; cmap[ 1][1] = 0.00; cmap[ 1][2] = 0.00;
    cmap[ 2][0] = 0.00; cmap[ 2][1] = 0.15; cmap[ 2][2] = 1.00;
    cmap[ 3][0] = 0.00; cmap[ 3][1] = 0.05; cmap[ 3][2] = 1.00;
    cmap[ 4][0] = 0.04; cmap[ 4][1] = 0.00; cmap[ 4][2] = 1.00;
    cmap[ 5][0] = 0.14; cmap[ 5][1] = 0.00; cmap[ 5][2] = 1.00;
    cmap[ 6][0] = 0.24; cmap[ 6][1] = 0.00; cmap[ 6][2] = 1.00;
    cmap[ 7][0] = 0.34; cmap[ 7][1] = 0.00; cmap[ 7][2] = 1.00;
    cmap[ 8][0] = 0.43; cmap[ 8][1] = 0.00; cmap[ 8][2] = 1.00;
    cmap[ 9][0] = 0.53; cmap[ 9][1] = 0.00; cmap[ 9][2] = 1.00;
    cmap[10][0] = 0.63; cmap[10][1] = 0.00; cmap[10][2] = 1.00;
    cmap[11][0] = 0.73; cmap[11][1] = 0.00; cmap[11][2] = 1.00;
    cmap[12][0] = 0.83; cmap[12][1] = 0.00; cmap[12][2] = 1.00;
    cmap[13][0] = 0.92; cmap[13][1] = 0.00; cmap[13][2] = 1.00;
    cmap[14][0] = 1.00; cmap[14][1] = 0.00; cmap[14][2] = 0.98;
    cmap[15][0] = 1.00; cmap[15][1] = 0.00; cmap[15][2] = 0.88;
    cmap[16][0] = 1.00; cmap[16][1] = 0.00; cmap[16][2] = 0.78;
    cmap[17][0] = 1.00; cmap[17][1] = 0.00; cmap[17][2] = 0.68;
    cmap[18][0] = 1.00; cmap[18][1] = 0.00; cmap[18][2] = 0.59;
    cmap[19][0] = 1.00; cmap[19][1] = 0.00; cmap[19][2] = 0.49;
    cmap[20][0] = 1.00; cmap[20][1] = 0.00; cmap[20][2] = 0.39;
    cmap[21][0] = 1.00; cmap[21][1] = 0.00; cmap[21][2] = 0.29;
    cmap[22][0] = 1.00; cmap[22][1] = 0.00; cmap[22][2] = 0.20;
    cmap[23][0] = 1.00; cmap[23][1] = 0.00; cmap[23][2] = 0.10;
    cmap[24][0] = 1.00; cmap[24][1] = 0.00; cmap[24][2] = 0.00;
/*
 *  Colors used for labels.
 */
    cmap[25][0] = 0.00; cmap[25][1] = 0.00; cmap[25][2] = 0.00;
    cmap[26][0] = 0.40; cmap[26][1] = 0.00; cmap[26][2] = 0.40;
    length[0] = NCOLORS;  length[1] = 3;
/*
 * Create Application object.  The Application object name is used to
 * determine the name of the resource file, which is "cn10.res" in
 * this case.
 */
    NhlRLClear(srlist);
    NhlRLSetString(srlist,NhlNappDefaultParent,"True");
    NhlRLSetString(srlist,NhlNappUsrDir,"./");
    NhlCreate(&appid,"cn10",NhlappClass,NhlDEFAULT_APP,srlist);

    if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
/*
 * Create a meta file object.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkMetaName,"./cn10c.ncgm");
        NhlRLSetMDFloatArray(srlist,NhlNwkColorMap,&cmap[0][0],2,length);
        NhlCreate(&work_id,"cn10Work",NhlncgmWorkstationClass,
                  NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 * Create an X11 workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetInteger(srlist,NhlNwkPause,True);
        NhlRLSetMDFloatArray(srlist,NhlNwkColorMap,&cmap[0][0],2,length);
        NhlCreate(&work_id,"cn10Work",NhlcairoWindowWorkstationClass,
              NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 * Create an older-style PostScript workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkPSFileName,"./cn10c.ps");
        NhlRLSetMDFloatArray(srlist,NhlNwkColorMap,&cmap[0][0],2,length);
        NhlCreate(&work_id,"cn10Work",NhlpsWorkstationClass,
                  NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 * Create an older-style PDF workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkPDFFileName,"./cn10c.pdf");
        NhlRLSetMDFloatArray(srlist,NhlNwkColorMap,&cmap[0][0],2,length);
        NhlCreate(&work_id,"cn10Work",NhlpdfWorkstationClass,
                  NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
             !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 * Create a cairo PS/PDF workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkFileName,"./cn10c");
        NhlRLSetString(srlist,NhlNwkFormat,(char*)wks_type);
        NhlRLSetMDFloatArray(srlist,NhlNwkColorMap,&cmap[0][0],2,length);
        NhlCreate(&work_id,"cn10Work",NhlcairoDocumentWorkstationClass,
                  NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 * Create a cairo PNG workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkFileName,"./cn10c");
        NhlRLSetString(srlist,NhlNwkFormat,(char*)wks_type);
        NhlRLSetMDFloatArray(srlist,NhlNwkColorMap,&cmap[0][0],2,length);
        NhlCreate(&work_id,"cn10Work",NhlcairoImageWorkstationClass,
                  NhlDEFAULT_APP,srlist);
    }
/*
 * Open the netCDF file.
 */
    sprintf( filename, "%s/cdf/cn10n.cdf", dir );
    ncid = ncopen(filename,NC_NOWRITE);
/*
 * Get the mound length.
 */
    xdim_id = ncdimid(ncid,"xdim");
    ydim_id = ncdimid(ncid,"ydim");
    mnd_id = ncvarid(ncid,"mound");
    ncdiminq(ncid,xdim_id,(char *)0,&xlen);
    ncdiminq(ncid,ydim_id,(char *)0,&ylen);
    xdim_id = ncvarid(ncid,"xdim");
    ydim_id = ncvarid(ncid,"ydim");
/*
 * Get data and dimensions values for the mound.
 */
    start[0] = start[1] = 0;
    count[0] = xlen; count[1] = ylen;
    ncvarget(ncid,mnd_id,(long const *)start,(long const *)count,mound);
    count[0] = xlen;
    ncvarget(ncid,xdim_id,(long const *)start,(long const *)count,xdim);
    count[0] = ylen;
    ncvarget(ncid,ydim_id,(long const *)start,(long const *)count,ydim);
/*
 * Create a data field.
 */
    icount[0] = xlen; icount[1] = ylen;
    NhlRLClear(srlist);
    NhlRLSetMDFloatArray(srlist,NhlNsfDataArray,&mound[0][0],2,icount);
    NhlRLSetFloat(srlist,NhlNsfMissingValueV,special_value);
    NhlRLSetFloat(srlist,NhlNsfXCStartV,ydim[0]);
    NhlRLSetFloat(srlist,NhlNsfXCEndV,ydim[ylen-1]);
    NhlRLSetFloat(srlist,NhlNsfYCStartV,xdim[0]);
    NhlRLSetFloat(srlist,NhlNsfYCEndV,xdim[xlen-1]);
    NhlCreate(&field1,"field1",NhlscalarFieldClass,appid,srlist);
/*
 * Create a ContourPlot object using the above data field.
 */
    NhlRLClear(srlist);
    NhlRLSetInteger(srlist,NhlNcnScalarFieldData,field1);
    NhlRLSetFloat(srlist,NhlNvpXF,0.15);
    NhlRLSetFloat(srlist,NhlNvpYF,0.9);
    NhlRLSetFloat(srlist,NhlNvpWidthF,0.79);
    NhlRLSetFloat(srlist,NhlNvpHeightF,0.79);
    NhlCreate(&con1,"con1",NhlcontourPlotClass,work_id,srlist);
/*
 * Picture 1:  A quick-and-dirty contour plot.
 */
    NhlDraw(con1);
    NhlFrame(work_id);
/*
 * Picture 2:  An improved contour plot.
 */
    NhlRLClear(srlist);
    NhlRLSetInteger(srlist,NhlNcnFillOn,True);
    NhlRLSetInteger(srlist,NhlNcnMonoFillColor,False);
    NhlRLSetFloat(srlist,NhlNcnLevelSpacingF,2.0);
    NhlRLSetFloat(srlist,NhlNcnLevelSpacingF,2.0);
    NhlRLSetInteger(srlist,NhlNcnSmoothingOn,True);
    NhlRLSetFloat(srlist,NhlNcnMaxPointDistanceF,0.0);
    NhlRLSetInteger(srlist,NhlNcnLineColor,0);
    NhlRLSetInteger(srlist,NhlNcnLineLabelFontColor,25);
    NhlRLSetInteger(srlist,NhlNcnLineLabelFont,22);
    NhlRLSetFloat(srlist,NhlNcnLineLabelFontHeightF,.02);
    NhlRLSetInteger(srlist,NhlNcnLineLabelInterval,3);
    NhlRLSetString(srlist,NhlNcnLineLabelPlacementMode,"COMPUTED");
    NhlRLSetInteger(srlist,NhlNcnHighLabelFont,22);
    NhlRLSetFloat(srlist,NhlNcnHighLabelFontHeightF,.025);
    NhlRLSetInteger(srlist,NhlNcnHighLabelFontColor,25);
    NhlRLSetInteger(srlist,NhlNcnInfoLabelOn,False);

    NhlRLSetInteger(srlist,NhlNtmXBMinorPerMajor,2);

    NhlRLSetInteger(srlist,NhlNtmYLMajorLineColor,26);
    NhlRLSetInteger(srlist,NhlNtmYLMinorLineColor,26);
    NhlRLSetFloat(srlist,NhlNtmYLMinorThicknessF,2.);
    NhlRLSetInteger(srlist,NhlNtmXBMajorLineColor,26);
    NhlRLSetInteger(srlist,NhlNtmXBMinorLineColor,26);
    NhlRLSetFloat(srlist,NhlNtmXBMinorThicknessF,2.);

    NhlRLSetInteger(srlist,NhlNtmXBLabelFont,21);
    NhlRLSetFloat(srlist,NhlNtmXBLabelFontHeightF,.03);
    NhlRLSetInteger(srlist,NhlNtmXBLabelFontColor,26);

    NhlRLSetInteger(srlist,NhlNtmYLLabelFont,21);
    NhlRLSetFloat(srlist,NhlNtmYLLabelFontHeightF,.03);
    NhlRLSetInteger(srlist,NhlNtmYLLabelFontColor,26);

    NhlRLSetInteger(srlist,NhlNtmBorderLineColor,26);

    NhlRLSetInteger(srlist,NhlNtiMainOn,True);
    NhlRLSetInteger(srlist,NhlNtiMainFont,26);
    NhlRLSetFloat(srlist,NhlNtiMainFontHeightF,.04);
    NhlRLSetString(srlist,NhlNtiMainString,"The Hot Zone");
    NhlRLSetFloat(srlist,NhlNtiMainOffsetYF,-0.025);
    NhlRLSetInteger(srlist,NhlNtiMainFontColor,26);
    NhlSetValues(con1,srlist);
    NhlDraw(con1);
    NhlFrame(work_id);
/*
 * Picture 3:  Zero in on the top of the mound.
 */
    start[0] = 2;
    start[1] = 4;
    count[0] = count[1] = 5;
    ncvarget(ncid,mnd_id,(long const *)start,(long const *)count,mound2);

    icount[0] = icount[1] = 5;
    NhlRLClear(srlist);
    NhlRLSetMDFloatArray(srlist,NhlNsfDataArray,&mound2[0][0],2,icount);
    NhlRLSetFloat(srlist,NhlNsfXCStartV,-20.);
    NhlRLSetFloat(srlist,NhlNsfXCEndV,20.);
    NhlRLSetFloat(srlist,NhlNsfYCStartV, -20.);
    NhlRLSetFloat(srlist,NhlNsfYCEndV,20.);
    NhlSetValues(field1,srlist);

    NhlRLClear(srlist);
    NhlRLSetString(srlist,NhlNtiMainString,"A closer look");
    NhlRLSetInteger(srlist,NhlNtmXBMinorPerMajor,4);
    NhlRLSetInteger(srlist,NhlNtmYLMinorPerMajor,4);
    NhlRLSetFloat(srlist,NhlNcnLevelSpacingF,0.15);
    for( i = 0; i <= 10; i++ ) colors[i] = i+9;
    colors[11] = 21;
    colors[12] = 23;
    NhlRLSetIntegerArray(srlist,NhlNcnFillColors,colors,13);
    NhlSetValues(con1,srlist);
    NhlDraw(con1);
    NhlFrame(work_id);
/*
 * Picture 4:  Plot an XyPlot of X values for a specific Y value.
 */
    NhlRLClear(srlist);
    NhlRLSetFloatArray(srlist,NhlNcaXArray,ydim,ylen);
    NhlRLSetFloatArray(srlist,NhlNcaYArray,&mound[5][0],ylen);
    NhlCreate(&y_dataid,"xyData",NhlcoordArraysClass,appid,srlist);

    NhlRLClear(srlist);
    NhlRLSetFloat(srlist,NhlNvpXF,.2);
    NhlRLSetFloat(srlist,NhlNvpYF,.85);
    NhlRLSetFloat(srlist,NhlNvpWidthF,.7);
    NhlRLSetFloat(srlist,NhlNvpHeightF,.7);

    NhlRLSetInteger(srlist,NhlNxyComputeYMax,False);
    NhlRLSetInteger(srlist,NhlNxyComputeYMin,False);

    NhlRLSetInteger(srlist,NhlNtiMainOn,True);
    NhlRLSetInteger(srlist,NhlNtiMainFont,26);
    NhlRLSetFloat(srlist,NhlNtiMainFontHeightF,.0275);
    NhlRLSetString(srlist,NhlNtiMainString,"For X = 10.");
    NhlRLSetFloat(srlist,NhlNtiMainOffsetYF,-0.00);
    NhlRLSetInteger(srlist,NhlNtiMainFontColor,26);
    NhlRLSetString(srlist,NhlNtiXAxisString,"Y");
    NhlRLSetInteger(srlist,NhlNtiXAxisFont,26);
    NhlRLSetInteger(srlist,NhlNtiXAxisFontColor,26);
    NhlRLSetFloat(srlist,NhlNtiXAxisFontHeightF,.025);
    NhlRLSetString(srlist,NhlNtiYAxisString,"F(Y)");
    NhlRLSetInteger(srlist,NhlNtiYAxisFont,26);
    NhlRLSetInteger(srlist,NhlNtiYAxisFontColor,26);
    NhlRLSetFloat(srlist,NhlNtiYAxisFontHeightF,.025);

    NhlRLSetInteger(srlist,NhlNtmXTBorderOn,False);
    NhlRLSetInteger(srlist,NhlNtmYRBorderOn,False);
    NhlRLSetInteger(srlist,NhlNtmXTOn,False);
    NhlRLSetInteger(srlist,NhlNtmYROn,False);
    NhlRLSetInteger(srlist,NhlNtmYLMajorLineColor,26);
    NhlRLSetInteger(srlist,NhlNtmYLMinorLineColor,26);
    NhlRLSetFloat(srlist,NhlNtmYLMinorThicknessF,2.);
    NhlRLSetInteger(srlist,NhlNtmXBMajorLineColor,26);
    NhlRLSetInteger(srlist,NhlNtmXBMinorLineColor,26);
    NhlRLSetFloat(srlist,NhlNtmXBMinorThicknessF,2.);

    NhlRLSetInteger(srlist,NhlNtmXBMinorPerMajor,2);
    NhlRLSetInteger(srlist,NhlNtmYLMinorPerMajor,2);
    NhlRLSetInteger(srlist,NhlNtmBorderLineColor,26);

    NhlRLSetInteger(srlist,NhlNtmXBLabelFont,21);
    NhlRLSetFloat(srlist,NhlNtmXBLabelFontHeightF,.025);
    NhlRLSetInteger(srlist,NhlNtmXBLabelFontColor,26);
    NhlRLSetInteger(srlist,NhlNtmYLLabelFont,21);
    NhlRLSetFloat(srlist,NhlNtmYLLabelFontHeightF,.025);
    NhlRLSetInteger(srlist,NhlNtmYLLabelFontColor,26);
    NhlCreate(&xy_id,"XyPlotData",NhlxyPlotClass,work_id,srlist);

    dataspec = NhlAddData(xy_id,"xyCoordData",y_dataid);

    NhlRLClear(srlist);
    NhlRLSetInteger(srlist,NhlNxyMonoLineColor,True);
    NhlRLSetInteger(srlist,NhlNxyLineColor,24);
    NhlRLSetFloat(srlist,NhlNxyLineThicknessF,2.);
    NhlSetValues(dataspec,srlist);

    grlist = NhlRLCreate(NhlGETRL);
    NhlRLClear(grlist);
    NhlRLGetFloat(grlist,NhlNtmYLTickStartF,&ymin);
    NhlRLGetFloat(grlist,NhlNtmYLTickEndF,&ymax);
    NhlGetValues(xy_id,grlist);

    NhlRLClear(srlist);
    NhlRLSetFloat(srlist,NhlNtrYMinF,ymin);
    NhlRLSetFloat(srlist,NhlNtrYMaxF,ymax);
    NhlSetValues(xy_id,srlist);

    NhlDraw(xy_id);
    NhlFrame(work_id);
/*
 * Picture 5:  Overlay the mound on top of Colorado.
 */
    NhlRLClear(srlist);

    NhlRLSetString(srlist,NhlNpmTitleDisplayMode,"always");

    NhlRLSetInteger(srlist,NhlNtiMainOn,True);
    NhlRLSetInteger(srlist,NhlNtiMainFont,26);
    NhlRLSetFloat(srlist,NhlNtiMainFontHeightF,0.037);
    NhlRLSetFloat(srlist,NhlNtiMainOffsetYF,-0.00);
    NhlRLSetInteger(srlist,NhlNtiMainFontColor,26);
    NhlRLSetString(srlist,NhlNtiMainString,"Mound over Colorado");

    NhlRLSetFloat(srlist,NhlNvpXF,.1);
    NhlRLSetFloat(srlist,NhlNvpYF,.9);
    NhlRLSetFloat(srlist,NhlNvpWidthF,.79);
    NhlRLSetFloat(srlist,NhlNvpHeightF,.79);

    NhlRLSetInteger(srlist,NhlNmpFillOn,False);
    NhlRLSetString(srlist,NhlNmpOutlineBoundarySets,"allBoundaries");
    NhlRLSetString(srlist,NhlNmpProjection,"LambertConformal");
    NhlRLSetFloat(srlist,NhlNmpLambertParallel1F,30.);
    NhlRLSetFloat(srlist,NhlNmpLambertParallel2F,45.);
    NhlRLSetFloat(srlist,NhlNmpLambertMeridianF,-100.);
    NhlRLSetString(srlist,NhlNmpLimitMode,"LatLon");
    NhlRLSetFloat(srlist,NhlNmpMinLatF,33.);
    NhlRLSetFloat(srlist,NhlNmpMaxLatF,44.);
    NhlRLSetFloat(srlist,NhlNmpMinLonF,-115.);
    NhlRLSetFloat(srlist,NhlNmpMaxLonF,-93.);
    NhlRLSetInteger(srlist,NhlNmpUSStateLineColor,25);
    NhlRLSetFloat(srlist,NhlNmpUSStateLineThicknessF,2.);
    NhlRLSetInteger(srlist,NhlNmpGridAndLimbOn,False);
    NhlRLSetInteger(srlist,NhlNmpPerimOn,True);
    NhlRLSetInteger(srlist,NhlNmpPerimLineColor,26);
    NhlRLSetFloat(srlist,NhlNmpPerimLineThicknessF,2.);
    NhlCreate(&mapid,"Map0",NhlmapPlotClass,work_id,srlist);

    NhlRLClear(srlist);
    icount[0] = xlen; icount[1] = ylen;
    NhlRLSetMDFloatArray(srlist,NhlNsfDataArray,&mound[0][0],2,icount);
    NhlRLSetFloat(srlist,NhlNsfXCStartV,-109.05);
    NhlRLSetFloat(srlist,NhlNsfXCEndV,-102.05);
    NhlRLSetFloat(srlist,NhlNsfYCStartV,37.);
    NhlRLSetFloat(srlist,NhlNsfYCEndV,41.);
    NhlCreate(&field2,"field2",NhlscalarFieldClass,appid,srlist);

    NhlRLClear(srlist);
    NhlRLSetInteger(srlist,NhlNcnScalarFieldData,field2);
    NhlRLSetInteger(srlist,NhlNcnFillOn,True);
    NhlRLSetInteger(srlist,NhlNcnMonoFillColor,False);
    NhlRLSetFloat(srlist,NhlNcnLevelSpacingF,8.0);
    NhlRLSetInteger(srlist,NhlNcnSmoothingOn,True);
    NhlRLSetFloat(srlist,NhlNcnMaxPointDistanceF,0.0);
    NhlRLSetInteger(srlist,NhlNcnLineColor,0);
    NhlRLSetInteger(srlist,NhlNcnLineLabelsOn,False);
    NhlRLSetInteger(srlist,NhlNcnHighLabelsOn,False);
    NhlRLSetInteger(srlist,NhlNcnInfoLabelOn,False);
    colors[0] = 3; colors[1] = 7; colors[2] = 11; colors[3] = 15;
    colors[4] = 19; colors[5] = 23;
    NhlRLSetIntegerArray(srlist,NhlNcnFillColors,colors,6);
    NhlCreate(&con2,"con2",NhlcontourPlotClass,work_id,srlist);

    NhlAddOverlay(mapid,con2,-1);
    NhlDraw(mapid);
    NhlFrame(work_id);
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

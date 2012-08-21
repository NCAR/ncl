/*
**      $Id: cn07c.c,v 1.8 2010-03-15 22:49:23 haley Exp $
*/
/***********************************************************************
*                                                                      *
*                Copyright (C)  1995                                   *
*        University Corporation for Atmospheric Research               *
*     The use of this Software is governed by a License Agreement      *
*                                                                      *
***********************************************************************/
/*
**  File:       cn07c.c
**
**  Author:     Ethan Alpert (converted to C by Mary Haley)
**          National Center for Atmospheric Research
**          PO 3000, Boulder, Colorado
**
**  Date:       Fri Oct  6 09:21:57 MDT 1995
**
**  Description:    Reads a netCDF file and produces a series of
**                  contour plots.
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
#include <ncarg/hlu/ScalarField.h>
#include <netcdf.h>

#define NCOLORS 17

int xbvalues[] = {-60,-75,-90,-105,-120,-135};
int ylvalues[] = {60,50,40,30,20};
char *xblabels[] = {"60W","75W","90W","105W","120W","135W"};
char *yllabels[] = {"60N","50N","40N","30N","20N"};

int main()
{
/*
 * Declare variables for the HLU routine calls.
 */
    int     appid, workid, field1, con1;
    int     srlist, i;
    ng_size_t    icount[2];
    float cmap[NCOLORS][3];
/*
 * Declare variables for getting information from netCDF file.
 */
    int     ncid, lon_id, lat_id, frtime_id, Z_id;
    float   Z[33][36], special_value;
    float   lon[36], lat[33];
    long  start[4], count[4], lonlen, latlen, frtimelen;
    long  frtime[7];
    char    filename[256];
    const char *dir = _NGGetNCARGEnv("data");
/*
 * Default is to create an NCGM file.
 */
    char const *wks_type = "ncgm";
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
    NhlCreate(&appid,"cn07",NhlappClass,NhlDEFAULT_APP,srlist);

	cmap[0][0] = 0.0; cmap[0][1] = 0.0; cmap[0][2] = 0.0;
	cmap[1][0] = 1.0; cmap[1][1] = 1.0; cmap[1][2] = 1.0;
	cmap[2][0] = 0.0; cmap[2][1] = 0.0; cmap[2][2] = 0.0;
	cmap[3][0] = 1.0; cmap[3][1] = 0.0; cmap[3][2] = 0.0;
	cmap[4][0] = 0.0; cmap[4][1] = 1.0; cmap[4][2] = 0.0;
	cmap[5][0] = 0.0; cmap[5][1] = 0.0; cmap[5][2] = 1.0;
	cmap[6][0] = 1.0; cmap[6][1] = 1.0; cmap[6][2] = 0.0;
	cmap[7][0] = 0.0; cmap[7][1] = 1.0; cmap[7][2] = 1.0;
	cmap[8][0] = 1.0; cmap[8][1] = 0.0; cmap[8][2] = 1.0;
	cmap[9][0] = 0.5; cmap[9][1] = 0.0; cmap[9][2] = 0.0;
	cmap[10][0] = 0.5; cmap[10][1] = 1.0; cmap[10][2] = 1.0;
	cmap[11][0] = 0.0; cmap[11][1] = 0.0; cmap[11][2] = 0.5;
	cmap[12][0] = 1.0; cmap[12][1] = 1.0; cmap[12][2] = 0.5;
	cmap[13][0] = 0.5; cmap[13][1] = 0.0; cmap[13][2] = 1.0;
	cmap[14][0] = 1.0; cmap[14][1] = 0.5; cmap[14][2] = 0.0;
	cmap[15][0] = 0.0; cmap[15][1] = 0.5; cmap[15][2] = 1.0;
	cmap[16][0] = 0.5; cmap[16][1] = 1.0; cmap[16][2] = 0.0;

    icount[0] = NCOLORS;
    icount[1] = 3;

    if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
/*
 * Create a meta file object.
 */
        NhlRLClear(srlist);
		NhlRLSetMDFloatArray(srlist,NhlNwkColorMap,&cmap[0][0],2,icount);
        NhlRLSetString(srlist,NhlNwkMetaName,"./cn07c.ncgm");
        NhlCreate(&workid,"cn07Work",NhlncgmWorkstationClass,
                  NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 * Create an X11 workstation.
 */
        NhlRLClear(srlist);
		NhlRLSetMDFloatArray(srlist,NhlNwkColorMap,&cmap[0][0],2,icount);
        NhlRLSetInteger(srlist,NhlNwkPause,True);
        NhlCreate(&workid,"cn07Work",NhlcairoWindowWorkstationClass,
              NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 * Create an older-style PostScript workstation.
 */
        NhlRLClear(srlist);
		NhlRLSetMDFloatArray(srlist,NhlNwkColorMap,&cmap[0][0],2,icount);
        NhlRLSetString(srlist,NhlNwkPSFileName,"./cn07c.ps");
        NhlCreate(&workid,"cn07Work",NhlpsWorkstationClass,
                  NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 * Create an older-style PDF workstation.
 */
        NhlRLClear(srlist);
		NhlRLSetMDFloatArray(srlist,NhlNwkColorMap,&cmap[0][0],2,icount);
        NhlRLSetString(srlist,NhlNwkPDFFileName,"./cn07c.pdf");
        NhlCreate(&workid,"cn07Work",NhlpdfWorkstationClass,
                  NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
             !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 * Create a cairo PS/PDF workstation.
 */
        NhlRLClear(srlist);
		NhlRLSetMDFloatArray(srlist,NhlNwkColorMap,&cmap[0][0],2,icount);
        NhlRLSetString(srlist,NhlNwkFileName,"./cn07c");
        NhlRLSetString(srlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&workid,"cn07Work",NhlcairoDocumentWorkstationClass,
                  NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 * Create a cairo PNG workstation.
 */
        NhlRLClear(srlist);
		NhlRLSetMDFloatArray(srlist,NhlNwkColorMap,&cmap[0][0],2,icount);
        NhlRLSetString(srlist,NhlNwkFileName,"./cn07c");
        NhlRLSetString(srlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&workid,"cn07Work",NhlcairoImageWorkstationClass,
                  NhlDEFAULT_APP,srlist);
    }
/*
 * Open NetCDF file containing Geo-Potential height forecast
 * information.
 */
    sprintf( filename, "%s/cdf/contour.cdf", dir );
    ncid = ncopen(filename,NC_NOWRITE);
/*
 * Get the lat/lon dimensions.
 */
    lat_id = ncdimid(ncid,"lat");
    lon_id = ncdimid(ncid,"lon");
    frtime_id  = ncdimid(ncid,"frtime");
    ncdiminq(ncid,lat_id,(char *)0,&latlen);
    ncdiminq(ncid,lon_id,(char *)0,&lonlen);
    ncdiminq(ncid,frtime_id,(char *)0,&frtimelen);
/*
 * Read in Z.
 */
    Z_id = ncvarid(ncid,"Z");
    start[0] = start[2] = start[3] = 0;
    start[1] = 3;
    count[0] = count[1] = 1;
    count[2] = latlen;
    count[3] = lonlen;
    ncvarget(ncid,Z_id,(long const *)start,(long const *)count,Z);
    ncattget(ncid,Z_id,"_FillValue",&special_value);
/*
 * Read in lat/lon/frtime values.
 */
    lat_id = ncvarid(ncid,"lat");
    count[0] = latlen;
    ncvarget(ncid,lat_id,(long const *)start,(long const *)count,lat);

    lon_id = ncvarid(ncid,"lon");
    count[0] = lonlen;
    ncvarget(ncid,lon_id,(long const *)start,(long const *)count,lon);

    frtime_id = ncvarid(ncid,"frtime");
    count[0] = frtimelen;
    ncvarget(ncid,frtime_id,(long const *)start,(long const *)count,frtime);
/*
 * Create a scalar field object and configure the missing values and
 * the start and end information.
 */
    icount[0] = latlen; icount[1] = lonlen;
    NhlRLClear(srlist);
    NhlRLSetMDFloatArray(srlist,NhlNsfDataArray,&Z[0][0],2,icount);
    NhlRLSetFloat(srlist,NhlNsfMissingValueV,special_value);
    NhlRLSetFloat(srlist,NhlNsfXCStartV,lon[0]);
    NhlRLSetFloat(srlist,NhlNsfXCEndV,lon[lonlen-1]);
    NhlRLSetFloat(srlist,NhlNsfYCStartV,lat[0]);
    NhlRLSetFloat(srlist,NhlNsfYCEndV,lat[latlen-1]);
    NhlCreate(&field1,"field1",NhlscalarFieldClass,appid,srlist);
/*
 * Create contour object.
 */
    NhlRLClear(srlist);
    NhlRLSetFloat(srlist,NhlNvpXF,.2);
    NhlRLSetFloat(srlist,NhlNvpYF,.8);
    NhlRLSetFloat(srlist,NhlNvpWidthF,.6);
    NhlRLSetFloat(srlist,NhlNvpHeightF,.6);
    NhlRLSetInteger(srlist,NhlNcnScalarFieldData,field1);
    NhlRLSetString(srlist,NhlNcnLevelSelectionMode,"ManualLevels");
    NhlRLSetFloat(srlist,NhlNcnMinLevelValF,5400.0);
    NhlRLSetFloat(srlist,NhlNcnMaxLevelValF,5950.0);
    NhlRLSetFloat(srlist,NhlNcnLevelSpacingF,50.0);
    NhlRLSetString(srlist,NhlNcnFillOn,"True");
    NhlRLSetString(srlist,NhlNcnLowLabelsOn,"True");
    NhlRLSetString(srlist,NhlNcnHighLabelsOn,"True");
    NhlRLSetFloat(srlist,NhlNtrXMinF,-140.0);
    NhlRLSetFloat(srlist,NhlNtrXMaxF,-52.5);
    NhlRLSetFloat(srlist,NhlNtrYMinF,20.0);
    NhlRLSetFloat(srlist,NhlNtrYMaxF,60.0);
    NhlRLSetString(srlist,NhlNtiMainString,"Geo-potential height @500mb");
    NhlRLSetString(srlist,NhlNtiXAxisString,"Lon");
    NhlRLSetString(srlist,NhlNtiYAxisString,"Lat");
    NhlRLSetString(srlist,NhlNtmXBMode,"EXPLICIT");
    NhlRLSetIntegerArray(srlist,NhlNtmXBValues,xbvalues,NhlNumber(xbvalues));
    NhlRLSetStringArray(srlist,NhlNtmXBLabels,xblabels,NhlNumber(xblabels));
    NhlRLSetString(srlist,NhlNtmYLMode,"EXPLICIT");
    NhlRLSetIntegerArray(srlist,NhlNtmYLValues,ylvalues,NhlNumber(ylvalues));
    NhlRLSetStringArray(srlist,NhlNtmYLLabels,yllabels,NhlNumber(yllabels));
    NhlRLSetString(srlist,NhlNtmXMajorGrid,"True");
    NhlRLSetString(srlist,NhlNtmYMajorGrid,"True");
    NhlRLSetString(srlist,NhlNtmXBMinorOn,"False");
    NhlRLSetString(srlist,NhlNtmYLMinorOn,"False");
    NhlCreate(&con1,"con1",NhlcontourPlotClass,workid,srlist);
/* 
 * Draw first frame
 */
    NhlDraw(con1);
    NhlFrame(workid);
/*
 * Loop on remaining time steps.
 */
    for( i = 1; i <= frtimelen-1; i++ ) {
/*
 * Read in new section of Z.
 */
        start[0] = i;
        start[1] = 3;
        start[2] = 0;
        start[3] = 0;
        count[0] = 1;
        count[1] = 1;
        count[2] = latlen;
        count[3] = lonlen;
        ncvarget(ncid,Z_id,(long const *)start,(long const *)count,Z);
/*
 * Create new scalar field.
 */
        NhlRLClear(srlist);
        icount[0] = latlen; icount[1] = lonlen;
        NhlRLSetMDFloatArray(srlist,NhlNsfDataArray,&Z[0][0],2,
                            icount);
        NhlSetValues(field1,srlist);
        NhlDraw(con1);
        NhlFrame(workid);
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

/*
 *      $Id: cn06c.c,v 1.8 2010-03-15 22:49:23 haley Exp $
 */
/***********************************************************************
*                                                                      *
*                Copyright (C)  1996                                   *
*        University Corporation for Atmospheric Research               *
*     The use of this Software is governed by a License Agreement      *
*                                                                      *
***********************************************************************/
/*
**  File:       cn06c.c
**
**  Author:     Ethan Alpert (converted to C by Mary Haley)
**          National Center for Atmospheric Research
**          PO 3000, Boulder, Colorado
**
**  Date:       Thu Jan  4 11:17:41 MDT 1996
**
**  Description:    Reads a netCDF file and produces a series of
**                  temperature contour plots.
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

int xbvalues[] = {-60,-75,-90,-105,-120,-135};
int ylvalues[] = {60,50,40,30,20};
char *xblabels[] = {"60W","75W","90W","105W","120W","135W"};
char *yllabels[] = {"60N","50N","40N","30N","20N"};

#define NLAT  33
#define NLON  36
#define NCOLORS 17

int main()
{
    extern void KtoF();
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
    int     ncid, lon_id, lat_id, frtime_id, Tid;
    float   T[NLAT][NLON], special_value;
    float   lon[NLON], lat[NLAT];
    long  start[4], count[4], lonlen, latlen, frtimelen;
    long  frtime[7];
    char    filename[256], *title, *hist, full_title[256];
    char    lat_name[128], lon_name[128];
    nc_type t_type;
    int     t_len;
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
    NhlCreate(&appid,"cn06",NhlappClass,NhlDEFAULT_APP,srlist);

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
        NhlRLSetString(srlist,NhlNwkMetaName,"./cn06c.ncgm");
		NhlRLSetMDFloatArray(srlist,NhlNwkColorMap,&cmap[0][0],2,icount);
        NhlCreate(&workid,"cn06Work",NhlncgmWorkstationClass,
                  NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 * Create an X11 workstation.
 */
        NhlRLClear(srlist);
		NhlRLSetMDFloatArray(srlist,NhlNwkColorMap,&cmap[0][0],2,icount);
        NhlRLSetString(srlist,NhlNwkPause,"True");
        NhlCreate(&workid,"cn06Work",NhlcairoWindowWorkstationClass,
              NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 * Create an older-style PostScript workstation.
 */
        NhlRLClear(srlist);
		NhlRLSetMDFloatArray(srlist,NhlNwkColorMap,&cmap[0][0],2,icount);
        NhlRLSetString(srlist,NhlNwkPSFileName,"./cn06c.ps");
        NhlCreate(&workid,"cn06Work",NhlpsWorkstationClass,
                  NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 * Create an older-style PDF workstation.
 */
        NhlRLClear(srlist);
		NhlRLSetMDFloatArray(srlist,NhlNwkColorMap,&cmap[0][0],2,icount);
        NhlRLSetString(srlist,NhlNwkPDFFileName,"./cn06c.pdf");
        NhlCreate(&workid,"cn06Work",NhlpdfWorkstationClass,
                  NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
             !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 * Create a cairo PS/PDF workstation.
 */
        NhlRLClear(srlist);
		NhlRLSetMDFloatArray(srlist,NhlNwkColorMap,&cmap[0][0],2,icount);
        NhlRLSetString(srlist,NhlNwkFileName,"./cn06c");
        NhlRLSetString(srlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&workid,"cn06Work",NhlcairoDocumentWorkstationClass,
                  NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 * Create a cairo PNG workstation.
 */
        NhlRLClear(srlist);
		NhlRLSetMDFloatArray(srlist,NhlNwkColorMap,&cmap[0][0],2,icount);
        NhlRLSetString(srlist,NhlNwkFileName,"./cn06c");
        NhlRLSetString(srlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&workid,"cn06Work",NhlcairoImageWorkstationClass,
                  NhlDEFAULT_APP,srlist);
    }
/*
 * Open NetCDF file.
 */
    sprintf( filename, "%s/cdf/contour.cdf", dir );
    ncid = ncopen(filename,NC_NOWRITE);
/*
 * Get title and history.
 */
    ncattinq(ncid,NC_GLOBAL,"title",&t_type,&t_len);
    title = (char *)malloc(t_len * nctypelen(t_type));
    ncattget(ncid,NC_GLOBAL,"title",(void *)title);
    ncattinq(ncid,NC_GLOBAL,"history",&t_type,&t_len);
    hist = (char *)malloc(t_len * nctypelen(t_type));
    ncattget(ncid,NC_GLOBAL,"history",(void *)hist);
    sprintf( full_title, "%s#C#%s", title, hist );
/*
 * Get the lat/lon dimensions.
 */
    lat_id = ncdimid(ncid,"lat");
    ncdiminq(ncid,lat_id,(char *)0,&latlen);

    lon_id = ncdimid(ncid,"lon");
    ncdiminq(ncid,lon_id,(char *)0,&lonlen);

    frtime_id  = ncdimid(ncid,"frtime");
    ncdiminq(ncid,frtime_id,(char *)0,&frtimelen);
/*
 * Read in T.
 */
    Tid = ncvarid(ncid,"T");
    start[0] = start[1] = start[2] = start[3] = 0;
    count[0] = count[1] = 1;
    count[2] = latlen;
    count[3] = lonlen;
    ncvarget(ncid,Tid,(long const *)start,(long const *)count,T);
    ncattget(ncid,Tid,"_FillValue",&special_value);
/*
 * Read in lat/lon/frtime values.
 */
    lat_id = ncvarid(ncid,"lat");
    count[0] = latlen;
    ncvarget(ncid,lat_id,(long const *)start,(long const *)count,lat);
    ncattget(ncid,lat_id,"long_name",(void *)lat_name);

    lon_id = ncvarid(ncid,"lon");
    count[0] = lonlen;
    ncvarget(ncid,lon_id,(long const *)start,(long const *)count,lon);
    ncattget(ncid,lon_id,"long_name",(void *)lon_name);

    frtime_id = ncvarid(ncid,"frtime");
    count[0] = frtimelen;
    ncvarget(ncid,frtime_id,(long const *)start,(long const *)count,frtime);
/*
 * Convert T from Degrees K to Degrees F.
 */
    KtoF(T);
/*
 * Create a scalar field object and configure the missing values and
 * the start and end information.
 */
    icount[0] = latlen; icount[1] = lonlen;
    NhlRLClear(srlist);
    NhlRLSetMDFloatArray(srlist,NhlNsfDataArray,&T[0][0],2,icount);
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
    NhlRLSetFloat(srlist,NhlNcnMinLevelValF,-40.0);
    NhlRLSetFloat(srlist,NhlNcnMaxLevelValF,110.0);
    NhlRLSetFloat(srlist,NhlNcnLevelSpacingF,10.0);
    NhlRLSetString(srlist,NhlNcnFillOn,"True");
    NhlRLSetString(srlist,NhlNcnMonoFillPattern,"True");
    NhlRLSetInteger(srlist,NhlNcnFillPatterns,0);
    NhlRLSetFloat(srlist,NhlNtrXMinF,-140.0);
    NhlRLSetFloat(srlist,NhlNtrXMaxF,-52.5);
    NhlRLSetFloat(srlist,NhlNtrYMinF,20.0);
    NhlRLSetFloat(srlist,NhlNtrYMaxF,60.0);
    NhlRLSetString(srlist,NhlNtiMainFuncCode,"#");
    NhlRLSetString(srlist,NhlNtiMainString,full_title);
    NhlRLSetFloat(srlist,NhlNtiMainFontHeightF,0.02);
    NhlRLSetString(srlist,NhlNtiXAxisString,lon_name);
    NhlRLSetString(srlist,NhlNtiYAxisString,lat_name);
    NhlRLSetString(srlist,NhlNtmXBMode,"EXPLICIT");
    NhlRLSetIntegerArray(srlist,NhlNtmXBValues,xbvalues,NhlNumber(xbvalues));
    NhlRLSetStringArray(srlist,NhlNtmXBLabels,xblabels,NhlNumber(xblabels));
    NhlRLSetString(srlist,NhlNtmYLMode,"EXPLICIT");
    NhlRLSetIntegerArray(srlist,NhlNtmYLValues,ylvalues,NhlNumber(ylvalues));
    NhlRLSetStringArray(srlist,NhlNtmYLLabels,yllabels,NhlNumber(yllabels));
    NhlRLSetString(srlist,NhlNcnLowLabelsOn,"True");
    NhlRLSetString(srlist,NhlNcnHighLabelsOn,"True");
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
 * Loop on remaining fields of data and draw contour.
 */
    for( i = 1; i <= frtimelen-1; i++ ) {
/*
 * Read in new section of Z.
 */
        start[0] = i;
        start[1] = 0;
        start[2] = 0;
        start[3] = 0;
        count[0] = 1;
        count[1] = 1;
        count[2] = latlen;
        count[3] = lonlen;
        ncvarget(ncid,Tid,(long const *)start,(long const *)count,T);
/*
 * Convert T from Degrees K to Degrees F.
 */
        KtoF(T);
/*
 * Create new scalar field.
 */
        NhlRLClear(srlist);
        icount[0] = latlen; icount[1] = lonlen;
        NhlRLSetMDFloatArray(srlist,NhlNsfDataArray,&T[0][0],2,
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

void KtoF(T)
float T[NLAT][NLON];
{
    int i, j;
/*
 * Convert T from Degrees K to Degrees F.
 */
    for( i = 0; i < NLAT; i++ ) {
        for( j = 0; j < NLON; j++ ) {
            T[i][j] = (T[i][j]-273.15) * 9./5. + 32.;
        }
    }
}


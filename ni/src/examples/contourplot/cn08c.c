/*
**      $Id: cn08c.c,v 1.1 1995-09-28 19:24:42 haley Exp $
*/
/***********************************************************************
*                                                                      *
*                Copyright (C)  1995                                   *
*        University Corporation for Atmospheric Research               *
*     The use of this Software is governed by a License Agreement      *
*                                                                      *
***********************************************************************/
/*
**  File:       cn08c.c
**
**  Author:     Ethan Alpert (converted to C by Mary Haley)
**          National Center for Atmospheric Research
**          PO 3000, Boulder, Colorado
**
**  Date:       Thu Sep 28 08:32:16 MDT 1995
**
**  Description:    Draws a vertical profile of temperature for
**                  longitudes separated by 5 degrees.
**
*/


#include <stdio.h>
#include <math.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/ResList.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/XWorkstation.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/hlu/ContourPlot.h>
#include <ncarg/hlu/ScalarField.h>
#include <netcdf.h>

main()
{
/*
 * Declare variables for the HLU routine calls.
 */
    int     appid, workid, field1, con1;
    int     srlist, i, j, k;
/*
 * Declare variables for getting information from netCDF file.
 */
    int   ncid, lon_id, lat_id, level_id, temp_id;
    float temp[10][33], special_value;
    float lon[36], lat[33], level[10];
    float min_lat, min_level, max_lat, max_level;
    long  start[4], count[4], lonlen, latlen, levellen;
    char  filename[256], string[50];
    const char *dir = _NGGetNCARGEnv("data");
/*
 * Default is to create an NCGM file.
 */
    int NCGM=1, X11=0, PS=0;
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
    NhlCreate(&appid,"cn08",NhlappClass,NhlDEFAULT_APP,srlist);

    if (NCGM) {
/*
 * Create a meta file object.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkMetaName,"./cn08c.ncgm");
        NhlCreate(&workid,"cn08Work",NhlncgmWorkstationClass,
                  NhlDEFAULT_APP,srlist);
    }
    else if (X11) {
/*
 * Create an XWorkstation object.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkPause,"True");
        NhlCreate(&workid,"cn08Work",NhlxWorkstationClass,
              NhlDEFAULT_APP,srlist);
    }
    else if (PS) {
/*
 * Create a PS workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkPSFileName,"./cn08c.ps");
        NhlCreate(&workid,"cn08Work",NhlpsWorkstationClass,
                  NhlDEFAULT_APP,srlist);
    }
/*
 * Open data file containing grid of global temperatures.
 */
    sprintf( filename, "%s/cdf/contour.cdf", dir );
    ncid = ncopen(filename,NC_NOWRITE);
/*
 * Get the lat/lon/level dimensions.
 */
    lat_id = ncdimid(ncid,"lat");
    lon_id = ncdimid(ncid,"lon");
    level_id  = ncdimid(ncid,"level");
    ncdiminq(ncid,lat_id,(char *)0,&latlen);
    ncdiminq(ncid,lon_id,(char *)0,&lonlen);
    ncdiminq(ncid,level_id,(char *)0,&levellen);
/*
 * Read in temperature values and convert from degrees F to degrees K.
 */
    temp_id = ncvarid(ncid,"T");
    start[0] = start[1] = start[2] = start[3] = 0;
    count[0] = 1; count[1] = levellen; count[2] = latlen; count[3] = 1;
    ncvarget(ncid,temp_id,(long const *)start,(long const *)count,temp);
    ncattget(ncid,temp_id,"_FillValue",&special_value);
    for( j = 0; j < levellen; j++ ) {
        for( k = 0; k < latlen; k++ ) {
            temp[j][k] = (temp[j][k] - 273.15) * 9./5. + 32.;
        }
    }
/*
 * Read in lat/lon/level values.
 */
    lat_id = ncvarid(ncid,"lat");
    count[0] = latlen;
    ncvarget(ncid,lat_id,(long const *)start,(long const *)count,lat);

    lon_id = ncvarid(ncid,"lon");
    count[0] = lonlen;
    ncvarget(ncid,lon_id,(long const *)start,(long const *)count,lon);

    level_id = ncvarid(ncid,"level");
    count[0] = levellen;
    ncvarget(ncid,level_id,(long const *)start,(long const *)count,level);
/*
 * Set up initial scalar field with longitude of temperature data.
 */
    count[0] = levellen; count[1] = latlen;
    NhlRLClear(srlist);
    NhlRLSetMDFloatArray(srlist,NhlNsfDataArray,&temp[0][0],2,
                         (int *)count);
    NhlRLSetFloat(srlist,NhlNsfMissingValueV,special_value);
    NhlRLSetFloat(srlist,NhlNsfXCStartV,lat[0]);
    NhlRLSetFloat(srlist,NhlNsfXCEndV,lat[latlen-1]);
    NhlRLSetFloatArray(srlist,NhlNsfXArray,lat,latlen);
    NhlRLSetFloatArray(srlist,NhlNsfYArray,level,levellen);
    NhlCreate(&field1,"field1",NhlscalarFieldClass,appid,srlist);
/*
 * Determine extents of grid
 */
    if(lat[0] < lat[latlen-1]) {
        min_lat = lat[0];
        max_lat = lat[latlen-1];
    }
    else {
        max_lat = lat[0];
        min_lat = lat[latlen-1];
    }
    if(level[0] < level[levellen-1]) {
        min_level = level[0];
        max_level = level[levellen-1];
    }
    else {
        max_level = level[0];
        min_level = level[levellen-1];
    }
/*
 * Create contour using manual spacing.
 */
    NhlRLClear(srlist);
    NhlRLSetFloat(srlist,NhlNvpXF,.2);
    NhlRLSetFloat(srlist,NhlNvpYF,.8);
    NhlRLSetFloat(srlist,NhlNvpWidthF, .6);
    NhlRLSetFloat(srlist,NhlNvpHeightF, .6);
    NhlRLSetString(srlist,NhlNcnFillOn, "True");
    NhlRLSetInteger(srlist,NhlNcnScalarFieldData, field1);
    NhlRLSetString(srlist,NhlNcnLevelSelectionMode, "ManualLevels");
    NhlRLSetInteger(srlist,NhlNcnMaxLevelCount, 25);
    NhlRLSetFloat(srlist,NhlNcnMinLevelValF, -80.0);
    NhlRLSetFloat(srlist,NhlNcnMaxLevelValF, 110.0);
    NhlRLSetFloat(srlist,NhlNcnLevelSpacingF, 10.0);
    NhlRLSetFloat(srlist,NhlNtrXMinF, min_lat);
    NhlRLSetFloat(srlist,NhlNtrXMaxF, max_lat);
    NhlRLSetFloat(srlist,NhlNtrYMinF, min_level);
    NhlRLSetFloat(srlist,NhlNtrYMaxF, max_level);
    NhlRLSetString(srlist,NhlNtrYReverse, "True");
    sprintf(string,"Longitude %g Degrees", lon[0] );
    NhlRLSetString(srlist,NhlNtiMainString,string);
    NhlCreate(&con1,"con1",NhlcontourPlotClass,workid,srlist);
/* 
 * Draw first step
 */
    NhlDraw(con1);
    NhlFrame(workid);
/*
 * Loop on remaining longitude values and reset the title every
 * iteration.
 */
    for( i = 1; i <= lonlen-1; i++ ) {
/*
 * Read in temperature values and convert from degrees F to degrees K.
 */
        start[0] = start[1] = start[2] = 0;
        start[3] = i;
        count[0] = 1; count[1] = levellen;
        count[2] = latlen; count[3] = 1;
        ncvarget(ncid,temp_id,(long const *)start,(long const *)count,
                 temp);
        for( j = 0; j < levellen; j++ ) {
            for( k = 0; k < latlen; k++ ) {
                temp[j][k] = (temp[j][k] - 273.15) * 9./5. + 32.;
            }
        }
        NhlRLClear(srlist);
        count[0] = levellen; count[1] = latlen;
        NhlRLSetMDFloatArray(srlist,NhlNsfDataArray,&temp[0][0],2,
                            (int *)count);
/*
 * Create new scalar field.
 */
        NhlSetValues(field1,srlist);
        NhlRLClear(srlist);
        sprintf(string,"Longitude %g Degrees", lon[i] );
        NhlRLSetString(srlist,NhlNtiMainString,string);
        NhlSetValues(con1,srlist);
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

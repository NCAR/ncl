/*
**      $Id: xy06c.c,v 1.12 2010-03-15 22:49:25 haley Exp $
*/
/***********************************************************************
*                                                                      *
*                Copyright (C)  1995                                   *
*        University Corporation for Atmospheric Research               *
*                All Rights Reserved                                   *
*                                                                      *
***********************************************************************/
/*
**  File:       xy06c.c
**
**  Author:     Mary Haley
**          National Center for Atmospheric Research
**          PO 3000, Boulder, Colorado
**
**  Date:       Thu Mar  9 16:11:06 MST 1995
**
** Description:    This example shows how to do several things:
**
**                 1) How to create an XyPlot object with multiple
**                    lines using the CoordArrays and Data objects.
**                 2) How to change the data to create a different
**                    plot, without having to create a new data object
**                    (using the NhlSetValues call).
**                 3) How to overlay a Legend object and to tweak
**                    several kinds of resources (see "xy06.res").
**                 4) How to use netCDF interface routines to open
**                    and access a netCDF file.
**                 5) How to use a kludgy method for using the resource
**                    file to specify which stations you want to observe.
**
**                 This example requires that you have the netCDF
**                 library built on your system!  It's available via
**                 anonymous ftp to unidata.ucar.edu.
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
#include <ncarg/hlu/XyPlot.h>
#include <ncarg/hlu/CoordArrays.h>
#include <netcdf.h>

/*
 * Define the maximum number of weather stations and hours in a day.
 * The actual stations you want to get data values for should be
 * defined in the "xy06.res" resource file using the
 * "xyExplicitLegendLabels" resource.
 *
 * You cannot request more than NSTATIONS station ids in the resource
 * file.
 */
#define NSTATIONS  8
#define NCOLORS    NSTATIONS+2
#define NHOURS     24

/*
 * Declare array that will hold station ids.
 */
NhlString station_arr[NSTATIONS];
NhlString *station_abrev = station_arr;

/*
 * Define the day for which we are getting data values
 * (March 18, 1995).
 */
char *date = "950318";

int main()
{
/*
 * Declare variables for the HLU routine calls.
 */
    int     appid, xworkid, plotid, dataid;
    int     grlist, rlist, i, j;
    ng_size_t    length[2];
    int     datadepid[1];
    int     *dspec = datadepid;
    ng_size_t   num_dspec;
    float   special_value = -9999.;
    float   cmap[NCOLORS][3];
/*
 * Declare variables for getting information from netCDF file.
 */
    int     ncid, stid, stdmid, tempid, presid, windid, recid;
    int     ndims, nvars, ngatts, nhour; 
    ng_size_t numids = NSTATIONS;
    int     num_values, have_value[NSTATIONS];
    long    stid_len, rec_len;
    long    ststart[2], stcount[2], var_index[1];
    char    filename[256], station_name[20], recname[50];
    const char *dir = _NGGetNCARGEnv("data");

    char const *wks_type = "x11";
/*
 * Declare 2-d arrays to hold data values.
 */
    float temp[NSTATIONS][NHOURS], pressure[NSTATIONS][NHOURS];
    float wind_speed[NSTATIONS][NHOURS];
/*
 * Initialize the HLU library and set up resource template.
 */
    NhlInitialize();
    rlist = NhlRLCreate(NhlSETRL);
/*
 * Modify the color map.  Color indices '0' and '1' are the background
 * and foreground colors respectively.
 */
    cmap[0][0] = 0.00; cmap[0][1] = 0.00; cmap[0][2] = 0.00;
    cmap[1][0] = 1.00; cmap[1][1] = 1.00; cmap[1][2] = 1.00;
    cmap[2][0] = 0.00; cmap[2][1] = 0.00; cmap[2][2] = 1.00;
    cmap[3][0] = 0.00; cmap[3][1] = 1.00; cmap[3][2] = 0.00;
    cmap[4][0] = 0.00; cmap[4][1] = 1.00; cmap[4][2] = 0.75;
    cmap[5][0] = 0.50; cmap[5][1] = 0.50; cmap[5][2] = 0.63;
    cmap[6][0] = 1.00; cmap[6][1] = 0.00; cmap[6][2] = 0.00;
    cmap[7][0] = 0.75; cmap[7][1] = 0.38; cmap[7][2] = 0.25;
    cmap[8][0] = 0.75; cmap[8][1] = 0.00; cmap[8][2] = 0.75;
    cmap[9][0] = 1.00; cmap[9][1] = 1.00; cmap[9][2] = 0.00;
    length[0] = NCOLORS;  length[1] = 3;
/*
 * Create Application object.  The Application object name is used to
 * determine the name of the resource file, which is "xy06.res" in
 * this case.
 */
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNappDefaultParent,"True");
    NhlRLSetString(rlist,NhlNappUsrDir,"./");
    NhlCreate(&appid,"xy06",NhlappClass,NhlDEFAULT_APP,rlist);

    if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
/*
 * Create a meta file object.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./xy06c.ncgm");
        NhlRLSetMDFloatArray(rlist,NhlNwkColorMap,&cmap[0][0],2,length);
        NhlCreate(&xworkid,"xy06Work",NhlncgmWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 * Create an X11 workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlRLSetMDFloatArray(rlist,NhlNwkColorMap,&cmap[0][0],2,length);
        NhlCreate(&xworkid,"xy06Work",NhlcairoWindowWorkstationClass,
              NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 * Create an older-style PostScript workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPSFileName,"./xy06c.ps");
        NhlRLSetMDFloatArray(rlist,NhlNwkColorMap,&cmap[0][0],2,length);
        NhlCreate(&xworkid,"xy06Work",NhlpsWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 * Create an older-style PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPDFFileName,"./xy06c.pdf");
        NhlRLSetMDFloatArray(rlist,NhlNwkColorMap,&cmap[0][0],2,length);
        NhlCreate(&xworkid,"xy06Work",NhlpdfWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
             !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 * Create a cairo PS/PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"./xy06c");
        NhlRLSetString(rlist,NhlNwkFormat, (char*)wks_type);
        NhlRLSetMDFloatArray(rlist,NhlNwkColorMap,&cmap[0][0],2,length);
        NhlCreate(&xworkid,"xy06Work",NhlcairoDocumentWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 * Create a cairo PNG workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"./xy06c");
        NhlRLSetString(rlist,NhlNwkFormat, (char*)wks_type);
        NhlRLSetMDFloatArray(rlist,NhlNwkColorMap,&cmap[0][0],2,length);
        NhlCreate(&xworkid,"xy06Work",NhlcairoImageWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
/*
 * We need to initialize a non-constant dummy array for our Data
 * object, otherwise we'll get error messages.
 */
    for( j = 0; j < NHOURS; j++ ) {
        temp[0][j] = (float)j;
    }
/*
 * Define a dummy Data object.  We do this so a DataSpec object is
 * created automatically and then we can use an NhlGetValues call to
 * get the names of the stations we want data values for.
 */
    NhlRLClear(rlist);
    NhlRLSetFloatArray(rlist,NhlNcaYArray,(float *)temp,NHOURS);
    NhlCreate(&dataid,"xyData",NhlcoordArraysClass,
              NhlDEFAULT_APP,rlist);
/*
 * The id from this dummy Data object will now become the resource
 * value for "xyCoordData".
 */
    NhlRLClear(rlist);
    NhlRLSetInteger(rlist,NhlNxyCoordData,dataid);
    NhlCreate(&plotid,"xyPlot1",NhlxyPlotClass,xworkid,rlist);
/*
 * Get the DataSpec object id.
 */
    grlist = NhlRLCreate(NhlGETRL);
    NhlRLClear(grlist);
    NhlRLGetIntegerArray(grlist,NhlNxyCoordDataSpec,&dspec,&num_dspec);
    NhlGetValues(plotid,grlist);
/*
 * Get station id names that have been set in resource file.  This
 * is a round-about way of doing things, but it makes it convenient to
 * be able to specify the stations we want.  We used the
 * 'xyExplicitLegendLabels' resource, because we also want to use the 
 * station names to label the lines in the legend.
 */
    NhlRLClear(grlist);
    NhlRLGetStringArray(grlist,NhlNxyExplicitLegendLabels,
                        &station_abrev,&numids);
    NhlGetValues(dspec[0],grlist);
/*
 * Make sure we didn't request too many stations or none at all.
 */
    if( numids > NSTATIONS || numids <= 0 ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
               "Number of requested stations (%d) is illegal.",numids);
        NhlPError(NhlFATAL,NhlEUNKNOWN,
               "Can only request 1-%d stations.",NSTATIONS);
        exit(3);
    }
/* 
 * Loop through all NHOURS hours.
 */
    for( nhour = 0; nhour < NHOURS; nhour++ ) {
/*
 * Initialize the data arrays to our special value that we have set
 * above.
 */
        num_values = 0;
        for( i = 0; i < numids; i++ ) {
            have_value[i] = 0;
            temp[i][nhour] = pressure[i][nhour] = wind_speed[i][nhour] = special_value;
        }
/*
 * Open the netCDF file for a particular hour.
 */
        sprintf( filename, "%s/cdf/%s%.2d_sao.cdf", dir, date, nhour );
        ncid = ncopen(filename,NC_NOWRITE);
/*
 * Get the station id dimension.
 */
        stdmid = ncdimid(ncid,"id_len");
        ncdiminq(ncid, stdmid, (char *)0, &stid_len );
/*
 * Get the record length and dimension name.
 */
        ncinquire(ncid, &ndims, &nvars, &ngatts, &recid);
        ncdiminq(ncid, recid, recname, &rec_len);
/*
 * Get the id of the station ids, temperature, pressure, and 
 * wind speed variables.
 */
        stid = ncvarid(ncid,"id");
        tempid = ncvarid(ncid,"T");
        presid = ncvarid(ncid,"PSL");
        windid = ncvarid(ncid,"SPD");
/*
 * Get data values for the stations we have selected.
 */
        ststart[1] = 0; stcount[0] = 1; stcount[1] = stid_len;
        for( i = 0; i < rec_len; i++ ) {
            ststart[0] = i; 
            ncvarget(ncid,stid,(long const *)ststart,
                               (long const *)stcount,station_name);
            for( j = 0; j < numids; j++ ) {
/* 
 * Check if this is one of the stations we've requested and that we
 * don't already have a data value for this station.
 */
                if( !have_value[j] &&
                    !strcmp(station_abrev[j],station_name) ) {

                    var_index[0] = i;
/*
 * Get the temperature value.
 */
                    ncvarget1(ncid,tempid,(long const *)var_index,
                              &temp[j][nhour]);
/*
 * Get the pressure value.
 */
                    ncvarget1(ncid,presid,(long const *)var_index,
                              &pressure[j][nhour]);
/*
 * Get the wind speed.
 */
                    ncvarget1(ncid,windid,(long const *)var_index,
                              &wind_speed[j][nhour]);
/*
 * Keep track of how many values we've received.
 */
                    have_value[j] = 1;
                    num_values++;
                    break;
                }
            }
/*
 * Check if we have received data values for each station.  If
 * so, then we don't need to loop through the rest of the stations.
 */
            if( num_values == numids ) break;
        }
/*
 * Let the user know if there are some stations missing.  Special
 * values will be used in this case.
 */
        for( j = 0; j < numids; j++ ) {
            if( !have_value[j]  ) {
                NhlPError(NhlWARNING,NhlEUNKNOWN,
                "Station %s not in netCDF file for hour %d",station_abrev[j],nhour);
                NhlPError(NhlWARNING,NhlEUNKNOWN,
                "Will substitute missing value = %g",special_value);
            }
        }
/*
 * Close the netCDF file.
 */
        ncclose(ncid);
    }
/*
 * Define the Data object.  Since only the Y values are specified here,
 * each Y value will be paired with its integer array index.  The data
 * id from this object will become the value for the XyPlot resource
 * "xyCoordData".
 */
    length[0] = numids;  length[1] = NHOURS;
    NhlRLClear(rlist);
    NhlRLSetMDFloatArray(rlist,NhlNcaYArray,&temp[0][0],2,length);
    NhlRLSetFloat(rlist,NhlNcaYMissingV,special_value);
    NhlCreate(&dataid,"xyData",NhlcoordArraysClass,
                  NhlDEFAULT_APP,rlist);
/*
 * The id for this Data object is now the resource value for
 * "xyCoordData".  Tweak some XyPlot resources in the resource file
 * ("xy06.res").
 */
    NhlRLClear(rlist);
    NhlRLSetInteger(rlist,NhlNxyCoordData,dataid);
    NhlCreate(&plotid,"xyPlot1",NhlxyPlotClass,xworkid,rlist);
/*
 * Draw the plot.
 */
    NhlDraw(plotid);
    NhlFrame(xworkid);
/*
 * Change the data in our Data object.  Notice we use NhlSetValues
 * instead of NhlCreate, so our data object will have the same
 * name as when we originally created it, "xyData".
 */
    NhlRLClear(rlist);
    NhlRLSetMDFloatArray(rlist,NhlNcaYArray,&pressure[0][0],2,length);
    NhlRLSetFloat(rlist,NhlNcaYMissingV,special_value);
    NhlCreate(&dataid,"xyData",NhlcoordArraysClass,
                  NhlDEFAULT_APP,rlist);
/*
 * The id for this Data object is now the resource value for
 * "xyCoordData".  Tweak some XyPlot resources in the resource file
 * ("xy06.res").
 */
    NhlRLClear(rlist);
    NhlRLSetInteger(rlist,NhlNxyCoordData,dataid);
    NhlCreate(&plotid,"xyPlot2",NhlxyPlotClass,xworkid,rlist);
/*
 * Draw the plot.
 */
    NhlDraw(plotid);
    NhlFrame(xworkid);
/*
 * Change the data in our Data object.  Notice we use NhlSetValues
 * instead of NhlCreate, so our data object will have the same
 * name as when we originally created it, "xyData".
 */
    NhlRLClear(rlist);
    NhlRLSetMDFloatArray(rlist,NhlNcaYArray,&wind_speed[0][0],2,length);
    NhlRLSetFloat(rlist,NhlNcaYMissingV,special_value);
    NhlCreate(&dataid,"xyData",NhlcoordArraysClass,
                  NhlDEFAULT_APP,rlist);
/*
 * The id for this Data object is now the resource value for
 * "xyCoordData".  Tweak some XyPlot resources in the resource file
 * ("xy06.res").
 */
    NhlRLClear(rlist);
    NhlRLSetInteger(rlist,NhlNxyCoordData,dataid);
    NhlCreate(&plotid,"xyPlot3",NhlxyPlotClass,xworkid,rlist);
/*
 * Draw the plot.
 */
    NhlDraw(plotid);
    NhlFrame(xworkid);
/*
 * NhlDestroy destroys the given id and all of its children so
 * destroying "appid" will destroy "xworkid" which will also destroy
 * "plotid".
 */
    NhlRLDestroy(rlist);
    NhlRLDestroy(grlist);
    NhlDestroy(xworkid);
    NhlDestroy(appid);
/*
 * Restores state.
 */
    NhlClose();
    exit(0);
}

/*
**      $Id: xy06c.c,v 1.4 1995-04-04 21:56:17 haley Exp $
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
#include <ncarg/hlu/XWorkstation.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/XyPlot.h>
#include <ncarg/hlu/CoordArrays.h>
#include <netcdf.h>

/*
 * Define the maximum number of weather stations and hours in a day.
 * The actual stations you want to get temperature values for should
 * be defined in the "xy06.res" resource file using the
 * "xyExplicitLegendLabels" resource.
 *
 * You cannot request more than NSTATIONS station ids in the resource
 * file.
 */
#define NSTATIONS  9
#define NHOURS     24

/*
 * Declare array that will hold station ids.
 */
NhlString station_arr[NSTATIONS];
NhlString *station_abrev = station_arr;

/*
 * Define the day for which we are getting temperature
 * values (March 18, 1995).
 */
char *date = "950318";

main()
{
/*
 * Declare variables for the HLU routine calls.
 */
    int     appid, xworkid, plotid, dataid;
    int     grlist, rlist, i, j;
    int     length[2];
    int     datadepid[1];
    int     *dspec = datadepid;
    int     num_dspec;
    float   special_value = -9999.;
/*
 * Declare variables for getting information from netCDF file.
 */
    int     ncid, stid, stdmid, tempid, recid;
    int     ndims, nvars, ngatts, nhour, numids = NSTATIONS;
    int     num_temps, have_temp[NSTATIONS];
    long    stid_len, rec_len;
    long    ststart[2], stcount[2], tempindex[1];
    char    filename[256], station_name[20], recname[50];
    const char *dir = _NGGetNCARGEnv("data");

    int NCGM=0;
/*
 * Declare 2-d array to hold temperature values.
 */
    float   temp[NSTATIONS][NHOURS];
/*
 * Initialize the HLU library and set up resource template.
 */
    NhlInitialize();
    rlist = NhlRLCreate(NhlSETRL);
/*
 * Create Application object.  The Application object name is used to
 * determine the name of the resource file, which is "xy06.res" in
 * this case.
 */
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNappDefaultParent,"True");
    NhlRLSetString(rlist,NhlNappUsrDir,"./");
    NhlCreate(&appid,"xy06",NhlappLayerClass,NhlDEFAULT_APP,rlist);

    if (NCGM) {
/*
 * Create a meta file object.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./xy06c.ncgm");
        NhlCreate(&xworkid,"xy06Work",NhlncgmWorkstationLayerClass,
                  NhlDEFAULT_APP,rlist);
    }
    else {
/*
 * Create an XWorkstation object.
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlCreate(&xworkid,"xy06Work",NhlxWorkstationLayerClass,
              NhlDEFAULT_APP,rlist);
    }
/*
 * We need to initialize a non-constant dummy array for our Data
 * object, otherwise we'll get error messages.
 */
    for( i = 0; i < NSTATIONS; i++ ) {
        for( j = 0; j < NHOURS; j++ ) {
            temp[i][j] = (float)j;
        }
    }
/*
 * Define a dummy Data object.  We do this so a DataSpec object is
 * created automatically and then we can use an NhlGetValues call to
 * get the names of the stations we want temperature values for.
 */
    length[0] = NSTATIONS;  length[1] = NHOURS;
    NhlRLClear(rlist);
    NhlRLSetMDFloatArray(rlist,NhlNcaYArray,&temp[0][0],2,length);
    NhlCreate(&dataid,"xyData",NhlcoordArraysLayerClass,
                  NhlDEFAULT_APP,rlist);
/*
 * The id from this dummy Data object will now become the resource
 * value for "xyCoordData".
 */
    NhlRLClear(rlist);
    NhlRLSetInteger(rlist,NhlNxyCoordData,dataid);
    NhlCreate(&plotid,"xyPlot1",NhlxyPlotLayerClass,xworkid,rlist);
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
    length[0] = numids;
/* 
 * Loop through all NHOURS hours.
 */
    for( nhour = 0; nhour < NHOURS; nhour++ ) {
/*
 * Initialize the temperature data array to our special value that we
 * have set above.
 */
        num_temps = 0;
        for( i = 0; i < numids; i++ ) {
            have_temp[i] = 0;
            temp[i][nhour] = special_value;
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
 * Get the id of the station ids and the temperature variables.
 */
        stid = ncvarid(ncid,"id");
        tempid = ncvarid(ncid,"T");
/*
 * Get temperature values for the stations we have selected.
 */
        ststart[1] = 0; stcount[0] = 1; stcount[1] = stid_len;
        for( i = 0; i < rec_len; i++ ) {
            ststart[0] = i; 
            ncvarget(ncid,stid,(long const *)ststart,
                               (long const *)stcount,station_name);
            for( j = 0; j < numids; j++ ) {
/* 
 * Check if this is one of the stations we've requested and that we
 * don't already have a temperature value for this station.
 */
                if( !have_temp[j] &&
                    !strcmp(station_abrev[j],station_name) ) {

                    tempindex[0] = i;
/*
 * Get the temperature value.
 */
                    ncvarget1(ncid,tempid,(long const *)tempindex,
                              &temp[j][nhour]);
                    have_temp[j] = 1;
                    num_temps++;
                    break;
                }
            }
/*
 * Check if we have received temperature values for each station.  If
 * so, then we don't need to loop through the rest of the stations.
 */
            if( num_temps == numids ) break;
        }
/*
 * Let the user know if there are some stations missing.  Special
 * values will be used in this case.
 */
        for( j = 0; j < numids; j++ ) {
            if( !have_temp[j]  ) {
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
    NhlRLClear(rlist);
    NhlRLSetMDFloatArray(rlist,NhlNcaYArray,&temp[0][0],2,length);
    NhlRLSetFloat(rlist,NhlNcaYMissingV,special_value);
    NhlCreate(&dataid,"xyData",NhlcoordArraysLayerClass,
                  NhlDEFAULT_APP,rlist);
/*
 * The id for this Data object is now the resource value for
 * "xyCoordData".  Tweak some XyPlot resources in the resource file
 * ("xy06.res").
 */
    NhlRLClear(rlist);
    NhlRLSetInteger(rlist,NhlNxyCoordData,dataid);
    NhlCreate(&plotid,"xyPlot1",NhlxyPlotLayerClass,xworkid,rlist);
/*
 * Draw the plot (to its parent X Workstation)
 */
    NhlDraw(plotid);
    NhlFrame(xworkid);
/*
 * Convert temperatures from Celsius to Fahrenheit.
 */
    for( i = 0; i < numids; i++ ) {
        for( j = 0; j < NHOURS; j++ ) {
            if(temp[i][j] != special_value) {
                temp[i][j] = 9./5.*temp[i][j]+32.;
            }
        }
    }
/*
 * Change the data in our Data object.  Notice we use NhlSetValues
 * instead of NhlCreate, so our data object will have the same
 * name as when we originally created it, "xyData".
 */
    NhlRLClear(rlist);
    NhlRLSetMDFloatArray(rlist,NhlNcaYArray,&temp[0][0],2,length);
    NhlSetValues(dataid,rlist);
/*
 * Create another XyPlot object with this new Data object.  We have to
 * create another object instead of just changing the current one,
 * because we want to change some resource values to title our axes
 * differently.
 */
    NhlRLClear(rlist);
    NhlRLSetInteger(rlist,NhlNxyCoordData,dataid);
    NhlCreate(&plotid,"xyPlot2",NhlxyPlotLayerClass,xworkid,rlist);
/*
 * Draw the plot (to its parent X Workstation)
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

/*
 *      $Id: st04c.c,v 1.1 1996-06-27 22:20:42 haley Exp $
 */
/***********************************************************************
*                                                                      *
*                Copyright (C)  1996                                   *
*        University Corporation for Atmospheric Research               *
*                All Rights Reserved                                   *
*                                                                      *
***********************************************************************/
/*
 *  File:       st04c.c
 *
 *  Author:     David Brown (converted by Mary Haley)
 *              National Center for Atmospheric Research
 *              PO 3000, Boulder, Colorado
 *
 *  Date:       Thu June 27 9:47:28 MST 1996
 *
 *  Description: This example shows a StreamlinePlot of 500 mb wind 
 *               vector data overlaid on a MapPlot. The streamlines
 *               are drawn over a VectorPlot of surface winds colored
 *               by surface pressure that in turn is drawn over a filled
 *               ContourPlot of surface temperature. Different intervals
 *               of the "temp1" colormap are used to color the contour
 *               levels and the vectors.
 *               The data represents 15 days of weather over North
 *               America in January, 1996.
 *               The data is extracted from NMC forcast data produced 
 *               at 12 hour intervals and converted to netcdf format 
 *               by Unidata. Most of the time steps in the files
 *               extracted from the original data are taken from the 
 *               0 and 6 hour forecast times. However, because some of the 
 *               original files were lost, certain time steps come from
 *               longer range forcasts. Also, several steps had to be
 *               excluded from the frame set because the data is 
 *               defective. The result is that there is an 
 *               apparent discontinuity between some of the frames 
 *               when the output is animated.
 */

#include <math.h>
#include <ncarg/gks.h>
#include <ncarg/ncargC.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/hlu/XWorkstation.h>
#include <ncarg/hlu/StreamlinePlot.h>
#include <ncarg/hlu/VectorPlot.h>
#include <ncarg/hlu/ScalarField.h>
#include <ncarg/hlu/MapPlot.h>
#include <ncarg/hlu/ContourPlot.h>
#include <ncarg/hlu/TextItem.h>
#include <netcdf.h>

/*
 * Depending on the value of the TIMESTEPS variable declared below,
 * this example example can generate up to 61 frames from the 64
 * timesteps in the data files. As shipped, only the first 20 frames 
 * are created. To see the complete plot uncomment the second 
 * assignment to TIMESTEPS. Some systems may not have enough physical
 * memory to allow all frames to be viewed as an animation.
 */
#define TIMESTEPS 20

/* #define TIMESTEPS 64 */


/*
 * Initialize netCDF filenames
 */
char *cdffiles[6] = {"Ustorm.cdf","Vstorm.cdf","Pstorm.cdf","Tstorm.cdf","U500storm.cdf","V500storm.cdf"};


main(int argc, char *argv[])
{
    int NCGM=1, X11=0, PS=0;
    int i, j, time, d, h;
    int appid, wid, cnid, vcid, stid, txid, amid, mpid, timeid;
    int vfield, vfield2, sfield, sfield2;
    int rlist, len_dims[2];
    long start[3], count[3];
    long latlen, lonlen, lat5len, lon5len, latplen, lonplen, lattlen, lontlen;
    long timelen, *timestep;
    int ncid[6], uid, vid, u5id, v5id, pid, tid;
    int latid, lonid, lat5id, lon5id, latpid, lonpid, lattid, lontid;
    float *lon, *lat;
    float *U, *V, *U5, *V5, *P, *T;
    char  filename[256];
    char  reftime[9];
    const char *dir = _NGGetNCARGEnv("data");
    char hour[25], day[25], mainstring[100];
/*
 * Initialize the high level utility library
 */
    NhlInitialize();
/*
 * Create an application object.
 */
    rlist = NhlRLCreate(NhlSETRL);
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNappUsrDir,"./");
    NhlRLSetString(rlist,NhlNappDefaultParent,"True");
    NhlCreate(&appid,"st04",NhlappClass,NhlDEFAULT_APP,rlist);

    if (NCGM) {
/*
 * Create a meta file workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./st04c.ncgm");
        NhlRLSetString(rlist,NhlNwkColorMap,"temp1");
        NhlCreate(&wid,"st04Work",
                  NhlncgmWorkstationClass,NhlDEFAULT_APP,rlist);
    }
    else if (X11) {
/*
 * Create an X workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlRLSetString(rlist,NhlNwkColorMap,"temp1");
        NhlCreate(&wid,"st04Work",NhlxWorkstationClass,appid,rlist);
    }

    else if (PS) {
/*
 * Create a PS workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPSFileName,"st04c.ps");
        NhlRLSetString(rlist,NhlNwkColorMap,"temp1");
        NhlCreate(&wid,"st04Work",NhlpsWorkstationClass,appid,rlist);
    }
/*
 * Open the netCDF files.
 */
    for( i = 0; i <= 5; i++ ) {
        sprintf( filename, "%s/cdf/%s", dir, cdffiles[i] );
        ncid[i] = ncopen(filename,NC_NOWRITE);
    }
/*
 * Get the data.
 */
    latid = ncdimid(ncid[0],"lat");
    lonid = ncdimid(ncid[0],"lon");
    ncdiminq(ncid[0],latid,(char *)0,&latlen);
    ncdiminq(ncid[0],lonid,(char *)0,&lonlen);

    uid = ncvarid(ncid[0],"u");
    vid = ncvarid(ncid[1],"v");
    U = (float *)malloc(sizeof(float)*latlen*lonlen);
    V = (float *)malloc(sizeof(float)*latlen*lonlen);
    start[0] = start[1] = start[2] = 0;
    count[0] = 1; count[1] = latlen; count[2] = lonlen;
    ncvarget(ncid[0],uid,(long const *)start,(long const *)count,U);
    ncvarget(ncid[1],vid,(long const *)start,(long const *)count,V);

    latid = ncvarid(ncid[0],"lat");
    lonid = ncvarid(ncid[0],"lon");
    lat = (float *)malloc(sizeof(float)*latlen);
    lon = (float *)malloc(sizeof(float)*lonlen);
    count[0] = latlen;
    ncvarget(ncid[0],latid,(long const *)start,(long const *)count,lat);
    count[0] = lonlen;
    ncvarget(ncid[0],lonid,(long const *)start,(long const *)count,lon);
/*
 * Create a VectorField of the surface wind data
 */
    len_dims[0] = latlen;
    len_dims[1] = lonlen;
    NhlRLClear(rlist);
    NhlRLSetMDFloatArray(rlist,NhlNvfUDataArray,U,2,len_dims);
    NhlRLSetMDFloatArray(rlist,NhlNvfVDataArray,V,2,len_dims);
    NhlRLSetFloat(rlist,NhlNvfXCStartV,lon[0]);
    NhlRLSetFloat(rlist,NhlNvfYCStartV,lat[0]);
    NhlRLSetFloat(rlist,NhlNvfXCEndV,lon[lonlen-1]);
    NhlRLSetFloat(rlist,NhlNvfYCEndV,lat[latlen-1]);
    NhlRLSetFloat(rlist,NhlNvfMissingUValueV, -9999.0);
    NhlCreate(&vfield,"VectorField",NhlvectorFieldClass,appid,rlist);
/*
 * Create a VectorField of 500 millibar wind data
 */
    lat5id = ncdimid(ncid[4],"lat");
    lon5id = ncdimid(ncid[4],"lon");
    ncdiminq(ncid[4],lat5id,(char *)0,&lat5len);
    ncdiminq(ncid[4],lon5id,(char *)0,&lon5len);

    u5id = ncvarid(ncid[4],"u");
    v5id = ncvarid(ncid[5],"v");
    U5 = (float *)malloc(sizeof(float)*lat5len*lon5len);
    V5 = (float *)malloc(sizeof(float)*lat5len*lon5len);
    start[0] = start[1] = start[2] = 0;
    count[0] = 1; count[1] = lat5len; count[2] = lon5len;
    ncvarget(ncid[4],u5id,(long const *)start,(long const *)count,U5);
    ncvarget(ncid[5],v5id,(long const *)start,(long const *)count,V5);

    free((float*)lat);
    free((float*)lon);
    lat5id = ncvarid(ncid[4],"lat");
    lon5id = ncvarid(ncid[4],"lon");
    lat = (float *)malloc(sizeof(float)*lat5len);
    lon = (float *)malloc(sizeof(float)*lon5len);
    count[0] = lat5len;
    ncvarget(ncid[4],lat5id,(long const *)start,(long const *)count,lat);
    count[0] = lonlen;
    ncvarget(ncid[4],lon5id,(long const *)start,(long const *)count,lon);

    len_dims[0] = lat5len;
    len_dims[1] = lon5len;
    NhlRLClear(rlist);
    NhlRLSetMDFloatArray(rlist,NhlNvfUDataArray,U5,2,len_dims);
    NhlRLSetMDFloatArray(rlist,NhlNvfVDataArray,V5,2,len_dims);
    NhlRLSetFloat(rlist,NhlNvfXCStartV, lon[0]);
    NhlRLSetFloat(rlist,NhlNvfYCStartV, lat[0]);
    NhlRLSetFloat(rlist,NhlNvfXCEndV, lon[lon5len-1]);
    NhlRLSetFloat(rlist,NhlNvfYCEndV, lat[lat5len-1]);
    NhlRLSetFloat(rlist,NhlNvfMissingUValueV, -9999.0);
    NhlCreate(&vfield2,"VectorField",NhlvectorFieldClass,appid,rlist);
/*
 * Create a ScalarField of surface pressure 
 */
    latpid = ncdimid(ncid[2],"lat");
    lonpid = ncdimid(ncid[2],"lon");
    ncdiminq(ncid[2],latpid,(char *)0,&latplen);
    ncdiminq(ncid[2],lonpid,(char *)0,&lonplen);

    pid = ncvarid(ncid[2],"p");
    P = (float *)malloc(sizeof(float)*latplen*lonplen);
    start[0] = start[1] = start[2] = 0;
    count[0] = 1; count[1] = latplen; count[2] = lonplen;
    ncvarget(ncid[2],pid,(long const *)start,(long const *)count,P);

    free((float*)lat);
    free((float*)lon);
    lat = (float *)malloc(sizeof(float)*latplen);
    lon = (float *)malloc(sizeof(float)*lonplen);
    latpid = ncvarid(ncid[2],"lat");
    lonpid = ncvarid(ncid[2],"lon");
    count[0] = latplen;
    ncvarget(ncid[2],latpid,(long const *)start,(long const *)count,lat);
    count[0] = lonplen;
    ncvarget(ncid[2],lonpid,(long const *)start,(long const *)count,lon);

    for( i = 0; i < latplen*lonplen; i++ ) {
        if( P[i] != -9999.0 ) P[i] /= 100.;
    }

    len_dims[0] = latplen;
    len_dims[1] = lonplen;
    NhlRLClear(rlist);
    NhlRLSetMDFloatArray(rlist,NhlNsfDataArray,P,2,len_dims);
    NhlRLSetFloat(rlist,NhlNsfXCStartV, lon[0]);
    NhlRLSetFloat(rlist,NhlNsfYCStartV, lat[0]);
    NhlRLSetFloat(rlist,NhlNsfXCEndV, lon[lonplen-1]);
    NhlRLSetFloat(rlist,NhlNsfYCEndV, lat[latplen-1]);
    NhlRLSetFloat(rlist,NhlNsfMissingValueV, -9999.0);
    NhlCreate(&sfield,"ScalarField",NhlscalarFieldClass,appid,rlist);
/*
 * Create a ScalarField of surface temperature 
 * (convert from Kelvin to Farenheit)
 */
    lattid = ncdimid(ncid[3],"lat");
    lontid = ncdimid(ncid[3],"lon");
    ncdiminq(ncid[3],lattid,(char *)0,&lattlen);
    ncdiminq(ncid[3],lontid,(char *)0,&lontlen);

    tid = ncvarid(ncid[3],"t");
    T = (float *)malloc(sizeof(float)*lattlen*lontlen);
    start[0] = start[1] = start[2] = 0;
    count[0] = 1; count[1] = lattlen; count[2] = lontlen;
    ncvarget(ncid[3],tid,(long const *)start,(long const *)count,T);

    free((float*)lat);
    free((float*)lon);
    lat = (float *)malloc(sizeof(float)*lattlen);
    lon = (float *)malloc(sizeof(float)*lontlen);
    lattid = ncvarid(ncid[3],"lat");
    lontid = ncvarid(ncid[3],"lon");
    count[0] = lattlen;
    ncvarget(ncid[3],lattid,(long const *)start,(long const *)count,lat);
    count[0] = lontlen;
    ncvarget(ncid[3],lontid,(long const *)start,(long const *)count,lon);

    for( i = 0; i < lattlen*lontlen; i++ ) {
        if( T[i] != -9999.0) T[i] = (T[i] - 273.15) * 9.0/5.0 + 32.0;
    }

    len_dims[0] = lattlen;
    len_dims[1] = lontlen;
    NhlRLClear(rlist);
    NhlRLSetMDFloatArray(rlist,NhlNsfDataArray,T,2,len_dims);
    NhlRLSetFloat(rlist,NhlNsfXCStartV, lon[0]);
    NhlRLSetFloat(rlist,NhlNsfYCStartV, lat[0]);
    NhlRLSetFloat(rlist,NhlNsfXCEndV, lon[lontlen-1]);
    NhlRLSetFloat(rlist,NhlNsfYCEndV, lat[lattlen-1]);
    NhlRLSetFloat(rlist,NhlNsfMissingValueV, -9999.0);
    NhlCreate(&sfield2,"ScalarField2",NhlscalarFieldClass,appid,rlist);
/*
 * Create a ContourPlot with surface temperature data
 */
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNcnFillOn,"true");
    NhlRLSetString(rlist,NhlNcnLinesOn,"false");
    NhlRLSetString(rlist,NhlNcnFillDrawOrder,"predraw");
    NhlRLSetInteger(rlist,NhlNcnScalarFieldData,sfield2);
    NhlCreate(&cnid,"contourplot",NhlcontourPlotClass,wid,rlist);
/*
 * Create a VectorPlot with the surface wind and pressure data
 */
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNvcUseScalarArray,"true");
    NhlRLSetInteger(rlist,NhlNvcVectorFieldData,vfield);
    NhlRLSetInteger(rlist,NhlNvcScalarFieldData,sfield);
    NhlCreate(&vcid,"vectorplot",NhlvectorPlotClass,wid,rlist);
/*
 * Create a StreamlinePlot with 500 mb wind data
 */
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNpmTitleDisplayMode,"always");
    NhlRLSetString(rlist,NhlNtiMainFuncCode,"~");
    NhlRLSetInteger(rlist,NhlNstVectorFieldData,vfield2);
    NhlCreate(&stid,"streamlineplot",NhlstreamlinePlotClass,wid,rlist);
/*
 * Create an annotation used to explain the streamline data
 */
    NhlCreate(&txid,"streamlineplotanno",NhltextItemClass,wid,0);
    amid = NhlAddAnnotation(stid,txid);
/*
 * Create a map object
 */
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNvpUseSegments,"true");
    NhlCreate(&mpid,"mapplot",NhlmapPlotClass,wid,rlist);
/*
 * Overlay everything on the MapPlot. The last object overlaid will
 * appear on top
 */
    NhlAddOverlay(mpid,cnid,-1);
    NhlAddOverlay(mpid,vcid,-1);
    NhlAddOverlay(mpid,stid,-1);
/*
 * Variables for manipulating the title string
 */
    timeid = ncdimid(ncid[1],"timestep");
    ncdiminq(ncid[1],timeid,(char *)0,&timelen);
    timeid = ncvarid(ncid[1],"timestep");
    timestep = (long *)malloc(sizeof(long)*timelen);
    start[0] = 0;
    count[0] = timelen;
    ncvarget(ncid[1],timeid,(long const *)start,(long const *)count,timestep);
    sprintf( hour, "00");
    sprintf( day, "05");
    
    timeid = ncvarid(ncid[1],"reftime");
    start[0] = 0; count[0] = 8;
    ncvarget(ncid[1],timeid,(long const *)start,(long const *)count,reftime);

    for( i = 0; i <= TIMESTEPS-1; i++ ) {
        if (i != 17 && i != 36 && i != 37) {
/*
 * Figure out the hour and day from the timestep, convert to strings
 * and build the title string
 */
            d = timestep[i] / 24 + 5;
            h = timestep[i] % 24;
            if (h > 9) {
                sprintf( hour, "%d", h );
            }
            else {
                sprintf( hour, "0%d", h );
            }
            if (d > 9) {
                sprintf(day, "%d", d );
            }
            else {
                sprintf(day, "0%d", d );
            }
/*
 * Set the new title string
 */
            sprintf(mainstring, "%s%s %s:00",reftime, day, hour);
            printf("%s\n",mainstring);
            NhlRLClear(rlist);
            NhlRLSetString(rlist,NhlNtiMainString,mainstring);
            NhlSetValues(stid,rlist);
/*
 * Modify the data objects with data for the current time step
 */
            start[0] = i; start[1] = start[2] = 0;
            count[0] = 1; count[1] = latlen; count[2] = lonlen;
            ncvarget(ncid[0],uid,(long const *)start,(long const *)count,U);
            ncvarget(ncid[1],vid,(long const *)start,(long const *)count,V);

            len_dims[0] = latlen;
            len_dims[1] = lonlen;
            NhlRLClear(rlist);
            NhlRLSetMDFloatArray(rlist,NhlNvfUDataArray,U,2,len_dims);
            NhlRLSetMDFloatArray(rlist,NhlNvfVDataArray,V,2,len_dims);
            NhlSetValues(vfield,rlist);

            start[0] = i; start[1] = start[2] = 0;
            count[0] = 1; count[1] = lat5len; count[2] = lon5len;
            ncvarget(ncid[4],u5id,(long const *)start,(long const *)count,U5);
            ncvarget(ncid[5],v5id,(long const *)start,(long const *)count,V5);

            len_dims[0] = lat5len;
            len_dims[1] = lon5len;
            NhlRLClear(rlist);
            NhlRLSetMDFloatArray(rlist,NhlNvfUDataArray,U5,2,len_dims);
            NhlRLSetMDFloatArray(rlist,NhlNvfVDataArray,V5,2,len_dims);
            NhlSetValues(vfield2,rlist);

            start[0] = i; start[1] = start[2] = 0;
            count[0] = 1; count[1] = latplen; count[2] = lonplen;
            ncvarget(ncid[2],pid,(long const *)start,(long const *)count,P);

            for( j = 0; j < latplen*lonplen; j++ ) {
                if( P[j] != -9999.0 ) P[j] /= 100.;
            }
            len_dims[0] = latplen;
            len_dims[1] = lonplen;
            NhlRLClear(rlist);
            NhlRLSetMDFloatArray(rlist,NhlNsfDataArray,P,2,len_dims);
            NhlSetValues(sfield,rlist);

            start[0] = i; start[1] = start[2] = 0;
            count[0] = 1; count[1] = lattlen; count[2] = lontlen;
            ncvarget(ncid[3],tid,(long const *)start,(long const *)count,T);

            for( j = 0; j < lattlen*lontlen; j++ ) {
                if( T[j] != -9999.0) T[j] = (T[j] - 273.15) * 9.0/5.0 + 32.0;
            }

            len_dims[0] = lattlen;
            len_dims[1] = lontlen;
            NhlRLClear(rlist);
            NhlRLSetMDFloatArray(rlist,NhlNsfDataArray,T,2,len_dims);
            NhlSetValues(sfield2,rlist);
/* 
 * Draw the plot
 */
            NhlDraw(mpid);
            NhlFrame(wid);
        }
    }
/* 
 *  Destroy the workstation object and exit.
 */
    NhlDestroy(wid);
    NhlClose();
    exit(0);
}

/*
 *      $Id: st04c.c,v 1.8 2010-03-15 22:49:24 haley Exp $
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
 *               0 and 6 hour forecast times. However, because some of
 *               the original files were lost, certain time steps come
 *               from longer range forcasts. Also, several steps had
 *               to be excluded from the frame set because the data is 
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
#include <ncarg/hlu/PDFWorkstation.h>
#include <ncarg/hlu/CairoWorkstation.h>
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
 *
 * #define TIMESTEPS 64
 */
#define TIMESTEPS 20

/*
 * Initialize netCDF filenames
 */
char *cdffiles[6] = {"Ustorm.cdf","Vstorm.cdf","Pstorm.cdf","Tstorm.cdf","U500storm.cdf","V500storm.cdf"};


int main(int argc, char *argv[])
{
    int i, j, d, h;
    int appid, wid, cnid, vcid, stid, txid, amid, mpid, tmid, stdmid;
    long stid_len;
    int vfield, vfield2, sfield, sfield2;
    int rlist;
    ng_size_t len_dims[2];
    long strt[1], cnt[1];
    long latlen, lonlen;
    long timelen;
    int *timestep;
    int ncid[6], uid, vid, u5id, v5id, pid, tid;
    int latid, lonid;
    float *lon, *lat;
    float *X, *Y;
    char  filename[256];
    char  *rftime;
    const char *dir = _NGGetNCARGEnv("data");
    char hour[3], day[3], mainstring[17];
    extern void get_2d_array(float *, long, long, int, int, long);
    char const *wks_type = "x11";

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

    if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
/*
 * Create a meta file workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./st04c.ncgm");
        NhlRLSetString(rlist,NhlNwkColorMap,"temp1");
        NhlCreate(&wid,"st04Work",
                  NhlncgmWorkstationClass,NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 * Create an X workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlRLSetString(rlist,NhlNwkColorMap,"temp1");
        NhlCreate(&wid,"st04Work",NhlcairoWindowWorkstationClass,appid,rlist);
    }

    else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 * Create an older-style PostScript workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPSFileName,"st04c.ps");
        NhlRLSetString(rlist,NhlNwkColorMap,"temp1");
        NhlCreate(&wid,"st04Work",NhlpsWorkstationClass,appid,rlist);
    }
    else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 * Create an older-style PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPDFFileName,"st04c.pdf");
        NhlRLSetString(rlist,NhlNwkColorMap,"temp1");
        NhlCreate(&wid,"st04Work",NhlpdfWorkstationClass,appid,rlist);
    }
    else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
             !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 * Create a cairo PS/PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"st04c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlRLSetString(rlist,NhlNwkColorMap,"temp1");
        NhlCreate(&wid,"st04Work",NhlcairoDocumentWorkstationClass,appid,rlist);
    }
    else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 * Create a cairo PNG workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"st04c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlRLSetString(rlist,NhlNwkColorMap,"temp1");
        NhlCreate(&wid,"st04Work",NhlcairoImageWorkstationClass,appid,rlist);
    }
/*
 * Open the netCDF files.
 */
    for( i = 0; i <= 5; i++ ) {
        sprintf( filename, "%s/cdf/%s", dir, cdffiles[i] );
        ncid[i] = ncopen(filename,NC_NOWRITE);
    }
/*
 * Get the lat/lon dimensions (they happen to be the
 * same for all files in this case)
 */
    latid = ncdimid(ncid[0],"lat");
    lonid = ncdimid(ncid[0],"lon");
    ncdiminq(ncid[0],latid,(char *)0,&latlen);
    ncdiminq(ncid[0],lonid,(char *)0,&lonlen);
    len_dims[0] = latlen;
    len_dims[1] = lonlen;
/*
 * Get the variable ids
 */
    uid = ncvarid(ncid[0],"u");
    vid = ncvarid(ncid[1],"v");
    pid = ncvarid(ncid[2],"p");
    tid = ncvarid(ncid[3],"t");
    u5id = ncvarid(ncid[4],"u");
    v5id = ncvarid(ncid[5],"v");
    latid = ncvarid(ncid[0],"lat");
    lonid = ncvarid(ncid[0],"lon");
/*
 * allocate space for arrays
 */
    X = (float *)malloc(sizeof(float)*latlen*lonlen);
    Y = (float *)malloc(sizeof(float)*latlen*lonlen);
    lat = (float *)malloc(sizeof(float)*latlen);
    lon = (float *)malloc(sizeof(float)*lonlen);
/*
 * Get lat/lon values (they are the same for all files)
 */
    strt[0] = 0;
    cnt[0] = latlen;
    ncvarget(ncid[0],latid,(long const *)strt,(long const *)cnt,lat);
    cnt[0] = lonlen;
    ncvarget(ncid[0],lonid,(long const *)strt,(long const *)cnt,lon);
/*
 * Get U and V data values
 */
    get_2d_array(X,latlen,lonlen,ncid[0],uid,0);
    get_2d_array(Y,latlen,lonlen,ncid[1],vid,0);
/*
 * Create a VectorField of the surface wind data
 */
    NhlRLClear(rlist);
    NhlRLSetMDFloatArray(rlist,NhlNvfUDataArray,X,2,len_dims);
    NhlRLSetMDFloatArray(rlist,NhlNvfVDataArray,Y,2,len_dims);
    NhlRLSetFloat(rlist,NhlNvfXCStartV,lon[0]);
    NhlRLSetFloat(rlist,NhlNvfYCStartV,lat[0]);
    NhlRLSetFloat(rlist,NhlNvfXCEndV,lon[lonlen-1]);
    NhlRLSetFloat(rlist,NhlNvfYCEndV,lat[latlen-1]);
    NhlRLSetFloat(rlist,NhlNvfMissingUValueV,-9999.0);
    NhlCreate(&vfield,"VectorField",NhlvectorFieldClass,appid,rlist);
/*
 * Create a VectorField of 500 millibar wind data
 *
 * Get U and V values
 */
    get_2d_array(X,latlen,lonlen,ncid[4],u5id,0);
    get_2d_array(Y,latlen,lonlen,ncid[5],v5id,0);

    NhlRLClear(rlist);
    NhlRLSetMDFloatArray(rlist,NhlNvfUDataArray,X,2,len_dims);
    NhlRLSetMDFloatArray(rlist,NhlNvfVDataArray,Y,2,len_dims);
    NhlRLSetFloat(rlist,NhlNvfXCStartV,lon[0]);
    NhlRLSetFloat(rlist,NhlNvfYCStartV,lat[0]);
    NhlRLSetFloat(rlist,NhlNvfXCEndV,lon[lonlen-1]);
    NhlRLSetFloat(rlist,NhlNvfYCEndV,lat[latlen-1]);
    NhlRLSetFloat(rlist,NhlNvfMissingUValueV,-9999.0);
    NhlCreate(&vfield2,"VectorField",NhlvectorFieldClass,appid,rlist);
/*
 * Create a ScalarField of surface pressure 
 *
 * Get P data values
 */
    get_2d_array(X,latlen,lonlen,ncid[2],pid,0);

    for( i = 0; i < latlen*lonlen; i++ ) {
        if( X[i] != -9999.0 ) {
            X[i] /= 100.;
        }
    }

    NhlRLClear(rlist);
    NhlRLSetMDFloatArray(rlist,NhlNsfDataArray,X,2,len_dims);
    NhlRLSetFloat(rlist,NhlNsfXCStartV,lon[0]);
    NhlRLSetFloat(rlist,NhlNsfYCStartV,lat[0]);
    NhlRLSetFloat(rlist,NhlNsfXCEndV,lon[lonlen-1]);
    NhlRLSetFloat(rlist,NhlNsfYCEndV,lat[latlen-1]);
    NhlRLSetFloat(rlist,NhlNsfMissingValueV,-9999.0);
    NhlCreate(&sfield,"ScalarField",NhlscalarFieldClass,appid,rlist);
/*
 * Create a ScalarField of surface temperature 
 * (convert from Kelvin to Farenheit)
 *
 * Get T data values
 */
    get_2d_array(X,latlen,lonlen,ncid[3],tid,0);
/*
 * Convert to Fahrenheit
 */
    for( i = 0; i < latlen*lonlen; i++ ) {
        if( X[i] != -9999.0) {
            X[i] = (X[i] - 273.15) * 9.0/5.0 + 32.0;
        }
    }

    NhlRLClear(rlist);
    NhlRLSetMDFloatArray(rlist,NhlNsfDataArray,X,2,len_dims);
    NhlRLSetFloat(rlist,NhlNsfXCStartV,lon[0]);
    NhlRLSetFloat(rlist,NhlNsfYCStartV,lat[0]);
    NhlRLSetFloat(rlist,NhlNsfXCEndV,lon[lonlen-1]);
    NhlRLSetFloat(rlist,NhlNsfYCEndV,lat[latlen-1]);
    NhlRLSetFloat(rlist,NhlNsfMissingValueV,-9999.0);
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
/*    NhlRLSetString(rlist,NhlNvpUseSegments,"true"); */
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
    tmid = ncdimid(ncid[1],"timestep");
    ncdiminq(ncid[1],tmid,(char *)0,&timelen);
    tmid = ncvarid(ncid[1],"timestep");
    timestep = (int *)malloc(sizeof(int)*timelen);

    strt[0] = 0;
    cnt[0] = timelen;
    ncvarget(ncid[1],tmid,(long const *)strt,(long const *)cnt,timestep);
    sprintf( hour, "00");
    sprintf( day, "05");
    
    stdmid = ncdimid(ncid[1],"timelen");
    ncdiminq(ncid[1], stdmid, (char *)0, &stid_len );
    tmid = ncvarid(ncid[1],"reftime");
    rftime = (char *)malloc((stid_len+1)*sizeof(char));

    strt[0] = 0; cnt[0] = stid_len;
    ncvarget(ncid[1],tmid,(long const *)strt,(long const *)cnt,rftime);

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
			strcpy(mainstring, rftime);
            sprintf(&mainstring[8], "%2s %2s:00", day, hour);
            printf("%s\n",mainstring);
            NhlRLClear(rlist);
            NhlRLSetString(rlist,NhlNtiMainString,mainstring);
            NhlSetValues(stid,rlist);
/*
 * Modify the data objects with data for the current time step
 *
 * Get U and V values
 */         
            get_2d_array(X,latlen,lonlen,ncid[0],uid,i);
            get_2d_array(Y,latlen,lonlen,ncid[1],vid,i);

            NhlRLClear(rlist);
            NhlRLSetMDFloatArray(rlist,NhlNvfUDataArray,X,2,len_dims);
            NhlRLSetMDFloatArray(rlist,NhlNvfVDataArray,Y,2,len_dims);
            NhlSetValues(vfield,rlist);
/*
 * Get U and V values
 */
            get_2d_array(X,latlen,lonlen,ncid[4],u5id,i);
            get_2d_array(Y,latlen,lonlen,ncid[5],v5id,i);

            NhlRLClear(rlist);
            NhlRLSetMDFloatArray(rlist,NhlNvfUDataArray,X,2,len_dims);
            NhlRLSetMDFloatArray(rlist,NhlNvfVDataArray,Y,2,len_dims);
            NhlSetValues(vfield2,rlist);
/*
 * Get P values
 */
            get_2d_array(X,latlen,lonlen,ncid[2],pid,i);

            for( j = 0; j < latlen*lonlen; j++ ) {
                if( X[j] != -9999.0 ) {
                    X[j] /= 100.;
                }
            }
            NhlRLClear(rlist);
            NhlRLSetMDFloatArray(rlist,NhlNsfDataArray,X,2,len_dims);
            NhlSetValues(sfield,rlist);
/*
 * Get T values
 */
            get_2d_array(X,latlen,lonlen,ncid[3],tid,i);
/*
 * Convert to Fahrenheit
 */
            for( j = 0; j < latlen*lonlen; j++ ) {
                if( X[j] != -9999.0) {
                    X[j] = (X[j] - 273.15) * 9.0/5.0 + 32.0;
                }
            }

            NhlRLClear(rlist);
            NhlRLSetMDFloatArray(rlist,NhlNsfDataArray,X,2,len_dims);
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

/*
 * function for reading in 3-d array from netCDF
 * file and converting it to a 2-d array.
 */
void get_2d_array(
    float *array,
    long latlen,
    long lonlen,
    int fid,
    int aid,
    long timestep                  
)
{
    long start[3], count[3];

    start[0] = timestep;
    start[1] = start[2] = 0;
    count[0] = 1; count[1] = latlen; count[2] = lonlen;
    ncvarget(fid,aid,(long const *)start,(long const *)count,array);
    return;
}


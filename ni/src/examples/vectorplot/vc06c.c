/*
 *  $Id: vc06c.c,v 1.8 2010-03-15 22:49:25 haley Exp $    
 */
/***********************************************************************
*                                                                      *
*                Copyright (C)  1996                                   *
*        University Corporation for Atmospheric Research               *
*                All Rights Reserved                                   *
*                                                                      *
***********************************************************************/
/*
 *  File:       vc06c.c
 *
 *  Author:     David Brown (converted by Lynn Hermanson)
 *              National Center for Atmospheric Research
 *              PO 3000, Boulder, Colorado
 *
 *  Date:       June 19, 1996
 *
 *
 *
 *    Description:  This example demonstrates several features of
 *                 VectorPlot:
 *                 1) Use of the vcMinDistanceF resource to reduce
 *                    the crowding of vector arrows in regions where
 *                    the transformation compresses the distance
 *                    between adjacent grid points.
 *                 2) Use of a scalarfield to determine the color
 *                    of the vector arrow fill.
 *                 3) VectorPlot as an overlay of MapPlot.
 *                 Successive frames show the result of increasing the
 *                 vcMinDistanceF in small steps. At the same time the
 *                 MapTransformation mpCenterLonF resource is decreased in
 *                 steps, causing the orthographic projection of the
 *                 northern hemisphere to appear to rotate when the
 *                 output is animated. Increasing the value of the
 *                 FRAME_COUNT variable will result in a smoother
 *                 animation.
 */

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <math.h>
#include <ncarg/gks.h>
#include <ncarg/ncargC.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/hlu/PDFWorkstation.h>
#include <ncarg/hlu/CairoWorkstation.h>
#include <ncarg/hlu/VectorPlot.h>
#include <ncarg/hlu/ScalarField.h>
#include <ncarg/hlu/VectorField.h>
#include <ncarg/hlu/MapPlot.h>
#include <ncarg/hlu/ResList.h>
#include <netcdf.h>


int main(int argc, char *argv[])
{
    char const *wks_type = "x11";
    int appid,wid,vcid,vfid, sfid, mpid;
    int rlist;
    float U[73][73],V[73][73], PSL[73][73];
    char  smindist0[6] ;
    char  smindist1[5] ;
    char  smindist2[4] ;
    char  title[35];
    char  smindist[7] ;
    char slongitude[100] ;
/*
 * Declare variables for getting information from netCDF file.
 */
    int   uv, p, u_id, v_id, p_id, lon_id, lat_id, FRAME_COUNT;
    int  i, mindistval, longitudeval;
    ng_size_t icount[3];
    float val;
    long  start[2], count[2], lonlen, latlen; 
    float CenLonF;
    char  filenameUV[256];
    char  filenamePsl[256];
    const char *dirUV = _NGGetNCARGEnv("data");
    const char *dirPsl = _NGGetNCARGEnv("data");
/*
 * Generate vector data array
 */
    FRAME_COUNT=13;
/*
 * Open the netCDF file.
 */
    sprintf( filenameUV, "%s/cdf/941110_UV.cdf", dirUV );
    uv = ncopen(filenameUV,NC_NOWRITE);
/*
 * Open the netCDF file.
 */
    sprintf( filenamePsl, "%s/cdf/941110_P.cdf", dirPsl );
    p = ncopen(filenamePsl,NC_NOWRITE);
/*
 * Initialize the high level utility library
 */
    NhlInitialize();
/*
 * Create an application context. Set the app dir to the current
 * directory so the application looks for a resource file in the working
 * directory. 
 */
    rlist = NhlRLCreate(NhlSETRL);
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNappUsrDir,"./");
    NhlCreate(&appid,"vc06",NhlappClass,NhlDEFAULT_APP,rlist);

    if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
/*
 * Create a meta file workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./vc06c.ncgm");
        NhlCreate(&wid,"vc06Work",
                  NhlncgmWorkstationClass,NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 * Create an X workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlCreate(&wid,"vc06Work",NhlcairoWindowWorkstationClass,appid,rlist);
    }

    else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 * Create an older-style PostScript workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPSFileName,"vc06c.ps");
        NhlCreate(&wid,"vc06Work",NhlpsWorkstationClass,appid,rlist);
    }
    else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 * Create an older-style PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPDFFileName,"vc06c.pdf");
        NhlCreate(&wid,"vc06Work",NhlpdfWorkstationClass,appid,rlist);
    }
    else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
             !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 * Create a cairo PS/PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"vc06c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"vc06Work",NhlcairoDocumentWorkstationClass,appid,rlist);
    }
    else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 * Create a cairo PNG workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"vc06c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"vc06Work",NhlcairoImageWorkstationClass,appid,rlist);
    }
/*
 * Get the U and V and lat/lon dimensions.
 */
    lat_id = ncdimid(uv,"lat");
    lon_id = ncdimid(uv,"lon");
    u_id = ncvarid(uv,"u");
    v_id = ncvarid(uv,"v");
    ncdiminq(uv,lat_id,(char *)0,&latlen);
    ncdiminq(uv,lon_id,(char *)0,&lonlen);

    start[0] = start[1] = 0;
    count[0] = latlen; count[1] = lonlen;
    ncvarget(uv,u_id,(long const *)start,(long const *)count,U);
    ncvarget(uv,v_id,(long const *)start,(long const *)count,V);
/*
 * Create a VectorField data object using the data set defined above.
 * By default the array bounds will define the data boundaries (zero-based,
 * as in C language conventions)
 */
    icount[0] = latlen; icount[1] = lonlen;

    NhlRLClear(rlist);
    NhlRLSetMDFloatArray(rlist,NhlNvfUDataArray,&U[0][0],2,icount);
    NhlRLSetMDFloatArray(rlist,NhlNvfVDataArray,&V[0][0],2,icount);
    NhlRLSetFloat(rlist,NhlNvfXCStartV, -180.0);
    NhlRLSetFloat(rlist,NhlNvfXCEndV, 180.0);
    NhlRLSetFloat(rlist,NhlNvfYCStartV,-90.0);
    NhlRLSetFloat(rlist,NhlNvfYCEndV, 90.0);
    NhlCreate(&vfid,"vectorfield",NhlvectorFieldClass,appid,rlist);
/*
 * Get the PSL and lat/lon dimensions.
 */
    lat_id = ncdimid(p,"lat");
    lon_id = ncdimid(p,"lon");
    p_id = ncvarid(p,"Psl");
    ncdiminq(p,lat_id,(char *)0,&latlen);
    ncdiminq(p,lon_id,(char *)0,&lonlen);

    start[0] = start[1] = 0;
    count[0] = latlen; count[1] = lonlen;
    ncvarget(p,p_id,(long const *)start,(long const *)count,PSL);
    icount[0] = latlen; icount[1] = lonlen;

    NhlRLClear(rlist);
    NhlRLSetMDFloatArray(rlist,NhlNsfDataArray,&PSL[0][0],2,icount);
    NhlRLSetFloat(rlist,NhlNsfXCStartV, -180.0);
    NhlRLSetFloat(rlist,NhlNsfXCEndV, 180.0);
    NhlRLSetFloat(rlist,NhlNsfYCStartV, -90.0);
    NhlRLSetFloat(rlist,NhlNsfYCEndV, 90.0);
    NhlCreate(&sfid,"scalarfield",NhlscalarFieldClass,appid,rlist);
/*
 * Create a VectorPlot object, supplying the VectorField object as data
 * Setting vcMonoFillArrowFillColor False causes VectorPlot to color the
 * vector arrows individually based, by default, on the vector magnitude.
 * Also supply the ScalarField object that will be used to determine the
 * color of each individual vector arrow.
 * Setting vcMonoVectorLineColor False causes VectorPlot to color the
 * vector arrows individually and setting vcUseScalarArray True results
 * in VectorPlot applying the colors based on the contents of the scalarfield.
 */
    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNvcRefMagnitudeF, 20.0);
    NhlRLSetString(rlist,NhlNvcUseScalarArray, "True");
    NhlRLSetString(rlist,NhlNvcFillArrowsOn, "True");
    NhlRLSetString(rlist,NhlNvcMonoFillArrowFillColor, "False");
    NhlRLSetFloat(rlist,NhlNvcMinFracLengthF, 0.25);
    NhlRLSetInteger(rlist,NhlNvcVectorFieldData,vfid);
    NhlRLSetInteger(rlist,NhlNvcScalarFieldData,sfid);
    NhlCreate(&vcid,"vectorplot",NhlvectorPlotClass,wid,rlist);

    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNmpProjection, "ORTHOGRAPHIC");
    NhlRLSetFloat(rlist,NhlNmpCenterLatF, 50);
    NhlCreate(&mpid,"mapplot",NhlmapPlotClass,wid,rlist);

    NhlAddOverlay(mpid,vcid, -1);
/*
 * Strings used to create fixed length numbers
 */
    strcpy(smindist0 ,"0.000");
    strcpy(smindist1 ,"0.00");
    strcpy( smindist2 ,"0.0");
/*
 * Create FRAME_COUNT frames, increasing the value of vcMinDistanceF
 * and decreasing the value of mpCenterLonF at each successive frame.
 *
 * Note that the first frame and the last frame are equivalent in
 * longitude.
 */
    for(i = (FRAME_COUNT-1);i > -1; i--){
        NhlRLClear(rlist);       
        CenLonF =  i * 360./(FRAME_COUNT-1);
        NhlRLSetFloat(rlist,NhlNmpCenterLonF,CenLonF);
        NhlSetValues(mpid,rlist);
/*
 * create fixed length strings representing the current longitude
 * and the value of vcMinDistanceF
 */
        longitudeval = (int)(i * 360./(FRAME_COUNT-1) + 0.5);

        sprintf(slongitude,"%d:S:o:N:",longitudeval);

        val = ((FRAME_COUNT-1) - i) * 0.0175/(FRAME_COUNT-1);
        mindistval = (int)(10000*val + 0.5);

        if (mindistval < 10){
            sprintf(smindist,"%s%d",smindist0,mindistval);
        }
        else {
            if (mindistval < 100){
                sprintf(smindist,"%s%d",smindist1,mindistval);
            }
            else {
                sprintf(smindist,"%s%d",smindist2,mindistval);
            }
        }

        NhlRLClear(rlist);
        
        strcpy(title,"Varying vcMinDistanceF :: ");
        strcat(title,smindist);
        
        NhlRLSetString(rlist,NhlNtiMainString,title);
        NhlRLSetString(rlist,NhlNtiXAxisString,slongitude);
        NhlRLSetFloat(rlist,NhlNvcMinDistanceF,val);
        NhlSetValues(vcid,rlist);

        NhlDraw(mpid);
        NhlFrame(wid);

    }/*end for*/
/*
 * Destroy the objects created, close the HLU library and exit.
 */
    NhlDestroy(mpid);
    NhlDestroy(wid);
    NhlDestroy(appid);
    NhlClose();
    exit(0);
}

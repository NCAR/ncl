/*
 *      $Id: vc09c.c,v 1.6 2010-03-15 22:49:25 haley Exp $
 */
/************************************************************************
 *                                                                      *
 *                Copyright (C)  1997                                   *
 *        University Corporation for Atmospheric Research               *
 *                All Rights Reserved                                   *
 *                                                                      *
 ************************************************************************
 *
 *   File:       vc09c.c
 *   Author:     David Brown
 *               National Center for Atmospheric Research
 *               PO 3000, Boulder, Colorado
 *               Converted to C by Scott Snodgrass
 *
 *   Date:       Wed Jul  9 08:06:16 MDT 1997
 *
 *   Description:    Does an animation of the January 1996 snow storm.
 *                   Wind vectors colored by temperature are animated
 *                   over a pressure field contour plot.
 */

#include <math.h>
#include <ncarg/gks.h>
#include <ncarg/ncargC.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/Title.h>
#include <ncarg/hlu/TextItem.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/hlu/PDFWorkstation.h>
#include <ncarg/hlu/CairoWorkstation.h>
#include <ncarg/hlu/VectorPlot.h>
#include <ncarg/hlu/ScalarField.h>
#include <ncarg/hlu/MapPlot.h>
#include <ncarg/hlu/ContourPlot.h>
#include <netcdf.h>

#define NLON 36
#define NLAT 33

int main ()
{

/*
 *  If zoom = 0 then this script will animate the entire United States.
 *  If zoom = 1 then this script will animate just the Great Lakes region
 *  of the United States.
 *
 */

    int ZOOM=0;

    int i, j, k, u_id, v_id, p_id, t_id, *time, *timestep;
    int rlist, uf, vf, pf, tf, tim_id, lat_id, lon_id, tit_id;
    int appid, wid, vfield, sfield, sfield2, mapid, vcid, cnid;
    int title_id1, title_id2, txid1;
    long timlen, latlen, lonlen, titlen;
    long start [3] = {0,0,0}, count [3]={0,0,0};
    float MinLat, MaxLat, MinLon, MaxLon;
    float *U, *V, *P, *T, *lat, *lon;
    ng_size_t len_dims[2];
    char Uname [256], Vname [256], Pname [256], Tname [256];
    char *reftime;
    char title [256];
    const char *dir = _NGGetNCARGEnv ("data");
    extern void get_2d_array(float *, long, long, int, int, long);
    const char *wks_type = "ncgm";

/*
 *  Create an application object.  It will look for a resource file
 *  named vc09.res
 */

    NhlInitialize();
    rlist= NhlRLCreate (NhlSETRL);
  
    NhlRLClear (rlist);
    NhlRLSetString (rlist,NhlNappUsrDir,"./");
    NhlRLSetString (rlist,NhlNappDefaultParent,"True");
    NhlCreate (&appid, "vc09", NhlappClass, NhlDEFAULT_APP, rlist);

/*
 *  Create an ncgmWorkstation object.
 */

    if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
       NhlRLClear (rlist);
       NhlRLSetString (rlist, NhlNwkMetaName, "./vc09c.ncgm");
       NhlRLSetString (rlist, NhlNwkColorMap, "temp1");
       NhlCreate (&wid, "vc09Work", NhlncgmWorkstationClass,
                   NhlDEFAULT_APP, rlist);
    }
    else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 *  Create an X11 workstation.
 */
      NhlRLClear (rlist);
      NhlRLSetString (rlist, NhlNwkPause, "True");
      NhlRLSetString (rlist, NhlNwkColorMap, "temp1");
      NhlCreate (&wid, "vc09Work", NhlcairoWindowWorkstationClass,
                   NhlDEFAULT_APP, rlist);
    }
    else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {

/*
 *  Create an older-style PostScript workstation.
 */
       NhlRLClear (rlist);
       NhlRLSetString (rlist, NhlNwkPSFileName, "vc09c.ps");
	   NhlRLSetString (rlist, NhlNwkColorMap, "temp1");
       NhlCreate (&wid, "vc09Work", NhlpsWorkstationClass,
                   NhlDEFAULT_APP, rlist);
    }
    else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 *  Create an older-style PDF workstation.
 */
       NhlRLClear (rlist);
       NhlRLSetString (rlist, NhlNwkPDFFileName, "vc09c.pdf");
	   NhlRLSetString (rlist, NhlNwkColorMap, "temp1");
       NhlCreate (&wid, "vc09Work", NhlpdfWorkstationClass,
                   NhlDEFAULT_APP, rlist);
    }
    else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
             !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 *  Create a cairo PS/PDF Workstation object.
 */
       NhlRLClear (rlist);
       NhlRLSetString (rlist, NhlNwkFileName, "vc09c");
       NhlRLSetString (rlist, NhlNwkFormat,(char*)wks_type);
	   NhlRLSetString (rlist, NhlNwkColorMap, "temp1");
       NhlCreate (&wid, "vc09Work", NhlcairoDocumentWorkstationClass,
                   NhlDEFAULT_APP, rlist);
    }
    else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 *  Create a cairo PNG Workstation object.
 */
       NhlRLClear (rlist);
       NhlRLSetString (rlist, NhlNwkFileName, "vc09c");
       NhlRLSetString (rlist, NhlNwkFormat,(char*)wks_type);
	   NhlRLSetString (rlist, NhlNwkColorMap, "temp1");
       NhlCreate (&wid, "vc09Work", NhlcairoImageWorkstationClass,
                   NhlDEFAULT_APP, rlist);
    }

/*
 *  Open the netcdf files.
 */

    sprintf (Uname, "%s/cdf/Ustorm.cdf",dir);
    sprintf (Vname, "%s/cdf/Vstorm.cdf",dir);
    sprintf (Pname, "%s/cdf/Pstorm.cdf",dir);
    sprintf (Tname, "%s/cdf/Tstorm.cdf",dir);

    uf = ncopen (Uname, NC_NOWRITE);
    vf = ncopen (Vname, NC_NOWRITE);
    pf = ncopen (Pname, NC_NOWRITE);
    tf = ncopen (Tname, NC_NOWRITE);

    lat_id = ncdimid (uf,"lat");
    lon_id = ncdimid (uf,"lon");
    tim_id = ncdimid (vf,"timestep");
    tit_id = ncdimid (vf,"timelen");

    ncdiminq (uf,lat_id,(char *)0,&latlen);
    ncdiminq (uf,lon_id,(char *)0,&lonlen);
    ncdiminq (vf,tim_id,(char *)0,&timlen);
    ncdiminq (vf,tit_id,(char *)0,&titlen);

    len_dims [0] = latlen;
    len_dims [1] = lonlen;

    U = (float *)malloc(sizeof(float)*latlen*lonlen);
    V = (float *)malloc(sizeof(float)*latlen*lonlen);
    P = (float *)malloc(sizeof(float)*latlen*lonlen);
    T = (float *)malloc(sizeof(float)*latlen*lonlen);
    lat = (float *)malloc(sizeof(float)*latlen);
    lon = (float *)malloc(sizeof(float)*lonlen);

    u_id = ncvarid (uf,"u");
    v_id = ncvarid (vf,"v");
    p_id = ncvarid (pf,"p");
    t_id = ncvarid (tf,"t");
    lat_id = ncvarid (uf,"lat");
    lon_id = ncvarid (uf,"lon");
    tim_id = ncvarid (vf,"timestep");
    tit_id = ncvarid (vf,"reftime");

    start[2] = start[1] = start[0] =0;
    count[0] = latlen;
    count [1] = count [2] = 0;
    ncvarget(uf,lat_id,(long const *)start,(long const *)count,lat);
    count[0] = lonlen;
    ncvarget(uf, lon_id, (long const *)start, (long const *)count, lon);

    count [0] = timlen;
    timestep = (int *) malloc (sizeof (int) * timlen);
    ncvarget (vf, tim_id, (long const *)start, (long const *)count, timestep);

    count [0] = titlen;
    reftime = (char *) malloc (sizeof (char) * (titlen+1));
    ncvarget (vf, tit_id, (long const *)start, (long const *)count, reftime);

/*
 * Get U and V data values
 */
    get_2d_array (U, latlen, lonlen, uf, u_id, 0);
    get_2d_array (V, latlen, lonlen, vf, v_id, 0);

    get_2d_array (P, latlen, lonlen, pf, p_id, 0);
    get_2d_array (T, latlen, lonlen, tf, t_id, 0);

    for (i=0; i < latlen*lonlen; i++) {
        if (P[i] != -9999.0 ) P [i] /= 100.0;
        if (T[i] != -9999.0 ) T [i] = (T[i] - 273.15) * 9.0 / 5.0 + 32.0;
    }

    NhlRLClear (rlist);
    NhlRLSetMDFloatArray (rlist, NhlNvfUDataArray, U, 2, len_dims);
    NhlRLSetMDFloatArray (rlist, NhlNvfVDataArray, V, 2, len_dims);
    NhlRLSetFloat   (rlist, NhlNvfXCStartV, lon [0] );
    NhlRLSetFloat   (rlist, NhlNvfYCStartV, lat [0] );
    NhlRLSetFloat   (rlist, NhlNvfXCEndV, lon [lonlen-1]);
    NhlRLSetFloat   (rlist, NhlNvfYCEndV, lat [latlen-1]);
    NhlRLSetInteger (rlist, NhlNvfXCStride, 2);
    NhlRLSetInteger (rlist, NhlNvfYCStride, 2);
    NhlRLSetFloat   (rlist, NhlNvfMissingUValueV, -9999.0);
    NhlCreate (&vfield, "VectorField", NhlvectorFieldClass, appid, rlist);

    NhlRLClear (rlist);
    NhlRLSetMDFloatArray (rlist, NhlNsfDataArray, P, 2, len_dims);
    NhlRLSetFloat   (rlist, NhlNsfXCStartV, lon [0] );
    NhlRLSetFloat   (rlist, NhlNsfYCStartV, lat [0] );
    NhlRLSetFloat   (rlist, NhlNsfXCEndV, lon [lonlen - 1]);
    NhlRLSetFloat   (rlist, NhlNsfYCEndV, lat [latlen - 1]);
    NhlRLSetInteger (rlist, NhlNsfXCStride, 2);
    NhlRLSetInteger (rlist, NhlNsfYCStride, 2);
    NhlRLSetFloat   (rlist, NhlNsfMissingValueV, -9999.0);
    NhlCreate (&sfield, "ScalarField", NhlscalarFieldClass, appid, rlist);

    NhlRLClear (rlist);
    NhlRLSetMDFloatArray (rlist, NhlNsfDataArray, T, 2, len_dims);
    NhlRLSetFloat   (rlist, NhlNsfXCStartV, lon [0]);
    NhlRLSetFloat   (rlist, NhlNsfYCStartV, lat [0]);
    NhlRLSetFloat   (rlist, NhlNsfXCEndV, lon [lonlen - 1]);
    NhlRLSetFloat   (rlist, NhlNsfYCEndV, lat [latlen - 1]);
    NhlRLSetInteger (rlist, NhlNsfXCStride, 2);
    NhlRLSetInteger (rlist, NhlNsfYCStride, 2);
    NhlRLSetFloat   (rlist, NhlNsfMissingValueV, -9999.0);
    NhlCreate (&sfield2, "ScalarField2", NhlscalarFieldClass, appid, rlist);

/*
 * To zoom in on a certain area of the first plot adjust the following
 * four numbers.
 *
 * The following four numbers will cause the plots to display the
 * entire United States.
 */

    if (ZOOM == 0) {
       MinLat = 18.0;
       MaxLat = 65.0;
       MinLon = -128.0;
       MaxLon = -58.0;
    }
    else

/*
 * The Following four numbers will zoom in on the great lakes region of 
 * the United States.
 */

    if (ZOOM == 1) {
       MinLat = 40.0;
       MaxLat = 60.0;
       MinLon = -100.0;
       MaxLon = -58.0;
    }

/*
 *  Create a map object
 */

    NhlRLClear (rlist);
    NhlRLSetFloat  (rlist, NhlNvpXF, 0.03);
    NhlRLSetFloat  (rlist, NhlNvpYF, 0.85);
    NhlRLSetFloat  (rlist, NhlNvpWidthF, 0.8);
    NhlRLSetFloat  (rlist, NhlNvpHeightF, 0.8);
    NhlRLSetString (rlist, NhlNvpUseSegments, "true");
    NhlRLSetFloat  (rlist, NhlNmpMinLatF, MinLat);
    NhlRLSetFloat  (rlist, NhlNmpMaxLatF, MaxLat);
    NhlRLSetFloat  (rlist, NhlNmpMinLonF, MinLon);
    NhlRLSetFloat  (rlist, NhlNmpMaxLonF, MaxLon);
    NhlRLSetFloat  (rlist, NhlNmpCenterLonF, -100.0);
    NhlRLSetFloat  (rlist, NhlNmpCenterLatF, 40.0);
    NhlRLSetString (rlist, NhlNmpGridAndLimbDrawOrder, "predraw");
    NhlCreate (&mapid, "map", NhlmapPlotClass, wid, rlist);

    NhlRLClear (rlist);
    NhlRLSetString  (rlist, NhlNcnFillOn, "True");
    NhlRLSetString  (rlist, NhlNcnLinesOn, "False");
    NhlRLSetString  (rlist, NhlNcnFillDrawOrder, "predraw");
    NhlRLSetInteger (rlist, NhlNcnScalarFieldData, sfield);
    NhlRLSetString  (rlist, NhlNpmLabelBarDisplayMode, "always");
    NhlRLSetFloat   (rlist, NhlNpmLabelBarHeightF, 0.075);
    NhlRLSetFloat   (rlist, NhlNpmLabelBarWidthF, 0.6);
    NhlRLSetString  (rlist, NhlNlbOrientation, "horizontal");
    NhlRLSetString  (rlist, NhlNlbPerimOn, "False");
    NhlRLSetString  (rlist, NhlNpmLabelBarSide, "top");
    NhlCreate (&cnid,"contourplot", NhlcontourPlotClass, wid, rlist);

/*
 *  Create a VectorPlot object using the above data field.
 */

    NhlRLClear(rlist);
    NhlRLSetString  (rlist, NhlNvcUseScalarArray, "true");
    NhlRLSetInteger (rlist, NhlNvcVectorFieldData, vfield);
    NhlRLSetInteger (rlist, NhlNvcScalarFieldData, sfield2);
    NhlRLSetFloat   (rlist, NhlNvcMinFracLengthF, 0.33);
    NhlRLSetString  (rlist, NhlNvcMonoLineArrowColor, "false");
    NhlRLSetString  (rlist, NhlNvcVectorDrawOrder, "predraw");
    NhlRLSetString  (rlist, NhlNpmLabelBarDisplayMode, "always");
    NhlRLSetFloat   (rlist, NhlNpmLabelBarWidthF, 0.1);
    NhlRLSetString  (rlist, NhlNlbPerimOn, "False");
    NhlCreate (&vcid, "vectorplot", NhlvectorPlotClass, wid, rlist);

    sprintf (title, "%s + %d", reftime, timestep [0]);

    NhlRLClear (rlist);
    NhlRLSetFloat  (rlist, NhlNvpXF, 0.03);
    NhlRLSetFloat  (rlist, NhlNvpYF, 0.85);
    NhlRLSetFloat  (rlist, NhlNvpWidthF, 0.8);
    NhlRLSetFloat  (rlist, NhlNvpHeightF, 0.8);
    NhlRLSetString (rlist, NhlNtiMainFuncCode, "~");
    NhlRLSetInteger(rlist, NhlNtiMainFont, 25);
    NhlRLSetString (rlist, NhlNtiMainString, title);
    NhlCreate (&title_id1, "Titles", NhltitleClass, wid, rlist);

    NhlRLClear (rlist);
    NhlRLSetFloat  (rlist, NhlNvpXF, 0.03);
    NhlRLSetFloat  (rlist, NhlNvpYF, 0.9);
    NhlRLSetFloat  (rlist, NhlNvpWidthF, 0.8);
    NhlRLSetFloat  (rlist, NhlNvpHeightF, 0.8);
    NhlRLSetString (rlist, NhlNtiMainString, "January 1996 Snow Storm");
    NhlRLSetInteger (rlist, NhlNtiMainFont, 25 );
    NhlCreate (&title_id2, "Titles", NhltitleClass, wid, rlist);

    NhlRLClear (rlist);
    NhlRLSetFloat  (rlist, NhlNtxPosXF, 0.25);
    NhlRLSetFloat  (rlist, NhlNtxPosYF, 0.08);
    NhlRLSetFloat (rlist, NhlNtxFontHeightF, 0.015 );
    NhlRLSetString (rlist, NhlNtxString, "Contours represent pressure field.:C:Vectors represent wind direction:C:colored by temperature." );
    NhlCreate (&txid1, "text", NhltextItemClass, wid, rlist);

    NhlAddOverlay(mapid,cnid,-1);
    NhlAddOverlay(mapid,vcid,-1);

    time = (int *) malloc (sizeof (int) * timlen);
    for (i = 0; i < timlen; i++) time [i] = timestep [i];

    j= 2*(timlen - 1)/3;

    for (i = j; i < timlen; i++)
    {
      if ((time[i] != 102) && (time[i] != 222) && (time[i] != 216)) { 

        get_2d_array (U, latlen, lonlen, uf, u_id, i);
        get_2d_array (V, latlen, lonlen, vf, v_id, i);

        get_2d_array (P, latlen, lonlen, pf, p_id, i);
        get_2d_array (T, latlen, lonlen, tf, t_id, i);

        NhlRLClear (rlist);
        NhlRLSetMDFloatArray (rlist, NhlNvfUDataArray, U, 2, len_dims);
        NhlRLSetMDFloatArray (rlist, NhlNvfVDataArray, V, 2, len_dims);
        NhlSetValues (vfield,rlist);

        for (k=0; k < latlen*lonlen; k++) {
           if (P[k] != -9999.0 ) P [k] /= 100.0;
           if (T[k] != -9999.0 ) T [k] = (T[k] - 273.15) * 9.0 / 5.0 + 32.0;
        }

        NhlRLClear (rlist);
        NhlRLSetMDFloatArray (rlist, NhlNsfDataArray, P, 2, len_dims);
        NhlSetValues (sfield, rlist);

        NhlRLClear (rlist);
        NhlRLSetMDFloatArray (rlist, NhlNsfDataArray, T, 2, len_dims);
        NhlSetValues (sfield2,rlist);

        sprintf (title, "%s + %d", reftime, timestep [i]);

        NhlRLClear (rlist);
        NhlRLSetString (rlist,  NhlNtiMainString, title);
        NhlSetValues (title_id1, rlist);

        NhlDraw (mapid);
        NhlDraw (title_id1);
        NhlDraw (title_id2);
		NhlDraw (txid1);
        NhlFrame (wid);
      }  
    }

/*
 *  Close the Netcdf files.
 */

    ncclose (uf);
    ncclose (vf);
    ncclose (pf);
    ncclose (tf);

/*
 *  Destroy the workstation object and exit.
 */

    NhlDestroy (wid);
    NhlClose ();
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
}

/*
 *      $Id: cn17c.c,v 1.6 2010-03-15 22:49:23 haley Exp $
 */
/***********************************************************************
*                                                                      *
*                Copyright (C)  1997                                   *
*        University Corporation for Atmospheric Research               *
*     The use of this Software is governed by a License Agreement      *
*                                                                      *
************************************************************************
 *
 *  File:       cn17c.c
 *
 *
 *  Author:     Mary Haley
 *              (original LLU example by Dave Kennison)
 *              National Center for Atmospheric Research
 *              PO Box 3000, Boulder, Colorado
 *
 *  Date:       Fri Apr 25 11:25:21 MST 1997
 *
 *  Description:  This example is somewhat similar to the LLU example
 *                "cpex10", which draws contours bands within a circle
 *                on a satellite map projection. It also shows how to
 *                use the NhlDataPolyline and NhlDataPolymarker routines
 *                to draw lines and markers on a map projection.
 *                
 *                This example uses the AnnoManager class to label each
 *                of the United States with a two-letter mnemonic. The
 *                labeling is not done in the map projection, however,
 *                so the text is not part of the map.  To show how you
 *                *can* get your text to be part of the map, this example
 *                has a second frame which mixes the LLUs and the HLUs
 *                to achieve this affect (since this *is* doable in the
 *                LLUs).
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
#include <ncarg/hlu/ScalarField.h>
#include <ncarg/hlu/AnnoManager.h>
#include <ncarg/hlu/TextItem.h>
/*
 * You must include ncarg/ncargC.h and ncarg/gks.h in order to get the
 * correct function prototyping for the low-level C routines.
 */
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define NCLS  100
#define NCOLORS  23
#define NDIM  50
#define NCIRC  100

/*
 * Declare the common block in which the angle at which the label of a
 * point on the globe is to be written and the latitude and longitude
 * of the point being labelled are transmitted to the routine PCMPXY,
 * in the package PLOTCHAR (this is for the LLU version of this plot).
 */

struct common1 {
  float pang, plat, plon;
} NGCALLC(pcmp04,PCMP04);

/*
 * Define the state-labelling data.
 */
typedef struct _Anno_List {
    NhlString   name;
    float       lat;
    float       lon;
} Anno_List;

Anno_List anno_list[] = {
  { "AL" , 33.0 ,  -86.5 },  { "AK" , 65.0 , -152.0 },
  { "AZ" , 34.7 , -111.5 },  { "AR" , 35.0 ,  -92.5 },
  { "CA" , 37.5 , -120.5 },  { "CO" , 39.0 , -105.8 },
  { "CT" , 41.6 ,  -72.6 },  { "DE" , 39.0 ,  -75.5 },
  { "FL" , 28.5 ,  -82.0 },  { "GA" , 32.5 ,  -83.0 },
  { "HI" , 20.0 , -157.0 },  { "ID" , 43.5 , -114.0 },
  { "IL" , 40.2 ,  -89.2 },  { "IN" , 40.0 ,  -86.0 },
  { "IA" , 42.0 ,  -93.2 },  { "KS" , 38.5 ,  -98.2 },
  { "KY" , 37.4 ,  -84.5 },  { "LA" , 31.2 ,  -92.5 },
  { "ME" , 45.5 ,  -69.0 },  { "MD" , 39.2 ,  -76.5 },
  { "MA" , 42.3 ,  -72.0 },  { "MI" , 44.0 ,  -85.0 },
  { "MN" , 46.0 ,  -94.5 },  { "MS" , 32.5 ,  -89.5 },
  { "MO" , 38.5 ,  -92.5 },  { "MT" , 47.0 , -109.5 },
  { "NE" , 41.5 ,  -99.5 },  { "NV" , 39.8 , -117.0 },
  { "NH" , 43.2 ,  -71.6 },  { "NJ" , 39.7 ,  -74.5 },
  { "NM" , 34.7 , -106.0 },  { "NY" , 43.0 ,  -75.0 },
  { "NC" , 35.5 ,  -79.5 },  { "ND" , 47.5 , -100.5 },
  { "OH" , 40.2 ,  -82.5 },  { "OK" , 35.6 ,  -97.5 },
  { "OR" , 44.0 , -120.2 },  { "PA" , 40.8 ,  -77.6 },
  { "RI" , 41.7 ,  -71.5 },  { "SC" , 34.0 ,  -80.5 },
  { "SD" , 44.5 , -100.5 },  { "TN" , 36.0 ,  -86.5 },
  { "TX" , 32.0 , -100.0 },  { "UT" , 39.5 , -111.5 },
  { "VT" , 44.2 ,  -72.5 },  { "VA" , 37.6 ,  -78.6 },
  { "WA" , 47.5 , -120.5 },  { "WV" , 38.5 ,  -80.8 },
  { "WI" , 44.5 ,  -89.5 },  { "WY" , 43.0 , -107.5 },
};

int main()
{
/*
 * Define variables for HLU objects and other stuff.
 */
    float x, y, zdat[NCLS][NCLS], xlat, xlon;
    float xlonrng, xlatrng, xlonstp, xlatstp, dist;
    float miss_val = 1.e12;
    ng_size_t count[2];
    int appid, wid, dataid, cnid, mpid, gsid, gkswid;
    int srlist, grlist, i, j;
    float dfce = 1.3, rtod = 57.2957795130823;
/*
 * cminlon, cmaxlon, cminlat, and cmaxlat are the four corners where we
 * want the contour plot to lie. ctrlat and ctrlon is the center of
 * the circular contour plot (and also where we want to put a marker at
 * the location of Boulder, CO). clat and clon will hold the lat/lon 
 * coordinates for the circle we want to contain the contour plot in.
 */
    float cminlon = -115., cmaxlon = -95.;
    float cminlat = 32., cmaxlat = 48.;
    float ctrlat = 40.;
    float ctrlon = -105.;
    float clat[NCIRC],clon[NCIRC];
/*
 * Define arrays to hold a list of two-character mnemonics
 * for the states, and the latitude and longitude of a point where the
 * mnemonic may be placed to label the state.
 */
    ng_size_t num_am_ids = NDIM;
    int *am_ids, text_ids[NDIM];
/*
 * Declare variables for defining color map.
 */
    ng_size_t length[2];
    float   cmap[NCOLORS][3];
/*
 * Default is to create an X11 window.
 */
    char const *wks_type = "x11";
/*
 * Initialize the HLU library and set up resource template.
 * A resource file is not used in this example, but if you did
 * want one, it would be called "cn17.res".
 */
    NhlInitialize();
    srlist = NhlRLCreate(NhlSETRL);
    grlist = NhlRLCreate(NhlGETRL);
    NhlRLClear(srlist);
    NhlRLSetString(srlist,NhlNappDefaultParent,"True");
    NhlRLSetString(srlist,NhlNappUsrDir,"./");
    NhlCreate(&appid,"cn17",NhlappClass,0,srlist);
/*
 * Modify the color map. Colors for contour fill areas varying from
 * blue to red.
 */
    cmap[0][0] = 0.00;
    cmap[0][1] = 0.00;
    cmap[0][2] = 0.00;
    cmap[1][0] = 1.00;
    cmap[1][1] = 1.00;
    cmap[1][2] = 1.00;
    cmap[2][0] = .6 ;
    cmap[2][1] = .6 ;
    cmap[2][2] = .6 ;
    cmap[3][0] = 0. ;
    cmap[3][1] = 0. ;
    cmap[3][2] = 0. ;
    cmap[4][0] = 1.;
    cmap[4][1] = 1.;
    cmap[4][2] = 1.;
    cmap[5][0] = .4;
    cmap[5][1] = .4;
    cmap[5][2] = .4;
    cmap[6][0] = 1.;
    cmap[6][1] = 1.;
    cmap[6][2] = 0.;
    for(i=8; i <= NCOLORS; i++ ) {
      cmap[i-1][0] = (float)(NCOLORS-i)/15.;
      cmap[i-1][1] = 0.;
      cmap[i-1][2] = (float)(i-8)/15.;
    }
    length[0] = NCOLORS;
    length[1] = 3;
    if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
/*
 * Create an NCGM workstation.
 */
      NhlRLClear(srlist);
      NhlRLSetString(srlist,NhlNwkMetaName,"./cn17c.ncgm");
      NhlRLSetMDFloatArray(srlist,NhlNwkColorMap,&cmap[0][0],2,length);
      NhlCreate(&wid,"cn17Work",NhlncgmWorkstationClass,0,srlist);
    }
    else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 * Create an X11 workstation.
 */
      NhlRLClear(srlist);
      NhlRLSetMDFloatArray(srlist,NhlNwkColorMap,&cmap[0][0],2,length);
      NhlRLSetString(srlist,NhlNwkPause,"True");
      NhlCreate(&wid,"cn17Work",NhlcairoWindowWorkstationClass,0,srlist);
    }
    else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 * Create an older-style PostScript workstation.
 */
      NhlRLClear(srlist);
      NhlRLSetMDFloatArray(srlist,NhlNwkColorMap,&cmap[0][0],2,length);
      NhlRLSetString(srlist,NhlNwkPSFileName,"./cn17c.ps");
      NhlCreate(&wid,"cn17Work",NhlpsWorkstationClass,0,srlist);
    }
    else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 * Create an older-style PDF workstation.
 */
      NhlRLClear(srlist);
      NhlRLSetMDFloatArray(srlist,NhlNwkColorMap,&cmap[0][0],2,length);
      NhlRLSetString(srlist,NhlNwkPDFFileName,"./cn17c.pdf");
      NhlCreate(&wid,"cn17Work",NhlpdfWorkstationClass,0,srlist);
    }
    else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
             !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 * Create a cairo PS/PDF workstation.
 */
      NhlRLClear(srlist);
      NhlRLSetMDFloatArray(srlist,NhlNwkColorMap,&cmap[0][0],2,length);
      NhlRLSetString(srlist,NhlNwkFileName,"./cn17c");
      NhlRLSetString(srlist,NhlNwkFormat,(char*)wks_type);
      NhlCreate(&wid,"cn17Work",NhlcairoDocumentWorkstationClass,0,srlist);
    }
    else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 * Create a cairo PNG workstation.
 */
      NhlRLClear(srlist);
      NhlRLSetMDFloatArray(srlist,NhlNwkColorMap,&cmap[0][0],2,length);
      NhlRLSetString(srlist,NhlNwkFileName,"./cn17c");
      NhlRLSetString(srlist,NhlNwkFormat,(char*)wks_type);
      NhlCreate(&wid,"cn17Work",NhlcairoImageWorkstationClass,0,srlist);
    }
/*
 * Create a "great" circle in lat/lon coordinates. We don't want to draw
 * any contour lines outside of this circle.
 */
    xlonrng = cmaxlon - cminlon;
    xlatrng = cmaxlat - cminlat;
    for(i=0; i < NCIRC; i++ ) {
      clon[i] = ctrlon + 7.*cos((i*6.28)/(float)(NCIRC-1));
      clat[i] = ctrlat + 7.*sin((i*6.28)/(float)(NCIRC-1));
    }
/*
 * Generate some dummy data to contour later.
 */
    xlonstp = xlonrng/(NCLS-1);
    xlatstp = xlatrng/(NCLS-1);
    for(i = 0; i < NCLS; i++ ) {
      xlon = cminlon + i * xlonstp;
      x = (float)i/(float)(NCLS-1);
      for(j = 0; j < NCLS; j++ ) {
        xlat = cminlat + j * xlatstp;
        dist = sqrt(pow((ctrlat - xlat),2.) + pow((ctrlon - xlon),2.));
/*
 * If xlat/xlon falls outside of circle, then we don't
 * want to contour this location.
 */
        if (dist <= 7.0) {
          y = (float)j/(float)(NCLS-1);
          zdat[j][i] = x*x + y*y + x*y + sin(9.*x)*cos(9.*y);
        }
        else {
          zdat[j][i] = miss_val;
        }
      }
    }
/*
 * AnnoManager objects allow the PlotManager to manipulate any View
 * class object as an annotation a uniform fashion. They allow
 * the user to set the View object's size and location relative to
 * the viewport of a Plot. They may be located relative to one
 * of the viewport sides, or, as in this example, aligned with the 
 * plot's data space (amTrackData is set True in the resource file).
 *
 * Create a TextItem for each place name to be included on the map.
 * Collect the object ids into an array.
 */
    for( i = 0; i < NDIM; i++ ) {
      NhlRLClear(srlist);
      NhlRLSetString(srlist,NhlNtxString,anno_list[i].name);
      NhlCreate(&text_ids[i],anno_list[i].name,NhltextItemClass,wid,srlist);
    }
/*
 * Create a MapPlot object.
 */
    NhlRLClear(srlist);
    NhlRLSetFloat(srlist,NhlNmpSatelliteAngle1F,7.*rtod*asin(1./dfce)/8.);
    NhlRLSetIntegerArray(srlist,NhlNpmAnnoViews,text_ids,NDIM);
    NhlCreate(&mpid,"MapPlot",NhlmapPlotClass,wid,srlist);
/*
 * Retrieve the ids of the AnnoManager objects created by the PlotManager
 * and then set their location in data coordinate space. The AnnoManager
 * objects are arranged in the same order as the TextItems in the
 * pmAnnoViews resource.
 */
    am_ids = (int *)malloc(sizeof(int)*NDIM);
    NhlRLClear(grlist);
    NhlRLGetIntegerArray(grlist,NhlNpmAnnoManagers,&am_ids,&num_am_ids);
    NhlGetValues(mpid,grlist);

    for( i = 0; i < num_am_ids; i++ ) {
      NhlRLClear(srlist);
      NhlRLSetFloat(srlist,NhlNamDataXF,anno_list[i].lon);
      NhlRLSetFloat(srlist,NhlNamDataYF,anno_list[i].lat);
      NhlSetValues(am_ids[i],srlist);
    }
/*
 * Create a ScalarField object.
 */
    count[0] = NCLS;
    count[1] = NCLS;
    NhlRLClear(srlist);
    NhlRLSetMDFloatArray(srlist,NhlNsfDataArray,&zdat[0][0],2,count);
    NhlRLSetFloat(srlist,NhlNsfMissingValueV,miss_val);
    NhlRLSetFloat(srlist,NhlNsfXCStartV,cminlon);
    NhlRLSetFloat(srlist,NhlNsfXCEndV,  cmaxlon);
    NhlRLSetFloat(srlist,NhlNsfYCStartV,cminlat);
    NhlRLSetFloat(srlist,NhlNsfYCEndV,  cmaxlat);
    NhlCreate(&dataid,"DataItem",NhlscalarFieldClass,appid,srlist);
/*
 * Create ContourPlot object.
 */
    NhlRLClear(srlist);
    NhlRLSetInteger(srlist,NhlNcnScalarFieldData,dataid);
    NhlRLSetString(srlist,NhlNtiMainString,"Satellite view of contour bands in a limited area (using HLUS)");
    NhlCreate(&cnid,"ContourPlot",NhlcontourPlotClass,wid,srlist);
/*
 * Overlay ContourPlot on MapPlot and draw.
 */
    NhlAddOverlay(mpid,cnid,-1);
    NhlDraw(mpid);
/*
 * Retrieve the GraphicStyle object that was created for us when
 * we created the Workstation object.  We can then draw polylines
 * and polymarkers on our MapPlot on behalf of the GraphicStyle object.
 */
    NhlRLClear(grlist);
    NhlRLGetInteger(grlist,NhlNwkDefGraphicStyleId,&gsid);
    NhlGetValues(wid,grlist);
/*
 * Draw circle around our contours.
 */
    NhlDataPolyline(mpid,gsid,clon,clat,NCIRC);
/*
 * Draw a polymarker at the position of Boulder, Colorado (where
 * NCAR is located).
 */
    NhlDataPolymarker(mpid,gsid,&ctrlon,&ctrlat,1);
/*
 * Advance the frame.
 */
    NhlFrame(wid);
/*     
 * Turn off our previous annotations since we are now annotating
 * with LLU calls.
 */
    for( i = 0; i < NDIM; i++ ) {
      NhlRLClear(srlist);
      NhlRLSetString(srlist,NhlNamOn,"false");
      NhlSetValues(am_ids[i],srlist);
    }
/*
 * Draw the map, circle, and polymarker.
 */
    NhlRLClear(srlist);
    NhlRLSetString(srlist,NhlNtiMainString,"Satellite view of contour bands in a limited area (using HLUs/LLUS)");
    NhlSetValues(cnid,srlist);
    NhlDraw(mpid);
    NhlDataPolyline(mpid,gsid,clon,clat,NCIRC);
    NhlDataPolymarker(mpid,gsid,&ctrlon,&ctrlat,1);
/*
 * Now, let's show how we can mix LLUs and HLUs so we can use Plotchar
 * to draw the text in the map projection (which we can't do in the
 * HLUs yet). First we need to get the GKS workstation id so we can
 * activate this workstation at the LLU level.
 */
    NhlRLClear(grlist);
    NhlRLGetInteger(grlist,NhlNwkGksWorkId,&gkswid);
    NhlGetValues(wid,grlist);
    gactivate_ws(gkswid);
/*
 * Here's where the LLU calls come in. We are using Plotchar to
 * draw the text in our map projection.
 */
    c_pcseti ("MAP",4);
    c_pcsetr ("ORV",1.e12);
    NGCALLC(pcmp04,PCMP04).pang = 45.;

    for( i = 0; i < NDIM; i++ ) {
      NGCALLC(pcmp04,PCMP04).plat = anno_list[i].lat;
      NGCALLC(pcmp04,PCMP04).plon = anno_list[i].lon;
      c_plchhq (0.,0.,anno_list[i].name,.5,0.,0.);
    }
    gdeactivate_ws(gkswid);
/*
 * Advance the frame.
 */
    NhlFrame(wid);
/*
 * NhlDestroy destroys the given id and all of its children.
 */
    NhlDestroy(wid);
/*
 * Restores state.
 */
    NhlClose();
    return(0);
}


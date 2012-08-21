/*
*************************************************************************
*                                                                       *
*                Copyright (C)  1996                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*************************************************************************
*
*   File:       vc05c.c
*
*   Author: Lynn Hermanson
*           National Center for Atmospheric Research
*           PO 3000, Boulder, Colorado
* 
*   Date:       April 10, 1996
*
*   Description:     Given a simple mathematically generated data set,
*                    which looks like the intersection of two vector fields
*                    with opposite directions,this program produces three
*                    plot frames which demonstrate the use of VectorPlot*1st-
*                    default resources, 2nd- overlayed on a world map, 3rd-
*                    overlayed on an orthographic projection of the globe
*                    centered on the north pole covering 90 to 40 degrees
*                    north latitude.
*
*  Begin by generating the data set called "increasingvectors".
*  The values in the array "U" are defined to be the magnitude of the x 
*  component of the vectors.  The values in the array "V" are defined to be
*  the magnitude of the y component of the vectors. The location of the 
*  vectors corresponds to the indices of the arrays. Each index pair will
*  eventually correspond to a latitude,longitude location on a map. 
*
*  ********* see resource file vc05.res ******
*/

#include <math.h>
#include <stdio.h>
#include <ncarg/gks.h>
#include <ncarg/ncargC.h>
#include <ncarg/hlu/hlu.h>

/*
 * Include a header file for each object created
 */

#include <ncarg/hlu/App.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/hlu/PDFWorkstation.h>
#include <ncarg/hlu/CairoWorkstation.h>
#include <ncarg/hlu/VectorField.h>
#include <ncarg/hlu/VectorPlot.h>
#include <ncarg/hlu/MapPlot.h>

static const int M=25, N=25;

int main(int argc, char *argv[])
{
    float       U[25][25];
    float       V[25][25];
    ng_size_t   len_dims[2];
    int         appid,wid,dataid,vcid,mapid;
    int         srlist;
    float       x,y;
    int         i,j;
    char const *wks_type = "x11";


for( i= 0;i < 25; i++){
    for( j= 0; j<25; j++) {
        x =  2.5*(i + 2)/1.4142;
        y =  2.5*(j + 2)/1.4142;
        U[j][i] = x;
       V[j][i] = y;
       }
  }
/*
 * Initialize the high level utility library
 */

    NhlInitialize();

/*
; Next,
; create an application context. Set the app dir to the current directory
; so the application looks for a resource file (vc05.res) in the working
; directory. The necessary resources
; will be set in this program as each object is created.

; Setup to choose the type of output workstation for all 3 frames.
; Choose to display output to an X11 workstation. Change the output from the
; default colorMap (random sequence of colors) to a sequence
; of colors so that vectors of increasing magnitude will move from cooler
; to hotter colors.
*/

    srlist = NhlRLCreate(NhlSETRL);
    NhlRLClear(srlist);
    NhlRLSetString(srlist,NhlNappUsrDir,"./");
    NhlCreate(&appid,"vc05",NhlappClass,NhlDEFAULT_APP,srlist);

    if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
/*
 * Create a meta file workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkMetaName,"./vc05c.ncgm");
        NhlCreate(&wid,"vc05Work",
                  NhlncgmWorkstationClass,0,srlist);
    }
    else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 * Create an X workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetInteger(srlist,NhlNwkPause,True);

/*;;;********* see resource file vc05.res ******
;;;	NhlRLSetString(srlist,NhlNwkColorMap, "temp1");
;;;*/

        NhlCreate(&wid,"vc05Work",NhlcairoWindowWorkstationClass,0,srlist);
    }

    else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 * Create an older-style PostScript workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkPSFileName,"./vc05c.ps");
        NhlCreate(&wid,"vc05Work",NhlpsWorkstationClass,0,srlist);
    }
    else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 * Create an older-style PDF workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkPDFFileName,"./vc05c.pdf");
        NhlCreate(&wid,"vc05Work",NhlpdfWorkstationClass,0,srlist);
    }
    else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
             !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 * Create a cairo PS/PDF workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkFileName,"./vc05c");
        NhlRLSetString(srlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"vc05Work",NhlcairoDocumentWorkstationClass,0,srlist);
    }
    else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 * Create a cairo PNG workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkFileName,"./vc05c");
        NhlRLSetString(srlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"vc05Work",NhlcairoImageWorkstationClass,0,srlist);
    }

/*
; BEGIN CREATING 1st FRAME

; Create the 1st VectorField data object using the data set generated above.
*/

    NhlRLClear(srlist);
    len_dims[0] = N;
    len_dims[1] = M;
    NhlRLSetMDFloatArray(srlist,NhlNvfUDataArray,&U[0][0],2,len_dims);
    NhlRLSetMDFloatArray(srlist,NhlNvfVDataArray,&V[0][0],2,len_dims);
    NhlCreate(&dataid,"increasingvectors",NhlvectorFieldClass,appid,
              srlist);
/*
; Create the 1st VectorPlot object, supplying the 1st VectorField object as data.
; Turn off black and white arrows and use the temp1 colors spread between
; the lowest and highest vector magnitudes. The default vectors appear as
; solid lines with arrowheads in a linear coordinate system with the
; origin at the lower left. Tickmarks with labels show the data coordinate
; range, and an informational label at the lower right gives the reference 
; vector value.
*/
    NhlRLClear(srlist);
    NhlRLSetInteger(srlist,NhlNvcVectorFieldData,dataid);

/*;;;********** see resource file vc05.res ******
;;;    NhlRLSetInteger(srlist,NhlNvcMonoLineArrowColor,False);
;;;    NhlRLSetInteger(srlist,NhlNvcMaxLevelCount, 61);
;;;*/

    NhlCreate(&vcid,"VectorPlot1",NhlvectorPlotClass,wid,srlist);

/*; Draw the 1st frame, signify end of 1st frame.*/

NhlDraw(vcid);
NhlFrame(wid);

/*; Destroy the objects created for the 1st frame. 
; Keep wid and appid for 2nd frame.*/

NhlDestroy(dataid);
NhlDestroy(vcid);

/*

; BEGIN CREATING 2nd FRAME

; Create the 2nd VectorField data object using the same data.
; Map the domain and range of the vector locations to the latitude
; and longitude ranges of the world map on which the vectors
; will be overlayed. Thus, cause the x axis locations to cover -180
; to 180 degrees longitude, and cause the y axis locations to cover
; -90 to 90 degrees latitude.
*/ 
       NhlRLClear(srlist);
       NhlRLSetMDFloatArray(srlist,NhlNvfUDataArray,&U[0][0],2,len_dims);
       NhlRLSetMDFloatArray(srlist,NhlNvfVDataArray,&V[0][0],2,len_dims);

/*;;;********* see resource file vc05.res ******   
;;;       NhlRLSetInteger(srlist,NhlNvfXCStartV, -180);
;;;       NhlRLSetInteger(srlist,NhlNvfXCEndV, 180);
;;;       NhlRLSetInteger(srlist,NhlNvfYCStartV, -90);
;;;       NhlRLSetInteger(srlist,NhlNvfYCEndV, 90);
;;;*/

       NhlCreate(&dataid,"increasingvectors2",NhlvectorFieldClass,appid,srlist);

/*
; Create the 2nd VectorPlot object, as in the 1st frame.
; Make the minimum vector length 1/3 of the reference vector length.
*/
        NhlRLClear(srlist);
        NhlRLSetInteger(srlist,NhlNvcVectorFieldData,dataid);

/*;;;********* see resource file vc05.res ******
;;;	NhlRLSetInteger(srlist,NhlNvcMonoLineArrowColor,False);
;;;    NhlRLSetInteger(srlist,NhlNvcMaxLevelCount, 61);
;;;	NhlRLSetFloat(srlist,NhlNvcMinFracLengthF, .33);
;;;*/

        NhlCreate(&vcid,"VectorPlot2",NhlvectorPlotClass,wid,srlist);
  
/*

; Create a mapPlot object consisting of, the default, gridded world map.
; Add a title object which says "vc05 frame 2".
; Turn off the grid lines. 
*/

    NhlRLClear(srlist);
    NhlRLSetString(srlist,NhlNpmTitleDisplayMode,"always");
    NhlRLSetString(srlist,NhlNtiMainString,"vc05 frame 2");

/*;;;********* see resource file vc05.res ******
;;;    NhlRLSetInteger(srlist,NhlNmpGridAndLimbOn, False);
;;;*/

    NhlCreate(&mapid,"Map1",NhlmapPlotClass,wid,srlist);
/*
; Overlay the VectorPlot2 VectorPlot object onto the Map1 MapPlot object.
*/
    NhlAddOverlay(mapid, vcid,-1);
/*
; Draw the 2nd frame. Signify end of 2nd frame.
*/
 
NhlDraw(mapid);
NhlFrame(wid);

/*
; Destroy the objects created for 2nd frame. Keep wid and appid for 3rd frame.
*/

NhlDestroy(dataid);
NhlDestroy(vcid);
NhlDestroy(mapid);

/*
; BEGIN CREATING 3rd FRAME

; Create a data object from the same data.
; Cause the x axis data locations to cover -180 to 180 degrees longitude.
; Cause the y axis data locations to cover 50 degrees latitude up to the
; north pole.
*/

    NhlRLClear(srlist);
    NhlRLSetMDFloatArray(srlist,NhlNvfUDataArray,&U[0][0],2,len_dims);
    NhlRLSetMDFloatArray(srlist,NhlNvfVDataArray,&V[0][0],2,len_dims);

/*;;;********* see resource file vc05.res ******
;;;    NhlRLSetInteger(srlist,NhlNvfXCStartV, -180);
;;;    NhlRLSetInteger(srlist,NhlNvfXCEndV, 180);
;;;    NhlRLSetInteger(srlist,NhlNvfYCStartV, 50);
;;;    NhlRLSetInteger(srlist,NhlNvfYCEndV, 90);
;;;*/

    NhlCreate(&dataid,"increasingvectors3",NhlvectorFieldClass,appid,srlist);
/*
; Create a VectorPlot object, supplying the VectorField object as data,
; as in frame 2. 
; Double the default vector length in NDC. The default was to dynamically
; calulate it by dividing viewport width by the number of data values.
; Display a horizontal label bar (at the bottom of the plot)
; showing the color levels and their corresponding magnitudes.
; Decrease the height of the label bar to .1 NDC so that it fits into the
; default viewPort.
*/

        NhlRLClear(srlist);
        NhlRLSetInteger(srlist,NhlNvcVectorFieldData,dataid);

/*;;;********* see resource file vc05.res ******
;;;        NhlRLSetInteger(srlist,NhlNvcMonoLineArrowColor,False);
;;;        NhlRLSetInteger(srlist,NhlNvcMaxLevelCount, 61);
;;;        NhlRLSetFloat(srlist,NhlNvcMinFracLengthF, .33);  
;;;        NhlRLSetFloat(srlist,NhlNvcRefLengthF, .05);
;;;        NhlRLSetString(srlist,NhlNpmLabelBarDisplayMode, "always");
;;;        NhlRLSetString(srlist,NhlNpmLabelBarSide, "bottom");
;;;        NhlRLSetString(srlist,NhlNlbOrientation, "horizontal");
;;;        NhlRLSetFloat(srlist,NhlNpmLabelBarHeightF, .11);
;;;        NhlRLSetFloat(srlist,NhlNpmLabelBarWidthF, .6);
;;;*/

        NhlCreate(&vcid,"VectorPlot3",NhlvectorPlotClass,wid,srlist);

/*
; Create an orthographic map of the world, centered at the north pole,
; making the points at all four of the frame edges be 50 degrees north
; latitude. Make the center of the top edge of the frame be 180 degrees 
; longitude, the center of the bottom edge of the frame correspond to
; 0 degrees longitude, the center of the left edge of the frame correspond
; to -90 degrees longitude,and the center of right edge of the frame
; correspond to +90 degrees longitude.
; Create a title object which says "vc05 frame 3".
; Turn off the grid lines.
*/
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNpmTitleDisplayMode,"always");
        NhlRLSetString(srlist,NhlNtiMainString,"vc05 frame 3");

/*;;;********* see resource file vc05.res ******
;;;        NhlRLSetString(srlist,NhlNmpProjection, "Orthographic");
;;;        NhlRLSetFloat(srlist,NhlNmpCenterLatF, 90.0); 
;;;        NhlRLSetFloat(srlist,NhlNmpCenterLonF, 0.0); 
;;;        NhlRLSetString(srlist,NhlNmpLimitMode, "points");
;;;        NhlRLSetFloat(srlist,NhlNmpBottomPointLatF, 50);
;;;        NhlRLSetFloat(srlist,NhlNmpTopPointLatF, 50);
;;;        NhlRLSetFloat(srlist,NhlNmpLeftPointLatF, 50);
;;;        NhlRLSetFloat(srlist,NhlNmpRightPointLatF, 50);
;;;        NhlRLSetFloat(srlist,NhlNmpBottomPointLonF, 0);
;;;        NhlRLSetFloat(srlist,NhlNmpTopPointLonF, 180);
;;;        NhlRLSetFloat(srlist,NhlNmpLeftPointLonF, -90);
;;;        NhlRLSetFloat(srlist,NhlNmpRightPointLonF, 90);
;;;        NhlRLSetInteger(srlist,NhlNmpGridAndLimbOn, False);
;;;*/

    NhlCreate(&mapid,"Map2",NhlmapPlotClass,wid,srlist);
/*
; Overlay the VectorPlot3 VectorPlot object onto the Map2 MapPlot object.
*/

NhlAddOverlay(mapid, vcid,-1);

/*
; Draw the 3rd frame. Signify end of 3rd frame.
*/
  
NhlDraw(mapid);
NhlFrame(wid);
/*
; Destroy all of the objects created, close the HLU library and exit.
*/

NhlDestroy(dataid);
NhlDestroy(vcid);
NhlDestroy(mapid);
NhlDestroy(wid);
NhlDestroy(appid);

NhlClose();
exit(0);
}

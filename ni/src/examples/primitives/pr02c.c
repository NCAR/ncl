/*;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1996                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       pr02c.c
;
;   Author: Lynn Hermanson
;           National Center for Atmospheric Research
;           PO 3000, Boulder, Colorado
; 
;   Date:       May 6, 1996
;
;   Description:     Given an array of xaxis values and 
;                    an array of yaxis values, where each 
;                    x,y pair corresponds to one vertex or
;                    the location of one point, draw a
;                    square (a polyline), four points (asterisks)
;                    at the corners of a square (a polymarker), and
;                    a rectangular filled area (a polygon),on to
;                    a blank canvas (an empty default logLinPlot).
;                    
;                    Polylines, polymarkers, and polygons, are
;                    refered to as primitives.
*/

#include <stdio.h>
#include <math.h>
#include <ncarg/gks.h>
#include <ncarg/ncargC.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/hlu/PDFWorkstation.h>
#include <ncarg/hlu/CairoWorkstation.h>
#include <ncarg/hlu/LogLinPlot.h>

int main(int argc, char *argv[])
{
    char const *wks_type = "x11";
    int rlist,grlist;
    int appid,wid,cid,gsid_pl,gsid_pm, gsid_pg;

/*These are the polyline points*/ /*a square*/
    float X[] = { 0.25,0.25,0.45,0.45,0.25 };
    float Y[] = { 0.25,0.45,0.45,0.25,0.25 };

/*These are the polymarker points*/ /*four corner points*/
    float U[] = { 0.35,0.35,0.55,0.55 };
    float V[] = { 0.35,0.55,0.55,0.35 };

/*These are the polygon points*/ /*a filled rectangle*/
    float PX[] = { 0.6,0.7,0.7,0.8,0.6 };
    float PY[] = { 0.2,0.8,0.8,0.2,0.2 };

/*
 * Initialize the high level utility library
 */
    NhlInitialize();

/* Next,
; create an application context. Set the app dir to the current directory
; so the application looks for a resource file in the working directory.
; ***See resource file pr02.res***
*/
   rlist = NhlRLCreate(NhlSETRL);
    grlist = NhlRLCreate(NhlGETRL);
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNappUsrDir,"./");
    NhlCreate(&appid,"pr02",NhlappClass,NhlDEFAULT_APP,rlist);


/*; Choose to display output to an X11 workstation.(set above)*/

    if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
/*
 * Create a meta file workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./pr02c.ncgm");
        NhlCreate(&wid,"pr02Work",
                  NhlncgmWorkstationClass,NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 * Create an X workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlCreate(&wid,"pr02Work",NhlcairoWindowWorkstationClass,appid,rlist);
    }

    else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 * Create an older-style PostScript workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPSFileName,"pr02c.ps");
        NhlCreate(&wid,"pr02Work",NhlpsWorkstationClass,appid,rlist);
    }
    else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 * Create an older-style PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPDFFileName,"pr02c.pdf");
        NhlCreate(&wid,"pr02Work",NhlpdfWorkstationClass,appid,rlist);
    }
    else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
             !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 * Create a cairo PS/PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"pr02c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"pr02Work",NhlcairoDocumentWorkstationClass,appid,rlist);
    }
    else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 * Create a cairo PNG workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"pr02c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"pr02Work",NhlcairoImageWorkstationClass,appid,rlist);
    }

/*; Create a blank plot object to place the primitives on.*/

    NhlRLClear(rlist);
    NhlCreate(&cid,"canvas",NhllogLinPlotClass,wid,rlist);

/*; Create a graphicPlot object consisting of  a thick blue square, large
; red filled circular corner points,and a dashed red triangle
; filled with green stripes.

; The graphicStyle object is NOT a stand-alone plot object. There
; must exist some kind of plot object which has already been drawn
; to place the primitives on.
*/

/* make polyline */

    NhlRLClear(rlist);

/*;;;**** See resource file pr02.res ****
;;;        NhlRLSetInteger(rlist,NhlNgsLineColor, 4);
;;;        NhlRLSetInteger(rlist,NhlNgsLineThicknessF, 3);
;;;*/

    NhlCreate(&gsid_pl,"pr02polyline",NhlgraphicStyleClass,wid,rlist);

/* make polymarker */

    NhlRLClear(rlist);

/*;;;**** See resource file pr02.res ****
;;;        NhlRLSetInteger(rlist,NhlNgsMarkerIndex, 16);
;;;        NhlRLSetFloat(rlist,NhlNgsMarkerSizeF, .02);
;;;        NhlRLSetInteger(rlist,NhlNgsMarkerColor, 2);
;;;*/

    NhlCreate(&gsid_pm,"pr02polymarker",NhlgraphicStyleClass,wid,rlist);

/* make polygon */

    NhlRLClear(rlist);

/*;;;**** See resource file pr02.res ****
;;;        NhlRLSetInteger(rlist,NhlNgsFillIndex, 1);
;;;        NhlRLSetInteger(rlist,NhlNgsFillColor, 3);
;;;        NhlRLSetString(rlist,NhlNgsEdgesOn, "true");
;;;        NhlRLSetInteger(rlist,NhlNgsEdgeColor, 2);
;;;        NhlRLSetInteger(rlist,NhlNgsEdgeThicknessF, 3);
;;;        NhlRLSetInteger(rlist,NhlNgsEdgeDashPattern, 1);
;;;*/

    NhlCreate(&gsid_pg,"pr02polygon",NhlgraphicStyleClass,wid,rlist);

/*; Draw the shapes onto the blank logLinPlot object.
; Signify end of frame.*/

NhlDraw(cid);

/* The last paramenter is the number of points required*/
NhlNDCPolyline(cid, gsid_pl, X, Y,5);
NhlNDCPolymarker(cid, gsid_pm, U, V,4);
NhlNDCPolygon(cid, gsid_pg, PX, PY,5);
NhlFrame(wid);

/*; Destroy all of the objects created, close the HLU library and exit.*/

NhlDestroy(cid);
NhlDestroy(wid);
NhlDestroy(appid);
NhlClose();
exit(0);
}










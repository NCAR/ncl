/*
 *      $Id: pr05c.c,v 1.5 2010-03-15 22:49:24 haley Exp $
 */
/***********************************************************************
*                                                                      *
*                Copyright (C)  1993                                   *
*        University Corporation for Atmospheric Research               *
*                All Rights Reserved                                   *
*                                                                      *
***********************************************************************/
/*
 *  File:       pr05c.c
 *
 *  Author:     David Brown
 *          	National Center for Atmospheric Research
 *          	PO 3000, Boulder, Colorado
 *
 *  Date:       Fri Oct 14 11:42:41 MDT 1994
 *
 *  Description:    Demonstrates graphics primitives drawn in data
 *                  space over a MapPlot. Data polygons are used to
 *                  color global zones and data polylines mark the
 *                  zonal boundaries. Line labels name the boundary
 *                  lines (e.g. arctic circle). Markers are drawn at
 *                  the poles. The three frames present three 
 *                  different map projections, showing how the 
 *                  primitives adapt to various levels of distortion.
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
#include <ncarg/hlu/MapPlot.h>

int main(int argc, char *argv[])

{
    char const *wks_type = "x11";
    int rlist;
    int appid,wid,canvas,gsid;
    int i;
    float px[5];
    float py[5];
    char *projection[] = {"orthographic","mollweide","stereographic"};

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
    NhlCreate(&appid,"pr05",NhlappClass,NhlDEFAULT_APP,rlist);

    if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
/*
 * Create a meta file workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./pr05c.ncgm");
        NhlCreate(&wid,"pr05Work",
                  NhlncgmWorkstationClass,NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 * Create an X workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlCreate(&wid,"pr05Work",NhlcairoWindowWorkstationClass,appid,rlist);
    }

    else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 * Create an older-style PostScript workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPSFileName,"pr05c.ps");
        NhlCreate(&wid,"pr05Work",NhlpsWorkstationClass,appid,rlist);
    }
    else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 * Create an older-style PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPDFFileName,"pr05c.pdf");
        NhlCreate(&wid,"pr05Work",NhlpdfWorkstationClass,appid,rlist);
    }
    else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
             !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 * Create a cairo PS/PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"pr05c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"pr05Work",NhlcairoDocumentWorkstationClass,appid,rlist);
    }
    else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 * Create a cairo PNG workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"pr05c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"pr05Work",NhlcairoImageWorkstationClass,appid,rlist);
    }
/*
 * Create a MapPlot that covers the entire NDC space 
 * to use as a drawing canvas
 */

    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNvpXF,0.0);
    NhlRLSetFloat(rlist,NhlNvpYF,1.0);
    NhlRLSetFloat(rlist,NhlNvpWidthF,1.0);
    NhlRLSetFloat(rlist,NhlNvpHeightF,1.0);
    NhlCreate(&canvas,"canvas",NhlmapPlotClass,wid,rlist);

/*
 * Create a GraphicStyle to control the primitive attributes.
 */

    NhlRLClear(rlist);
    NhlCreate(&gsid,"style",NhlgraphicStyleClass,wid,rlist);

    for (i = 0; i < 3; i++) {

/*
 * Set the map projection
 */
	    NhlRLClear(rlist);
	    NhlRLSetString(rlist,NhlNmpProjection,projection[i]);
	    NhlSetValues(canvas,rlist);
/*
 * Draw polygons representing the 5 major zones of the globe, beginning
 * with the tropical zone.
 * Turn edges off and set the marker color.
 */
	    NhlRLClear(rlist);
	    NhlRLSetInteger(rlist,NhlNgsMarkerColor,0);
	    NhlRLSetString(rlist,NhlNgsEdgesOn,"false");
	    NhlRLSetInteger(rlist,NhlNgsFillColor,2);
	    NhlSetValues(gsid,rlist);

	    py[0] = -23.5;
	    py[1] = 23.5;
	    py[2] = 23.5;
	    py[3] = -23.5;
	    py[4] = -23.5;
	    px[0] = 360.;
	    px[1] = 360.;
	    px[2] = 0.;
	    px[3] = 0.;
	    px[4] = 360.;
	    NhlDataPolygon(canvas,gsid,px,py,5);

/*
 * Next draw the north and south temperate zones
 */
	    NhlRLClear(rlist);
	    NhlRLSetInteger(rlist,NhlNgsFillColor,3);
	    NhlSetValues(gsid,rlist);

	    py[0] = 23.5;
	    py[1] = 66.5;
	    py[2] = 66.5;
	    py[3] = 23.5;
	    py[4] = 23.5;
	    px[0] = 360.;
	    px[1] = 360.;
	    px[2] =  0.;
	    px[3] = 0.;
	    px[4] = 360.;
	    NhlDataPolygon(canvas,gsid,px,py,5);

	    py[0] = -23.5;
	    py[1] = -66.5;
	    py[2] = -66.5;
	    py[3] = -23.5;
	    py[4] = -23.5;
	    px[0] = 360.;
	    px[1] = 360.;
	    px[2] =  0.;
	    px[3] = 0.;
	    px[4] = 360.;
	    NhlDataPolygon(canvas,gsid,px,py,5);
/*
 * Draw the frigid zones
 */
	    NhlRLClear(rlist);
	    NhlRLSetInteger(rlist,NhlNgsFillColor,4);
	    NhlSetValues(gsid,rlist);

	    py[0] = 90.;
	    py[1] = 66.5;
	    py[2] = 66.5;
	    py[3] = 90.;
	    py[4] = 90.;
	    px[0] = 360.;
	    px[1] = 360.;
	    px[2] =  0.;
	    px[3] = 0.;
	    px[4] = 360.;
	    NhlDataPolygon(canvas,gsid,px,py,5);

	    py[0] = -90.;
	    py[1] = -66.5;
	    py[2] = -66.5;
	    py[3] = -90.;
	    py[4] = -90.;
	    px[0] = 360.;
	    px[1] = 360.;
	    px[2] =  0.;
	    px[3] = 0.;
	    px[4] = 360.;
	    NhlDataPolygon(canvas,gsid,px,py,5);
/*
 * Draw the map outlines and grid
 */
	    NhlDraw(canvas);
/*
 * Draw markers at each pole
 */
	    px[0] = 0;
	    px[1] = 0;
	    py[0] = 90;
	    py[1] = -90;
	    NhlDataPolymarker(canvas,gsid,px,py,2);

/*
 * Draw polylines at each of the major latitudinal boundary lines,
 * beginning with the equator. Use the line label to name each of the
 * lines. The '|' character is inserted between each label character 
 * to allow the labels to track the curve of each line more precisely.
 */
	    NhlRLClear(rlist);
	    NhlRLSetString(rlist,NhlNgsLineLabelString,"e|q|u|a|t|o|r");
	    NhlSetValues(gsid,rlist);

	    px[0] = 360;
	    px[1] = 0;
	    py[0] = 0;
	    py[1] = 0;
	    NhlDataPolyline(canvas,gsid,px,py,2);

/*
 * Tropic of cancer
 */
	    NhlRLClear(rlist);
	    NhlRLSetString(rlist,NhlNgsLineLabelString,
			   "t|r|o|p|i|c o|f c|a|n|c|e|r");
	    NhlSetValues(gsid,rlist);

	    px[0] = 360;
	    px[1] = 0;
	    py[0] = 23.5;
	    py[1] = 23.5;
	    NhlDataPolyline(canvas,gsid,px,py,2);
/*
 * Tropic of capricorn (Note: currently there is a limit on the 
 * number of characters in a line label that prevents the '|'
 * character from being used between each letter in a label 
 * of this length).
 */
	    NhlRLClear(rlist);
	    NhlRLSetString(rlist,NhlNgsLineLabelString,
			   "tr|o|p|ic of c|a|p|r|i|c|o|rn");
	    NhlSetValues(gsid,rlist);

	    px[0] = 360;
	    px[1] = 0;
	    py[0] = -23.5;
	    py[1] = -23.5;
	    NhlDataPolyline(canvas,gsid,px,py,2);
/*
 * Arctic circle
 */
	    NhlRLClear(rlist);
	    NhlRLSetString(rlist,NhlNgsLineLabelString,
			   "a|r|c|t|i|c c|i|r|c|l|e");
	    NhlSetValues(gsid,rlist);

	    px[0] = 360;
	    px[1] = 0;
	    py[0] = 66.5;
	    py[1] = 66.5;
	    NhlDataPolyline(canvas,gsid,px,py,2);
/*
 * Antarctic circle
 */
	    NhlRLClear(rlist);
	    NhlRLSetString(rlist,NhlNgsLineLabelString,
			   "|a|n|t|a|r|c|t|i|c c|i|r|c|l|e");
	    NhlSetValues(gsid,rlist);

	    px[0] = 360;
	    px[1] = 0;
	    py[0] = -66.5;
	    py[1] = -66.5;
	    NhlDataPolyline(canvas,gsid,px,py,2);

	    NhlFrame(wid);
    }

    NhlClose();
    exit(0);
}

/*
**      $Id: xy07c.c,v 1.8 2010-03-15 22:49:25 haley Exp $
*/
/************************************************************************
*                                                                       *
*                Copyright (C)  1995                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
************************************************************************/
/*
**  File:       xy07c.c
**
**  Author:     Mary Haley (converted from example "agex11")
**          National Center for Atmospheric Research
**          PO 3000, Boulder, Colorado
**
**  Date:       Wed Jan 4 17:48:02 MST 1995
**
**  Description:   This example is similar to the ncargex Autograph
**                 example "agex11".  It shows how to draw a "scattergram".
**                 It also shows one way on how to modify the color map
**                 so we can get a different background/foreground color.
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

#define NPTS  250
#define pow5(x)  ((x)*(x)*(x)*(x)*(x))

/*
 * Create data arrays for XyPlot object.
 */
float xdra[NPTS], ydra[NPTS];

int main()
{
    int     appid,xworkid,plotid,dataid[2];
    int     rlist, i, j;
    ng_size_t len[2];
    float   x;
    float cmap[4][3];
    char datastr[10];
    extern float fran();
    char const *wks_type = "x11";
/*
 * Initialize the HLU library and set up resource template.
 */
    NhlInitialize();

    rlist = NhlRLCreate(NhlSETRL);
/*
 * Change the color map so we can have a white background, a black
 * foreground and two colors defined for our markers.  Color '0' is
 * the background color and '1' is the foreground color.
 */
    cmap[0][0] = cmap[0][1] = cmap[0][2] = 1.;
    cmap[1][0] = cmap[1][1] = cmap[1][2] = 0.;
    cmap[2][1] = cmap[2][2] = 0.;
    cmap[2][0] = 1.;
    cmap[3][0] = cmap[3][1] = 0.;
    cmap[3][2] = 1.;
    len[0] = 4;  len[1] = 3;
/*
 * Create Application object.  The Application object name is used to
 * determine the name of the resource file, which is "xy07.res" in
 * this case.
 */
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNappDefaultParent,"True");
    NhlRLSetString(rlist,NhlNappUsrDir,"./");
    NhlCreate(&appid,"xy07",NhlappClass,NhlDEFAULT_APP,rlist);

    if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
/*
 * Create an NCGMWorkstation object.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./xy07c.ncgm");
        NhlRLSetMDFloatArray(rlist,NhlNwkColorMap,&cmap[0][0],2,len);
        NhlCreate(&xworkid,"xy07Work",NhlncgmWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 * Create an X11 workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlRLSetMDFloatArray(rlist,NhlNwkColorMap,&cmap[0][0],2,len);
        NhlCreate(&xworkid,"xy07Work",NhlcairoWindowWorkstationClass,
              NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 * Create an older-style PostScript workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPSFileName,"./xy07c.ps");
        NhlRLSetMDFloatArray(rlist,NhlNwkColorMap,&cmap[0][0],2,len);
        NhlCreate(&xworkid,"xy07Work",NhlpsWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 * Create an older-style PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPDFFileName,"./xy07c.pdf");
        NhlRLSetMDFloatArray(rlist,NhlNwkColorMap,&cmap[0][0],2,len);
        NhlCreate(&xworkid,"xy07Work",NhlpdfWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
             !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 * Create a cairo PS/PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"./xy07c");
        NhlRLSetString(rlist,NhlNwkFormat, (char*)wks_type);
        NhlRLSetMDFloatArray(rlist,NhlNwkColorMap,&cmap[0][0],2,len);
        NhlCreate(&xworkid,"xy07Work",NhlcairoDocumentWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 * Create a cairo PNG workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"./xy07c");
        NhlRLSetString(rlist,NhlNwkFormat, (char*)wks_type);
        NhlRLSetMDFloatArray(rlist,NhlNwkColorMap,&cmap[0][0],2,len);
        NhlCreate(&xworkid,"xy07Work",NhlcairoImageWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
/*
 * Since we have two sets of points that we want to color differently,
 * we need to create two Data obects here.
 */

    for( j = 0; j < 2; j++ ) {
/*
 * Initialize data.
 */
        for( i = 0; i < NPTS; i++ ) {
            x = 2.*(fran()-.5);
            xdra[i] = .5 + pow5(x);
            x = 2.*(fran()-.5);
            ydra[i] = .5 + pow5(x);
        }
/*
 * Define a data object.  Note that we are naming each object differently
 * so we can distinguish them in the resource file.
 */
        NhlRLClear(rlist);
        NhlRLSetFloatArray(rlist,NhlNcaXArray,xdra,NhlNumber(xdra));
        NhlRLSetFloatArray(rlist,NhlNcaYArray,ydra,NhlNumber(ydra));
        sprintf( datastr, "xyData%1d", j );
        NhlCreate(&dataid[j],datastr,NhlcoordArraysClass,
                  NhlDEFAULT_APP,rlist);
    }
/*
 * Create the XyPlot object.
 */
    NhlRLClear(rlist);
    NhlRLSetIntegerArray(rlist,NhlNxyCoordData,dataid,NhlNumber(dataid));
    NhlCreate(&plotid,"xyPlot",NhlxyPlotClass,xworkid,rlist);

    NhlRLDestroy(rlist);
/*
 * Draw the plot.
 */
    NhlDraw(plotid);
/*
 * This flushes the buffer and then clears the Workstation.
 */
    NhlFrame(xworkid);
/*
 * NhlDestroy destroys the given id and all of its children
 * so destroying xworkwid will also destroy plotid.
 */
    NhlDestroy(xworkid);
/*
 * Restores state.
 */
    NhlDestroy(appid);
    NhlClose();

    exit(0);
}

float fran()
{
/*
 * Pseudo-random-number generator.
 */
    static double x = 2.718281828459045;
    extern double fmod();
    x = fmod(9821.*x+.211327,1.);
    return((float)x);
}

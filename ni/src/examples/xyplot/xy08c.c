/*
**      $Id: xy08c.c,v 1.2 1995-05-04 19:55:24 haley Exp $
*/
/************************************************************************
*                                                                       *
*                Copyright (C)  1995                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
************************************************************************/
/*
**  File:       xy08c.c
**
**  Author:     Mary Haley (copied from example "agex13")
**          National Center for Atmospheric Research
**          PO 3000, Boulder, Colorado
**
**  Date:       Thu Jan 26 13:56:37 MST 1995
**
**  Description: This example is similar to the ncargex Autograph
**               example "agex13".  It shows how to use Irregular
**               points to change the transformation of your plot.
*/

#include <stdio.h>
#include <math.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/ResList.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/XWorkstation.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/XyPlot.h>
#include <ncarg/hlu/CoordArrTable.h>
#include <ncarg/hlu/TickMark.h>

#define NCOLORS  3
/* 
 * Create data arrays
 */
#define NCURVE  3

float *xcoord[NCURVE];
float *xdra[NCURVE],*ydra[NCURVE];
int len[NCURVE] = {12,37,61};

/*
 * Create array for customizing the plot.
 */
float explicit_values[14];

main()
{
    int     appid,xworkid,plotid,dataid;
    int     rlist, i, j, clen[2];
    float   cmap[NCOLORS][3];
    FILE    *fp;

    int NCGM=0;
/*
 * Fill the data arrays.
 */
    for( i = 0; i < 14; i++ ) explicit_values[i] = pow(2.,(float)(i-6));
    fp = fopen("xy08.asc","r");
    if (fp == (FILE *)NULL) {
        NhlPError(NhlFATAL,NhlEUNKNOWN, "Cannot open data file xy08.asc" );
        exit(3);
    }
    for( i = 0; i < NCURVE; i++ ) {
        xcoord[i] = (float *)malloc(sizeof(float)*(len[i]*2));
        for( j = 0; j < len[i]*2; j++ ) {
            fscanf(fp,"%g",&xcoord[i][j]);
            xcoord[i][j] = pow(2.,((xcoord[i][j]-15.)/2.5));
        }
    }
    for( i = 0; i < NCURVE; i++ ) {
        xdra[i] = (float *)malloc(sizeof(float)*len[i]);
        ydra[i] = (float *)malloc(sizeof(float)*len[i]);
        for( j = 0; j < (len[i]*2)-1; j+=2 ) {
            xdra[i][j/2] = xcoord[i][j];
            ydra[i][j/2] = xcoord[i][j+1];
			printf( "x = %g y = %g\n", xdra[i][j/2], ydra[i][j/2] );
        }
    }
/*
 * Initialize the HLU library and set up resource template
 */
    NhlInitialize();

    rlist = NhlRLCreate(NhlSETRL);
/*
 * Modify the color map.  Color indices '0' and '1' are the background
 * and foreground colors respectively.
 */
    cmap[0][0] = 1.00; cmap[0][1] = 1.00; cmap[0][2] = 1.00;
    cmap[1][0] = 0.00; cmap[1][1] = 0.00; cmap[1][2] = 0.00;
    cmap[2][0] = 0.00; cmap[2][1] = 0.00; cmap[2][2] = 1.00;

    clen[0] = NCOLORS;  clen[1] = 3;
/*
 * Create Application object.  The Application object name is used to
 * determine the name of the resource file, which is "xy08.res" in this
 * case.
 */
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNappDefaultParent,"True");
    NhlRLSetString(rlist,NhlNappUsrDir,"./");
    NhlCreate(&appid,"xy08",NhlappClass,NhlDEFAULT_APP,0);

    if (NCGM) {
/*
 * Create a meta file object.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./xy08c.ncgm");
        NhlRLSetMDFloatArray(rlist,NhlNwkColorMap,&cmap[0][0],2,clen);
        NhlCreate(&xworkid,"xy08Work",NhlncgmWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
    else {
/*
 * Create an XWorkstation object.
 */
        NhlRLClear(rlist);
        NhlRLSetMDFloatArray(rlist,NhlNwkColorMap,&cmap[0][0],2,clen);
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlCreate(&xworkid,"xy08Work",NhlxWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
/*
 * Create a CoordArrTable object to define the data for the XyPlot
 * object. The id array from this object will become the value for the
 * XyPlot resource, "xyCoordData".
 */
    NhlRLClear(rlist);
    NhlRLSetIntegerArray(rlist,NhlNctXTableLengths,len,NCURVE);
    NhlRLSetArray(rlist,NhlNctXTable,xdra,NhlTPointer,sizeof(NhlPointer),
                  NCURVE);
    NhlRLSetIntegerArray(rlist,NhlNctYTableLengths,len,NCURVE);
    NhlRLSetArray(rlist,NhlNctYTable,ydra,NhlTPointer,sizeof(NhlPointer),
                  NCURVE);
    NhlCreate(&dataid,"xyData",NhlcoordArrTableClass,NhlDEFAULT_APP,
              rlist);
/*
 * Create the XyPlot object and customize tick marks.
 */
    NhlRLClear(rlist);
    NhlRLSetFloatArray(rlist,NhlNtmXBValues,explicit_values,
                    NhlNumber(explicit_values));
    NhlRLSetFloatArray(rlist,NhlNtmYLValues,explicit_values,
                    NhlNumber(explicit_values));
    NhlRLSetFloatArray(rlist,NhlNxyXIrregularPoints,explicit_values,
                    NhlNumber(explicit_values));
    NhlRLSetFloatArray(rlist,NhlNxyYIrregularPoints,explicit_values,
                    NhlNumber(explicit_values));
    NhlRLSetInteger(rlist,NhlNxyCoordData,dataid);
    NhlCreate(&plotid,"xyPlot",NhlxyPlotClass,xworkid,rlist);
/*
 * Draw the plot.
 */
    NhlDraw(plotid);
    NhlFrame(xworkid);
/*
 * NhlDestroy destroys the given id and all of its children
 * so destroying xworkwid will also destroys plotid.
 */
    NhlRLDestroy(rlist);
    NhlDestroy(xworkid);
    NhlDestroy(appid);
/*
 * Restores state.
 */
    NhlClose();

    exit(0);
}

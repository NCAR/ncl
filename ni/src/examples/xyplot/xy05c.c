/*
**      $Id: xy05c.c,v 1.5 1995-02-18 00:53:50 boote Exp $
*/
/***********************************************************************
*                                                                      *
*                Copyright (C)  1995                                   *
*        University Corporation for Atmospheric Research               *
*                All Rights Reserved                                   *
*                                                                      *
***********************************************************************/
/*
**  File:       xy05c.c
**
**  Author:     Mary Haley
**          National Center for Atmospheric Research
**          PO 3000, Boulder, Colorado
**
**  Date:       Fri Jan 27 08:24:42 MST 1995
**
** Description:    This example shows how to create an XyPlot object
**                 with multiple lines using the CoordArrays and
**                 multiple Data objects.  Using multiple Data
**                 objects allows you to have a different number of
**                 points in each line.
**
**                 Some of the XyPlot marker resources are tweaked in
**                 the resource file to show how to change the
**                 appearance of these multiple lines.  This example
**                 also shows you how to use the xyYIrregularPoints
**                 resource to define your own Y axis values.
**
**                 The "CoordArrays" object is used to set up the data,
**                 and the "DataDep" object is used to describe
**                 attributes of the data being plotted, like the
**                 marker styles and sizes.
*/

#include <stdio.h>
#include <math.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/ResList.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/XWorkstation.h>
#include <ncarg/hlu/XyPlot.h>
#include <ncarg/hlu/CoordArrays.h>

#define NCURVE  4
#define PI100 .031415926535898
float *ydra[NCURVE];

int len[NCURVE] = {500,200,400,300};

main()
{
    int     appid,xworkid,plotid,dataid[NCURVE],datadepid[NCURVE];
    int     *dspec = datadepid;
    int     num_dspec;
    int     rlist,grlist;
    int     i, j;
    float   theta;
    float   explicit_values[10];
    char    datastr[11];
/*
 * Initialize some data for the XyPlot object.
 */
    for( j = 0; j < NCURVE; j++ ) {
        ydra[j] = (float *)malloc(sizeof(float)*len[j]);
        if (ydra[j] == NULL) {
            NhlPError(NhlFATAL,NhlEUNKNOWN,"Unable to malloc space for ydra array");
            exit(3);
        }
        for( i = 0; i < len[j]; i++ ) {
            theta = PI100*(float)(i);
            ydra[j][i] = (j+1)*100.+.9*(float)(i)*sin(theta);
        }
    }
/*
 * Set up the array of points we want to use for the Y axis.
 */
    for( i = 0; i < 10; i++ ) {
        explicit_values[i] = pow(2.,(float)(i+4));
    }
/*
 * Initialize the HLU library and set up resource template
 */
    NhlInitialize();
    rlist = NhlRLCreate(NhlSETRL);
/*
 * Create Application and XWorkstation objects.  The Application
 * object name is used to determine the name of the resource file,
 * which is "xy05.res" in this case.
 */
    NhlCreate(&appid,"xy05",NhlappLayerClass,NhlDEFAULT_APP,0);
    NhlCreate(&xworkid,"xy05Work",NhlxWorkstationLayerClass,appid,0);
/*
 * Define the Data objects.  Since only the Y values are specified here,
 * each Y value will be paired with its integer array index.  The array of
 * data ids from these objects will become the value for the XyPlot
 * resource "xyCurveData".
 */
    for( i = 0; i < NCURVE; i++ ) {
        NhlRLClear(rlist);
        NhlRLSetFloatArray(rlist,NhlNcaYArray,ydra[i],len[i]);
        sprintf(datastr,"xyData%1d",i+1);
        NhlCreate(&dataid[i],datastr,NhlcoordArraysLayerClass,
                  NhlDEFAULT_APP,rlist);
    }

/*
 * Create the XyPlot object which is created as a child of the
 * XWorkstation object.  The resources that are being changed are done
 * in the "xy05.res" file, and they will affect this XyPlot object.
 */
    NhlRLClear(rlist);
    NhlRLSetIntegerArray(rlist,NhlNxyCoordData,dataid,NhlNumber(dataid));
    NhlRLSetFloatArray(rlist,NhlNxyYIrregularPoints,explicit_values,
                    NhlNumber(explicit_values));
    NhlCreate(&plotid,"XYPlot",NhlxyPlotLayerClass,xworkid,rlist);

/*
 * Tweak XyDataSpec resources...
 */
    grlist = NhlRLCreate(NhlGETRL);
    NhlRLGetIntegerArray(grlist,NhlNxyCoordDataSpec,&dspec,&num_dspec);
    NhlGetValues(plotid,grlist);
    NhlRLDestroy(grlist);

    if(NCURVE != num_dspec){
	NhlPError(NhlFATAL,NhlEUNKNOWN,"Dspecs don't match Data???");
	exit(0);
    }

    NhlRLClear(rlist);
    NhlRLSetInteger(rlist,NhlNxyLineColor,NhlBACKGROUND);
    NhlSetValues(dspec[0],rlist);

    NhlRLClear(rlist);
    NhlRLSetInteger(rlist,NhlNxyLineColor,NhlFOREGROUND);
    NhlSetValues(dspec[1],rlist);

    NhlRLClear(rlist);
    NhlRLSetInteger(rlist,NhlNxyLineColor,100);
    NhlSetValues(dspec[2],rlist);

    NhlRLClear(rlist);
    NhlRLSetInteger(rlist,NhlNxyLineColor,120);
    NhlSetValues(dspec[3],rlist);

/*
 * Draw the plot (to its parent X Workstation)
 */
    NhlDraw(plotid);
    NhlFrame(xworkid);

/*
 * NhlDestroy destroys the given id and all of its children
 * so destroying "appid will destroy "xworkid" which will also destroy "plotid".
 */
    NhlRLDestroy(rlist);
    NhlDestroy(appid);
/*
 * Restores state.
 */
    NhlClose();

    exit(0);
}

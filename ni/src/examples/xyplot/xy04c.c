/*
**      $Id: xy04c.c,v 1.1 1995-02-06 18:07:06 haley Exp $
*/
/************************************************************************
*                                                                       *
*                Copyright (C)  1995                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
************************************************************************/
/*
**  File:       xy04c.c
**
**  Author:     Mary Haley
**          National Center for Atmospheric Research
**          PO 3000, Boulder, Colorado
**
**  Date:       Fri Jan 27 08:24:42 MST 1995
**
**  Description:    This program shows how to create an XY plot object with
**                  multiple lines in the plot.  Some of the XY line resource
**                  are tweaked in the resource file to show how to change the
**                  appearance of these multiple lines.
**
**                  The "CoordArrTable" object is used to set up the data,
**                  and the "DataDep" object is used to describe attributes
**                  of the data being plotted, like the line color and the
**                  dash patterns.
*/

#include <stdio.h>
#include <math.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/ResList.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/XWorkstation.h>
#include <ncarg/hlu/XyPlot.h>
#include <ncarg/hlu/CoordArrTable.h>

#define NPTS  500
#define NCURVE  4
#define PI100 0.031415926535898

float ydra[NCURVE][NPTS];
float *yval[NCURVE];
int leny[NCURVE] = {NPTS,NPTS,NPTS,NPTS};

/*
 * Set up arrays of labels, colors, and dash patterns for each curve.
 */
int colors[NCURVE] = {50, 75, 100, 40};
int xydash[NCURVE] = {0, 5, 10, 15};
char *explabs[NCURVE] ={"Curve 1","Curve 2","Curve 3", "Curve 4"};

main()
{
	int     appid,xworkid,plotid,dataid,datadepid;
	int     rlist;
	int     i, j;
	float   theta;
/*
 * Initialize XY data
 */
	for( j = 0; j < NCURVE; j++ ) {
		for( i = 0; i < NPTS; i++ ) {
			theta = PI100*(float)(i);
			ydra[j][i] = (j+1)*100.+.9*(float)(i)*sin(theta);
		}
		yval[j] = ydra[j];
    }
/*
 * Initialize the HLU library and set up resource template
 */
	NhlInitialize();
	rlist = NhlRLCreate(NhlSETRL);
/*
 * Create application and X workstation object
 */
	NhlCreate(&appid,"xy04",NhlappLayerClass,NhlDEFAULT_APP,0);
	NhlCreate(&xworkid,"xy04Work",NhlxWorkstationLayerClass,appid,0);
/*
 * Define the data object.  Since only the Y values are specified here, each
 * Y value will be paired with its integer array index.  The id for this
 * object will then later be used as the value for the Data Dep resource,
 * "dsDataItem".
 */
	NhlRLClear(rlist);
	NhlRLSetString(rlist,NhlNctYTableType,NhlTFloat);
	NhlRLSetIntegerArray(rlist,NhlNctYTableLengths,leny,NCURVE);
	NhlRLSetArray(rlist,NhlNctYTable,yval,NhlTPointer,sizeof(NhlPointer),
                  NCURVE);
	NhlCreate(&dataid,"XYCoord",NhlcoordArrTableLayerClass,NhlDEFAULT_APP,
              rlist);
/*
 * Define Data Dependent resources.  Here's where you specify the arrays
 * for defining the color, label, and dash pattern of each line, 
 */
	NhlRLClear(rlist);
	NhlRLSetInteger(rlist,NhlNdsDataItem,dataid);
	NhlRLSetIntegerArray(rlist,NhlNxyColors,colors,NhlNumber(colors));
	NhlRLSetIntegerArray(rlist,NhlNxyDashPatterns,xydash,NhlNumber(xydash));
	NhlRLSetStringArray(rlist,NhlNxyExplicitLabels,explabs,NhlNumber(explabs));
	NhlCreate(&datadepid,"XYDep",NhlxyDataDepLayerClass,NhlDEFAULT_APP,rlist);
/*
 * This new Data Dependent object is now the resource value for xyCurveData.
 * Tweak some XYPlot resources as well (in the resource file).
 */
	NhlRLClear(rlist);
	NhlRLSetInteger(rlist,NhlNxyCurveData,datadepid);
	NhlCreate(&plotid,"XYPlot",NhlxyPlotLayerClass,xworkid,rlist);
/*
 * Draw the plot (to its parent X Workstation)
 */
	NhlDraw(plotid);
	NhlFrame(xworkid);
/*
 * NhlDestroy destroys the given id and all of its children
 * so destroying "xworkid" will also destroy plotid.
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

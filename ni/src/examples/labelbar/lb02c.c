
/************************************************************************
*                                                                       *
*                Copyright (C)  1995                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
************************************************************************/
/*
 *      File:           lb02c.c
 *
 *      Author:         Bob Lackman
 *          National Center for Atmospheric Research
 *          PO 3000, Boulder, Colorado
 *
 *      Date:           Fri Jan 13 18:31:18 MDT 1995
 *
 *      Description:    Demonstrates the LabelBar Object
 *                      Creates color bars with every 5th index of the
 *                      114 different colors in the default colormap.
 */
#include <stdio.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/ResList.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/LabelBar.h>
#include <ncarg/hlu/XWorkstation.h>
		
main()
{
	int appid, wid, pid;
	int rlist;
	int colors[22];
	char *line_labels[22];

/*
 * Initialize data values
 */
	colors[0] = 1;
	colors[1] = 6;
	colors[2] = 11;
	colors[3] = 16;
	colors[4] = 21;
	colors[5] = 26;
	colors[6] = 31;
	colors[7] = 36;
	colors[8] = 41;
	colors[9] = 46;
	colors[10] = 51;
	colors[11] = 56;
	colors[12] = 61;
	colors[13] = 66;
	colors[14] = 71;
	colors[15] = 76;
	colors[16] = 81;
	colors[17] = 86;
	colors[18] = 91;
	colors[19] = 96;
	colors[20] = 101;
	colors[21] = 106;
	line_labels[0] = "Color Index 1 ";
	line_labels[1] = "Color Index 6 ";
	line_labels[2] = "Color Index 11";
	line_labels[3] = "Color Index 16";
	line_labels[4] = "Color Index 21";
	line_labels[5] = "Color Index 26";
	line_labels[6] = "Color Index 31";
	line_labels[7] = "Color Index 36";
	line_labels[8] = "Color Index 41";
	line_labels[9] = "Color Index 46";
	line_labels[10] = "Color Index 51";
	line_labels[11] = "Color Index 56";
	line_labels[12] = "Color Index 61";
	line_labels[13] = "Color Index 66";
	line_labels[14] = "Color Index 71";
	line_labels[15] = "Color Index 76";
	line_labels[16] = "Color Index 81";
	line_labels[17] = "Color Index 86";
	line_labels[18] = "Color Index 91";
	line_labels[19] = "Color Index 96";
	line_labels[20] = "Color Index 101";
	line_labels[21] = "Color Index 106";

/*
 * Initialize the high level utility library
 */

	NhlInitialize();

/*
 * Create an application context. Set the app dir to the current directory
 * so the application looks for a resource file in the working directory.
 * In this example the resource file supplies the plot title only.
 */
        rlist = NhlRLCreate(NhlSETRL);
        NhlRLClear(rlist);
	NhlRLSetString(rlist,NhlNappUsrDir,"./");
	NhlCreate(&appid,"lb02",NhlappLayerClass,NhlDEFAULT_APP,rlist);

/*
 * Create an XWorkstation object.
 */
	NhlRLClear(rlist);
	NhlRLSetInteger(rlist,NhlNwkPause,True);
	NhlCreate(&wid,"lb02Work",NhlxWorkstationLayerClass,NhlDEFAULT_APP,
									rlist);
/*
 * Create a plot with 22 color indices (Every 5th one of the default
 * workstation colormap.
 */

	NhlRLClear(rlist);
	NhlRLSetIntegerArray(rlist,NhlNlbFillColors,colors,22);
	NhlRLSetStringArray(rlist,NhlNlbLabelStrings,line_labels,22);
	NhlRLSetFloat(rlist,NhlNvpXF,0.);
	NhlRLSetFloat(rlist,NhlNvpYF,1.);
	NhlRLSetFloat(rlist,NhlNvpWidthF,1.);
	NhlRLSetFloat(rlist,NhlNvpHeightF,1.);
	NhlCreate(&pid,"LabelBar",
		  NhllabelBarLayerClass,wid,rlist);

	NhlDraw(pid);
	NhlFrame(wid);
	NhlDestroy(pid);
	NhlDestroy(wid);
	NhlDestroy(appid);
	NhlClose();
	exit(0);
}

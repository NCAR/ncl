
/************************************************************************
*                                                                       *
*                Copyright (C)  1995                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
************************************************************************/
/*
 *      File:           lg03c.c
 *
 *      Author:         Bob Lackman
 *          National Center for Atmospheric Research
 *          PO 3000, Boulder, Colorado
 *
 *      Date:           Fri Jan 13 18:31:18 MDT 1995
 *
 *      Description:    Demonstrates a Legend of 5 line types.
 */
#include <stdio.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/ResList.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/Legend.h>
#include <ncarg/hlu/XWorkstation.h>
		

main()
{
	int appid, wid, pid;
	int rlist;
	char *labels[5];
	int colors[5];
	int types[5];
	int item_ind[5];
	float item_hgt, lnthik[5];

/*
 * Initialize data values
 */
	labels[0] = "Line_Type_0";
	labels[1] = "Line_Type_1";
	labels[2] = "Line_Type_2";
	labels[3] = "Line_Type_3";
	labels[4] = "Line_Type_4";
	colors[0] = 40;
	colors[1] = 57;
	colors[2] = 65;
	colors[3] = 80;
	colors[4] = 90;
	lnthik[0] = 4.;
	lnthik[1] = 4.;
	lnthik[2] = 4.;
	lnthik[3] = 4.;
	lnthik[4] = 4.;

/*
 * Item types of 0 are lines, 1 are markers
 */

	types[0] = 0;
	types[1] = 0;
	types[2] = 0;
	types[3] = 0;
	types[4] = 0;
	item_ind[0] =  2;
	item_ind[1] =  3;
	item_ind[2] =  4;
	item_ind[3] =  5;
	item_ind[4] =  6;

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
	NhlCreate(&appid,"lg03",NhlappLayerClass,NhlDEFAULT_APP,rlist);

/*
 * Create an XWorkstation object.
 */
	NhlRLClear(rlist);
	NhlRLSetInteger(rlist,NhlNwkPause,True);
	NhlCreate(&wid,"lg03Work",NhlxWorkstationLayerClass,NhlDEFAULT_APP,
									rlist);
/*
 * Specify the viewport extent of the object.
 */

        NhlRLClear(rlist);
	NhlRLSetFloat(rlist,NhlNvpXF,0.);
	NhlRLSetFloat(rlist,NhlNvpYF,1.);
	NhlRLSetFloat(rlist,NhlNvpWidthF,1.);
	NhlRLSetFloat(rlist,NhlNvpHeightF,1.);

/*
 * Specify the line types for the legend.
 */

	NhlRLSetInteger(rlist,NhlNlgItemCount,5);
	NhlRLSetString(rlist,NhlNlgMonoItemType,"False");
	NhlRLSetFloat(rlist,NhlNlgLabelFontHeightF,.03);
	NhlRLSetStringArray(rlist,NhlNlgLabelStrings,labels,5);
	NhlRLSetIntegerArray(rlist,NhlNlgItemTypes,types,5);

/*
 * Set the dashed lines and the line characters to the same colors.
 */

	NhlRLSetIntegerArray(rlist,NhlNlgItemColors,colors,5);
	NhlRLSetIntegerArray(rlist,NhlNlgItemStringColors,colors,5);
	NhlRLSetIntegerArray(rlist,NhlNlgItemIndexes,item_ind,5);
	NhlRLSetString(rlist,NhlNlgMonoItemThickness,"False");
	NhlRLSetFloatArray(rlist,NhlNlgItemThicknesses,lnthik,5);
	NhlRLSetFloat(rlist,NhlNlgItemFontHeightF,.03);
	NhlCreate(&pid,"Legend",
		  NhllegendLayerClass,wid,rlist);

	NhlDraw(pid);
	NhlFrame(wid);
	NhlDestroy(pid);
	NhlDestroy(wid);
	NhlDestroy(appid);
	NhlClose();
	exit(0);
}

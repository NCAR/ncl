
/************************************************************************
*                                                                       *
*                Copyright (C)  1995                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
************************************************************************/
/*
 *      File:           tx04c.c
 *
 *      Author:         Bob Lackman
 *          National Center for Atmospheric Research
 *          PO 3000, Boulder, Colorado
 *
 *      Date:           Fri Jan 06 18:31:18 MDT 1995
 *
 *      Description:    Demonstrates the TextItem Object
 *                      Writes "NCAR Graphics" in a series of
 *                      114 different colors. (The default colormap.)
 */
#include <stdio.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/ResList.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/TextItem.h>
#include <ncarg/hlu/XWorkstation.h>
		
main()
{
	int appid, wid, pid;
	int rlist;
	int M = 114;
	int i;

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
	NhlCreate(&appid,"tx04",NhlappLayerClass,NhlNOPARENT,rlist);

/*
 * Create an XWorkstation object.
 */
	NhlRLClear(rlist);
	NhlRLSetInteger(rlist,NhlNwkPause,True);
	NhlCreate(&wid,"tx04Work",NhlxWorkstationLayerClass,NhlNULL_LAYER,
									rlist);
/*
 * Create 114 plots varying the fill color of the text bounding box
 * to all entries of the default workstation color map.
 */

for( i = 1; i <= M; i++ ) {
        NhlRLClear(rlist);
	NhlRLSetInteger(rlist,NhlNtxBackgroundFillColor,i);

	NhlCreate(&pid,"TextItems",
		  NhltextItemLayerClass,wid,rlist);

	NhlDraw(pid);
	NhlFrame(wid);
}
	NhlDestroy(pid);
	NhlDestroy(wid);
	NhlDestroy(appid);
	NhlClose();
	exit(0);
}

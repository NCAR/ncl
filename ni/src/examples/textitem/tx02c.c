
/************************************************************************
*                                                                       *
*                Copyright (C)  1995                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
************************************************************************/
/*
 *      File:           tx02c.c
 *
 *      Author:         Bob Lackman
 *          National Center for Atmospheric Research
 *          PO 3000, Boulder, Colorado
 *
 *      Date:           Fri Jan 06 18:31:18 MDT 1995
 *
 *      Description:    Demonstrates the TextItem Object
 *                      writing "NCAR Graphics" in a filled
 *                      colored font.  Turn on the bounding box.
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
	NhlCreate(&appid,"tx02",NhlappLayerClass,NhlDEFAULT_APP,rlist);

/*
 * Create an XWorkstation object.
 */
	NhlRLClear(rlist);
	NhlRLSetInteger(rlist,NhlNwkPause,True);
	NhlCreate(&wid,"tx02Work",NhlxWorkstationLayerClass,NhlDEFAULT_APP,
									rlist);
/*
 * Specify the viewport extent of the object.
 */

        NhlRLClear(rlist);
	NhlRLSetFloat(rlist,NhlNvpXF,.2);
	NhlRLSetFloat(rlist,NhlNvpYF,.8);
	NhlRLSetFloat(rlist,NhlNvpWidthF,.6);
	NhlRLSetFloat(rlist,NhlNvpHeightF,.6);

	NhlCreate(&pid,"TextItems",
		  NhltextItemLayerClass,wid,rlist);

	NhlDraw(pid);
	NhlFrame(wid);
	NhlDestroy(pid);
	NhlDestroy(wid);
	NhlDestroy(appid);
	NhlClose();
	exit(0);
}

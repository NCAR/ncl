
/*
 *      $Id: mp02c.c,v 1.4 1995-02-11 07:26:53 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		mp02c.c
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Oct 14 11:42:41 MDT 1994
 *
 *	Description:	Demonstrates individual control of MapPlot areas
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/gks.h>
#include <ncarg/ncargC.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/XWorkstation.h>
#include <ncarg/hlu/MapPlot.h>

main(int argc, char *argv[])
{

	int appid,wid,mapid;
	int rlist,grlist;
/*
 * String arrays for specifying areas
 */

	NhlString fill_specs[] = { "mexico","bolivia","brazil","nicaragua",
				       "cuba","haiti","canada"};

	NhlString outline_specs[] = { "argentina","paraguay","colombia",
					  "us-colorado","us-texas",
					  "us-kentucky" };

	NhlString mask_specs[] = 
		{ "us-colorado","us-texas","us-kentucky",
			  "bolivia","paraguay","nicaragua","oceans" };
/*
 * Initialize the high level utility library
 */

	NhlInitialize();
/*
 * Create an application context. Set the app dir to the current directory
 * so the application looks for a resource file in the working directory.
 * The resource file sets most of the Contour resources that remain fixed
 * throughout the life of the Contour object.
 */
        rlist = NhlRLCreate(NhlSETRL);
        NhlRLClear(rlist);
	NhlRLSetString(rlist,NhlNappUsrDir,"./");
	NhlCreate(&appid,"mp02",NhlappLayerClass,NhlDEFAULT_APP,rlist);

#if NCGM
/*
 * Create a meta file workstation
 */
        rlist = NhlRLCreate(NhlSETRL);
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./mp02c.ncgm");
        NhlCreate(&wid,"mp02Work",
                  NhlncgmWorkstationLayerClass,NhlDEFAULT_APP,rlist);
#else
/*
 * Create an X workstation
 */
        NhlRLClear(rlist);
	NhlCreate(&wid,"mp02Work",NhlxWorkstationLayerClass,appid,rlist);
#endif

/*
 * Create a plot focusing on North and South America;
 * Outlines are on by default; turn fill on.
 * By default the geophysical boundary set is used both for outline and fill.
 */

	NhlRLClear(rlist);
	NhlRLSetString(rlist,NhlNovTitleDisplayMode,"Always");
	NhlRLSetString(rlist,NhlNtiMainString,"mp02c - Frame 1");
	NhlRLSetString(rlist,NhlNmpFillOn,"True");
	NhlRLSetString(rlist,NhlNmpProjection,"Orthographic");
	NhlRLSetFloat(rlist,NhlNmpCenterLatF,10.0);
	NhlRLSetFloat(rlist,NhlNmpCenterLonF,-90.0);
	NhlRLSetFloat(rlist,NhlNmpCenterRotF,45.0);
	NhlRLSetString(rlist,NhlNmpLimitMode,"LatLon");
	NhlRLSetFloat(rlist,NhlNmpMinLatF,-60.0);
	NhlRLSetFloat(rlist,NhlNmpMaxLatF,60.0);
	NhlRLSetFloat(rlist,NhlNmpMinLonF,-135.0);
	NhlRLSetFloat(rlist,NhlNmpMaxLonF,-45.0);
	
/*
 * Highlight selected countries using their "political" color.
 */

	NhlRLSetStringArray(rlist,NhlNmpFillAreaSpecifiers,
			    fill_specs,NhlNumber(fill_specs));

	NhlCreate(&mapid,"Map0",NhlmapPlotLayerClass,wid,rlist);
	NhlDraw(mapid);
	NhlFrame(wid);

/*
 * Individually outline some other countries and some US states.
 */

	NhlRLClear(rlist);  
	NhlRLSetString(rlist,NhlNtiMainString,"mp02c - Frame 2");
	NhlRLSetStringArray(rlist,NhlNmpOutlineSpecifiers,
			    outline_specs,NhlNumber(outline_specs));
	NhlSetValues(mapid,rlist);

	NhlDraw(mapid);
	NhlFrame(wid);
/*
 * Turn off the base geophysical set for outlines and fill, leaving only
 * the specified areas.
 * Also change the specification, 'canada' to 'canada*', 
 * in order to draw all areas belonging to Canada.
 * Note that another color, mpDefaultFillColor, is used for all areas
 * within the map projection that are otherwise not drawn, including the
 * oceans. If you look closely, you will see that the Canadian lakes 
 * are not drawn in the color used in the previous frame for the ocean.
 * The wild card specification, 'canada*', picks up all the lakes of Canada. 
 * Lakes are drawn using mpInlandWaterFillColor, which is, by default, 
 * set to the same color as mpOceanFillColor.
 */

	fill_specs[6] = "canada*";
	NhlRLClear(rlist);  
	NhlRLSetString(rlist,NhlNtiMainString,"mp02c - Frame 3");
	NhlRLSetString(rlist,NhlNmpFillBoundarySets,"NoBoundaries");
	NhlRLSetString(rlist,NhlNmpOutlineBoundarySets,"NoBoundaries");
	NhlRLSetStringArray(rlist,NhlNmpFillAreaSpecifiers,
			    fill_specs,NhlNumber(fill_specs));
	NhlSetValues(mapid,rlist);

	NhlDraw(mapid);
	NhlFrame(wid);
/*
 * You can also specify area groupings using certain predefined 
 * string constants: set 'continents' on to demonstrate.
 * Masking an area is different from not explicitly drawing it. To enable
 * masking you must explicitly turn area masking and then create an area
 * mask specification list containing the name of each area to be masked.
 * There is an order of precedence for fill and masking. Explicitly 
 * named areas take precedence over area groupings, and small areas take 
 * precedence over enclosing larger areas. Otherwise masking takes 
 * precedence over filling.
 * >>> Masking or filling individual US states causes processing time and
 * >>> memory requirements to increase substantially. Hopefully the 
 * >>> performance can be improved before the release.
 */

	fill_specs[0] = "continents";
	fill_specs[1] = "us";
	NhlRLClear(rlist); 
	NhlRLSetString(rlist,NhlNtiMainString,"mp02c - Frame 4");
	NhlRLSetString(rlist,NhlNmpFillBoundarySets,"NoBoundaries");
	NhlRLSetStringArray(rlist,NhlNmpFillAreaSpecifiers,
			    fill_specs,NhlNumber(fill_specs));
	NhlRLSetString(rlist,NhlNmpAreaMaskingOn,"True");
	NhlRLSetStringArray(rlist,NhlNmpMaskAreaSpecifiers,
			    mask_specs,NhlNumber(mask_specs));
	NhlSetValues(mapid,rlist);

	NhlDraw(mapid);
	NhlFrame(wid);

/*
 * Destroy the objects created, close the HLU library and exit.
 */

	NhlDestroy(mapid);
	NhlDestroy(wid);
	NhlDestroy(appid);
	NhlClose();
	exit(0);
}

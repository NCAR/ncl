/*
 * This example demonstrates how to read and manipulate colormaps.
 *
 * The first frame displays the default colormap.
 * The second frame shows how to alter an entry in the colormap.
 * The third frame shows how to create a completely new colormap.
 */

#include <strings.h>

#include <ncarg/hlu/App.h>
#include <ncarg/hlu/ResList.h>
#include <ncarg/hlu/XWorkstation.h>
#include <ncarg/hlu/Workstation.h>
#include <ncarg/hlu/LabelBar.h>
#include <ncarg/hlu/hlu.h>


main()
{
	int i;
	int appid,wks,lbar,rlist,glist;

	char *colorindices[114];

	int num_dims;
	int *len_dims;
	float *cmap;

	float newcmap[114*3];

/*
 * ###########
 * # Frame 1 #
 * ###########
 * Display the default colormap.
 *
 * Initialize labels for the color map entries
 */
	for(i=0; i<114; ++i)
	{
	   colorindices[i] = (char*)malloc(4*sizeof(char));
	   sprintf(colorindices[i],"%d",i);
	}
 
/*
 * Initialize libraries and create a resource list.
 */
        NhlInitialize();
        rlist = NhlRLCreate(NhlSETRL);

        NhlRLClear(rlist);
        NhlCreate(&appid,"appid",NhlappLayerClass,NhlDEFAULT_APP,rlist);
/*
 * Create an XWorkstation object.
 */
        NhlRLClear(rlist);
        NhlCreate(&wks,"wks",NhlxWorkstationLayerClass,NhlDEFAULT_APP,rlist);
/*
 * Create a labelbar object. 
 */
        NhlRLClear(rlist);

	/* Assign the labels */
        NhlRLSetStringArray(rlist,"lbLabelStrings",colorindices,114);

	/* Label every 17th entry */
        NhlRLSetInteger(rlist,"lbLabelStride",17);

	/* Single pattern used for fill */
        NhlRLSetString(rlist,"lbMonoFillPattern","True");

	/* Set fill pattern to solid */
        NhlRLSetString(rlist,"lbFillPattern","SolidFill");

	/* No lines between colors */
        NhlRLSetString(rlist,"lbBoxLinesOn","False");

	/* Display 114 entries */
        NhlRLSetInteger(rlist,"lbBoxCount",114);

	/* Turn off labelbar perimeter */
        NhlRLSetString(rlist,"lbPerimOn","False");

	/* Plot title */
        NhlRLSetString(rlist,"lbTitleString","Default Colormap");

	/* Title font */
        NhlRLSetString(rlist,"lbTitleFont","Helvetica-bold");

	/* Label font */
        NhlRLSetString(rlist,"lbLabelFont","Helvetica");

	/* Set viewport to max size */
        NhlRLSetFloat(rlist,"vpXF",0.0);
        NhlRLSetFloat(rlist,"vpYF",1.0);
        NhlRLSetFloat(rlist,"vpHeightF",1.0);
        NhlRLSetFloat(rlist,"vpWidthF",1.0);

        NhlCreate(&lbar,"lbar",NhllabelBarLayerClass,wks,rlist);

/*
 * Draw and frame the labelbar
 */
	NhlDraw(lbar);
	NhlFrame(wks);

/*
 * ###########
 * # Frame 2 #
 * ###########
 * Alter a single entry in the colormap.
 *
 * Get the current colormap for the workstation pointed to by wks.
 * The colormap is stored in a 3xN variable where N is the length of
 * the colormap.  Each entry in the color map consists of a vector
 * of 3 normalized red-green-blue color values.  Upon return from
 * a call to NhlGetValues, all values are stored in a vector.
 *
 * Note:  At the time of writing this script, most of the NCL functions
 * for color map manipulation were not yet available.  So, the rest
 * of this example may no longer be the most effective and simplest
 * way to change color map entries.
 */
        glist = NhlRLCreate(NhlGETRL);
        NhlRLClear(glist);
	NhlRLGetMDFloatArray(glist,"wkColorMap",&cmap,&num_dims,&len_dims);
        NhlGetValues(wks,glist);
/*
 * Change the first entry in the colormap array to red.
 */
	cmap[0] = 1.0;
	cmap[1] = 0.0;
	cmap[2] = 0.0;
/*
 * Assign the new color map to the workstation object.
 */
        NhlRLClear(rlist);
	NhlRLSetMDFloatArray(rlist,"wkColorMap",cmap,num_dims,len_dims);
	NhlSetValues(wks,rlist);
/*
 * Add a different title.
 */
        NhlRLClear(rlist);
	NhlRLSetString(rlist,"lbTitleString","Entry 0 set to Red");
	NhlSetValues(lbar,rlist);
/*
 * Draw and frame the labelbar.
 */
	NhlDraw(lbar);
	NhlFrame(wks);

/*
 * ###########
 * # Frame 3 #
 * ###########
 * Create and assign a new colormap.
 */

/*
 * Assign new RGB values to each entry of the colormap.
 */
	for(i=0; i<114*3; i+=3)
	{
		newcmap[i] = 1.0-((i/3)/113.0);
		newcmap[i+1] = (i/3)/113.0;
		newcmap[i+2] = (i/3)/113.0;
	}
/*
 * Assign the new color map to the workstation object.
 */
        NhlRLClear(rlist);
	NhlRLSetMDFloatArray(rlist,"wkColorMap",newcmap,num_dims,len_dims);
	NhlSetValues(wks,rlist);
/*
 * Assign a new title.
 */
        NhlRLClear(rlist);
	NhlRLSetString(rlist,"lbTitleString","New colormap");
	NhlSetValues(lbar,rlist);
/*
 * Draw and frame the labelbar
 */
	NhlDraw(lbar);
	NhlFrame(wks);

	NhlDestroy(lbar);
	NhlClose();
	exit (0);
}

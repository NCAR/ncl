/*
 * The first frame in this example demonstrates how to set the view port 
 * for a contour plot.
 * Note: no data is used in this example, so the output appears
 * only as a bounding box with tickmarks.
 *
 * The second frame in this example demonstrates how to produce multiple
 * plots on a single frame.
 */

#include <ncarg/hlu/App.h>
#include <ncarg/hlu/ResList.h>
#include <ncarg/hlu/XWorkstation.h>
#include <ncarg/hlu/Contour.h>
#include <ncarg/hlu/hlu.h>

main()
{
        int appid,wks,con1,rlist;

/*
 * Initialize the graphics libraries and create a resource list that
 * is normally used to assign name/value pairs within objects.  Then
 * clear (empty) this list, and create an application object.  This
 * object manages multiple resource databases used by seperate objects.
 */
        NhlInitialize();
        rlist = NhlRLCreate(NhlSETRL);

        NhlRLClear(rlist);
        NhlCreate(&appid,"basic01",NhlappLayerClass,NhlDEFAULT_APP,rlist);

/*
 * ###########
 * # FRAME 1 #
 * ###########
 * Choose the type of output you want to create.  You may write your
 * output to an NCGM, file, X workstation window, or a PostScript file. 
 * This example writes to an X Workstation.
 */
        NhlRLClear(rlist);
        NhlCreate(&wks,"wks",NhlxWorkstationLayerClass,NhlDEFAULT_APP,rlist);

/*
 * Create a plot object.  In this example, we will create a contour plot.
 *
 * Four view class resources, vpXF, vpYF, vpWidthF, and vpHeightF, are
 * assigned values in the following create call.  The combination of
 * these four resources determines where the plot will display in the
 * output window.  The values of these resources are specified in 
 * Normalized Device Coordinates (NDCs).  In this two-dimensional coordinate 
 * system (0,0) specifies the lower-left corner and (1,1) specifies the 
 * uper-right corner of a plot.
 */
        NhlRLClear(rlist);
	NhlRLSetFloat(rlist,"vpXF",0.05); 
	NhlRLSetFloat(rlist,"vpYF",0.95); 
	NhlRLSetFloat(rlist,"vpWidthF",0.4); 
	NhlRLSetFloat(rlist,"vpHeightF",0.4); 
        NhlCreate(&con1,"con1",NhlcontourLayerClass,wks,rlist);

/*
 * Draw the plot. 
 */
	NhlDraw(con1);

/*
 * The frame call updates and then clears the workstation.
 * Anything written to the workstation after a frame call is made will be
 * drawn in a subsequent frame. 
 */
	NhlFrame(wks);

/*
 * ###########
 * # FRAME 2 #
 * ###########
 *
 * This example demonstrates drawing multiple plots in a single frame.
 *
 * Calling draw again will produce the identical plot that was drawn in the
 * first frame.
 */
	NhlDraw(con1);

/*
 * To add another plot to the same frame, we first need to reset the 
 * viewport resources so that the next plot does not overwrite the first
 * one.  The setvalues expression is used to set resources after an object
 * has already been created.  The first argument, "con1", in the setvalues
 * expression specifes an object id of a plot that was generated earlier
 * with the create call.  This is then followed by a list of resource value
 * pairs that apply to the object.
 */
        NhlRLClear(rlist);
	NhlRLSetFloat(rlist,"vpXF",0.55); 
	NhlRLSetFloat(rlist,"vpYF",0.45); 
	NhlRLSetFloat(rlist,"vpWidthF",0.2); 
	NhlRLSetFloat(rlist,"vpHeightF",0.2); 
        NhlSetValues(con1,rlist);

/*
 * Because of the new viewport resource settings, calling draw produces 
 * a plot in the lower-right quadrant of the frame.
 */
	NhlDraw(con1);

/*
 * Updates and clear the workstation.
 */
	NhlFrame(wks);

/*
 * Clean up (deleting the parent object recursively deletes all of its 
 * children).
 */
	NhlDestroy(con1);
	NhlClose();
	exit (0);
}

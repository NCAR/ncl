/*
 * This example demonstrates how to draw a contour plot using mostly
 * defaults.  Note: no data is used in this example, so the output appears
 * only as a bounding box with tickmarks.
 * 
 * The mininum set of steps needed for creating any plot involve the following:
 *
 *   1. Initialize the graphics libraries
 *   2. Choose the type of output 
 *   3. Create a plot object
 *   4. Draw the plot
 *   5. Call frame
 *   6. Clean up memory
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
 * ##########
 * # STEP 1 #
 * ##########
 * Initialize the graphics libraries and create a resource list that
 * is normally used to assign name/value pairs within objects.  Then
 * clear (empty) this list, and create an application object.  This
 * object manages multiple resource databases used by seperate objects.  
 *
 * The first argument, "&appid", is a variable that identifies the object.
 * The second argument, "basic01", sets the name of the object being created.
 * The third argument, "NhlappLayerClass", identifies the type or class 
 * of the created object. 
 * The fourth argument, "NhlDEFAULT_APP", specifies the id of the objects 
 * parent.  In this case, the object has no parent, so the constant 
 * "NhlDEFAULT_APP" is used. 
 * The fifth argument, "rlist", is the resource list modifiers to be used
 * when creating the object.  In this example, no modifications are made to
 * default values.
 *
 * These steps are always required when writing an HLU application program.
 */

        NhlInitialize();
        rlist = NhlRLCreate(NhlSETRL);

        NhlRLClear(rlist);
        NhlCreate(&appid,"basic01",NhlappLayerClass,NhlDEFAULT_APP,rlist);

/*
 * ##########
 * # STEP 2 #
 * ##########
 * Choose the type of output you want to create.  You may write your
 * output to an NCAR Computer Graphics Metafile (NCGM) and view it later using
 * the NCAR Graphics utilities ctrans or idt.  You may also write your
 * output directly into a window of a workstation running the X Window system
 * (as demonstrated in this example), or you can write your ouput into 
 * a PostScript file.  
 *
 * The first argument, "&wks", is a variable that identifies the object.
 * The second argument, '"wks"', sets the name of the object being created.
 * The third argument, "xWorkstationLayerClass", identifies the type or class 
 * of the object to create.  In this case an X workstation.
 * The fourth argument, "NhlDEFAULT_APP", specifies the id of the objects 
 * parent.  In this case, the object has no parent, so the constant 
 * "NhlDEFAULT_APP" is used. 
 * The fifth argument, "rlist", is the resource list modifiers to be used
 * when creating the object.  In this example, no modifications are made to
 * default values.
 */

        NhlRLClear(rlist);
        NhlCreate(&wks,"wks",NhlxWorkstationLayerClass,NhlDEFAULT_APP,rlist);

/*
 * ##########
 * # STEP 3 #
 * ##########
 * Create a plot object.  In this example, we will create a contour plot,
 * but we could have just as easily created any other type of plot such as
 * an Xy plot, or a Map plot.
 *
 * The first argument, "&con1", is a variable that identifies the object.
 * The second create call argument, '"con1"', sets the name of the object.
 * This is an arbitrary name and does not have to match the variable object
 * identifier used in the first parameter.
 * The third argument, "contourLayerClass", identifies the type or class
 * of the object to create.  In this case, the type is a contour plot. 
 * The third argument, "wks", specifies the id of the object's parent.  By 
 * specifying the id of the X workstation created earlier, the plot will
 * be drawn into an X window.
 * The fifth argument, "rlist", is the resource list modifiers to be used
 * when creating the object.  In this example, no modifications are made to
 * default values.
 */

        NhlRLClear(rlist);
        NhlCreate(&con1,"con1",NhlcontourLayerClass,wks,rlist);

/*
 * ##########
 * # STEP 4 #
 * ##########
 * This step draws the plot into the X workstation window.  The argument to
 * the draw function is the variable name of the object that you want to
 * draw.
 */
	NhlDraw(con1);

/*
 * ##########
 * # STEP 5 #
 * ##########
 * The frame call updates and then clears the workstation.
 */
	NhlFrame(wks);

/*
 * ##########
 * # STEP 6 #
 * ##########
 * This is the final step used for cleanup.  The delete
 * function deletes variables from the NCL and frees the
 * symbol name from the symbol table.  Deleting a parent object
 * automatically deletes all of its children.  The close function
 * is used to tell the HLU library that the programmer is done 
 * using it, and to free up any memory that it can. 
 */
	NhlDestroy(con1);
        NhlClose();
	exit(0);
}

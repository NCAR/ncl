/*
 * $Id: basic04c.c,v 1.3 1995-04-03 04:43:10 haley Exp $
 */
/***********************************************************************
*                                                                      *
*                            Copyright (C)  1995                       *
*                 University Corporation for Atmospheric Research      *
*                            All Rights Reserved                       *
*                                                                      *
************************************************************************
*
*      File:            basic04c.c
*
*      Author:          Tim Scheitlin (converted by Ed Stautler)
*                       National Center for Atmospheric Research
*                       PO 3000, Boulder, Colorado
*
*      Date:            Mon Mar 20 10:43:42 MST 1995
*
*      Description:     This example demonstrates how to select and
*                       change the workstation device for drawing your
*                       output to an NCGM file or an X workstation
*                       window using the following steps.
*
*                       1. Create output workstation objects.
*                       2. Create the data for the plots.
*                       3. Create the contour objects.
*                       4. Draw the contour objects.
*                       5. Call frame.
*                       6. Clean up memory.
*/

#include <ncarg/hlu/App.h>
#include <ncarg/hlu/ResList.h>
#include <ncarg/hlu/XWorkstation.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/ContourPlot.h>
#include <ncarg/hlu/ScalarField.h>
#include <ncarg/hlu/hlu.h>

main ()
{
        int appid,nwks,xwks,ncon,xcon,field1,rlist;

        int data1[5][5] = { {3,4,4,5,5},
                            {2,3,5,5,4},
                            {2,4,5,4,4},
                            {3,4,4,4,3},
                            {3,3,3,3,3} };
        int dims[2] = { 5, 5 };

/*
 * ##########
 * # STEP 1 #
 * ##########
 * Initialize the graphics libraries and create a resource list that
 * is normally used to assign name/value pairs within objects.  Then
 * clear (empty) this list, and create an application object.  This
 * object manages multiple resource databases used by seperate objects.
 */
        NhlInitialize();
        rlist = NhlRLCreate(NhlSETRL);

        NhlRLClear(rlist);
        NhlCreate(&appid,"appid",NhlappLayerClass,NhlDEFAULT_APP,rlist);

/*
 * ##########
 * # STEP 2 #
 * ##########
 * For each type of output you must create a workstation object using create.
 *
 * The first argument, "&xwks", is a variable that identifies the object.
 * The second argument, '"xwks"', to the create call sets the name of the 
 * object being created. The third argument, "NhlxWorkstationLayerClass", or 
 * "NhlncgmWorkstationLayerClass" identifies the type or class of the object 
 * to create. In this case an X workstation or an NCGM workstation.
 * The fourth argument, "NhlDEFAULT_APP", specifies the id of the objects 
 * parent.  In this case, the object has no parent, so the constant 
 * "NhlDEFAULT_APP" is used.  The fifth argument, "rlist", is the resource 
 * list modifiers to be used when creating the object.
 */
        NhlRLClear(rlist);
        NhlCreate(&xwks,"xwks",NhlxWorkstationLayerClass,NhlDEFAULT_APP,rlist);
/*
 * The resource, wkMetaName, lets you specify the name of the output NCGM
 * file.  In this example, it is called basic04.ncgm.  If omitted, the 
 * default name, gmeta,  will be used.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,"wkMetaName","basic04.ncgm");
        NhlCreate(&nwks,"nwks",NhlncgmWorkstationLayerClass,NhlDEFAULT_APP,
                  rlist);
/*
 * Create a scalar field object that will be used as a data set for a
 * contour object.  The sfDataArray resource is used to assign a data
 * array to a scalar field data object.
 */
        NhlRLClear(rlist);
        NhlRLSetMDIntegerArray(rlist,"sfDataArray",&data1[0][0],2,dims);
        NhlCreate(&field1,"field1",NhlscalarFieldLayerClass,
                  NhlDEFAULT_APP,rlist);

/*
 * ##########
 * # STEP 3 #
 * ##########
 * Create the object(s) you want to draw.
 *
 * Create a contour object to draw into the X workstation.
 * Assign data using the cnScalarFieldData resource.
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,"cnScalarFieldData",field1);
        NhlCreate(&xcon,"xcon",NhlcontourPlotLayerClass,xwks,rlist);
/*
 * Create an empty contour object to draw into the ncgm workstation.
 * Assign data using the cnScalarFieldData resource.
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,"cnScalarFieldData",field1);
        NhlCreate(&ncon,"ncon",NhlcontourPlotLayerClass,nwks,rlist);

/*
 * ##########
 * # STEP 4 #
 * ##########
 * Draw the objects
 */
	NhlDraw(xcon);
	NhlDraw(ncon);

/*
 * ##########
 * # STEP 5 #
 * ##########
 * Call frame to update and clear the workstations
 */
	NhlFrame(xwks);
	NhlFrame(nwks);

/*
 * ##########
 * # STEP 6 #
 * ##########
 * Clean up memory.
 */
	NhlDestroy(xwks);
	NhlDestroy(nwks);

	NhlClose();
	exit (0);
}

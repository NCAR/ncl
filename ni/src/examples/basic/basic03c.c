/*
 * $Id: basic03c.c,v 1.11 1996-01-04 16:45:14 haley Exp $
 */
/***********************************************************************
*                                                                      *
*                            Copyright (C)  1995                       *
*                 University Corporation for Atmospheric Research      *
*                            All Rights Reserved                       *
*                                                                      *
************************************************************************
*
*      File:            basic03c.c
*
*      Author:          Tim Scheitlin (converted by Ed Stautler)
*                       National Center for Atmospheric Research
*                       PO 3000, Boulder, Colorado
*
*      Date:            Mon Mar 20 10:43:42 MST 1995
*
*      Description:     This example demonstrates how to:
*                       1. Create a scalar field data object and assign
*                          it to a plot.
*                       2. Set resources using a resource file.
*                       3. Set resources during object creation.
*                       4. Set resources after object creation.
*/

#include <ncarg/hlu/App.h>
#include <ncarg/hlu/ResList.h>
#include <ncarg/hlu/CairoWorkstation.h>
#include <ncarg/hlu/ContourPlot.h>
#include <ncarg/hlu/ScalarField.h>

int main()
{
    int appid1,appid2,wks,wks2,con1,con2,con3,field1,rlist;

    int data1[5][5] = { {3,4,4,5,5}, 
                        {2,3,5,5,4},
                        {2,4,5,4,4},
                        {3,4,4,4,3},
                        {3,3,3,3,3} };

    ng_size_t  dims[2] = { 5, 5 };

/*
 * Initialize the graphics libraries and create a resource list that
 * is normally used to assign name/value pairs within objects.  Then
 * clear (empty) this list, and create an application object.  This
 * object manages multiple resource databases used by separate objects.
 */
    NhlInitialize();
    rlist = NhlRLCreate(NhlSETRL);

    NhlRLClear(rlist);
    NhlCreate(&appid1,"appid1",NhlappClass,NhlDEFAULT_APP,rlist);
/*
 * ###########
 * # FRAME 1 #
 * ###########
 * This frame demonstrates how to create and assign data to a contour plot.
 *
 * Create an X workstation.
 */
    NhlRLClear(rlist);
    NhlRLSetInteger(rlist,NhlNwkPause,True);
    NhlCreate(&wks,"wks",NhlcairoWindowWorkstationClass,NhlDEFAULT_APP,rlist);
/*
 * Create a scalar field object that will be used as a data set for a 
 * contour object.  The sfDataArray resource is used to assign a data
 * array to a scalar field data object.
 */
    NhlRLClear(rlist);
    NhlRLSetMDIntegerArray(rlist,"sfDataArray",&data1[0][0],2,dims);
    NhlCreate(&field1,"field1",NhlscalarFieldClass,
              NhlDEFAULT_APP,rlist);
/*
 * Create a contour plot object and assign the data using the
 * cnScalarFieldData resource.
 */
    NhlRLClear(rlist);
    NhlRLSetInteger(rlist,"cnScalarFieldData",field1);
    NhlCreate(&con1,"con1",NhlcontourPlotClass,wks,rlist);
/*
 * Draw the plot. 
 */
    NhlDraw(con1);
/*
 * Update and clear the workstation.
 */
    NhlFrame(wks);
/*
 * ###########
 * # FRAME 2 #
 * ###########
 * This frame demonstrates how to set resources using a resource file.
 *
 * Resources are read from a resource file called basic03.res because
 * the first argument in the create call is "basic03". This resource file
 * is only read at the time an application object is created.
 * The resource file contains resource assignments that control the
 * characteristics of a plot.
 */
    NhlRLClear(rlist);
    NhlCreate(&appid2,"basic03",NhlappClass,NhlDEFAULT_APP,rlist);
/*
 * Create another workstation window and make it a child of the
 * new application object by using the appid2 variable as the argument
 * for the parent id.  By making this a child of the application
 * object, the resources that are set in the basic03.res resource
 * file will apply to this object and its children.
 */
    NhlRLClear(rlist);
    NhlCreate(&wks2,"wks2",NhlcairoWindowWorkstationClass,appid2,rlist);
/*
 * Create another contour plot object and assign the data.
 * Notice that the parent id is wks2, making the contour object
 * a child of the new workstation.
 */
    NhlRLClear(rlist);
    NhlRLSetInteger(rlist,"cnScalarFieldData",field1);
    NhlCreate(&con2,"con2",NhlcontourPlotClass,wks2,rlist);
/*
 * The contour object is drawn with filled contours because there is
 * a resource in basic03.res that specifies that contour fill is on.
 */
    NhlDraw(con2);
/*
 * Updates and clear the workstation.
 */
    NhlFrame(wks2);
/*
 * ###########
 * # FRAME 3 #
 * ###########
 * This frame demonstrates how resources can be set when an object is
 * created.  
 *
 * A variable length list of resource name/value pairs specifies
 * a resource and its value.  In this example contour line labels are turned
 * off by setting the "cnLineLabelsOn" resource to "False".
 */
    NhlRLClear(rlist);
    NhlRLSetInteger(rlist,"cnScalarFieldData",field1);
    NhlRLSetString(rlist,"cnLineLabelsOn","False");
    NhlCreate(&con3,"con3",NhlcontourPlotClass,wks2,rlist);
/*
 * Draw the contour object.
 */
    NhlDraw (con3);
/*
 * Update and clear the workstation.
 */
    NhlFrame(wks2);
/*
 * ###########
 * # FRAME 4 #
 * ###########
 * This frame demonstrates how to use the setvalues expression to set 
 * resources for an object that has already been created.
 *
 * The setvalues expression is used to assign values to the resources
 * of a object whose id is given as the first argument in the expression.
 * In this example, that argument is "con3."
 *
 * Any resource that is valid for the con3 object can be set in the following
 * expression.  In this example, setting "cnFillOn" to "False" turns 
 * contour fill off.  By default, cnFillOn is "False", but since it
 * is set to "True" in the resource file, we can override that value by
 * using the setvalues expression.  
 */
    NhlRLClear(rlist);
    NhlRLSetString(rlist,"cnFillOn","False");
    NhlSetValues(con3,rlist);
/*
 * Draw the contour object.
 */
    NhlDraw (con3);
/*
 * Update and clear the workstation
 */
    NhlFrame(wks2);
/*
 * Clean up (deleting the parent object recursively deletes all of its 
 * children).
 */
    NhlDestroy(wks);
    NhlDestroy(appid1);
    NhlDestroy(appid2);

    NhlClose();
    exit (0);
}

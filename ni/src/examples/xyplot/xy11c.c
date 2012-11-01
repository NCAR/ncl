/*
 *      $Id: xy11c.c,v 1.8 1995-04-07 10:55:18 boote Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  1993                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
************************************************************************/
/*
 *  File:       xy11c.c
 *
 *  Author:     Jeff W. Boote
 *          National Center for Atmospheric Research
 *          PO 3000, Boulder, Colorado
 *
 *  Date:       Thu Apr 22 15:47:33 MDT 1993
 *
 *  Description:    This example program demonstrates how to display
 *          your graphics to an X window, and then copy it
 *          into a meta file.
 */
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/ResList.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/CairoWorkstation.h>
#include <ncarg/hlu/CoordArrays.h>
#include <ncarg/hlu/XyPlot.h>

/*
 * Create data arrays to create plot
 */

float Temp[]=   {   -13.500000,
            -10.500000,
            -5.500000,
            -3.100000,
            -1.300000,
            -6.700000,
            -12.900000,
            -13.100000,
            -17.700001,
            -16.900000,
            -21.299999,
            -37.099998,
            -40.099998,
            -41.900002,
            -42.099998,
            -62.700001,
            -59.299999,
            -60.700001,
            -52.099998,
            -55.099998,
            -55.900002,
            -63.500000,
            -57.900002,
            -60.500000,
            -58.099998,
            -60.900002,
            -54.900002,
            -57.299999
        };

float Pressure[] =  {   835.000000,
                832.000000,
                827.000000,
                821.000000,
                791.000000,
                693.000000,
                627.000000,
                606.000000,
                560.000000,
                555.000000,
                500.000000,
                383.000000,
                355.000000,
                339.000000,
                314.000000,
                209.000000,
                192.000000,
                143.000000,
                111.000000,
                100.000000,
                95.300003,
                76.400002,
                62.599998,
                58.299999,
                47.299999,
                30.000000,
                27.200001,
                21.500000
            };


int main()
{
    int     appid,xworkid,ncgmwid,plotid,dataid;
    int     rlist;

    /*
     * Init the HLU library
     */
    NhlInitialize();

    rlist = NhlRLCreate(NhlSETRL);

    NhlCreate(&appid,"xy11",NhlappClass,NhlDEFAULT_APP,0);

    /*
     * Create the Workstation objects.
     * (Setting the color information.)
     */
    NhlRLClear(rlist);
    NhlCreate(&xworkid,"xy11xWork",NhlcairoWindowWorkstationClass,
                                   NhlDEFAULT_APP,rlist);

    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNwkMetaName,"xy11c.ncgm");
    NhlCreate(&ncgmwid,"xy11ncgmWork",NhlncgmWorkstationClass,
                            NhlDEFAULT_APP,rlist);

    NhlRLClear(rlist);
    NhlRLSetFloatArray(rlist,NhlNcaXArray,Temp,NhlNumber(Temp));
    NhlRLSetFloatArray(rlist,NhlNcaYArray,Pressure,NhlNumber(Pressure));
    NhlCreate(&dataid,"mydata",NhlcoordArraysClass,NhlDEFAULT_APP,
                                    rlist);


    /*
     * Create the Plot object - notice it is created as a child of
     * the X workstation object.
     */
    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNvpXF,.25);
    NhlRLSetFloat(rlist,NhlNvpYF,.75);
    NhlRLSetFloat(rlist,NhlNvpWidthF,.5);
    NhlRLSetFloat(rlist,NhlNvpHeightF,.5);

    NhlRLSetInteger(rlist,NhlNxyCoordData,dataid);

    NhlRLSetInteger(rlist,NhlNtiMainOn,True);
    NhlRLSetInteger(rlist,NhlNtiXAxisOn,True);
    NhlRLSetInteger(rlist,NhlNtiYAxisOn,True);

    NhlCreate(&plotid,"xy_plot",NhlxyPlotClass,xworkid,rlist);

    NhlRLDestroy(rlist);

    /*
     * Draw the Plot - It draws to its parent X Workstation.
     */
    NhlDraw(plotid);

    /*
     * This flushes the X buffer and then clears the Workstation.
     * NhlNwkPause is set to True so the clear will wait until the
     * user clicks in the Window - ie. this call blocks until that time.
     */
    NhlFrame(xworkid);

    /*
     * This call essentially re-parents plotid to a new parent (ncgmwid).
     * so that the next time plotid is drawn it will draw to its new
     * parent.
     */
    NhlChangeWorkstation(plotid,ncgmwid);

    NhlDraw(plotid);

    /*
     * There is no pause resource for the NcgmWorkstation class so this
     * call just flushes the graphics buffer and advances the meta file
     * to the next frame.
     */
    NhlFrame(ncgmwid);

    /*
     * NhlDestroy destroys the given id and all of its children
     * so destroying ncgmwid will also destroys plotid.
     */
    NhlDestroy(ncgmwid);
    NhlDestroy(xworkid);

    /*
     * Restores state.
     */
    NhlClose();

    exit(0);
}

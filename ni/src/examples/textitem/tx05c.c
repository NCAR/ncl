/*
 *  $Id: tx05c.c,v 1.4 1995-04-07 10:54:40 boote Exp $
 */
/***********************************************************************
*                                                                      *
*                Copyright (C)  1995                                   *
*        University Corporation for Atmospheric Research               *
*                All Rights Reserved                                   *
*                                                                      *
***********************************************************************/
/*
 *      File:           tx05c.c
 *
 *      Author:         Fred Clare
 *                      National Center for Atmospheric Research
 *                      PO 3000, Boulder, Colorado
 *
 *      Date:           Fri Mar  3 12:47:43 MST 1995
 *
 *      Description:    Demonstrates the TextItem object with text having
 *                      various heights and at various angles.
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/ResList.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/TextItem.h>
#include <ncarg/hlu/XWorkstation.h>
#include <ncarg/hlu/NcgmWorkstation.h>

#define MAX(A,B)       (((A) > (B)) ? (A) : (B))

main()
{
    int   wid, pid, rlist;
    float height, angle, dtr=0.017453292519943;
    float bkg_color[] = {1., 1., 1.};
    float x_coord, y_coord;
    int NCGM=0;
/*
 *  Initialize.
 */ 

    NhlOpen();
    rlist = NhlRLCreate(NhlSETRL);

    if (NCGM) {
/*
 * Create a meta file workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./tx05c.ncgm");
        NhlRLSetFloatArray(rlist,NhlNwkBackgroundColor,bkg_color,3);
        NhlCreate(&wid,"tx05Work",NhlncgmWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
    else {
/*
 *  Create an XWorkstation object.
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlRLSetFloatArray(rlist,NhlNwkBackgroundColor,bkg_color,3);
        NhlCreate(&wid,"tx05Work",
                  NhlxWorkstationClass,NhlDEFAULT_APP, rlist);
    }
/*
 *  Create a TextItem object.
 */
    NhlSetColor(wid,1, 0.0, 0.0, 1.0);
    NhlSetColor(wid,2, 0.4, 0.0, 0.4);

    NhlCreate(&pid,"TextItems",NhltextItemClass,wid,0);

/*
 *  Set text font and string.
 */

    NhlRLClear(rlist);
    NhlRLSetInteger(rlist,NhlNtxFont ,22);
    NhlRLSetString(rlist,NhlNtxString, "NCAR");
    NhlSetValues(pid,rlist);

/*
 *  Draw string with various heights and at various angles.
 */

    angle = 0.;
    while (angle < 136.) {
      x_coord = 0.3 + 0.4*cos(dtr*angle);
      y_coord = 0.2 + 0.4*sin(dtr*angle);
      height  = 0.0005*(136.-angle);

      NhlRLClear(rlist);
      NhlRLSetFloat(rlist,NhlNtxAngleF, angle);
      NhlRLSetFloat(rlist,NhlNtxPosXF, x_coord);
      NhlRLSetFloat(rlist,NhlNtxPosYF, y_coord);
      NhlRLSetFloat(rlist,NhlNtxFontHeightF, height);
      NhlSetValues(pid,rlist);
      NhlDraw(pid);

      angle   = angle + MAX(210.*height,1.);
    }

/*
 *  Text strings at specific angles.
 */

    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNtxAngleF, 180.0);
    NhlRLSetInteger(rlist,NhlNtxFont ,22);
    NhlRLSetInteger(rlist,NhlNtxFontColor ,1);
    NhlRLSetFloat(rlist,NhlNtxFontHeightF, 0.04);
    NhlRLSetFloat(rlist,NhlNtxPosXF,  0.25);
    NhlRLSetFloat(rlist,NhlNtxPosYF,  0.34);
    NhlRLSetString(rlist,NhlNtxString, "NCAR");
    NhlSetValues(pid,rlist);
    NhlDraw(pid);

    NhlRLSetInteger(rlist,NhlNtxFontColor ,2);
    NhlRLSetFloat(rlist,NhlNtxFontHeightF, 0.03);
    NhlRLSetFloat(rlist,NhlNtxAngleF, 0.0);
    NhlRLSetFloat(rlist,NhlNtxPosYF,  0.4);
    NhlRLSetString(rlist,NhlNtxString, "180 degrees");
    NhlSetValues(pid,rlist);
    NhlDraw(pid);

    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNtxAngleF, -45.0);
    NhlRLSetInteger(rlist,NhlNtxFont ,22);
    NhlRLSetInteger(rlist,NhlNtxFontColor ,1);
    NhlRLSetFloat(rlist,NhlNtxFontHeightF, 0.04);
    NhlRLSetFloat(rlist,NhlNtxPosXF,  0.7);
    NhlRLSetFloat(rlist,NhlNtxPosYF,  0.6);
    NhlRLSetString(rlist,NhlNtxString, "NCAR");
    NhlSetValues(pid,rlist);
    NhlDraw(pid);

    NhlRLSetInteger(rlist,NhlNtxFontColor ,2);
    NhlRLSetFloat(rlist,NhlNtxAngleF, 0.0);
    NhlRLSetFloat(rlist,NhlNtxFontHeightF, 0.03);
    NhlRLSetFloat(rlist,NhlNtxPosXF,  0.73);
    NhlRLSetFloat(rlist,NhlNtxPosYF,  0.65);
    NhlRLSetInteger(rlist,NhlNtxJust ,NhlCENTERLEFT);
    NhlRLSetString(rlist,NhlNtxString, "-45 degrees");
    NhlSetValues(pid,rlist);
    NhlDraw(pid);

/*
 *  Label the plot.
 */

    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNtxAngleF, 0.0);
    NhlRLSetInteger(rlist,NhlNtxFont ,25);
    NhlRLSetInteger(rlist,NhlNtxJust ,NhlCENTERLEFT);
    NhlRLSetInteger(rlist,NhlNtxFontColor ,2);
    NhlRLSetFloat(rlist,NhlNtxFontHeightF, 0.05);
    NhlRLSetFloat(rlist,NhlNtxPosXF,  0.2);
    NhlRLSetFloat(rlist,NhlNtxPosYF,  0.84);
    NhlRLSetString(rlist,NhlNtxString, "Text heights &");
    NhlSetValues(pid,rlist);
    NhlDraw(pid);

    NhlRLSetFloat(rlist,NhlNtxPosYF, 0.76);
    NhlRLSetString(rlist,NhlNtxString, "Text angles");
    NhlSetValues(pid,rlist);
    NhlDraw(pid);
    
    NhlFrame(wid);

    NhlDestroy(pid);
    NhlDestroy(wid);
    NhlClose();
    exit(0);
}

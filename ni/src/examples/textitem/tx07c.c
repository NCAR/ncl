/*
 *  $Id: tx07c.c,v 1.4 1995-04-07 10:54:44 boote Exp $
 */
/***********************************************************************
*                                                                      *
*                Copyright (C)  1995                                   *
*        University Corporation for Atmospheric Research               *
*                All Rights Reserved                                   *
*                                                                      *
***********************************************************************/
/*
 *      File:           tx07c.c
 *
 *      Author:         Fred Clare
 *                      National Center for Atmospheric Research
 *                      PO 3000, Boulder, Colorado
 *
 *      Date:           Fri Mar  3 12:40:58 MST 1995
 *
 *      Description:    Demonstrates TextItem text spacings and
 *                      aspect ratios.
 */

#include <stdio.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/ResList.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/TextItem.h>
#include <ncarg/hlu/XWorkstation.h>
#include <ncarg/hlu/NcgmWorkstation.h>

main()
{
    int   i, wid, exid, labid, rlist;
    float ypos, aspect;
    float bkg_color[] = {1., 1., 1.}, spacings[] = {0.0, 1.5, 0.6};
    char  label[25];
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
        NhlRLSetString(rlist,NhlNwkMetaName,"./tx07c.ncgm");
        NhlRLSetFloatArray(rlist,NhlNwkBackgroundColor,bkg_color,3);
        NhlCreate(&wid,"tx07Work",NhlncgmWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
    else {
/*
 * Create an XWorkstation object.
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlRLSetFloatArray(rlist,NhlNwkBackgroundColor,bkg_color,3);
        NhlCreate(&wid,"tx07Work",
                  NhlxWorkstationClass,NhlDEFAULT_APP, rlist);
    }
/*
 * Create two TextItem objects.
 */
    NhlSetColor(wid,1,0.0, 0.0, 1.0);
    NhlSetColor(wid,2,0.4, 0.0, 0.4);
    NhlSetColor(wid,3,1.0, 0.0, 0.0);

    NhlCreate(&exid,"Example String",NhltextItemClass,wid,0);
    NhlCreate(&labid,"Label",NhltextItemClass,wid,0);

/*
 *  Set up example string.
 */

    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNtxString, "NCAR Graphics");
    NhlRLSetInteger(rlist,NhlNtxFontColor, 1);
    NhlRLSetInteger(rlist,NhlNtxFont, 25);
    NhlRLSetFloat(rlist,NhlNtxPosXF,  0.5);
    NhlRLSetFloat(rlist,NhlNtxFontHeightF, .05);
    NhlSetValues(exid,rlist);

/*
 *  Set up label string.
 */

    NhlRLClear(rlist);
    NhlRLSetInteger(rlist,NhlNtxFontColor, 2);
    NhlRLSetInteger(rlist,NhlNtxFont, 21);
    NhlRLSetFloat(rlist,NhlNtxPosXF,  0.5);
    NhlRLSetFloat(rlist,NhlNtxFontHeightF, .04);
    NhlSetValues(labid,rlist);

/*
 *  Spacings
 */

    for (i = 0; i < 3; i++) {
      ypos = 0.83 - 0.17 * (float)i;
      NhlRLClear(rlist);
      sprintf(label,"txConstantSpacingF = %3.1f",spacings[i]);
      NhlRLSetString(rlist,NhlNtxString,label);
      NhlRLSetFloat(rlist,NhlNtxPosYF,ypos);
      NhlSetValues(labid,rlist);
      NhlDraw(labid);
  
      NhlRLClear(rlist);
      NhlRLSetFloat(rlist,NhlNtxConstantSpacingF,spacings[i]);
      NhlRLSetFloat(rlist,NhlNtxPosYF,ypos - 0.07);
      NhlSetValues(exid,rlist);
      NhlDraw(exid);
    }

/*
 *  Aspect ratios.
 */

    for (i = 0; i < 2; i++) {
      ypos = 0.32 - 0.17 * (float)i;
      aspect = 1.3 * (float)i + 0.7;
      NhlRLClear(rlist);
      sprintf(label,"txFontAspectF = %3.1f",aspect);
      NhlRLSetString(rlist,NhlNtxString,label);
      NhlRLSetFloat(rlist,NhlNtxPosYF,ypos);
      NhlSetValues(labid,rlist);
      NhlDraw(labid);
  
      NhlRLClear(rlist);
      NhlRLSetFloat(rlist,NhlNtxConstantSpacingF,0.0);
      NhlRLSetFloat(rlist,NhlNtxPosYF,ypos - 0.07);
      NhlRLSetFloat(rlist,NhlNtxFontAspectF,aspect);
      NhlSetValues(exid,rlist);
      NhlDraw(exid);
    }

/*
 *  Plot title.
 */

    NhlRLClear(rlist);
    NhlRLSetInteger(rlist,NhlNtxFontColor, 3);
    NhlRLSetInteger(rlist,NhlNtxFont, 25);
    NhlRLSetFloat(rlist,NhlNtxPosYF,  0.93);
    NhlRLSetFloat(rlist,NhlNtxFontHeightF, .045);
    NhlRLSetString(rlist,NhlNtxString,"Text Spacings and Aspect Ratios");
    NhlSetValues(labid,rlist);
    NhlDraw(labid);

    NhlFrame(wid);

/*
 *  Close things down.
 */

    NhlDestroy(exid);
    NhlDestroy(labid);
    NhlDestroy(wid);
    NhlClose();
    exit(0);
}

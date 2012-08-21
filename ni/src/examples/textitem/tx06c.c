/*
 *  $Id: tx06c.c,v 1.7 2010-03-15 22:49:24 haley Exp $
 */
/***********************************************************************
*                                                                      *
*                Copyright (C)  1995                                   *
*        University Corporation for Atmospheric Research               *
*                All Rights Reserved                                   *
*                                                                      *
***********************************************************************/
/*
 *      File:           tx06c.c
 *
 *      Author:         Fred Clare
 *                      National Center for Atmospheric Research
 *                      PO 3000, Boulder, Colorado
 *
 *      Date:           Fri Mar  3 12:42:08 MST 1995
 *
 *      Description:    Demonstrates TextItem text justifications.
 */

#include <stdio.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/ResList.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/TextItem.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/hlu/PDFWorkstation.h>
#include <ncarg/hlu/CairoWorkstation.h>

int main()
{
    int   wid, pid, rlist;
    char *labels[] = {"Top Left"  ,"Center Left" , "Bottom Left"  ,
                      "Top Center" ,"Center Center", "Bottom Center", 
                      "Top Right"  ,"Center Right" , "Bottom Right"  };
    float char_height = 0.035, Xmarker_height = .03, y_coord;
    float bkg_color[] = {1., 1., 1.};
    int   just;
    NhlBoundingBox  t_box;
    char const *wks_type = "x11";

/*
 *  Initialize and set up application context.
 */ 

    NhlOpen();
    rlist = NhlRLCreate(NhlSETRL);
    NhlRLClear(rlist);

    if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
/*
 * Create a meta file workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./tx06c.ncgm");
        NhlRLSetFloatArray(rlist,NhlNwkBackgroundColor,bkg_color,3);
        NhlCreate(&wid,"tx06Work",NhlncgmWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 * Create an X11 workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlRLSetFloatArray(rlist,NhlNwkBackgroundColor,bkg_color,3);
        NhlCreate(&wid,"tx06Work",
                  NhlcairoWindowWorkstationClass,NhlDEFAULT_APP, rlist);
    }
    else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 * Create an older-style PostScript workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPSFileName,"./tx06c.ps");
        NhlRLSetFloatArray(rlist,NhlNwkBackgroundColor,bkg_color,3);
        NhlCreate(&wid,"tx06Work",NhlpsWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 * Create an older-style PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPDFFileName,"./tx06c.pdf");
        NhlRLSetFloatArray(rlist,NhlNwkBackgroundColor,bkg_color,3);
        NhlCreate(&wid,"tx06Work",NhlpdfWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
             !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 * Create a cairo PS/PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"./tx06c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlRLSetFloatArray(rlist,NhlNwkBackgroundColor,bkg_color,3);
        NhlCreate(&wid,"tx06Work",NhlcairoDocumentWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 * Create a cairo PNG workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"./tx06c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlRLSetFloatArray(rlist,NhlNwkBackgroundColor,bkg_color,3);
        NhlCreate(&wid,"tx06Work",NhlcairoImageWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
/*
 * Create a TextItem object.
 */
    NhlSetColor(wid,1,0.0, 0.0, 1.0);
    NhlSetColor(wid,2,1.0, 0.0, 0.0);
    NhlSetColor(wid,3,0.4, 0.0, 0.4);

    NhlCreate(&pid,"TextItem",NhltextItemClass,wid,0);

/*
 *  Run through the text justifications.
 */

    for (just = 0; just < 9; just++) {
  
/*
 *  Set up and draw a text string.
 */

      NhlRLClear(rlist);
      NhlRLSetString(rlist,NhlNtxString, labels[just]);
      NhlRLSetInteger(rlist,NhlNtxFontColor, 1);
      NhlRLSetInteger(rlist,NhlNtxJust, just);
      NhlRLSetInteger(rlist,NhlNtxFont, 22);
      NhlRLSetFloat(rlist,NhlNtxFontHeightF, char_height);
      NhlRLSetFloat(rlist,NhlNtxPosXF, 0.5);
      y_coord = 0.08*just+0.1;
      if ( (just % 3) == 0) {
        y_coord += 0.45*char_height;
      }
      else if ( (just % 3) == 2) {
        y_coord -= 0.45*char_height;
      }
      NhlRLSetFloat(rlist,NhlNtxPosYF, y_coord);
      NhlSetValues(pid,rlist);
      NhlDraw(pid);

/*
 *  Mark the justification point.
 */

      NhlRLClear(rlist);
      NhlRLSetString(rlist,NhlNtxString, "X");
      NhlRLSetInteger(rlist,NhlNtxJust, NhlCENTERCENTER);
      NhlRLSetInteger(rlist,NhlNtxFont, 22);
      NhlRLSetFloat(rlist,NhlNtxFontHeightF, Xmarker_height);
      NhlRLSetInteger(rlist,NhlNtxFontColor, 2);
      NhlSetValues(pid,rlist);
      NhlDraw(pid);

    }
/*
 *  Label the plot.
 */

    NhlRLClear(rlist);
    NhlRLSetInteger(rlist,NhlNtxFont ,25);
    NhlRLSetInteger(rlist,NhlNtxJust ,NhlCENTERCENTER);
    NhlRLSetInteger(rlist,NhlNtxFontColor ,3);
    NhlRLSetFloat(rlist,NhlNtxFontHeightF, 0.045);
    NhlRLSetFloat(rlist,NhlNtxPosXF,  0.5);
    NhlRLSetFloat(rlist,NhlNtxPosYF,  0.92);
    NhlRLSetString(rlist,NhlNtxString, "Text Justifications");
    NhlSetValues(pid,rlist);
    NhlDraw(pid);

    NhlRLSetFloat(rlist,NhlNtxPosXF, 0.5+0.5*char_height);
    NhlRLSetFloat(rlist,NhlNtxPosYF, 0.84);
    NhlRLSetString(rlist,NhlNtxString, 
        " - Marks the justification point");
    NhlSetValues(pid,rlist);
    NhlDraw(pid);

    NhlGetBB(pid, &t_box);
    NhlRLSetInteger(rlist,NhlNtxFont ,22);
    NhlRLSetInteger(rlist,NhlNtxFontColor ,2);
    NhlRLSetFloat(rlist,NhlNtxFontHeightF, Xmarker_height);
    NhlRLSetFloat(rlist,NhlNtxPosXF,  t_box.l);
    NhlRLSetFloat(rlist,NhlNtxPosYF,  0.837);
    NhlRLSetInteger(rlist,NhlNtxJust ,NhlCENTERRIGHT);
    NhlRLSetString(rlist,NhlNtxString, "X");
    NhlSetValues(pid,rlist);
    NhlDraw(pid);

    NhlFrame(wid);

    NhlDestroy(pid);
    NhlDestroy(wid);
    NhlClose();
    exit(0);
}

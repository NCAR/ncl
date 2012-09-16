/***********************************************************************
*                                                                      *
*                Copyright (C)  1995                                   *
*        University Corporation for Atmospheric Research               *
*                All Rights Reserved                                   *
*                                                                      *
***********************************************************************/
/*
**  File:       tx04c.c
**
**  Author:     Jeff Boote
**          National Center for Atmospheric Research
**          PO 3000, Boulder, Colorado
**
**  Date:       Tue Jan 24 10:44:51 MST 1995
**
**  Description:         Demonstrates TextItem object.
*/

#include <stdio.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/TextItem.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/hlu/PDFWorkstation.h>
#include <ncarg/hlu/CairoWorkstation.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

int main()
{
    int app_id;
    int text_item_id;
    int workstation_id;
    int i,rlist;
    char const *wks_type = "x11";
/*
 * Initialize the high level utility library
 */

    NhlInitialize();
/*
 * Create an application context.  Set the app dir to the current
 * directory so the application looks for the resource file in the
 * execution directory.
 */
    rlist = NhlRLCreate(NhlSETRL);
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNappDefaultParent,"True");
    NhlRLSetString(rlist,NhlNappUsrDir,"./");
    NhlCreate(&app_id,"tx04",NhlappClass,NhlDEFAULT_APP,rlist);
    
    if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
/*
 * Create a meta file workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./tx04c.ncgm");
        NhlCreate(&workstation_id,"tx04Work",
                  NhlncgmWorkstationClass,NhlDEFAULT_APP,rlist); 
    }
    else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 *  Create an X11 workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlCreate(&workstation_id,"tx04Work",
                  NhlcairoWindowWorkstationClass,NhlDEFAULT_APP, rlist);
    }
    else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 * Create an older-style PostScript workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPSFileName,"./tx04c.ps");
        NhlCreate(&workstation_id,"tx04Work",
                  NhlpsWorkstationClass,NhlDEFAULT_APP,rlist); 
    }
    else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 * Create an older-style PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPDFFileName,"./tx04c.pdf");
        NhlCreate(&workstation_id,"tx04Work",
                  NhlpdfWorkstationClass,NhlDEFAULT_APP,rlist); 
    }
    else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
             !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 * Create a cairo PS/PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"./tx04c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&workstation_id,"tx04Work",
                  NhlcairoDocumentWorkstationClass,NhlDEFAULT_APP,rlist); 
    }
    else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 * Create a cairo PNG workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"./tx04c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&workstation_id,"tx04Work",
                  NhlcairoImageWorkstationClass,NhlDEFAULT_APP,rlist); 
    }
/*
 * This is the only creation of a text object for this entire program.
 */
    NhlRLClear(rlist);
    NhlRLSetInteger(rlist,NhlNtxJust,4);
    NhlRLSetInteger(rlist,NhlNtxFont,25);
    NhlRLSetFloat(rlist,NhlNtxFontHeightF,.03);
    NhlRLSetString(rlist,NhlNtxString,
                   "TextItem - VARIOUS CAPABILITIES");
    NhlRLSetFloat(rlist,NhlNtxPosXF,.5);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.95);
    NhlCreate(&text_item_id,"tx04TextItem",NhltextItemClass,
          workstation_id,rlist);

    NhlDraw(text_item_id);

    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNtxFontHeightF,.015);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.7);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.86);
    NhlRLSetString(rlist,NhlNtxString,
               "HIGH-QUALITY CHARACTERS USED BELOW");
    NhlSetValues(text_item_id,rlist);

    NhlDraw(text_item_id);

    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.7);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.5);    
    NhlRLSetString(rlist,NhlNtxString,
               "MEDIUM-QUALITY CHARACTERS USED BELOW");
    NhlSetValues(text_item_id,rlist);

    NhlDraw(text_item_id);

    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.58);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.81);   
    NhlRLSetString(rlist,NhlNtxString,"NhlNtxString");
    NhlSetValues(text_item_id,rlist);

    NhlDraw(text_item_id);

    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.46);
    NhlSetValues(text_item_id,rlist);

    NhlDraw(text_item_id);

    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.82);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.81);        
    NhlRLSetString(rlist,NhlNtxString,"RESULT");
    NhlSetValues(text_item_id,rlist);

    NhlDraw(text_item_id);

    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.46);
    NhlSetValues(text_item_id,rlist);

    NhlDraw(text_item_id);

    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.82);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.78);
    NhlRLSetString(rlist,NhlNtxString,"------");
    NhlSetValues(text_item_id,rlist);

    NhlDraw(text_item_id);

    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.43);
    NhlSetValues(text_item_id,rlist);

    NhlDraw(text_item_id);

    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.58);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.78);
    NhlRLSetString(rlist,NhlNtxString,"------------");
    NhlSetValues(text_item_id,rlist);

    NhlDraw(text_item_id);

    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.43);
    NhlSetValues(text_item_id,rlist);

    NhlDraw(text_item_id);
/*
 * Below are examples using the function control characters. In each
 * iteration of the loop a new string is set using call NhlSetValues.
 * In order to demonstrate what the value of 'txString' is, the
 * function code is temporarily changed causing the entire contents of
 * 'txString' to be drawn.
 */

    for(i = 0; i< 12; i++) {
        switch(i) {
        case 0:
            NhlRLClear(rlist);
            NhlRLSetString(rlist,NhlNtxString,"~L~A");
            NhlSetValues(text_item_id,rlist);
            break;
        case 1:
            NhlRLClear(rlist);
            NhlRLSetString(rlist,NhlNtxString,"~IGL~A");
            NhlSetValues(text_item_id,rlist);
            break;
        case 2:
            NhlRLClear(rlist);
            NhlRLSetString(rlist,NhlNtxString,
                       "A~S~2~N~+B~S~2~N~");
            NhlSetValues(text_item_id,rlist);
            break;
        case 3:
            NhlRLClear(rlist);
            NhlRLSetString(rlist,NhlNtxString,"A~S~B");
            NhlSetValues(text_item_id,rlist);
            break;
        case 4:
            NhlRLClear(rlist);
            NhlRLSetString(rlist,NhlNtxString,"A~SPU~B");
            NhlSetValues(text_item_id,rlist);
            break;
        case 5:
            NhlRLClear(rlist);
            NhlRLSetString(rlist,NhlNtxString,"~GIU~+");
            NhlSetValues(text_item_id,rlist);
            break;
        case 6:
            NhlRLClear(rlist);
            NhlRLSetString(rlist,NhlNtxString,"~1045~");
            NhlSetValues(text_item_id,rlist);
            break;
        case 7:
            NhlRLClear(rlist);
            NhlRLSetString(rlist,NhlNtxString,"10~S~10~S~100");
            NhlSetValues(text_item_id,rlist);
            break;
        case 8:
            NhlRLClear(rlist);
            NhlRLSetString(rlist,NhlNtxString,"X~B1~2~S1~3");
            NhlSetValues(text_item_id,rlist);
            break;
        case 9:
            NhlRLClear(rlist);
            NhlRLSetString(rlist,NhlNtxString,
                       "X~B1~2~S~3~N~Y~S~2");
            NhlSetValues(text_item_id,rlist);
            break;
        case 10:
            NhlRLClear(rlist);
            NhlRLSetString(rlist,NhlNtxString,
                       "X~S~A~B~1~NN~ABC");
            NhlSetValues(text_item_id,rlist);
            break;
        case 11:
            NhlRLClear(rlist);
            NhlRLSetString(rlist,NhlNtxString,
                       "1.3648~L1~410~S~-13");
            NhlSetValues(text_item_id,rlist);
            break;
        }
/*
 * Change function code so full string will be drawn
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNtxFuncCode,'$');
        NhlRLSetFloat(rlist,NhlNtxFontHeightF,.01);
        NhlRLSetFloat(rlist,NhlNtxPosXF,.58);
        NhlRLSetFloat(rlist,NhlNtxPosYF, .78 - (i+1) * .02);
        NhlSetValues(text_item_id,rlist);

        NhlDraw(text_item_id);
/*
 * Change function code back so result string will be drawn
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNtxFuncCode,'~');
        NhlRLSetFloat(rlist,NhlNtxPosXF,.82);
        NhlSetValues(text_item_id,rlist);

        NhlDraw(text_item_id);
/*
 * Change to medium font quality and set function code so full string
 * will be drawn.
 */     
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNtxFontQuality, NhlMEDIUM);
        NhlRLSetInteger(rlist,NhlNtxFuncCode,'$');
        NhlRLSetFloat(rlist,NhlNtxPosXF,.58);
        NhlRLSetFloat(rlist,NhlNtxPosYF, .42 - (i+1) * .02);
        NhlSetValues(text_item_id,rlist);

        NhlDraw(text_item_id);
/*
 * Change function code back to get result string to draw
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNtxFuncCode,'~');
        NhlRLSetFloat(rlist,NhlNtxPosXF,.82);
        NhlSetValues(text_item_id,rlist);

        NhlDraw(text_item_id);

        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNtxFontQuality,NhlHIGH);
        NhlSetValues(text_item_id,rlist);
    }
/* 
 * Examples of setting font height.
 */
    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.25);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.86);
    NhlRLSetFloat(rlist,NhlNtxFontHeightF,.01);
    NhlRLSetString(rlist,NhlNtxString,"NhlNtxFontHeightF = .01");
    NhlSetValues(text_item_id,rlist);

    NhlDraw(text_item_id);
    
    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.25);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.84);
    NhlRLSetFloat(rlist,NhlNtxFontHeightF,.015);
    NhlRLSetString(rlist,NhlNtxString,"NhlNtxFontHeightF = .015");
    NhlSetValues(text_item_id,rlist);

    NhlDraw(text_item_id);

    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.25);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.80);
    NhlRLSetFloat(rlist,NhlNtxFontHeightF,.02);
    NhlRLSetString(rlist,NhlNtxString,"NhlNtxFontHeightF = .02");
    NhlSetValues(text_item_id,rlist);

    NhlDraw(text_item_id);

    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.25);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.75);
    NhlRLSetFloat(rlist,NhlNtxFontHeightF,.025);
    NhlRLSetString(rlist,NhlNtxString,"NhlNtxFontHeightF = .025");
    NhlSetValues(text_item_id,rlist);

    NhlDraw(text_item_id);

/*
 * Examples setting the rotation angle of the text
 */
    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.26);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.47);
    NhlRLSetFloat(rlist,NhlNtxFontHeightF,.015);
    NhlRLSetInteger(rlist,NhlNtxJust,1);
    NhlRLSetString(rlist,NhlNtxString,"NhlNtxAngleF = 0.0");
    NhlRLSetFloat(rlist,NhlNtxAngleF,0.0);
    NhlSetValues(text_item_id,rlist);

    NhlDraw(text_item_id);

    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.26);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.5);
    NhlRLSetString(rlist,NhlNtxString,"NhlNtxAngleF = 45.0");
    NhlRLSetFloat(rlist,NhlNtxAngleF,45.0);
    NhlSetValues(text_item_id,rlist);

    NhlDraw(text_item_id);

    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.23);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.50);
    NhlRLSetString(rlist,NhlNtxString,"NhlNtxAngleF = 90.0");
    NhlRLSetFloat(rlist,NhlNtxAngleF,90.0);
    NhlSetValues(text_item_id,rlist);

    NhlDraw(text_item_id);

    NhlRLClear(rlist);

    NhlRLSetFloat(rlist,NhlNtxPosXF,.20);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.5);
    NhlRLSetString(rlist,NhlNtxString,"NhlNtxAngleF = 135.0");
    NhlRLSetFloat(rlist,NhlNtxAngleF,135.0);
    NhlSetValues(text_item_id,rlist);

    NhlDraw(text_item_id);

    NhlRLClear(rlist);

    NhlRLSetFloat(rlist,NhlNtxPosXF,.20);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.47);
    NhlRLSetString(rlist,NhlNtxString,"NhlNtxAngleF = 180.0");
    NhlRLSetFloat(rlist,NhlNtxAngleF,180.0);
    NhlSetValues(text_item_id,rlist);

    NhlDraw(text_item_id);

    NhlRLClear(rlist);

    NhlRLSetFloat(rlist,NhlNtxPosXF,.20);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.44);
    NhlRLSetString(rlist,NhlNtxString,"NhlNtxAngleF = 225.0");
    NhlRLSetFloat(rlist,NhlNtxAngleF,225.0);
    NhlSetValues(text_item_id,rlist);

    NhlDraw(text_item_id);

    NhlRLClear(rlist);

    NhlRLSetFloat(rlist,NhlNtxPosXF,.23);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.44);
    NhlRLSetString(rlist,NhlNtxString,"NhlNtxAngleF = 270.0");
    NhlRLSetFloat(rlist,NhlNtxAngleF,270.0);
    NhlSetValues(text_item_id,rlist);

    NhlDraw(text_item_id);

    NhlRLClear(rlist);

    NhlRLSetFloat(rlist,NhlNtxPosXF,.26);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.44);
    NhlRLSetString(rlist,NhlNtxString,"NhlNtxAngleF = 315.0");
    NhlRLSetFloat(rlist,NhlNtxAngleF,315.0);
    NhlSetValues(text_item_id,rlist);

    NhlDraw(text_item_id);

    NhlRLClear(rlist);

    NhlRLSetFloat(rlist,NhlNtxAngleF,0.0);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.25);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.15);
    NhlRLSetInteger(rlist,NhlNtxJust, 4);
    NhlRLSetString(rlist,NhlNtxString,"NhltxJust = 4");
    NhlSetValues(text_item_id,rlist);

    NhlDraw(text_item_id);

    NhlRLClear(rlist);

    NhlRLSetFloat(rlist,NhlNtxAngleF,0.0);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.25);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.18);
    NhlRLSetInteger(rlist,NhlNtxJust, 1);
    NhlRLSetString(rlist,NhlNtxString,"NhltxJust = 1");
    NhlSetValues(text_item_id,rlist);

    NhlDraw(text_item_id);

    NhlRLClear(rlist);

    NhlRLSetFloat(rlist,NhlNtxAngleF,0.0);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.25);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.12);
    NhlRLSetInteger(rlist,NhlNtxJust, 7);
    NhlRLSetString(rlist,NhlNtxString,"NhlNtxJust = 7");
    NhlSetValues(text_item_id,rlist);

    NhlDraw(text_item_id);

/*
 * End of first frame
 */
    NhlFrame(workstation_id);

/*
* Remainder of calls demonstrate the various fonts available for use
* with the text item. Note that the NhlNtxString values demonstrate a
* mid-string font change
*/
    i =0;

    NhlRLClear(rlist);

    NhlRLSetInteger(rlist,NhlNtxFuncCode,'$');
    NhlRLSetInteger(rlist,NhlNtxFont, 0);
    NhlRLSetInteger(rlist,NhlNtxJust, 4);
    NhlRLSetFloat(rlist,NhlNtxFontHeightF,.02);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.5);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.95 - (i++) * .027);
    NhlRLSetString(rlist,NhlNtxString,
               "pwritx database : $F21$NhlNtxFont = 0");
    NhlSetValues(text_item_id,rlist);


    NhlDraw(text_item_id);

    NhlRLClear(rlist);

    NhlRLSetInteger(rlist,NhlNtxFont, 1);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.5);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.95 - (i++) * .027);
    NhlRLSetString(rlist,NhlNtxString,"default : $F21$NhlNtxFont = 1");
    NhlSetValues(text_item_id,rlist);

    NhlDraw(text_item_id);

    NhlRLClear(rlist);

    NhlRLSetInteger(rlist,NhlNtxFont, 2);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.5);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.95 - (i++) * .027);
    NhlRLSetString(rlist,NhlNtxString,
               "cartographic roman : $F21$NhlNtxFont = 2");
    NhlSetValues(text_item_id,rlist);


    NhlDraw(text_item_id);
    NhlRLClear(rlist);

    NhlRLSetInteger(rlist,NhlNtxFont, 3);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.5);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.95 - (i++) * .027);
    NhlRLSetString(rlist,NhlNtxString,
               "cartographic greek : $F21$NhlNtxFont = 3");
    NhlSetValues(text_item_id,rlist);


    NhlDraw(text_item_id);

    NhlRLClear(rlist);

    NhlRLSetInteger(rlist,NhlNtxFont, 4);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.5);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.95 - (i++) * .027);
    NhlRLSetString(rlist,NhlNtxString,
               "simplex roman: $F21$NhlNtxFont = 4");
    NhlSetValues(text_item_id,rlist);


    NhlDraw(text_item_id);

    NhlRLClear(rlist);

    NhlRLSetInteger(rlist,NhlNtxFont, 5);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.5);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.95 - (i++) * .027);
    NhlRLSetString(rlist,NhlNtxString,
               "simplex greek: $F21$NhlNtxFont = 5");
    NhlSetValues(text_item_id,rlist);


    NhlDraw(text_item_id);

    NhlRLClear(rlist);
    
    NhlRLSetInteger(rlist,NhlNtxFont, 6);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.5);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.95 - (i++) * .027);
    NhlRLSetString(rlist,NhlNtxString,
               "simplex script: $F21$NhlNtxFont = 6");
    NhlSetValues(text_item_id,rlist);


    NhlDraw(text_item_id);

    NhlRLClear(rlist);

    NhlRLSetInteger(rlist,NhlNtxFont, 7);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.5);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.95 - (i++) * .027);
    NhlRLSetString(rlist,NhlNtxString,
               "complex roman: $F21$NhlNtxFont = 7");
    NhlSetValues(text_item_id,rlist);


    NhlDraw(text_item_id);

    NhlRLClear(rlist);

    NhlRLSetInteger(rlist,NhlNtxFont, 8);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.5);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.95 - (i++) * .027);
    NhlRLSetString(rlist,NhlNtxString,
                   "complex greek: $F21$NhlNtxFont = 8");
    NhlSetValues(text_item_id,rlist);


    NhlDraw(text_item_id);

    NhlRLClear(rlist);

    NhlRLSetInteger(rlist,NhlNtxFont, 9);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.5);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.95 - (i++) * .027);
    NhlRLSetString(rlist,NhlNtxString,
               "complex script: $F21$NhlNtxFont = 9");
    NhlSetValues(text_item_id,rlist);


    NhlDraw(text_item_id);

    NhlRLClear(rlist);

    NhlRLSetInteger(rlist,NhlNtxFont, 10);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.5);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.95 - (i++) * .027);
    NhlRLSetString(rlist,NhlNtxString,
               "complex script: $F21$NhlNtxFont = 10");
    NhlSetValues(text_item_id,rlist);


    NhlDraw(text_item_id);

    NhlRLClear(rlist);

    NhlRLSetInteger(rlist,NhlNtxFont, 11);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.5);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.95 - (i++) * .027);
    NhlRLSetString(rlist,NhlNtxString,
               "complex cyrillic: $F21$NhlNtxFont = 11");
    NhlSetValues(text_item_id,rlist);


    NhlDraw(text_item_id);

    NhlRLClear(rlist);

    NhlRLSetInteger(rlist,NhlNtxFont, 12);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.5);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.95 - (i++) * .027);
    NhlRLSetString(rlist,NhlNtxString,
               "duplex roman: $F21$NhlNtxFont = 12");
    NhlSetValues(text_item_id,rlist);


    NhlDraw(text_item_id);

    NhlRLClear(rlist);

    NhlRLSetInteger(rlist,NhlNtxFont, 13);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.5);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.95 - (i++) * .027);
    NhlRLSetString(rlist,NhlNtxString,
               "triplex roman: $F21$NhlNtxFont = 13");
    NhlSetValues(text_item_id,rlist);


    NhlDraw(text_item_id);

    NhlRLClear(rlist);

    NhlRLSetInteger(rlist,NhlNtxFont, 14);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.5);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.95 - (i++) * .027);
    NhlRLSetString(rlist,NhlNtxString,
               "triplex italic: $F21$NhlNtxFont = 14");
    NhlSetValues(text_item_id,rlist);


    NhlDraw(text_item_id);

    NhlRLClear(rlist);

    NhlRLSetInteger(rlist,NhlNtxFont, 15);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.5);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.95 - (i++) * .027);
    NhlRLSetString(rlist,NhlNtxString,
               "gothic german: $F21$NhlNtxFont = 15");
    NhlSetValues(text_item_id,rlist);


    NhlDraw(text_item_id);

    NhlRLClear(rlist);

    NhlRLSetInteger(rlist,NhlNtxFont, 16);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.5);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.95 - (i++) * .027);
    NhlRLSetString(rlist,NhlNtxString,
               "gothic english: $F21$NhlNtxFont = 16");
    NhlSetValues(text_item_id,rlist);


    NhlDraw(text_item_id);

    NhlRLClear(rlist);

    NhlRLSetInteger(rlist,NhlNtxFont, 17);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.5);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.95 - (i++) * .027);
    NhlRLSetString(rlist,NhlNtxString,
               "gothic italian: $F21$NhlNtxFont = 17");
    NhlSetValues(text_item_id,rlist);


    NhlDraw(text_item_id);

    NhlRLClear(rlist);

    NhlRLSetInteger(rlist,NhlNtxFont, 18);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.5);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.95 - (i++) * .027);
    NhlRLSetString(rlist,NhlNtxString,
               "math symbols: $F21$NhlNtxFont = 18");
    NhlSetValues(text_item_id,rlist);


    NhlDraw(text_item_id);

    NhlRLClear(rlist);

    NhlRLSetInteger(rlist,NhlNtxFont, 19);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.5);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.95 - (i++) * .027);
    NhlRLSetString(rlist,NhlNtxString,
               "symbols set1: $F21$NhlNtxFont = 19");
    NhlSetValues(text_item_id,rlist);


    NhlDraw(text_item_id);

    NhlRLClear(rlist);

    NhlRLSetInteger(rlist,NhlNtxFont, 20);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.5);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.95 - (i++) * .027);
    NhlRLSetString(rlist,NhlNtxString,
               "symbols set2: $F21$NhlNtxFont = 20");
    NhlSetValues(text_item_id,rlist);


    NhlDraw(text_item_id);

    NhlRLClear(rlist);

    NhlRLSetInteger(rlist,NhlNtxFont, 21);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.5);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.95 - (i++) * .027);
    NhlRLSetString(rlist,NhlNtxString,
                   "helvetica: $F21$NhlNtxFont = 21");
    NhlSetValues(text_item_id,rlist);


    NhlDraw(text_item_id);

    NhlRLClear(rlist);

    NhlRLSetInteger(rlist,NhlNtxFont, 22);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.5);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.95 - (i++) * .027);
    NhlRLSetString(rlist,NhlNtxString,
               "helvetica bold: $F21$NhlNtxFont = 22");
    NhlSetValues(text_item_id,rlist);


    NhlDraw(text_item_id);

    NhlRLClear(rlist);

    NhlRLSetInteger(rlist,NhlNtxFont, 25);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.5);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.95 - (i++) * .027);
    NhlRLSetString(rlist,NhlNtxString,
               "times-roman : $F21$NhlNtxFont = 25");
    NhlSetValues(text_item_id,rlist);


    NhlDraw(text_item_id);

    NhlRLClear(rlist);

    NhlRLSetInteger(rlist,NhlNtxFont, 26);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.5);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.95 - (i++) * .027);
    NhlRLSetString(rlist,NhlNtxString,
               "times-roman bold : $F21$NhlNtxFont = 26");
    NhlSetValues(text_item_id,rlist);


    NhlDraw(text_item_id);

    NhlRLClear(rlist);

    NhlRLSetInteger(rlist,NhlNtxFont, 29);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.5);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.95 - (i++) * .027);
    NhlRLSetString(rlist,NhlNtxString,"courier : $F21$NhlNtxFont = 29");
    NhlSetValues(text_item_id,rlist);


    NhlDraw(text_item_id);

    NhlRLClear(rlist);

    NhlRLSetInteger(rlist,NhlNtxFont, 30);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.5);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.95 - (i++) * .027);
    NhlRLSetString(rlist,NhlNtxString,
               "courier bold : $F21$NhlNtxFont = 30");
    NhlSetValues(text_item_id,rlist);


    NhlDraw(text_item_id);

    NhlRLClear(rlist);

    NhlRLSetInteger(rlist,NhlNtxFont, 33);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.5);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.95 - (i++) * .027);
    NhlRLSetString(rlist,NhlNtxString,"greek : $F21$NhlNtxFont = 33");
    NhlSetValues(text_item_id,rlist);


    NhlDraw(text_item_id);

    NhlRLClear(rlist);

    NhlRLSetInteger(rlist,NhlNtxFont, 34);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.5);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.95 - (i++) * .027);
    NhlRLSetString(rlist,NhlNtxString,
               "math-symbols : $F21$NhlNtxFont = 34");
    NhlSetValues(text_item_id,rlist);


    NhlDraw(text_item_id);

    NhlRLClear(rlist);

    NhlRLSetInteger(rlist,NhlNtxFont, 35);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.5);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.95 - (i++) * .027);
    NhlRLSetString(rlist,
               NhlNtxString,"text-symbols : $F21$NhlNtxFont = 35");
    NhlSetValues(text_item_id,rlist);


    NhlDraw(text_item_id);

    NhlRLClear(rlist);

    NhlRLSetInteger(rlist,NhlNtxFont, 36);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.5);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.95 - (i++) * .027);
    NhlRLSetString(rlist,NhlNtxString,"weather1 : $F21$NhlNtxFont = 36");
    NhlSetValues(text_item_id,rlist);


    NhlDraw(text_item_id);

    NhlRLClear(rlist);
    NhlRLSetInteger(rlist,NhlNtxFont, 37);
    NhlRLSetFloat(rlist,NhlNtxPosXF,.5);
    NhlRLSetFloat(rlist,NhlNtxPosYF,.95 - (i++) * .027);
    NhlRLSetString(rlist,NhlNtxString,"weather2 : $F21$NhlNtxFont = 37");
    NhlSetValues(text_item_id,rlist);

    NhlDraw(text_item_id);
/*
 * End of second and final frame
 */
    NhlFrame(workstation_id);

/*
 * clean up and close hlu library
 */

    NhlDestroy(text_item_id);
    NhlDestroy(workstation_id);
    NhlDestroy(app_id);
    NhlClose();
    exit(0);
}

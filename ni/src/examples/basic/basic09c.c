/***********************************************************************
 *                                                                     *
 *                             Copyright (C)  1996                     *
 *               University Corporation for Atmospheric Research       *
 *                             All Rights Reserved                     *
 *                                                                     *
 ***********************************************************************
 *    
 *   File:         basic09c.c
 *
 *   Author:       Mary Haley
 *                 National Center for Atmospheric Research
 *                 PO 3000, Boulder, Colorado  80303
 *
 *   Date:         Wed Jul 10 08:49:48 MDT 1996
 *
 *   Description: This example displays all the available fonts
 *
 */

#include <stdio.h>

#include <ncarg/hlu/App.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/hlu/XWorkstation.h>
#include <ncarg/hlu/TextItem.h>

/*
 * 'min' function
 */
#define min(x,y)  ((x) < (y) ? (x) : (y))

/*
 *  List of available fonts (by number and name)
 */

char *fonts[31][2] = {{"1","default"},
        {"2","cartographic_roman"},
        {"3","cartographic_greek"},
        {"4","simplex_roman"},
        {"5","simplex_greek"},
        {"6","simplex_script"},
        {"7","complex_roman"},
        {"8","complex_greek"},
        {"9","complex_script"},
        {"10","complex_italic"},
        {"11","complex_cyrillic"},
        {"12","duplex_roman"},
        {"13","triplex_roman"},
        {"14","triplex_italic"},
        {"15","gothic_german"},
        {"16","gothic_english"},
        {"17","gothic_italian"},
        {"18","math_symbols"},
        {"19","symbol_set1"},
        {"20","symbol_set2"},
        {"21","helvetica"},
        {"22","helvetica-bold"},
        {"25","times-roman"},
        {"26","times-bold"},
        {"29","courier"},
        {"30","courier-bold"},
        {"33","greek"},
        {"34","math-symbols"},
        {"35","text-symbols"},
        {"36","weather1"},
        {"37","weather2"}};

main()
{

    int appid,wid,txid1,txid2,txid3,txid4,rlist;
    int num_fonts = 31, num_lines = 18;
    int i, j, div, mod;
    float k;
    char string[50];
/*
 * Set the display. Default is to display output to an X workstation.
 */
    int NCGM=0, X11=1, PS=0;
/*
 * Initialize the high level utility library and create application.
 */
    NhlInitialize();

    rlist = NhlRLCreate(NhlSETRL);
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNappUsrDir,"./");
    NhlCreate(&appid,"basic09",NhlappClass,NhlDEFAULT_APP,rlist);


    if (NCGM) {
/*
 * Create a metafile workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./basic09c.ncgm");
        NhlCreate(&wid,"wks",NhlncgmWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
    
    if (X11) {
/*
 * Create an X workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlCreate(&wid,"wks",NhlxWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
    if (PS) {
/*
 * Create a PS workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPSFileName,"./basic09c.ps");
        NhlCreate(&wid,"wks",NhlpsWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
/*
 * We only want num_lins lines on a page
 */
    div = num_fonts / num_lines;
    mod = num_fonts % num_lines;
    if (mod) {
        div++;
    }
/* 
 * Create header to put on all frames
 */
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNtxFuncCode,"*");
    NhlRLSetFloat(rlist,NhlNtxPosXF,0.32);
    NhlRLSetFloat(rlist,NhlNtxPosYF,0.98);
    NhlRLSetFloat(rlist,NhlNtxFontHeightF,0.02);
    NhlRLSetInteger(rlist,NhlNtxFontColor,3);
    NhlRLSetString(rlist,NhlNtxString,"font number : font name");
    NhlCreate(&txid1,"TextItem1",NhltextItemClass,wid,rlist);

    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNtxPosXF,0.72);
    NhlRLSetFloat(rlist,NhlNtxPosYF,0.98);
    NhlRLSetFloat(rlist,NhlNtxFontHeightF,0.02);
    NhlRLSetInteger(rlist,NhlNtxFontColor,5);
    NhlRLSetString(rlist,NhlNtxString,"font example");
    NhlCreate(&txid2,"TextItem2",NhltextItemClass,wid,rlist);
    for( j = 1; j <= div; j++ ) {
        NhlDraw(txid1);
        NhlDraw(txid2);
        k = 0.90;
/*
 * Loop over each font
 */
        for( i=(j-1)*num_lines; i <= min(num_fonts-1,j*num_lines-1); i++ ) {
            sprintf( string, "%s : %s", fonts[i][0], fonts[i][1] );
/*
 * Font number and name
 */
            NhlRLClear(rlist);
            NhlRLSetInteger(rlist,NhlNtxJust,2);
            NhlRLSetString(rlist,NhlNtxFuncCode,"*");
            NhlRLSetFloat(rlist,NhlNtxPosXF,0.1);
            NhlRLSetFloat(rlist,NhlNtxPosYF,k);
            NhlRLSetFloat(rlist,NhlNtxFontHeightF,0.02);
            NhlRLSetInteger(rlist,NhlNtxFontColor,3);
            NhlRLSetString(rlist,NhlNtxString,string);
            NhlCreate(&txid3,"TextItem3",NhltextItemClass,wid,rlist);
/*
 * Actual font drawn using the words 'NCAR Graphics'
 */
            NhlRLClear(rlist);
            NhlRLSetInteger(rlist,NhlNtxJust,2);
            NhlRLSetString(rlist,NhlNtxFont,fonts[i][1] );
            NhlRLSetFloat(rlist,NhlNtxPosXF,0.6);
            NhlRLSetFloat(rlist,NhlNtxPosYF,k);
            NhlRLSetFloat(rlist,NhlNtxFontHeightF,0.02);
            NhlRLSetInteger(rlist,NhlNtxFontColor,5);
            NhlRLSetString(rlist,NhlNtxString,"NCAR Graphics");
            NhlCreate(&txid4,"TextItem4",NhltextItemClass,wid,rlist);

            NhlDraw(txid3);
            NhlDraw(txid4);
            NhlDestroy(txid3);
            NhlDestroy(txid4);
            k -= 0.05;
        }
        NhlFrame(wid);
    }
/*
 * Clean up.
 */
    NhlDestroy(wid);
    NhlClose();
}

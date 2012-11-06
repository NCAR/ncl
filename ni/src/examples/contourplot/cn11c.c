/*
 *      $Id: cn11c.c,v 1.4 2010-03-15 22:49:23 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  1993                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
************************************************************************/
/*
 *  File:       cn11c.c
 *
 *  Author:     Ethan Alpert
 *          National Center for Atmospheric Research
 *          PO 3000, Boulder, Colorado
 *
 *  Date:       Fri Apr 23 14:19:18 MDT 1993
 *
 *  Description:    Demonstrates how to combine LLU and HLU calls
 */
#include <stdio.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/TickMark.h>
#include <ncarg/hlu/Title.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/hlu/PDFWorkstation.h>
#include <ncarg/hlu/CairoWorkstation.h>
#include <ncarg/gks.h>
#include <ncarg/ncargC.h>

#include "cn11c.h"

/*
* Define label strings for EXPLICIT mode tick mark placement
*/
char *labels[] = { "90:S:o:N:S", "60:S:o:N:S", "30:S:o:N:S", "EQ", 
            "30:S:o:N:N", "60:S:o:N:N", "90:S:o:N:N" };
/*
* Specify data locations for above labels
*/
float labellocs[] = { -90.0, -60.0, -30.0, 0.0, 30.0, 60.0, 90.0 };
        
int main()
{
    int appid, wid, pid, pid1, gkswid;
    float rwrk[5000];
    int iwrk[1000];
    NhlBoundingBox thebox;
    int rlist, grlist;
    char const *wks_type = "x11";
/*
 * Initialize the high level utility library
 */

    NhlInitialize();

/*
 * Create an application context. Set the app dir to the current directory
 * so the application looks for a resource file in the working directory.
 * In this example the resource file supplies the plot title only.
 */
    rlist = NhlRLCreate(NhlSETRL);
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNappUsrDir,"./");
    NhlCreate(&appid,"cn11",NhlappClass,NhlDEFAULT_APP,rlist);

    if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
/*
 * Create a meta file workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./cn11c.ncgm");
        NhlCreate(&wid,"cn11Work",
                  NhlncgmWorkstationClass,NhlDEFAULT_APP,rlist); 
    }
    else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 * Create an X workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlCreate(&wid,"cn11Work",
                  NhlcairoWindowWorkstationClass,NhlDEFAULT_APP,rlist);
    }
    else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 * Create an older-style PostScript workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPSFileName,"./cn11c.ps");
        NhlCreate(&wid,"cn11Work",
                  NhlpsWorkstationClass,NhlDEFAULT_APP,rlist); 
    }
    else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 * Create an older-style PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPDFFileName,"./cn11c.pdf");
        NhlCreate(&wid,"cn11Work",
                  NhlpdfWorkstationClass,NhlDEFAULT_APP,rlist); 
    }
    else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
             !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 * Create a cairo PS/PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"./cn11c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"cn11Work",
                  NhlcairoDocumentWorkstationClass,NhlDEFAULT_APP,rlist); 
    }
    else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 * Create a cairo PNG workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkFileName,"./cn11c");
        NhlRLSetString(rlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"cn11Work",
                  NhlcairoImageWorkstationClass,NhlDEFAULT_APP,rlist); 
    }

/*
* Retrieve GKS workstation id from the workstation object
*/
    grlist = NhlRLCreate(NhlGETRL);
    NhlRLClear(grlist);
    NhlRLGetInteger(grlist,NhlNwkGksWorkId,&gkswid);
    NhlGetValues(wid,grlist);
/* 
* The following are calls to the low level NCAR graphics library to
* draw labeled contour lines
*/
    gactivate_ws(gkswid);
    c_cpseti("SET",0);
    c_cpsetr("XC1",-90.0);
    c_cpsetr("XCM",90.0);
    c_cpsetr("YC1",0.0);
    c_cpsetr("YCN",9.0);
    c_cpsetr("CIS",5.0);
    c_cpsetr("DPS",.02);
    c_cpseti("LIS",2);
/*
* Note that Y axis user coordinates are set to grid coordinates rather than
* data coordinates. This is done because the input data is irregularly spaced
* in the Y direction. Basicly this "tricks" CONPACK into thinking the grid
* points are evenly spaced.
*/ 
    c_set(0.2,0.8,0.2,0.8,-90.0,90.0,0.0,9.0,1);
    c_cprect(T,73,73,10,rwrk,5000,iwrk,1000);
    c_cpcldr(T,rwrk,iwrk);
    gdeactivate_ws(gkswid);
/*
* End of NCAR graphics section. Note deactivation of GKS workstation
*/

        NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNvpXF,.2);
    NhlRLSetFloat(rlist,NhlNvpYF,.8);
    NhlRLSetFloat(rlist,NhlNvpWidthF,.6);
    NhlRLSetFloat(rlist,NhlNvpHeightF,.6);
    NhlRLSetFloat(rlist,NhlNtmYLDataTopF,100.0);
    NhlRLSetFloat(rlist,NhlNtmYLDataBottomF,1000.0);
    NhlRLSetFloat(rlist,NhlNtmXBDataRightF,90.0);
    NhlRLSetFloat(rlist,NhlNtmXBDataLeftF,-90.0);
    NhlRLSetInteger(rlist,NhlNtmYLStyle,NhlIRREGULAR);
    NhlRLSetInteger(rlist,NhlNtmXBMode,NhlEXPLICIT);
    NhlRLSetInteger(rlist,NhlNtmXBMinorOn,False);
    NhlRLSetFloatArray(rlist,
               NhlNtmXBValues,labellocs,NhlNumber(labellocs));

/*
* Array "level" contains original grid point data locations in Y direction.
* Providing the grid points to the TickMark object as the control points
* for the IRREGULAR style transformation, means that these points will be
* evenly spaced along the Y axis. Since this is how CONPACK thinks the
* points are spaced, the tick marks will correctly correspond with the 
* data coordinates. See the HLU User's Guide Prototype document for a complete
* discussion of IRREGULAR style transformations.
*/
    NhlRLSetStringArray(rlist,
                NhlNtmXBLabels,labels,NhlNumber(labels));
    NhlRLSetFloatArray(rlist,
               NhlNtmYLIrregularPoints,level,NhlNumber(level));

    NhlCreate(&pid,"TickMarksForContour",
          NhltickMarkClass,wid,rlist);

/*
* Retrieves bounding box information from tick mark object so Title object
* can be correctly configured.
*/ 
    NhlGetBB(pid,&thebox);  

        NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNvpXF,thebox.l);
    NhlRLSetFloat(rlist,NhlNvpYF,thebox.t);
    NhlRLSetFloat(rlist,NhlNvpWidthF,thebox.r-thebox.l);
    NhlRLSetFloat(rlist,NhlNvpHeightF,thebox.t-thebox.b);
/*
* These offsets are computed in order to center the labels around the plot
* viewport rather than the bounding box of the TickMark object.
*/
    NhlRLSetFloat(rlist,NhlNtiMainOffsetXF,(.2 - thebox.l)/2.0);
    NhlRLSetFloat(rlist,NhlNtiXAxisOffsetXF,(.2 - thebox.l)/2.0);
    NhlRLSetFloat(rlist,NhlNtiYAxisOffsetYF,(.2 - thebox.b)/2.0);

    NhlCreate(&pid1,"TitlesForContour",NhltitleClass,wid,rlist);

    NhlDraw(pid);
    NhlDraw(pid1);
    NhlFrame(wid);
    NhlDestroy(pid);
    NhlDestroy(pid1);
    NhlDestroy(wid);
    NhlDestroy(appid);
    NhlClose();
    exit(0);
}

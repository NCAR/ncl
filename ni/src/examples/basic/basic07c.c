/***********************************************************************
*                                                                      *
*                Copyright (C)  1995                                   *
*        University Corporation for Atmospheric Research               *
*                all rights reserved                                   *
*                                                                      *
************************************************************************
*
*      File:           basic07c.c
*
*      Author:         Bob Lackman 
*          National Center for Atmospheric Research
*          PO 3000, Boulder, Colorado
*
*      Date:           Fri May 25 18:31:18 mdt 1995
*
*      Description:    Demonstrates creating 3 simultaneous workstations.
*                      The TextItem output states which type of workstation,
*                      out of NCGM, PDF, PostScript, and X11.
*/

#include <ncarg/hlu/App.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/hlu/PDFWorkstation.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/CairoWorkstation.h>
#include <ncarg/hlu/TextItem.h>

int main()
{
      int appid, widx,widn,widp,widpdf, pidx,pidn,pidp,pidpdf;
      int srlist;
      int i;
/*
 * Initialize the high level utility library
 */
      NhlInitialize();
/*
 * Create an application context. Set the app dir to the current
 * directory so the application looks for a resource file in the
 * working directory. In this example the resource file supplies the
 * plot title only.
 */
      srlist = NhlRLCreate(NhlSETRL);
      NhlRLClear(srlist);
      NhlRLSetString(srlist,NhlNappUsrDir,"./");
      NhlRLSetString(srlist,NhlNappDefaultParent,"True");
      NhlCreate(&appid,"basic07",NhlappClass,NhlDEFAULT_APP,srlist);

/*
 * Create an NCGM workstation.
 */
      NhlRLClear(srlist);
      NhlRLSetString(srlist,NhlNwkMetaName,"basic07c.ncgm");
      NhlCreate(&widn,"basic07ncgm",NhlncgmWorkstationClass,NhlDEFAULT_APP,
                srlist);
/*
 * Create an older-style PostScript workstation.
 */
      NhlRLClear(srlist);
      NhlRLSetString(srlist,NhlNwkPSFileName,"basic07c.ps");
      NhlRLSetString(srlist,NhlNwkOrientation,"portrait");
      NhlRLSetString(srlist,NhlNwkPSFormat,"ps");
      NhlCreate(&widp,"basic07ps",NhlpsWorkstationClass,NhlDEFAULT_APP,
                srlist);
/*
 * Create an older-style PDF workstation.
 */
      NhlRLClear(srlist);
      NhlRLSetString(srlist,NhlNwkPDFFileName,"basic07c.pdf");
      NhlRLSetString(srlist,NhlNwkOrientation,"portrait");
      NhlRLSetString(srlist,NhlNwkPDFFormat,"pdf");
      NhlCreate(&widpdf,"basic07pdf",NhlpdfWorkstationClass,NhlDEFAULT_APP,
                srlist);
/*
 * Create an X Workstation.
 */
      NhlRLClear(srlist);
      NhlRLSetString(srlist,NhlNwkPause,"True");
      NhlCreate(&widx,"basic07x11",NhlcairoWindowWorkstationClass,NhlDEFAULT_APP,
                srlist);
/*
 * Create four plots, one for each workstation type.
 *
 *  Use color index 2
 */
      i = 2;
      NhlRLClear(srlist);
      NhlRLSetInteger(srlist,NhlNtxBackgroundFillColor,i);
      NhlCreate(&pidx,"TextItems",NhltextItemClass,widx,srlist);

      NhlCreate(&pidn,"TextItems",NhltextItemClass,widn,srlist);

      NhlCreate(&pidp,"TextItems",NhltextItemClass,widp,srlist);

      NhlCreate(&pidpdf,"TextItems",NhltextItemClass,widpdf,srlist);

      NhlDraw(pidx);
      NhlDraw(pidn);
      NhlDraw(pidp);
      NhlDraw(pidpdf);
      NhlFrame(widx);
      NhlFrame(widp);
      NhlFrame(widpdf);
      NhlFrame(widn);
 
      NhlDestroy(pidx);
      NhlDestroy(pidn);
      NhlDestroy(pidp);
      NhlDestroy(pidpdf);
      NhlDestroy(widx);
      NhlDestroy(widp);
      NhlDestroy(widpdf);
      NhlDestroy(widn);
      NhlDestroy(appid);

      NhlClose();
      exit (0);
}

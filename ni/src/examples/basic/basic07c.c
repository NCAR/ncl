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
*                      out of NCGM, PostScript, and X11.
*/

#include <ncarg/hlu/App.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/hlu/XWorkstation.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/TextItem.h>

main()
{
      int appid, widx,widn,widp, pidx,pidn,pidp;
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
 * Create a PostScript workstation.
 */
      NhlRLClear(srlist);
      NhlRLSetString(srlist,NhlNwkPSFileName,"basic07c.ps");
      NhlRLSetString(srlist,NhlNwkOrientation,"portrait");
      NhlRLSetString(srlist,NhlNwkPSFormat,"ps");
      NhlCreate(&widp,"basic07ps",NhlpsWorkstationClass,NhlDEFAULT_APP,
                srlist);
/*
 * Create an X Workstation.
 */
      NhlRLClear(srlist);
      NhlRLSetString(srlist,NhlNwkPause,"True");
      NhlCreate(&widx,"basic07x11",NhlxWorkstationClass,NhlDEFAULT_APP,
                srlist);
/*
 * Create three plots, one for each workstation type.
 *
 *  Use color index 2
 */
      i = 2;
      NhlRLClear(srlist);
      NhlRLSetInteger(srlist,NhlNtxBackgroundFillColor,i);
      NhlCreate(&pidx,"TextItems",NhltextItemClass,widx,srlist);

      NhlCreate(&pidn,"TextItems",NhltextItemClass,widn,srlist);

      NhlCreate(&pidp,"TextItems",NhltextItemClass,widp,srlist);

      NhlDraw(pidx);
      NhlDraw(pidn);
      NhlDraw(pidp);
      NhlFrame(widx);
      NhlFrame(widp);
      NhlFrame(widn);
 
      NhlDestroy(pidx);
      NhlDestroy(pidn);
      NhlDestroy(pidp);
      NhlDestroy(widx);
      NhlDestroy(widp);
      NhlDestroy(widn);
      NhlDestroy(appid);

      NhlClose();
      exit (0);
}

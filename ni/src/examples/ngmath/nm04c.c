/*
 *      $Id: nm04c.c,v 1.9 2010-03-15 22:49:24 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  1997                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
************************************************************************/
/*
 *  File:       nm04c.c
 *
 *  Author:     Mary Haley (taken from one of Fred Clare's examples)
 *          National Center for Atmospheric Research
 *          PO 3000, Boulder, Colorado
 *
 *  Date:       Tue Dec 16 08:28:46 MST 1997
 *
 *  Description: Simple 3D interpolation.
 */

#include <stdio.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>
#include <ncarg/ngmath.h>
#include <math.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/hlu/PDFWorkstation.h>
#include <ncarg/hlu/CairoWorkstation.h>

#define NUM  1000
#define NX     21
#define NY     21
#define NZ     21

int main()
{
  int  appid,wid,gkswid;
  int  srlist, grlist;
  const char *wks_type = "ncgm";
  int  i, j, k, ier;
  float xi[NUM], yi[NUM], zi[NUM], u[NUM];
  float xo[NX], yo[NY], zo[NZ], *output, outr[NZ][NY][NX];
  float xmin = -2.0, ymin = -2.0, zmin = -2.0;
  float xmax =  2.0, ymax =  2.0, zmax =  2.0;
/*
 *  Create random data in three space and define a function.
 */
  for (i = 0; i < NUM; i++) {
    xi[i] = xmin+(xmax-xmin)*((float) rand() / (float) RAND_MAX);
    yi[i] = ymin+(ymax-ymin)*((float) rand() / (float) RAND_MAX);
    zi[i] = zmin+(zmax-zmin)*((float) rand() / (float) RAND_MAX);
	u[i] = xi[i]*xi[i] + yi[i]*yi[i] + zi[i]*zi[i];
  }
/*
 *  Create the output grid.
 */
  for (i = 0; i < NX; i++) {
    xo[i] = xmin + ( (float) i / (float) (NX-1)) * (xmax-xmin);
  }
  for (j = 0; j < NY; j++) {
    yo[j] = ymin + ( (float) j / (float) (NY-1)) * (ymax-ymin);
  }
  for (k = 0; k < NZ; k++) {
    zo[k] = zmin + ( (float) k / (float) (NZ-1)) * (zmax-zmin);
  }
/*
 *  Interpolate.
 */
  output = c_dsgrid3s(NUM, xi, yi, zi, u, NX, NY, NZ, xo, yo, zo, &ier);
  if (ier != 0) {
    printf(" Error %d returned from nm04c\n",ier);
    exit(1);
  }

/*
 * Initialize the high level utility library
 */
    NhlInitialize();
/*
 * Create an application context. Set the app dir to the current directory
 * so the application looks for a resource file in the working directory.
 * In this example the resource file supplies the plot title only.
 */
    srlist = NhlRLCreate(NhlSETRL);
    grlist = NhlRLCreate(NhlGETRL);
    NhlRLClear(srlist);
    NhlRLSetString(srlist,NhlNappUsrDir,"./");
    NhlCreate(&appid,"nm04",NhlappClass,NhlDEFAULT_APP,srlist);

    if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
/*
 * Create a meta file workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkMetaName,"./nm04c.ncgm");
        NhlCreate(&wid,"nm04Work",
                  NhlncgmWorkstationClass,NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 * Create an X workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetInteger(srlist,NhlNwkPause,True);
        NhlCreate(&wid,"nm04Work",NhlcairoWindowWorkstationClass,NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 * Create an older-style PostScript workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkPSFileName,"./nm04c.ps");
        NhlCreate(&wid,"nm04Work",NhlpsWorkstationClass,NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 * Create an older-style PDF workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkPDFFileName,"./nm04c.pdf");
        NhlCreate(&wid,"nm04Work",NhlpdfWorkstationClass,NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
             !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 * Create a cairo PS/PDF workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkFileName,"./nm04c");
        NhlRLSetString(srlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"nm04Work",NhlcairoDocumentWorkstationClass,NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 * Create a cairo PNG workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkFileName,"./nm04c");
        NhlRLSetString(srlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"nm04Work",NhlcairoImageWorkstationClass,NhlDEFAULT_APP,srlist);
    }
/*
 * Get Workstation ID.
 */
	NhlRLClear(grlist);
	NhlRLGetInteger(grlist,NhlNwkGksWorkId,&gkswid);
	NhlGetValues(wid,grlist);
/*
 * There's no HLU object for surface plots yet, so we need to call the
 * LLUs to get a surface plot.
 */
        gactivate_ws (gkswid);
/*
 * Reverse the dimension order before plotting.
 */
        for (i = 0; i < NX; i++) {
          for (j = 0; j < NY; j++) {
            for (k = 0; k < NZ; k++) {
              outr[k][j][i] = output[i*NZ*NY + j*NZ + k];
            }
          }
        }
	c_tdez3d(NX, NY, NZ, xo, yo, zo, &outr[0][0][0], 3.0, 2., -35., 65., 6);
        gdeactivate_ws (gkswid);
	NhlFrame(wid);
/*
 * NhlDestroy destroys the given id and all of its children.
 */
    NhlDestroy(wid);
/*
 * Restores state.
 */
    NhlClose();
    exit(0);
}

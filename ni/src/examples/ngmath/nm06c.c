/*
 *      $Id: nm06c.c,v 1.7 2010-03-15 22:49:24 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  1997                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
************************************************************************/
/*
 *  File:       nm06c.c
 *
 *  Author:     Mary Haley (taken from one of Fred Clare's examples)
 *              National Center for Atmospheric Research
 *              PO 3000, Boulder, Colorado
 *
 *  Date:       Tue Dec 16 08:28:46 MST 1997
 *
 *  Description: Smoothing in a simple 2D interpolation.
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


#define NUM  171
#define NX    21
#define NY    21

int main()
{
  int  i, j, ier;
  float xi[NUM], yi[NUM], zi[NUM];
  float xo[NX], yo[NY], *output, output2[NX][NY], outr[NY][NX];
  float xminin = -0.2, yminin = -0.2, xmaxin = 1.2, ymaxin = 1.2;
  float xminot =  0.0, yminot =  0.0, xmaxot = 1.0, ymaxot = 1.0;
  float rho = 3., theta = -54., phi = 32.;
  int    appid,wid,gkswid;
  int    srlist, grlist;
  const char *wks_type = "ncgm";
 
/*
 *  Create random data in three space and define a function.
 */
  for (i = 0; i < NUM; i++) {
    xi[i] = xminin+(xmaxin-xminin)*((float) rand() / (float) RAND_MAX);
    yi[i] = yminin+(ymaxin-yminin)*((float) rand() / (float) RAND_MAX);
    zi[i] = (xi[i]-0.25)*(xi[i]-0.25) + (yi[i]-0.50)*(yi[i]-0.50);
  }
 
/*
 *  Create the output grid.
 */
  for (i = 0; i < NX; i++) {
    xo[i] = xminot + ( (float) i / (float) (NX-1)) * (xmaxot-xminot);
  }
  for (j = 0; j < NY; j++) {
    yo[j] = yminot + ( (float) j / (float) (NY-1)) * (ymaxot-yminot);
  }

/*
 *  Interpolate using c_dsgrid2s.
 */
  output = c_dsgrid2s(NUM, xi, yi, zi, NX, NY, xo, yo, &ier);
  if (ier != 0) {
    printf(" Error %d returned from nm06c\n",ier);
    exit(1);
  }
/*
 *  Interpolate using c_dspnt2s.
 */
  for( i = 0; i < NX; i++ ) {
	for( j = 0; j < NY; j++ ) {
      c_dspnt2s(NUM, xi, yi, zi, 1, &xo[i], &yo[j], &output2[i][j], &ier);
	}
  }
  if (ier != 0) {
    printf(" Error %d returned from nm06c\n",ier);
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
    NhlCreate(&appid,"nm06",NhlappClass,NhlDEFAULT_APP,srlist);

    if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
/*
 * Create a meta file workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkMetaName,"./nm06c.ncgm");
        NhlCreate(&wid,"nm06Work",
                  NhlncgmWorkstationClass,NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 * Create an X workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetInteger(srlist,NhlNwkPause,True);
        NhlCreate(&wid,"nm06Work",NhlcairoWindowWorkstationClass,NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 * Create an older-style PostScript workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkPSFileName,"./nm06c.ps");
        NhlCreate(&wid,"nm06Work",NhlpsWorkstationClass,NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 * Create an older-style PDF workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkPDFFileName,"./nm06c.pdf");
        NhlCreate(&wid,"nm06Work",NhlpdfWorkstationClass,NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
             !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 * Create a cairo PS/PDF workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkFileName,"./nm06c");
        NhlRLSetString(srlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"nm06Work",NhlcairoDocumentWorkstationClass,NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 * Create a cairo PNG workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkFileName,"./nm06c");
        NhlRLSetString(srlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"nm06Work",NhlcairoImageWorkstationClass,NhlDEFAULT_APP,srlist);
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
/*
 *  Reverse the array indices for plotting with tdez2d, since
 *  c_dsgrid returns its array in column dominate order.
 */
        for (i = 0; i < NX; i++) {
          for (j = 0; j < NY; j++) {
            outr[j][i] = output[i*NY+j];
          }
        }
	gactivate_ws (gkswid);
        c_tdez2d(NX, NY, xo, yo, &outr[0][0], rho, theta, phi, 6);
	gdeactivate_ws (gkswid);
        NhlFrame(wid);

        for (i = 0; i < NX; i++) {
          for (j = 0; j < NY; j++) {
            outr[j][i] = output2[i][j];
          }
        }
	gactivate_ws (gkswid);
        c_tdez2d(NX, NY, xo, yo, &outr[0][0], rho, theta, phi, 6);
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

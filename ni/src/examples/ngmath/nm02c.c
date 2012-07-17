/*
 *      $Id: nm02c.c,v 1.4 2010-03-15 22:49:24 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  1997                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
************************************************************************/
/*
 *  File:       nm02c.c
 *
 *  Author:     Mary Haley (taken from one of Fred Clare's examples)
 *          National Center for Atmospheric Research
 *          PO 3000, Boulder, Colorado
 *
 *  Date:       Mon Dec 15 16:01:30 MST 1997
 *
 *  Description:  Simple example of natural neighbor linear regridding.
 */

#include <math.h>
#include <stdio.h>
#include <ncarg/gks.h>
#include <ncarg/ncargC.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/ngmath.h>
/*
 * Include a header file for each object created
 */

#include <ncarg/hlu/App.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/hlu/PDFWorkstation.h>
#include <ncarg/hlu/CairoWorkstation.h>

#define Npts    6
#define NumXOut 21
#define NumYOut 21

extern void drwsrfc (int WKID, int nx, int ny, float *x, float *y, float *z,
              float s1, float s2, float s3);

int main(int argc, char *argv[])
{
    float  xo[NumXOut], yo[NumYOut], *out;
    float x[] = {0.00, 1.00, 0.00, 1.00, 0.40, 0.75};
    float y[] = {0.00, 0.00, 1.00, 1.00, 0.20, 0.65};
    float z[] = {0.00, 0.00, 0.00, 0.00, 1.25, 0.80};
	float  xc, yc;
	int    ier;
    int    appid,wid,gkswid;
    int    srlist, grlist;
    int    i;
    const char *wks_type = "ncgm";

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
    NhlCreate(&appid,"nm02",NhlappClass,NhlDEFAULT_APP,srlist);

    if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
/*
 * Create a meta file workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkMetaName,"./nm02c.ncgm");
        NhlCreate(&wid,"nm02Work",
                  NhlncgmWorkstationClass,NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 * Create an X workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetInteger(srlist,NhlNwkPause,True);
        NhlCreate(&wid,"nm02Work",NhlcairoWindowWorkstationClass,NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 * Create an older-style PostScript workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkPSFileName,"./nm02c.ps");
        NhlCreate(&wid,"nm02Work",NhlpsWorkstationClass,NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 * Create an older-style PDF workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkPDFFileName,"./nm02c.pdf");
        NhlCreate(&wid,"nm02Work",NhlpdfWorkstationClass,NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
             !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 * Create a cairo PS/PDF workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkFileName,"./nm02c");
        NhlRLSetString(srlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"nm02Work",NhlcairoDocumentWorkstationClass,NhlDEFAULT_APP,srlist);
    }
    else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 * Create a cairo PNG workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkFileName,"./nm02c");
        NhlRLSetString(srlist,NhlNwkFormat,(char*)wks_type);
        NhlCreate(&wid,"nm02Work",NhlcairoImageWorkstationClass,NhlDEFAULT_APP,srlist);
    }
	xc = 1./(NumXOut-1.);
	for( i = 0; i < NumXOut; i++ ) {
	  xo[i] = i * xc;
	}
	yc = 1./(NumYOut-1.);
	for( i = 0; i < NumYOut; i++ ) {
	  yo[i] = i * yc;
	}
	c_nnseti("IGR",1);
	out = c_natgrids(Npts, y, x, z, NumXOut, NumYOut, xo, yo ,&ier);
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
	drwsrfc(gkswid,NumXOut,NumYOut,xo,yo,out,15.,-25.,90.);
	gdeactivate_ws (gkswid);
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

float armn(int, float *);
float armx(int, float *);

/*
 * Procedure drwsrfc uses the NCAR Graphics function c_srface to
 * draw a surface plot of the data values in z.
 *
 * The point of observation is calculated from the 3D coordinate 
 * (s1, s2, s3); the point looked at is the center of the surface.
 *
 *  nx     -  Dimension of the X-axis variable x.
 *  ny     -  Dimension of the Y-axis variable y.
 *  x      -  An array of X-axis values.
 *  y      -  An array of Y-axis values.
 *  z      -  An array dimensioned for nx x ny containing data
 *            values for each (X,Y) coordinate.
 *  s1     -  X value for the eye position.
 *  s2     -  Y value for the eye position.
 *  s3     -  Z value for the eye position.
 *  iwk    -  Work space dimensioned for at least 2*nx*ny.
 *
 */

void drwsrfc (int WKID, int nx, int ny, float *x, float *y, float *z,
              float s1, float s2, float s3)
{
    Gcolr_rep colval;
    float xmn, xmx, ymn, ymx, zmn, zmx, eye[6];
    int *iwk;

	iwk = (int *)malloc(2*nx*ny*sizeof(int));

	colval.rgb.red = 1.;
	colval.rgb.green = 1.;
	colval.rgb.blue = 1.;
	gset_colr_rep(WKID,0,&colval);
	colval.rgb.red = 0.;
	colval.rgb.green = 0.;
	colval.rgb.blue = 0.;
	gset_colr_rep(WKID,1,&colval);
/*
 *  Find the extreme data values.
 */
    xmn = armn(nx, x);
    xmx = armx(nx, x);
    ymn = armn(ny, y);
    ymx = armx(ny, y);
    zmn = armn(nx * ny, z);
    zmx = armx(nx * ny, z);

    if ( (s1 == 0.) &&  (s2 == 0.) &&  (s3 == 0.) ) {
       s1 = -3.;
       s2 = -1.5;
       s3 = 0.75;
    }
    eye[0] = 5. * s1 * (xmx-xmn);
    eye[1] = 5. * s2 * (ymx-ymn);
    eye[2] = 5. * s3 * (zmx-zmn);
    eye[3] = 0.5 * (xmx-xmn);
    eye[4] = 0.5 * (ymx-ymn);
    eye[5] = 0.5 * (zmx-zmn);
/*
 *  Plot the surface.
 */
    c_srface (x,y,z,iwk,nx,nx,ny,eye,0.);
    free(iwk);
}

float armn(int num, float *x)
{
   int i;
   float amin;
   amin = x[0];
   for (i = 1 ; i < num ; i++)
      if (x[i] < amin) amin = x[i];
   return(amin);
}
float armx(int num, float *x)
{
   int i;
   float amax;
   amax = x[0];
   for (i = 1 ; i < num ; i++)
      if (x[i] > amax) amax = x[i];
   return(amax);
}

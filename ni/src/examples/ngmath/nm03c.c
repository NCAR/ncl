/*
 *      $Id: nm03c.c,v 1.10 2010-03-15 22:49:24 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  1997                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
************************************************************************/
/*
 *  File:       nm03c.c
 *
 *  Author:     Mary Haley (taken from one of Fred Clare's examples)
 *          National Center for Atmospheric Research
 *          PO 3000, Boulder, Colorado
 *
 *  Date:       Fri Dec 12 09:43:32 MST 1997
 *
 *  Description: How to compute aspects and slopes.
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
#include <ncarg/hlu/ScalarField.h>
#include <ncarg/hlu/ContourPlot.h>
#include <ncarg/hlu/VectorPlot.h>

#define NumIn   171
#define NumXOut  21
#define NumYOut  21
#define RAD2DEG 57.29578

extern void drwsrfc (int nx, int ny, float *x, float *y, float *z,
              float s1, float s2, float s3);

int main(int argc, char *argv[])
{

/*
 *  Coordinate data are defined as random numbers between
 *  -0.2 and 1.2. and are explicitly defined here for uniformity
 *  across platforms.
 */
  float x[] = {
             1.16,  0.47,  0.29,  0.72,  0.52,  1.12,  0.33,  0.20,  0.30,
             0.78,  0.92,  0.52,  0.44,  0.22, -0.10,  0.11,  0.59,  1.13,
             0.68,  1.11,  0.93,  0.29,  0.74,  0.43,  0.87,  0.87, -0.10,
             0.26,  0.85,  0.00, -0.02,  1.01, -0.12,  0.65,  0.39,  0.96,
             0.39,  0.38,  0.94, -0.03, -0.17,  0.00,  0.03,  0.67, -0.06,
             0.82, -0.03,  1.08,  0.37,  1.02, -0.11, -0.13,  1.03,  0.61,
             0.26,  0.18,  0.62,  0.42,  1.03,  0.72,  0.97,  0.08,  1.18,
             0.00,  0.69,  0.10,  0.80,  0.06,  0.82,  0.20,  0.46,  0.37,
             1.16,  0.93,  1.09,  0.96,  1.00,  0.80,  0.01,  0.12,  1.01,
             0.48,  0.79,  0.04,  0.42,  0.48, -0.18,  1.16,  0.85,  0.97,
             0.14,  0.40,  0.78,  1.12,  1.19,  0.68,  0.65,  0.41,  0.90,
             0.84, -0.11, -0.01, -0.02, -0.10,  1.04,  0.58,  0.61,  0.12,
            -0.02, -0.03,  0.27,  1.17,  1.02,  0.16, -0.17,  1.03,  0.13,
             0.04, -0.03,  0.15,  0.00, -0.01,  0.91,  1.20,  0.54, -0.14,
             1.03,  0.93,  0.42,  0.36, -0.10,  0.57,  0.22,  0.74,  1.15,
             0.40,  0.82,  0.96,  1.09,  0.42,  1.13,  0.24,  0.51,  0.60,
             0.06,  0.38,  0.15,  0.59,  0.76,  1.16,  0.02,  0.86,  1.14,
             0.37,  0.38,  0.26,  0.26,  0.07,  0.87,  0.90,  0.83,  0.09,
             0.03,  0.56, -0.19,  0.51,  1.07, -0.13,  0.99,  0.84,  0.22
            };
  float y[] = {
            -0.11,  1.07,  1.11, -0.17,  0.08,  0.09,  0.91,  0.17, -0.02,
             0.83,  1.08,  0.87,  0.46,  0.66,  0.50, -0.14,  0.78,  1.08,
             0.65,  0.00,  1.03,  0.06,  0.69, -0.16,  0.02,  0.59,  0.19,
             0.54,  0.68,  0.95,  0.30,  0.77,  0.94,  0.76,  0.56,  0.12,
             0.05, -0.07,  1.01,  0.61,  1.04, -0.07,  0.46,  1.07,  0.87,
             0.11,  0.63,  0.06,  0.53,  0.95,  0.78,  0.48,  0.45,  0.77,
             0.78,  0.29,  0.38,  0.85, -0.10,  1.17,  0.35,  1.14, -0.04,
             0.34, -0.18,  0.78,  0.17,  0.63,  0.88, -0.12,  0.58, -0.12,
             1.00,  0.99,  0.45,  0.86, -0.15,  0.97,  0.99,  0.90,  0.42,
             0.61,  0.74,  0.41,  0.44,  1.08,  1.06,  1.18,  0.89,  0.74,
             0.74, -0.06,  0.00,  0.99,  0.03,  1.00, -0.04,  0.24,  0.65,
             0.12,  0.13, -0.09, -0.05,  1.03,  1.07, -0.02,  1.18,  0.19,
             0.03, -0.03,  0.86,  1.12,  0.38,  0.72, -0.20, -0.08, -0.18,
             0.32,  0.13, -0.19,  0.93,  0.81,  0.31,  1.09, -0.03,  1.01,
            -0.17,  0.84, -0.11,  0.45,  0.18,  0.23,  0.81,  0.39,  1.09,
            -0.05,  0.58,  0.53,  0.96,  0.43,  0.48,  0.96, -0.03,  1.13,
             1.16,  0.16,  1.15,  0.57,  0.13,  0.71,  0.35,  1.04,  0.62,
             1.03,  0.98,  0.31,  0.70,  0.97,  0.87,  1.14,  0.08,  1.19,
             0.88,  1.00,  0.51,  0.03,  0.17,  1.01,  0.44,  0.17, -0.11
            };

  float z[NumIn];
  float *out, xo[NumXOut], yo[NumYOut], xc, yc;
  float u[NumXOut][NumYOut], v[NumXOut][NumYOut], uvtmp;
  ng_size_t  len_dims[2];
  int   appid,wid,dataid,cnid,vfid,vcid,gkswid;
  int   srlist, grlist;
  int   i, j, ier;
  const char *wks_type = "ncgm";

/*
 * Initialize the high level utility library
 */
  NhlInitialize();
/*
 * Create an application context. Set the app dir to the current directory
 * so the application looks for a resource file in the working directory.
 */
  srlist = NhlRLCreate(NhlSETRL);
  grlist = NhlRLCreate(NhlGETRL);
  NhlRLClear(srlist);
  NhlRLSetString(srlist,NhlNappUsrDir,"./");
  NhlCreate(&appid,"nm03",NhlappClass,NhlDEFAULT_APP,srlist);

  len_dims[0] = 9;
  len_dims[1] = 3;
  if (!strcmp(wks_type,"ncgm") || !strcmp(wks_type,"NCGM")) {
/*
 * Create a meta file workstation.
 */
	NhlRLClear(srlist);
	NhlRLSetString(srlist,NhlNwkMetaName,"./nm03c.ncgm");
	NhlCreate(&wid,"nm03Work",
			  NhlncgmWorkstationClass,NhlDEFAULT_APP,srlist);
  }
  else if (!strcmp(wks_type,"x11") || !strcmp(wks_type,"X11")) {
/*
 * Create an X workstation.
 */
	NhlRLClear(srlist);
	NhlRLSetInteger(srlist,NhlNwkPause,True);
	NhlCreate(&wid,"nm03Work",NhlcairoWindowWorkstationClass,NhlDEFAULT_APP,srlist);
  }
  else if (!strcmp(wks_type,"oldps") || !strcmp(wks_type,"OLDPS")) {
/*
 * Create an older-style PostScript workstation.
 */
	NhlRLClear(srlist);
	NhlRLSetString(srlist,NhlNwkPSFileName,"./nm03c.ps");
	NhlCreate(&wid,"nm03Work",NhlpsWorkstationClass,NhlDEFAULT_APP,srlist);
  }
  else if (!strcmp(wks_type,"oldpdf") || !strcmp(wks_type,"OLDPDF")) {
/*
 * Create an older-style PDF workstation.
 */
	NhlRLClear(srlist);
	NhlRLSetString(srlist,NhlNwkPDFFileName,"./nm03c.pdf");
	NhlCreate(&wid,"nm03Work",NhlpdfWorkstationClass,NhlDEFAULT_APP,srlist);
  }
  else if (!strcmp(wks_type,"pdf") || !strcmp(wks_type,"PDF") ||
           !strcmp(wks_type,"ps") || !strcmp(wks_type,"PS")) {
/*
 * Create a cairo PS/PDF workstation.
 */
	NhlRLClear(srlist);
	NhlRLSetString(srlist,NhlNwkFileName,"./nm03c");
	NhlRLSetString(srlist,NhlNwkFormat,(char*)wks_type);
	NhlCreate(&wid,"nm03Work",NhlcairoDocumentWorkstationClass,NhlDEFAULT_APP,srlist);
  }
  else if (!strcmp(wks_type,"png") || !strcmp(wks_type,"PNG")) {
/*
 * Create a cairo PNG workstation.
 */
	NhlRLClear(srlist);
	NhlRLSetString(srlist,NhlNwkFileName,"./nm03c");
	NhlRLSetString(srlist,NhlNwkFormat,(char*)wks_type);
	NhlCreate(&wid,"nm03Work",NhlcairoImageWorkstationClass,NhlDEFAULT_APP,srlist);
  }

  for (i = 0 ; i < NumIn ; i++) {
    z[i] = (x[i]-0.25)*(x[i]-0.25) + (y[i]-0.50)*(y[i]-0.50);
  }
  xc = 1./(NumXOut-1.);
  for( i = 0; i < NumXOut; i++ ) {
	xo[i] = i * xc;
  }
  yc = 1./(NumYOut-1.);
  for( i = 0; i < NumYOut; i++ ) {
	yo[i] = i * yc;
  }
/*
 *  Turn on gradient estimate calculations, flag calculation of
 *  aspects and slopes, set flag to return aspects and slopes in
 *  radians.
 */
  c_nnseti("igr",1);
  c_nnseti("sdi",1);
  c_nnseti("rad",1);
/*
 *  Do the interpolation.
 */
  out = c_natgrids(NumIn, y, x, z, NumYOut, NumXOut, yo, xo, &ier);
  if (ier != 0) {
     printf (" Error return from c_natgrids = %d\n",ier);
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
	drwsrfc(NumYOut,NumXOut,yo,xo,out,10.,-25.,50.);
	gdeactivate_ws (gkswid);
/*
 *  Get the aspects.
 */
  for (i = 0 ; i < NumXOut ; i++) {
    for (j = 0 ; j < NumYOut ; j++) {
      c_nngetaspects(i, j, &uvtmp, &ier);
      u[i][j] = sin(uvtmp);
      v[i][j] = cos(uvtmp);
    }
  }
/*
 * Create a VectorField object; then use its id as the value of
 * the 'vcVectorFieldData' resource when creating the VectorPlot object.
 */
  len_dims[0] = NumXOut;
  len_dims[1] = NumYOut;

  NhlRLClear(srlist);
  NhlRLSetMDFloatArray(srlist,NhlNvfUDataArray,&v[0][0],2,len_dims);
  NhlRLSetMDFloatArray(srlist,NhlNvfVDataArray,&u[0][0],2,len_dims);
  NhlCreate(&vfid,"vectorfield",NhlvectorFieldClass,appid,srlist);

  NhlRLClear(srlist);
  NhlRLSetInteger(srlist,NhlNvcVectorFieldData,vfid);
  NhlCreate(&vcid,"VectorPlot",NhlvectorPlotClass,wid,srlist);

  NhlDraw(vcid);
  NhlFrame(wid);
/*
 *  Get the slopes.
 */
  for (i = 0 ; i < NumXOut ; i++) {
    for (j = 0 ; j < NumYOut ; j++) {
      c_nngetslopes(i, j, &uvtmp, &ier);
      u[i][j] = RAD2DEG*uvtmp;
    }
  }

/*
 * Create a ScalarField data object using the data set defined above.
 */
    NhlRLClear(srlist);
    len_dims[0] = NumXOut;
    len_dims[1] = NumYOut;
    NhlRLSetMDFloatArray(srlist,NhlNsfDataArray,&u[0][0],2,len_dims);
    NhlCreate(&dataid,"data",NhlscalarFieldClass,appid,srlist);
    NhlRLClear(srlist);
    NhlRLSetInteger(srlist,NhlNcnScalarFieldData,dataid);
    NhlCreate(&cnid,"ContourPlot",NhlcontourPlotClass,wid,srlist);

    NhlDraw(cnid);
    NhlFrame(wid);
/*
 * Destroy the objects created, close the HLU library and exit.
 */
    NhlDestroy(vfid);
    NhlDestroy(dataid);
    NhlDestroy(cnid);
    NhlDestroy(wid);
    NhlDestroy(appid);

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

void drwsrfc (int nx, int ny, float *x, float *y, float *z,
              float s1, float s2, float s3)
{
    float xmn, xmx, ymn, ymx, zmn, zmx, eye[6];
    int *iwk;

	iwk = (int *)malloc(2*nx*ny*sizeof(int));

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

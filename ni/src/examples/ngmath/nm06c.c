/*
 *      $Id: nm06c.c,v 1.2 1997-12-17 16:14:22 haley Exp $
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
 *          National Center for Atmospheric Research
 *          PO 3000, Boulder, Colorado
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
#include <ncarg/hlu/XWorkstation.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/PSWorkstation.h>


#define NUM  171
#define NX    21
#define NY    21

extern void    drwtd2(int, int, int, float *, float *, float *,
                      float, float, float, int);

main()
{
  int  i, j, k, ier;
  float xi[NUM], yi[NUM], zi[NUM];
  float xo[NX], yo[NY], *output, output2[NX][NY];
  float xminin = -0.2, yminin = -0.2, xmaxin = 1.2, ymaxin = 1.2;
  float xminot =  0.0, yminot =  0.0, xmaxot = 1.0, ymaxot = 1.0;
  float xeye = 1.3, yeye = -1.8, zeye = 3.6;
  int    appid,wid,gkswid;
  int    srlist, grlist;
  int    NCGM=1, X11=0, PS=0;
 
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

    if (NCGM) {
/*
 * Create a meta file workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkMetaName,"./nm06c.ncgm");
        NhlCreate(&wid,"nm06Work",
                  NhlncgmWorkstationClass,NhlDEFAULT_APP,srlist);
    }
    else if (X11) {
/*
 * Create an X workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetInteger(srlist,NhlNwkPause,True);
        NhlCreate(&wid,"nm06Work",NhlxWorkstationClass,NhlDEFAULT_APP,srlist);
    }
    else if (PS) {
/*
 * Create a meta file workstation.
 */
        NhlRLClear(srlist);
        NhlRLSetString(srlist,NhlNwkPSFileName,"./nm06c.ps");
        NhlCreate(&wid,"nm06Work",NhlpsWorkstationClass,NhlDEFAULT_APP,srlist);
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
    drwtd2(gkswid, NX, NY, xo, yo, output, xeye, yeye, zeye, -6);
    drwtd2(gkswid, NX, NY, xo, yo, output2, xeye, yeye, zeye, -6);
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

#define MTRI 40000
#define DTOR .017453292519943
#ifndef MAX
#define MAX(A,B)        (((A) > (B)) ? (A) : (B))
#endif
 
#ifndef MIN
#define MIN(A,B)        (((A) < (B)) ? (A) : (B))
#endif


int   itwk[MTRI], ntri;
float rtri[MTRI][10], rtwk[2][MTRI];

void drwtd2(int gkswid, int nx, int ny, float *x, float *y, float *z, 
            float s1, float s2, float s3, int style)
{
/*
 * Procedure c_drwtd2 uses the NCAR Graphics functions in Tdpack to
 * draw a surface plot of the data values in z.
 *
 * The point of observation is specified by (s1, s2, s3); the point 
 * looked at is the center of the surface.  If s1 = s2 = s3 = 0., 
 * then the observation point is calculated automatically.
 *
 *  nx     -  Dimension of the X-axis variable x.
 *  ny     -  Dimension of the Y-axis variable y.
 *  x      -  An array of X-axis values.
 *  y      -  An array of Y-axis values.
 *  z      -  An array dimensioned for nx X ny containing data
 *            values for each (X,Y) coordinate.
 *  s1     -  x value for the eye position.
 *  s2     -  y value for the eye position.
 *  s3     -  z value for the eye position.
 *  style  -  A style index defining the colors used to shade the
 *            surface as per:
 *
 *               1  -  wire frame
 *               2  -  gray shades underneath; gray shades on top.
 *               3  -  gray shades underneath; red shades on top.
 *               4  -  gray shades underneath; green shades on top.
 *               5  -  gray shades underneath; blue shades on top.
 *               6  -  gray shades underneath; cyan shades on top.
 *               7  -  gray shades underneath; magenta shades on top.
 *
 *            If style is positive, then a white backgound is used;
 *            if style is the negative of any of the above values, then
 *            a black background is used.
 */
    float *fz;
    int   i, j;
    float ang1=-35., ang2=25., rmul=2.9, shde=0.1, shdr=0.8;
    float xmin, xmax, ymin, ymax, zmin, zmax, xrng, yrng, zrng, 
          xmid, ymid, zmid;
    float p, q, r, xsl, ysl, zsl, xeye, yeye, zeye;
    Gcolr_rep colval;
/*
 *  Set foreground and background colors.
 */
    if (style >= 0) {
       colval.rgb.red = 1.;
       colval.rgb.green = 1.;
       colval.rgb.blue = 1.;
       gset_colr_rep(gkswid,0,&colval);
       colval.rgb.red = 0.;
       colval.rgb.green = 0.;
       colval.rgb.blue = 0.;
       gset_colr_rep(gkswid,1,&colval);
    }
    else {
       colval.rgb.red = 0.;
       colval.rgb.green = 0.;
       colval.rgb.blue = 0.;
       gset_colr_rep(gkswid,0,&colval);
       colval.rgb.red = 1.;
       colval.rgb.green = 1.;
       colval.rgb.blue = 1.;
       gset_colr_rep(gkswid,1,&colval);
    }

/*
 *  Find mins and maxs.
 */
    xmin = x[0];
    xmax = x[0];
    for (i = 1; i < nx; i++) {
      xmin = MIN(xmin,x[i]);
      xmax = MAX(xmax,x[i]);
    }
    ymin = y[0];
    ymax = y[0];
    for (j = 1; j < ny; j++) {
      ymin = MIN(ymin,y[j]);
      ymax = MAX(ymax,y[j]);
    }
    zmin = z[0];
    zmax = z[0];
    for (i = 1; i < nx * ny; i++) {
      zmin = MIN(zmin, z[i]);
      zmax = MAX(zmax, z[i]);
    }
    xrng = xmax-xmin;
    yrng = ymax-ymin;
    zrng = zmax-zmin;
    xmid = 0.5*(xmin+xmax);
    ymid = 0.5*(ymin+ymax);
    zmid = 0.5*(zmin+zmax);
 
    colval.rgb.red = 1.;
    colval.rgb.green = 0.;
    colval.rgb.blue = 0.;
    gset_colr_rep(gkswid,2,&colval);
    colval.rgb.red = 0.;
    colval.rgb.green = 1.;
    colval.rgb.blue = 0.;
    gset_colr_rep(gkswid,3,&colval);
    colval.rgb.red = 0.;
    colval.rgb.green = 0.;
    colval.rgb.blue = 1.;
    gset_colr_rep(gkswid,4,&colval);
    colval.rgb.red = 0.;
    colval.rgb.green = 1.;
    colval.rgb.blue = 1.;
    gset_colr_rep(gkswid,5,&colval);
    colval.rgb.red = 1.;
    colval.rgb.green = 0.;
    colval.rgb.blue = 1.;
    gset_colr_rep(gkswid,6,&colval);
    colval.rgb.red = 1.;
    colval.rgb.green = 1.;
    colval.rgb.blue = 0.;
    gset_colr_rep(gkswid,7,&colval);
    colval.rgb.red = 0.7;
    colval.rgb.green = 0.7;
    colval.rgb.blue = 0.7;
    gset_colr_rep(gkswid,8,&colval);
 
    for (i = 11; i < 43; i++) {
      p = 1. - (float) (i-11)/31.;
      q = 1. - shdr * (float) (i-11)/31.;
      colval.rgb.red = p;
      colval.rgb.green = p;
      colval.rgb.blue = p;
      gset_colr_rep(gkswid,i,&colval);
      colval.rgb.red = q;
      colval.rgb.green = q;
      colval.rgb.blue = q;
      gset_colr_rep(gkswid,i+32,&colval);
      colval.rgb.red = q;
      colval.rgb.green = shde*q;
      colval.rgb.blue = shde*q;
      gset_colr_rep(gkswid,i+64,&colval);
      colval.rgb.red = shde*q;
      colval.rgb.green = q;
      colval.rgb.blue = shde*q;
      gset_colr_rep(gkswid,i+96,&colval);
      colval.rgb.red = shde*q;
      colval.rgb.green = shde*q;
      colval.rgb.blue = q;
      gset_colr_rep(gkswid,i+128,&colval);
      colval.rgb.red = shde*q;
      colval.rgb.green = q;
      colval.rgb.blue = q;
      gset_colr_rep(gkswid,i+160,&colval);
      colval.rgb.red = q;
      colval.rgb.green = shde*q;
      colval.rgb.blue = q;
      gset_colr_rep(gkswid,i+192,&colval);
    }
 
/*
 *  Rearrange the array, since Tdpack expects an array ordered as 
 *  per Fortran.
 */
    fz = (float *) calloc(nx*ny,sizeof(float));
  
    for (i = 0; i < nx; i++) {
      for (j = 0; j < ny; j++) {
         fz[j*nx+i] = z[i*ny+j];
      }
    } 

/*
 *  Define plotting styles.
 */
    xsl = 0.05*xrng;
    ysl = 0.05*yrng;
    zsl = 0.00*zrng;
    c_tdstrs(1, -1, 0, -1, 0, -1, 1, 0, xsl, ysl, zsl);
    c_tdstrs(1, -1, 0, -1, 0, -1, 1, 0, xsl, ysl, zsl);
    c_tdstrs(2, 43,74, 43, 74, 1, 1, 0, xsl, ysl, zsl);
    c_tdstrs(3, 43,74, 75,106, 1, 1, 0, xsl, ysl, zsl);
    c_tdstrs(4, 43,74,107,138, 1, 1, 0, xsl, ysl, zsl);
    c_tdstrs(5, 43,74,139,170, 1, 1, 0, xsl, ysl, zsl);
    c_tdstrs(6, 43,74,171,202, 1, 1, 0, xsl, ysl, zsl);
    c_tdstrs(7, 43,74,203,234, 1, 1, 0, xsl, ysl, zsl);

/*
 *  Create the triangle list representing a surface.
 */
    ntri = 0;
    c_tdstri(x,nx,y,ny,fz,nx,&rtri[0][0],MTRI,&ntri,style);
    if (ntri == MTRI) {
      printf("Triangle list overflow in c_tdstri\n");
      exit(1);
    }
    free(fz);

/*
 *  Determine a default eye position if none is specified.
 */
    if ( (s1==0.) && (s2==0.) && (s3==0.) ) {
      r = rmul * (float) sqrt( (float) (xrng*xrng + yrng*yrng + zrng*zrng));
      xeye = xmid + r * (float) cos( (double) (DTOR*ang1)) * 
                (float) cos( (double) (DTOR*ang2));
      yeye = ymid + r * (float) sin( (double) (DTOR*ang1)) * 
                (float) cos( (double) (DTOR*ang2));
      zeye = zmid + r * (float) sin( (double) (DTOR*ang2));
    }
    else {
      xeye = s1;
      yeye = s2;
      zeye = s3;
    }

/*
 *  Initialize Tdpack.
 */
    c_tdinit (xeye, yeye, zeye, xmid, ymid, zmid,
              xmid, ymid, zmid+0.1*zrng, 0);
  
/*
 *  Order the triangles.
 */
    c_tdotri(&rtri[0][0], MTRI, &ntri, &rtwk[0][0], itwk, 1);
    if (ntri == MTRI) {
      printf("Triangle list overflow in c_tdotri\n");
      exit(1);
    }

/*
 *  Draw the triangles.
 */
    c_tddtri(&rtri[0][0], MTRI, &ntri, itwk);
	c_frame();
}


/*
 *  $Id: c_shex03.c,v 1.4 2003-05-22 17:24:47 haley Exp $
 */

#include <math.h>
#include <stdlib.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>
#include <ncarg/ngmath.h>

#define NX      21
#define NY      31
#define NZ      41
#define NI    1000
#define XMIN  -2.0
#define XMAX   2.0
#define YMIN  -2.0
#define YMAX   2.0
#define ZMIN  -2.0
#define ZMAX   2.0

#define IWTYPE 1
#define WKID   1

float dsrnd1();

/*
 *  Create a tube-like isosurface.
 */
main () 
{
  float xi[NI],yi[NI],zi[NI],ui[NI],xo[NX],yo[NY],zo[NZ],*ff;
  float *zor;
  int   i,j,k,ier;

  for (i = 0; i < NI; i++) {
    xi[i] = XMIN+(XMAX-(XMIN))*dsrnd1();    
    yi[i] = YMIN+(YMAX-(YMIN))*dsrnd1();    
    zi[i] = ZMIN+(ZMAX-(ZMIN))*dsrnd1();    
    ui[i] = 0.75*xi[i]*xi[i] - 1.6*yi[i]*yi[i] + 2.0*zi[i]*zi[i];
  }

  for (i = 0; i < NX; i++) {
    xo[i] = XMIN+((float)i/(float)(NX-1))*(XMAX-(XMIN));
  }
  for (j = 0; j < NY; j++) {
    yo[j] = YMIN+((float)j/(float)(NY-1))*(YMAX-(YMIN));
  }
  for (k = 0; k < NZ; k++) {
    zo[k] = ZMIN+((float)k/(float)(NZ-1))*(ZMAX-(ZMIN));
  }
 
  ff = c_shgrid(NI,xi,yi,zi,ui,NX,NY,NZ,xo,yo,zo,&ier);
  if (ier != 0) {
    printf("Error return from c_csa3s: %d\n",ier);
    exit(1);
  }

/*
 *  Rearrange the C array "ff" for input to c_tdez3d.
 */
  zor = (float *) calloc(NX*NY*NZ, sizeof(float));
  if (zor == NULL) {
    printf("Unable to allocate temp space\n");
    exit(1);
  }
  for (i = 0; i < NX; i++) {
    for (j = 0; j < NY; j++) {
      for (k = 0; k < NZ; k++) {
        zor[k*NX*NY + j*NX + i] = ff[i*NZ*NY + j*NZ + k];
      }
    }
  }

/*
 *  Draw plot.
 */
/*
 *  Open GKS, open and activate a workstation.
 */
  gopen_gks("stdout",0);
  gopen_ws(WKID, NULL, IWTYPE);
  gactivate_ws(WKID);
 
  c_tdez3d(NX, NY, NZ, xo, yo, zo, zor, 0.7, 2.3, -13., 75., 6);
  c_frame();

/*
 *  Deactivate and close workstation, close GKS.
 */
  gdeactivate_ws(WKID);
  gclose_ws(WKID);
  gclose_gks();
}

/*
 *  Portable random number generator.  This is used instead of 
 *  the rand entry in the Standard C Library for compaitbility 
 *  with the Fortran example.
 */
float dsrnd1()
{
#define MPLIER 16807
#define MODLUS 2147483647
#define MOBYMP 127773
#define MOMDMP 2836
#define JSEED  123456789

  int hvlue, lvlue, testv;
  static int nextn, ifrst = 0;

  if (ifrst == 0) {
    nextn = JSEED;
    ifrst = 1;
  }

  hvlue = nextn / MOBYMP;
  lvlue = nextn%MOBYMP;
  testv = MPLIER*lvlue - MOMDMP*hvlue;

  if (testv > 0) {
    nextn = testv;
  }
  else {
    nextn = testv + MODLUS;
  }

  return((float)nextn/(float) MODLUS);
}

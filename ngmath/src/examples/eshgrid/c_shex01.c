/*
 *  $Id: c_shex01.c,v 1.3 2003-05-22 17:24:47 haley Exp $
 */

#include <math.h>
#include <stdlib.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>
#include <ncarg/ngmath.h>

#define N        1331
#define NEAREST   500
#define FARTHER  N-NEAREST
#define IWTYPE 1
#define WKID   1

/*
 *  Define the size of the triangle list for plotting with Tdpack.
 */
#define MTRI 150000

float dsrnd1();

/*
 *  Test c_shgetnp in package Shgrid.
 */
main () 
{
  float x[N],y[N],z[N],px,py,pz,dotsize;
  int   i, ier, npts[NEAREST],mpts[FARTHER],ntri;

/*
 *  Declare the triangle-list array and a couple of temporary arrays to
 *  be used in sorting the list.
 */
  float rtri[MTRI][10],rtwk[2][MTRI];
  int   itwk[MTRI];

/*
 *  Generate an array of randomly-positioned points in the unit cube.
 */
  for (i = 0; i < N; i++) {
    x[i] = dsrnd1();    
    y[i] = dsrnd1();    
    z[i] = dsrnd1();    
  }

/*
 *  Specify the reference point from which we want to find the NEAREST
 *  nearest points.
 */
      px = 0.5;
      py = 0.5;
      pz = 0.5;

/*
 *  Plot the near and far points.
 */

/*
 *  Open GKS, open and activate a workstation.
 */
  gopen_gks("stdout",0);
  gopen_ws(WKID, NULL, IWTYPE);
  gactivate_ws(WKID);
 
  c_tdsetr("VPB", 0.09);
  c_tdsetr("VPT", 0.99);
  c_tdsetr("VPL", 0.11);
  c_tdsetr("VPR", 1.00);
  c_tdinit(4.6, 3.0, 3.3, 0.5, 0.5, 0.5, 0.5, 0.5, 2.7, 0.);

/*
 *  Set up some colors using the standard Tdpack entry for that.
 */
  c_tdclrs(WKID, 1, 0., 0.8, 8, 37, 8);

/*
 *  Define style indices for shades of gray, green, and red.
 */
  c_tdstrs(1,  8, 37,   8,  37, 1, 1, 0, 0.05, 0.05, 0.);
  c_tdstrs(3,  8, 37,  68,  97, 1, 1, 0, 0.05, 0.05, 0.);
  c_tdstrs(4,  8, 37,  98, 127, 1, 1, 0, 0.05, 0.05, 0.);

/*
 *  Store the indices of the nearest points in npts and the complement
 *  of that set (with respect to the entire input dataset) in mpts.
 */
  npts[0] = c_shgetnp(px,py,pz,N,x,y,z,0,&ier);
  for (i = 1; i < N; i++) {
    if (i < NEAREST) {
      npts[i] = c_shgetnp(px,py,pz,N,x,y,z,1,&ier);
    }
    else {
      mpts[i-NEAREST] = c_shgetnp(px,py,pz,N,x,y,z,1,&ier);
    }
  }

/*
 *  Plot the near points in green.
 */
  ntri = 0;
  dotsize = 0.02;
  for (i = 0; i < NEAREST; i++) {
    c_tdmtri(-5, x[npts[i]], y[npts[i]], z[npts[i]], dotsize,
             &rtri[0][0], MTRI, &ntri, 4, 0.,0.,0.,1.,1.,1.);
  }

/*
 *  Plot the farther points in gray.
 */
  for (i = 0; i < FARTHER; i++) {
    c_tdmtri(-5, x[mpts[i]], y[mpts[i]], z[mpts[i]], dotsize,
             &rtri[0][0], MTRI, &ntri, 1, 0.,0.,0.,1.,1.,1.);
  }

/*
 *  Mark the reference point in red.
 */
  c_tdmtri(-5,px,py,pz,1.2*dotsize,&rtri[0][0],MTRI,&ntri,3,0.,0.,0.,1.,1.,1.);

/*
 *  Draw;
 */
  c_tdotri(&rtri[0][0], MTRI, &ntri, &rtwk[0][0], itwk, 0);
  c_tddtri(&rtri[0][0], MTRI, &ntri, itwk);

/*
 *  Draw a box around the perimeter.
 */
  c_tdgrds(0., 1., 0., 1., 0., 1., -1., -1., -1.,11,0);
  c_tdgrds(0., 1., 0., 1., 0., 1., -1., -1., -1.,11,1);

/*
 *  Label the plot.
 */
  c_set(0.,1.,0.,1.,0.,1.,0.,1.,1);
  c_plchhq(0.5,0.95,":F26:Find the nearest N points in three space",
           0.025,0.,0.);
  c_plchhq(0.05,0.17,":F22:Red ball = reference point",0.02,0.,-1.);
  c_plchhq(0.05,0.12,":F22:Green balls = near points",0.02,0.,-1.);
  c_plchhq(0.05,0.07,":F22:Gray balls = far points",0.02,0.,-1.);

  c_frame();

/*
 *  Deactivate and close workstation, close GKS.
 */
  gdeactivate_ws(WKID);
  gclose_ws(WKID);
  gclose_gks();
}

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

/*
 *  $Id: c_ftex07.c,v 1.1 2002-08-03 00:37:30 fred Exp $
 */
#include <stdio.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>
#include <math.h>
#include <ncarg/ngmath.h>

void c_drw(int, float [], float [], int, float [], float [], 
                float [], float []);

/*
 *  This program illustrates the use of c_ftcurvs1 to
 *  interpolate a smoothing tension spline for data
 *  in the plane.
 */

/*
 *  Specify the number of original data points and number of points
 *  in the interpoaltion.
 */
#define IDIM   4
#define IOUT 101

/*
 *  Specify plot output parameters.
 */
#define IWTYPE 1
#define WKID   1

main()
{
/*
 *  Declare arrays.
 */
  float xinc, xo[IOUT], yo[IOUT], xoo[IOUT], yoo[IOUT];
  float d, sigma;
  int   dflg, ier;
 
/*
 *  Specify the original data points in the plane.
 */
  float x[] = { 0.5, -1.5,  0.5,  1.5};
  float y[] = { 1.5,  0.0, -2.5, -1.0};

/*
 *  Specify a uniform observational weight.
 */
  dflg = 1;
  d    = 0.2;

/*
 *  Tension factor.
 */
  c_ftsetr("sigma",1.);

/*
 *  Smoothing factor (larger values result in smoother curves).
 */
  c_ftseti("sf2",1);   /*  Flags use of user-set smoothing and eps.  */
  c_ftsetr("smt",(float) IDIM);

/*
 *  Computational tolerance value.
 */
  c_ftsetr("eps",sqrt(2./(float)IDIM));

/*
 *  Compute a smoothing spline.
 */
  ier = c_ftcurvs1(IDIM, x, y, dflg, &d, IOUT, 0., 1., xo, yo);
  if (ier != 0) {
    printf("\nc_ftcurvs1 - error %d in smoothing spline calculation",ier);
    exit(ier);
  }

/* 
 *  Now use c_ftcurvs1 to compute an interpolating tension
 *  spline by setting the smoothing parameter to zero.
 */ 
  c_ftsetr("smt",0.);
  c_ftsetr("eps",0.);
  ier = c_ftcurvs1(IDIM, x, y, dflg, &d, IOUT, 0., 1., xoo, yoo);
  if (ier != 0) {
    printf("\nc_ftcurvs1 - error %d in tension spline calculation",ier);
    exit(ier);
  }

/*
 *  Draw plot.
 */
  c_drw(IDIM, x, y, IOUT, xo, yo, xoo, yoo);
  
}
void c_drw(int n, float x[], float y[], int m, float xo[], float yo[],
           float xoo[], float yoo[])
{

  float x0, y0;

  Gcolr_rep   rgb;
  Gpoint      plist[2];
  Gpoint_list pmk;

/*
 *  Open GKS, open and activate a workstation.
 */
  gopen_gks("stdout",0);
  gopen_ws(WKID, NULL, IWTYPE);
  gactivate_ws(WKID);

/*
 *  Define a color table
 */
  rgb.rgb.red = rgb.rgb.green = rgb.rgb.blue = 1.;
  gset_colr_rep(WKID,0,&rgb);

  rgb.rgb.red = rgb.rgb.green = rgb.rgb.blue = 0.;
  gset_colr_rep(WKID,1,&rgb);

  rgb.rgb.red = 1.;
  rgb.rgb.green = rgb.rgb.blue = 0.;
  gset_colr_rep(WKID,2,&rgb);

  rgb.rgb.red = rgb.rgb.green = 0.;
  rgb.rgb.blue = 1.;
  gset_colr_rep(WKID,3,&rgb);

/*
 *  Main title.
 */
  c_pcseti("FN",25);
  c_plchhq(0.5,0.95,"Demo for c_ftcurvs1",30.,0.,0.);
  
/*
 *  Background.
 */
  c_set(0.1, 0.95, 0.2, 0.90, -2., 3., -3., 2., 1);
  c_gaseti("LTY",1);
  c_pcseti("FN",21);
  c_labmod("(F4.1)","(F4.1)",4,4,20,20,0,0,0);
  x0 = -2.;
  y0 = -3.;
  c_gridal(5,4,5,4,1,1,10,x0,y0);

/*
 *  Draw interpolated smoothing spline and informational label.
 */
  gset_line_colr_ind(2);
  gset_linewidth(3.);
  c_curve(xo,yo,m);
  plist[0].x = 0.250;
  plist[1].x = 0.750;
  plist[0].y = 0.625;
  plist[1].y = 0.625;
  pmk.num_points = 2;
  pmk.points = plist;
  gpolyline(&pmk);
  c_plchhq(0.875,0.625,"Smoothing spline",24.,0.,-1.);

/*
 *  Draw interpolated tension spline and informational label.
 */
  gset_line_colr_ind(3);
  gset_linewidth(3.);
  c_curve(xoo,yoo,m);
  plist[0].x = 0.250;
  plist[1].x = 0.750;
  plist[0].y = 0.125;
  plist[1].y = 0.125;
  pmk.num_points = 2;
  pmk.points = plist;
  gpolyline(&pmk);
  c_plchhq(0.875,0.125,"Tension spline",24.,0.,-1.);

/*
 *  Mark the input data points and draw informational label.
 */
  c_ngdots(x,y,n,0.13,1);
  x0 =  0.500;
  y0 = -0.375;
  c_ngdots(&x0,&y0,1,0.13,1);
  c_plchhq(0.875,-0.375,"Original data points",24.,0.,-1.);

  c_frame();

/*
 *  Deactivate and close workstation, close GKS.
 */
  gdeactivate_ws(WKID);
  gclose_ws(WKID);
  gclose_gks();
}

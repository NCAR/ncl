/*
 *  $Id: c_ftex04.c,v 1.3 2003-08-07 20:06:07 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>
#include <ncarg/ngmath.h>

void c_drwft4(int, float [], float [], int, 
              float [], float [], float [], float [], float []);
void c_bkgft4(float, float, float, float, float, float,
              char *, float, float, float, int);

/*
 *  Example of c_ftkurv and c_ftkurvd.
 */

#define IDIM  11
#define IOUT 201

#define IWTYPE 1
#define WKID   1

main()
{
  float x[] = {  3.0,  4.0,  9.0, 16.0, 21.0, 27.0, 
                34.0, 36.0, 34.0, 26.0, 18.0       };
  float y[] = {  2.4,  9.6, 14.4, 12.0,  9.6,  8.4,
                13.2, 21.6, 30.0, 37.2, 38.4       };
  float xinc, xo[IOUT], yo[IOUT], xs[IOUT], ys[IOUT],
              xd[IOUT], yd[IOUT], xdd[IOUT], ydd[IOUT],
              xp[IDIM], yp[IDIM], u[IOUT];
  float tinc;
  int   i;

/*
 *  Set up the array of parameter values where we want to obtain
 *  interpolated values.
 */
  tinc = 1./(IOUT-1.);
  for (i = 0; i < IOUT; i++) {
    u[i] = tinc * (float) i;
  }

/*
 *  Get the interpolated points.
 */
  c_ftkurv(IDIM, x, y, IOUT, u, xo, yo);

/*
 *  Get the derivatives (this returns the interpolsted values as well).
 */
  c_ftkurvd(IDIM, x, y, IOUT, u, xs, ys, xd, yd, xdd, ydd);

/*
 *  Draw plot.
 */
  c_drwft4(IDIM, x, y, IOUT, xo, yo, u, xd, yd);

}

void c_drwft4(int n, float x[], float y[], int m, 
              float xo[], float yo[], float u[], float xd[], float yd[])
{
  int i;

  Gcolr_rep rgb;
  Gpoint plist[IDIM];
  Gpoint_list pmk;

/*
 *  Open GKS, open and activate a workstation.
 */
  gopen_gks("stdout",0);
  gopen_ws(WKID, NULL, IWTYPE);
  gactivate_ws(WKID);

/*
 * Define a color table
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
 * Graph the interpolated function values and mark the original
 * input data points.
 */
  c_bkgft4(0.,40.,0.,40.,0.15,0.85,"Demo for c_ftkurv",0.03,0.5,0.93,0);
  c_gridal(4,5,4,5,1,1,10,0.,0.);
  c_curve(xo,yo,m);

/*
 *  Mark the input data points.
 */
  for (i = 0; i < n; i++) {
    plist[i].x = x[i];
    plist[i].y = y[i];
  }
  gset_marker_size(2.);
  gset_marker_colr_ind(3);
  pmk.num_points = n;
  pmk.points = plist;
  gpolymarker(&pmk);
  c_frame();

/*
 *  Plot the first derivatives.
 */
  c_set(0.,1.,0.,1.,0.,1.,0.,1.,1);
  c_pcseti("fn",21);
  c_plchhq(0.5,0.95,"Derivatives from c_ftkurvd",0.035,0.,0.);
  c_bkgft4(0.,1.,-80.,80.,0.55,0.87,"dx/du",0.030,0.65,0.82,1);
  c_gridal(5,5,4,5,1,1,10,0.,-80.);
  c_curve(u,xd,m);
  c_bkgft4(0.,1.,-40.,80.,0.1,0.42,"dy/du",0.030,0.39,0.37,1);
  c_gridal(5,5,3,5,1,1,10,0.,-40.);
  c_curve(u,yd,m);
  c_frame();

/*
 *  Deactivate and close workstation, close GKS.
 */
  gdeactivate_ws(WKID);
  gclose_ws(WKID);
  gclose_gks();
}

void c_bkgft4(float xleft, float xright, float ybot, float ytop,
              float ypos_bot, float ypos_top, char *label, float lab_size,
              float lab_posx, float lab_posy, int zero_line) {
  c_set(0.,1.,0.,1.,0.,1.,0.,1.,1);
  c_pcseti("fn",21);
  c_plchhq(lab_posx,lab_posy,label,lab_size,0.,0.0);
  c_set(0.17,0.87,ypos_bot,ypos_top,xleft,xright, ybot, ytop, 1);
  if (zero_line != 0) {
     gset_line_colr_ind(2);
     c_line(xleft, 0., xright, 0.);
     c_sflush();
     gset_line_colr_ind(1);
  }
  c_gaseti("lty",1);
  c_pcseti("fn",21);
  c_gasetr("xls",0.02);
  c_gasetc("xlf","(f4.1)");
  c_gasetr("yls",0.02);
  c_gasetc("ylf","(f5.1)");
  c_gasetr("xmj",0.02);
  c_gasetr("ymj",0.02);
}

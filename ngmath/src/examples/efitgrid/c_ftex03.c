/*
 *  $Id: c_ftex03.c,v 1.4 2003-08-07 20:06:07 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>
#include <ncarg/ngmath.h>

void c_drwft3(float, float, int, float [], float [],
              int, float [], float [], float []);
void c_bkgft3(float, float, float, char *, float, float, float);

/*
 *  Example of c_ftcurvs and c_ftcurvps.
 */

#define IDIM  10
#define IOUT 201

#define IWTYPE 1
#define WKID   1

main()
{
  float x[] = { 0.000, 0.210, 0.360, 0.540, 1.000,
                1.500, 1.970, 2.300, 2.500, 2.700};
  float y[] = { 0.000, 2.600, 3.000, 2.500, 0.000,
               -1.000, 0.000, 0.800, 0.920, 0.700};
  float xinc, xo[IOUT], yos[IOUT], yosp[IOUT];
  float p = 3., d, xr = 5., xl = -1.;
  int   i;

/*
 *  Create the output X coordinate array.
 */
  xinc =  (xr-xl)/(IOUT-1);
  for (i = 0; i < IOUT; i++) {
    xo[i] = xl + xinc*i;
  }

/*
 *  Calculate the interpolated values.
 */
  d = 0.3;
  c_ftcurvs(IDIM, x, y, 1, &d, IOUT, xo, yos);
/*
 *  Calculate the interpolated values for a periodic function.
 */
  c_ftcurvps(IDIM, x, y, p, 1, &d, IOUT, xo, yosp);

/*
 *  Draw plot.
 */
  c_drwft3(xl, xr, IDIM, x, y, IOUT, xo, yos, yosp);
  
}
void c_drwft3(float xl, float xr, int n, float x[], float y[],
              int m, float xo[], float yos[], float yosp[])
{

  float ypos_top = 0.95, yb, yt;
  int   i;

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

  gset_clip_ind(GIND_CLIP);

/*
 * Graph the interpolated function values and mark the original
 * input data points.
 */
  yb = -2.0;
  yt =  4.0;
  c_bkgft3(xl,xr,ypos_top,"c_curvs",0.4,yb,yt);
  c_gridal(6,5,3,1,1,1,10,xl,yb);
  c_curve(xo,yos,m);

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

/*
 *  Graph the periodic function.
 */
  c_bkgft3(xl,xr,ypos_top-0.47,"c_curvps",0.4,yb,yt);
  c_gridal(6,5,3,1,1,1,10,xl,yb);
  c_curve(xo,yosp,m);

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
 *  Deactivate and close workstation, close GKS.
 */
  gdeactivate_ws(WKID);
  gclose_ws(WKID);
  gclose_gks();
}

void c_bkgft3(float xleft, float xright, float ypos, char *label, 
              float xlp, float yb, float yt) {
  c_set(0.,1.,0.,1.,0.,1.,0.,1.,1);
  c_pcseti("fn",21);
  c_plchhq(xlp,ypos-0.03,label,0.035,0.,-1.0);
  c_set(0.13,0.93,ypos-0.35,ypos,xleft,xright, yb, yt, 1);
  gset_line_colr_ind(2);
  c_line(xleft, 0., xright, 0.);
  c_sflush();
  gset_line_colr_ind(1);
  c_gaseti("lty",1);
  c_pcseti("fn",21);
  c_gasetr("xls",0.02);
  c_gasetc("xlf","(f4.1)");
  c_gasetr("yls",0.02);
  c_gasetc("ylf","(f5.2)");
  c_gasetr("xmj",0.02);
  c_gasetr("ymj",0.02);
}

/*
 *  $Id: c_ftex02.c,v 1.4 2003-08-07 20:06:07 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>
#include <ncarg/ngmath.h>

void c_bkgft2(float, float, float, char *, float, float, float);
void c_drwft2(float, float, int, float [], float [], int,
              float [], float [], float []);
void c_drwprd(float, float, float);

/*
 *  Example of c_ftkurvp and c_ftkurvpi.
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
  float xinc, xo[IOUT], yo[IOUT], yi[IOUT];
  float period = 3., xr = 5., xl = -1.;
  int   i;

/*
 *  Create the output X coordinate array.
 */
  xinc =  (xr-xl)/(IOUT-1);
  for (i = 0; i < IOUT; i++) {
    xo[i] = xl + xinc*i;
  }

/*
 *  Calculate the interpolated values and the integral.
 */
  c_ftcurvp(IDIM, x, y, period, IOUT, xo, yo);
  for (i = 0; i < IOUT; i++) {
    c_ftcurvpi(0., xo[i], period, IDIM, x, y, yi+i);
  }

/*
 *  Draw plot.
 */
  c_drwft2(xl,xr,IDIM,x,y,IOUT,xo,yo,yi);

}

void c_drwft2(float xl,float xr, int n, float x[], float y[], 
              int m, float xo[], float yo[], float yi[])
{

  int   i;
  float yb, yt, ypos_top = 0.85;
 
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
 * Plot the main title.
 */
  gset_clip_ind(GIND_NO_CLIP);
  c_plchhq(.5,.95,":F25:Demo for c_ftcurvp, c_ftcurvpi",0.03,0.,0.);

/*
 * Graph the interpolated function values and mark the original
 * input data points.
 */
  yb = -2.0;
  yt =  3.0;
  c_bkgft2(xl,xr,ypos_top,"Function",0.42,yb,yt);
  c_gridal(6,5,5,1,1,1,10,xl,yb);
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

/*
 *  Graph the integral.
 */
  yb = -1.0;
  yt =  4.0;
  c_bkgft2(xl,xr,ypos_top-0.47,"Integral (from X = 0.)",0.2,yb,yt);
  c_gridal(6,5,5,1,1,1,10,xl,yb);
  c_curve(xo,yi,m);

/*
 *  Indicate the period.
 */
  c_drwprd(0.,3.,6.5);
  
  c_frame();
/*
 *  Deactivate and close workstation, close GKS.
 */
  gdeactivate_ws(WKID);
  gclose_ws(WKID);
  gclose_gks();
}

void c_bkgft2(float xleft, float xright, float ypos, char *label, 
              float xlp, float yb, float yt) {
  c_set(0.,1.,0.,1.,0.,1.,0.,1.,1);
  c_pcseti("fn",21);
  c_plchhq(xlp,ypos-0.03,label,0.025,0.,-1.0);
  c_set(0.13,0.93,ypos-0.25,ypos,xleft,xright, yb, yt, 1);
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

void c_drwprd(float xl, float xr, float y)
{
/*
 *  Draws a bounding indicator for the period of the function.
 */
  float xx[2], yy[2], xmid, yoff = 0.4, yi, xb, xe;

  gset_line_colr_ind(2);
  xmid = 0.5*(xr-xl);
  c_plchhq(xmid,y,":F25:Period",0.02,0.,0.);

/*
 *  Vertical lines at the period limits.
 */
  c_line(xl, y+yoff, xl, y-yoff);
  c_line(xr, y+yoff, xr, y-yoff);

/*
 *  Horizontal lines between label and vertical lines.
 */
  c_pcseti("te",1);
  c_pcgetr("xb",&xb);
  c_pcgetr("xe",&xe);
  c_line(xl, y, c_cfux(xb)-0.09, y);

/*
 *  Left arrow.
 */
  yi = 0.5*yoff;
  c_line(xl, y, xl+yi, y+yi);
  c_line(xl, y, xl+yi, y-yi);

  c_line(xr, y, c_cfux(xe)+0.09, y);
  
/*
 *  Right arrow.
 */
  c_line(xr, y, xr-yi, y+yi);
  c_line(xr, y, xr-yi, y-yi);
}

/*
 *  $Id: c_ftex01.c,v 1.5 2003-08-07 20:06:07 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>
#include <ncarg/ngmath.h>

void c_bkgft1(float, char *, float, float);
void c_drwft1(int, float [], float [], int, float [], float [], 
              float [], float []);

/*
 *  Example of c_ftcurvd, c_ftcurvd, and c_ftcurvi.
 */

#define IDIM  11
#define IOUT 201

#define IWTYPE 1
#define WKID   1

main()
{
  float x[] = { 0.00,   2.00,   5.00,   8.00,  10.00,  13.00,
               15.00,  18.00,  21.00,  23.00,  30.00};
  float y[] = { 1.00,   0.81,   0.00,  -0.81,  -1.00,  -0.84,
               -0.56,   0.04,   0.73,   1.18,   2.00};
  float xinc, xo[IOUT], yo[IOUT], yd[IOUT], yi[IOUT];
  int   i;

/*
 *  Create the output X coordinate array.
 */
  xinc =  30./(IOUT-1);
  for (i = 0; i < IOUT; i++) {
    xo[i] = xinc * i;
  }

/*
 *  Require that the derivatives of the interpolated curve are
 *  zero at the end points.
 */
  c_ftseti("sf1",   0);
  c_ftsetr("sl1", 0.0);
  c_ftsetr("sln", 0.0);

/*
 *  Calculate the interpolated values, the derivative, and the integral.
 */
  c_ftcurv(IDIM, x, y, IOUT, xo, yo);
  c_ftcurvd(IDIM, x, y, IOUT, xo, yd);
  for (i = 0; i < IOUT; i++) {
    c_ftcurvi(0., xo[i], IDIM, x, y, yi+i);
  }

/*
 *  Draw plot.
 */
  c_drwft1(IDIM,x,y,IOUT,xo,yo,yd,yi);

}

void c_drwft1(int n, float x[], float y[], int m, float xo[], float yo[], 
              float yd[], float yi[])
{

  int   i;
  float yb, yt, ypos_top = 0.88;

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
  c_plchhq(.5,.95,":F25:Demo for c_ftcurv, c_ftcurvd, c_ftcurvi",0.03,0.,0.);

/*
 * Graph the interpolated function values and mark the original
 * input data points.
 */
  yb = -1.0;
  yt =  2.0;
  c_bkgft1(ypos_top,"Function",yb,yt);
  c_gridal(6,5,3,1,1,1,10,0.0,yb);
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
 *  Graph the derivatives.
 */
  yb = -0.3;
  yt =  0.3;
  c_bkgft1(ypos_top-0.3,"Derivative",yb,yt);
  c_gridal(6,5,3,1,1,1,10,0.0,yb);
  c_curve(xo,yd,m);

/*
 *  Graph the integral.
 */
  yb = -6.0;
  yt = 10.0;
  c_bkgft1(ypos_top-0.6,"Integral",yb,yt);
  c_gridal(6,5,4,1,1,1,10,0.0,yb);
  c_curve(xo,yi,m);
  
  c_frame();
/*
 *  Deactivate and close workstation, close GKS.
 */
  gdeactivate_ws(WKID);
  gclose_ws(WKID);
  gclose_gks();
}

void c_bkgft1(float ypos, char *label, float yb, float yt) {
  c_set(0.,1.,0.,1.,0.,1.,0.,1.,1);
  c_pcseti("fn",21);
  c_plchhq(.2,ypos-0.03,label,0.025,0.,-1.0);
  c_set(0.13,0.93,ypos-0.2,ypos,0.0,30.0, yb, yt, 1);
  gset_line_colr_ind(2);
  c_line(0.,0.,30.,0.); 
  c_sflush();
  gset_line_colr_ind(1);
  c_gaseti("lty",1);
  c_pcseti("fn",21);
  c_gasetr("xls",0.02);
  c_gasetc("xlf","(i3)");
  c_gasetr("yls",0.02);
  c_gasetc("ylf","(f5.1)");
  c_gasetr("xmj",0.02);
  c_gasetr("ymj",0.02);
}

/*
 *  $Id: c_csex03.c,v 1.1 1998-12-10 00:09:08 fred Exp $
 */

#include <stdio.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>
#include <ncarg/ngmath.h>

void c_bkgft1(float, char *, float, float);
void c_drwft1(int, float [], float [], int, float [], float [], 
              float []);

#define NDATA   9
#define NPTS  101
#define IWTYPE  1
#define WKID    1

/*
 *  Demo of extrapolation into data sparse regions using c_csa1xs.
 */
main () 
{
  float xo[NPTS],yo1[NPTS],yo2[NPTS],xinc;

/*
 *  Specify the input data and initial weighting array.
 */
  float  xi[] = {0.0, 0.1, 0.2,  0.3,  0.4, 0.5, 0.6, 0.9, 1.0};
  float  yi[] = {0.0, 0.2, 0.4, -0.4, -1.0, 0.2, 0.5, 0.4, 0.0};

  float smth=0.,wts=-1.;
  int i,knots=8,nderiv=0,ier;

/*
 *  Specify the output coordinates.
 */
  xinc = 1./ (float) (NPTS-1);
  for (i = 0; i < NPTS; i++) {
    xo[i] = (float) i * xinc;
  }

/*
 *  Calculate the approximating curve with no extrapolation.
 */
  ier = c_csa1xs(NDATA,xi,yi,&wts,knots,smth,nderiv,NPTS,xo,yo1);
  if (ier != 0) {
    printf("Error return from c_csa1xs: %d\n",ier);
    exit(1);
  }

/*
 *  Calculate the approximating curve with extrapolation into
 *  data sparse regions.
 */
  smth = 1.;
  ier = c_csa1xs(NDATA,xi,yi,&wts,knots,smth,nderiv,NPTS,xo,yo2);
  if (ier != 0) {
    printf("Error return from c_csa1xs: %d\n",ier);
    exit(1);
  }

/*
 *  Draw plot.
 */
  c_drwft1(NDATA,xi,yi,NPTS,xo,yo1,yo2);

}

void c_drwft1(int n, float x[], float y[], int m, float xo[], 
              float curve1[], float curve2[])
{

  int   i;
  float yb, yt, ypos_top = 0.86;

  Gcolr_rep rgb;
  Gpoint plist[NDATA];
  Gpoint_list pmk;

/*
 *  Open GKS, open and activate a workstation.
 */
  gopen_gks("stdout",0);
  gopen_ws(WKID, NULL, IWTYPE);
  gactivate_ws(WKID);

/*
 * Define a color table.
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
  gset_clip_ind(0);
  c_plchhq(.5,.95,":F21:Data sparse areas",0.03,0.,0.);

/*
 *  Graph the approximation curve where all input coordinates were
 *  wieghted equally.
 */
  yb = -2.0;
  yt =  1.0;
  c_bkgft1(ypos_top,"smth = 0.",yb,yt);
  c_gridal(5,5,3,1,1,1,10,0.0,yb);
  c_curve(xo,curve1,m);

/*
 *  Mark the input data points.
 */
  for (i = 0; i < n; i++) {
    plist[i].x = x[i];
    plist[i].y = y[i];
  }
  gset_marker_size(2.2);
  gset_marker_colr_ind(3);
  pmk.num_points = n;
  pmk.points = plist;
  gpolymarker(&pmk);

/*
 *  Graph the approximation curve where the second input coordinate was
 *  given a weight of 0.5 .
 */
  c_bkgft1(ypos_top-0.45,"smth = 1.",yb,yt);
  c_gridal(5,5,3,1,1,1,10,0.0,yb);
  c_curve(xo,curve2,m);
  gpolymarker(&pmk);

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
  c_plchhq(0.17,ypos-0.2,label,0.03,0.,-1.0);
  c_set(0.13,0.93,ypos-0.28,ypos,0.0,1., yb, yt, 1);
  gset_line_colr_ind(2);
  c_line(0.,0.,1.,0.); 
  c_sflush();
  gset_line_colr_ind(1);
  c_gaseti("lty",1);
  c_pcseti("fn",21);
  c_gasetr("xls",0.02);
  c_gasetc("xlf","(f3.1)");
  c_gasetr("yls",0.02);
  c_gasetc("ylf","(f5.1)");
  c_gasetr("xmj",0.02);
  c_gasetr("ymj",0.02);
}

/*
 *  $Id: c_csex01.c,v 1.4 2003-05-22 17:24:18 haley Exp $
 */

#include <stdio.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>
#include <ncarg/ngmath.h>

/*
 *  Function prototypes for plotting backgrounds and curves.
 */
void c_bkgft1(float, char *, float, float);
void c_drwft1(int, float [], float [], int, float [], float [], 
              float [], float []);

/*
 *  The number of input data points.
 */
#define NDATA  10

/*
 *  The number of output data points.
 */
#define NPTS  101

/*
 *  The GKS workstation type (NCGM).
 */
#define IWTYPE 1

/*
 *  The GKS workstaton identifier.
 */
#define WKID    1

/*
 * This example illustrates the effects of using differing numbers
 * of knots in calls to c_csa1s with the same input data.
 */
main () 
{

/*
 *  Set up the output arrays.
 */
  float xo[NPTS],*yo4,*yo7,*yo9,xinc;

/*
 *  Define the input data.
 */
  float xi[] = {0.0, 0.1,  0.2,  0.3, 0.5,  0.6, 0.65,  0.8,  0.9, 1.};
  float yi[] = {0.0, 0.8, -0.9, -0.9, 0.9,  1.0, 0.90, -0.8, -0.8, 0.};

  int i,knots,ier;

/*
 *  Create the output X coordinate array.
 */
  xinc = 1./ (float) (NPTS-1);
  for (i = 0; i < NPTS; i++) {
    xo[i] = (float) i * xinc;
  }

/*
 *  Calculate the approximated function values using differing
 *  number of knots.
 */
  knots = 4;
  yo4 = c_csa1s(NDATA,xi,yi,knots,NPTS,xo,&ier);
  if (ier != 0) {
    printf("Error return from c_csa1s: %d\n",ier);
    exit(1);
  }
  knots = 7;
  yo7 = c_csa1s(NDATA,xi,yi,knots,NPTS,xo,&ier);
  if (ier != 0) {
    printf("Error return from c_csa1s: %d\n",ier);
    exit(1);
  }
  knots = 9;
  yo9 = c_csa1s(NDATA,xi,yi,knots,NPTS,xo,&ier);
  if (ier != 0) {
    printf("Error return from c_csa1s: %d\n",ier);
    exit(1);
  }
  
/*
 *  Draw plot.
 */
  c_drwft1(NDATA,xi,yi,NPTS,xo,yo4,yo7,yo9);

}

void c_drwft1(int n, float x[], float y[], int m, float xo[], 
              float curve1[], float curve2[], float curve3[])
{
/*
 *  This function uses NCAR Graphics to plot three curves on
 *  the same picture showing the results from calling c_csa1x with
 *  differing number of knots.  The values for the curves are
 *  contained in arrays curve1, curve2, and curve3.
 */
  int   i;
  float yb, yt, ypos_top = 0.88;

/*
 *  Declare variables used in GKS calls.
 */
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
  gset_clip_ind(GIND_NO_CLIP);
  c_plchhq(.5,.95,":F21:Demo for c_csa1s",0.035,0.,0.);

/*
 *  Draw a background grid for the first curve.
 */
  yb = -1.2;
  yt =  1.2;
  c_bkgft1(ypos_top,"knots = 4",yb,yt);
  c_gridal(5,5,4,1,1,1,10,0.0,yb);

/*
 *  Graph the approximated function values for knots=4.
 */
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
 *  Graph the approximated function values for knots=7.
 */
  yb = -1.2;
  yt =  1.2;
  c_bkgft1(ypos_top-0.3,"knots = 7",yb,yt);
  c_gridal(5,5,4,1,1,1,10,0.0,yb);
  c_curve(xo,curve2,m);

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
 *  Graph the approximated function values for knots=9.
 */
  yb = -1.2;
  yt =  1.2;
  c_bkgft1(ypos_top-0.6,"knots = 9",yb,yt);
  c_gridal(5,5,4,1,1,1,10,0.0,yb);
  c_curve(xo,curve3,m);

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
  
  c_frame();

/*
 *  Deactivate and close workstation, close GKS.
 */
  gdeactivate_ws(WKID);
  gclose_ws(WKID);
  gclose_gks();
}

/*
 *  Draw a background.
 */
void c_bkgft1(float ypos, char *label, float yb, float yt) {
  c_set(0.,1.,0.,1.,0.,1.,0.,1.,1);

/*
 *   Plot the curve label using font 21 (Helvetica).
 */
  c_pcseti("fn",21);
  c_plchhq(.25,ypos-0.03,label,0.025,0.,-1.0);

/*
 *  Draw a horizontal line at Y=0. using color index 2.
 */
  c_set(0.13,0.93,ypos-0.2,ypos,0.0,1., yb, yt, 1);
  gset_line_colr_ind(2);
  c_line(0.,0.,1.,0.); 
  c_sflush();
  gset_line_colr_ind(1);

/*
 *  Set Gridal parameters.
 *
 *
 *   Set lty to indicate that the Plotchar routine PLCHHQ should be used.
 */
  c_gaseti("lty",1);

/*
 *   Size and format for X axis labels.
 */
  c_gasetr("xls",0.02);
  c_gasetc("xlf","(f3.1)");

/*
 *   Size and format for X axis labels.
 */
  c_gasetr("yls",0.02);
  c_gasetc("ylf","(f5.1)");

/*
 *  Length of major tick marks for the X and Y axes.
 */
  c_gasetr("xmj",0.02);
  c_gasetr("ymj",0.02);
}

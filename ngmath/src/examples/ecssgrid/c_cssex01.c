#
#       $Id: c_cssex01.c,v 1.3 1999-05-25 18:14:32 fred Exp $
#

#include <stdio.h>
#include <math.h>

/*
 * Function prototypes for ngmath functions.
 */
#include <ncarg/ngmath.h>

/*
 *  Function prototype for the function that draws label boxes.
 */
void c_draw_box(float, float, float, float, int);

/*
 * Function prototypes for NCARG and NCARG-GKS routines
 */
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define IWTYPE 8
#define WKID   1

/*
 *  Number of input data coordinates.
 */
#define N      7

/*
 *  Number of points to be used in drawing arcs.
 */
#define NARC  50

/*
 *  Constants for converting between radians and degrees.
 */
#define  D2R   0.017453293
#define  R2D  57.295780000

main()
{
  int i, j, k, np, ns, indx;
  Gcolr_rep rgb;

  float xc[2*N], yc[2*N], zc[2*N], rc[2*N];
  float rlat1, rlon1, rlat2, rlon2, pnm, rcd;
  int   numv=0, nca, nv[2*N];

/*
 *  Example of Delaunay triangulation and Voronoi diagram
 *  on the surface of a sphere.
 */

/*
 *  Input dataset on the globe (latitudes and longitudes in degrees).
 *  These data points do not cover the globe, but rather are confined
 *  to the nothern hemisphere and are enclosed by a boundary.
 */
  float rlat[N] = {  70.,  70., 70., 85., 60., 60., 65.};
  float rlon[N] = {-160., -70.,  0., 20., 50., 80.,140.};

/*
 *  Declare the arrays for holding the points for drawing
 *  circular arcs.
 */
  float arclat[NARC], arclon[NARC];

/*
 *  Arrays for Cartesian coordinates of input data.
 */
  float x[N], y[N], z[N];

/*
 *  Pointer to array of triangle indices; number of triangles; error value.
 */
  int *ltri, nt, ier;

/*
 *  c_supmap arguments.
 */
  float plm1[2] = {0., 0.};
  float plm2[2] = {0., 0.};
  float plm3[2] = {0., 0.};
  float plm4[2] = {0., 0.};

/*
 *  Set x and y to the values of rlon and rlat, respectively,
 *  in radians.
 */
  for (k = 0; k < N; k++) {
    x[k] = D2R*rlon[k];
    y[k] = D2R*rlat[k];
  }

/*
 *  Transform the spherical coordinates X and Y to Cartesian
 *  coordinates (X,Y,Z) on the unit sphere (X**2 +  Y**2 + Z**2 = 1).
 */
  c_cstrans(N, y, x, x, y, z);

/*
 *  Create the triangulation.
 */
  ltri = c_csstri(N, x, y, z, &nt, &ier);

/*
 *  Get the circumcenters of the Delaunay triangles.
 */
  c_csvoro(N, x, y, z, 0, 1, xc, yc, zc, rc, &nca, &numv, nv, &ier);

/*
 *  Plot the Delaunay triangulation, the circumcircles, and
 *  the Voronoi polygons on a sphere.
 *
 */

/*
 *  Open GKS, open and actibvate a workstation.
 */
  gopen_gks ("stdout",0);
  gopen_ws (WKID, NULL, IWTYPE);
  gactivate_ws(WKID);

/*
 *  Color table.
 */
  rgb.rgb.red = 0.; rgb.rgb.green = 0.; rgb.rgb.blue = 0.;
  gset_colr_rep(WKID,0,&rgb);
  rgb.rgb.red = 1.; rgb.rgb.green = 1.; rgb.rgb.blue = 1.;
  gset_colr_rep(WKID,1,&rgb);
  rgb.rgb.red = 0.; rgb.rgb.green = 1.; rgb.rgb.blue = 1.;
  gset_colr_rep(WKID,2,&rgb);
  rgb.rgb.red = 1.; rgb.rgb.green = 1.; rgb.rgb.blue = 0.;
  gset_colr_rep(WKID,3,&rgb);
  rgb.rgb.red = 1.; rgb.rgb.green = 0.; rgb.rgb.blue = 0.;
  gset_colr_rep(WKID,4,&rgb);
  rgb.rgb.red = 0.; rgb.rgb.green = 1.; rgb.rgb.blue = 0.;
  gset_colr_rep(WKID,5,&rgb);
  rgb.rgb.red = 0.; rgb.rgb.green = .8; rgb.rgb.blue = 0.;
  gset_colr_rep(WKID,6,&rgb);

/*
 *  Draw a map of the North Atlantic, as seen by a satellite.
 */
  gset_line_colr_ind(6);
  c_mapstr ("SA", 4.);
  c_mappos (0.175, 0.975, 0.025, 0.825);
  c_supmap (7, 72.5, 127.5, 0., plm1, plm2, plm3, plm4, 1,
            -1000, 5, 0, &ier);

/*
 *  Plot the circumcircles whose circumcenters lie in one of
 *  the Delaunay triangles composed of original data points
 *  (exclude the pseudo points added to complete the
 *  triangulation on the whole sphere).
 */
  gset_linewidth(2.0);
  gset_line_colr_ind(4);
  for (i = 4; i < nca; i++) {
    c_csscoord(xc[i], yc[i], zc[i], &rlat1, &rlon1, &pnm);
    rlat2 = R2D*rlat1;
    rlon2 = R2D*rlon1;
    rcd   = R2D*rc[i];

    c_nggcog(rlat2, rlon2, rcd, arclat, arclon, NARC);
    c_mapit (arclat[0], arclon[0], 0);
    for (k = 1; k < NARC-1; k++) {
      c_mapit (arclat[k], arclon[k], 1);
    }
    c_mapit (arclat[NARC-1], arclon[NARC-1], 1);
    c_mapiq();
  }

/*
 *  Draw the Voronoi polygons (note that c_csvcoro has already
 *  been call once above with no intervening call that would
 *  change the work arrays).
 */
  for (i = 0; i < N; i++) {
    c_csvoro(N, x, y, z, i, 0, xc, yc, zc, rc, &nca, &numv, nv, &ier);
    for (j = 1; j < numv; j++) {
      c_csscoord(xc[nv[j-1]], yc[nv[j-1]], zc[nv[j-1]],
                 &rlat1, &rlon1, &pnm);
      c_csscoord(xc[nv[j]], yc[nv[j]], zc[nv[j]],
                 &rlat2, &rlon2, &pnm);
      rlat1 = R2D*rlat1;
      rlon1 = R2D*rlon1;
      rlat2 = R2D*rlat2;
      rlon2 = R2D*rlon2;

      gset_line_colr_ind(3);
      c_mapgci(rlat1, rlon1, rlat2, rlon2, NARC, arclat, arclon); 
      c_mapit (rlat1, rlon1, 0);
      for (k = 0; k < NARC; k++) {
        c_mapit (arclat[k], arclon[k], 1);
      }
      c_mapit (rlat2, rlon2, 1);
      c_mapiq();
    }
  }
 
/*
 *  Draw the Delaunay triangles.
 */
  gset_linewidth(2.0);
  gset_line_colr_ind(2);
  for (np = 0; np < nt; np++) {
    indx = 3*np;
    for (ns = 0; ns < 3; ns++) {
      c_mapgci(rlat[ltri[indx+ns]], rlon[ltri[indx+ns]], 
               rlat[ltri[indx+(ns+1)%3]], rlon[ltri[indx+(ns+1)%3]],
               NARC, arclat, arclon);
      c_mapit (rlat[ltri[indx+ns]], rlon[ltri[indx+ns]], 0);
      for (i = 0; i < NARC; i++) {
        c_mapit (arclat[i], arclon[i], 1);
      }
      c_mapit (rlat[ltri[indx+(ns+1)%3]], rlon[ltri[indx+(ns+1)%3]], 1);
      c_mapiq();
    }
  }

/*
 *  Mark the original data points (create a circular
 *  marker by drawing five concentric circles).
 */
  gset_line_colr_ind(3);
  for (k = 1; k < 6; k++) {
    for (i = 0; i < N; i++) {
      c_nggcog (rlat[i], rlon[i], 0.2 * (float) k, arclat, arclon, NARC);      
      c_mapit (arclat[0], arclon[0], 0);
      for (j = 1; j < NARC-1; j++) {
        c_mapit (arclat[j], arclon[j], 1);
      }
      c_mapit (arclat[NARC-1], arclon[NARC-1], 1);
      c_mapiq();
    }
  }

/*
 *  Mark the circumcircle centers.
 */
  gset_line_colr_ind(5);
  for (k = 0; k < 5; k++) {
    for (i = 4; i < 10; i++) {
      c_csscoord(xc[i], yc[i], zc[i], &rlat2, &rlon2, &pnm);
      rlat1 = R2D*rlat2;
      rlon1 = R2D*rlon2;
      c_nggcog (rlat1, rlon1, 0.1 * (float) k, arclat, arclon, NARC);
      c_mapit (arclat[0], arclon[0], 0);
      for (j = 1; j < NARC-1; j++) {
        c_mapit (arclat[j], arclon[j], 1);
      }
      c_mapit (arclat[NARC-1], arclon[NARC-1], 1);
      c_mapiq();
    }
  }

/*
 *  Legend.
 */
  c_set(0.,1.,0.,1.,0.,1.,0.,1.,1);
  c_draw_box(0.02, 0.945, 0.12, 0.955, 2);
  c_pcseti("CC",1);
  c_plchhq(0.14,0.95,":F22:Delaunay triangles",.025,0.,-1.0);
  c_draw_box(0.02, 0.895, 0.12, 0.905, 3);
  c_plchhq(0.14,0.90,":F22:Voronoi polygons",.025,0.,-1.0);
  c_draw_box(0.02, 0.845, 0.12, 0.855, 4);
  c_plchhq(0.14,0.85,":F22:Circumcircles",.025,0.,-1.0);

  c_frame();

  gdeactivate_ws (WKID);
  gclose_ws (WKID);
  gclose_gks();

}

/*
 *  Draw a color-filled box specified by two corner points and
 *  a color index..
 */
void c_draw_box(float xll, float yll, float xur, float yur, int cindex)
{
  Gpoint vbox[5];
  Gpoint_list pl;

  vbox[0].x = xll;
  vbox[0].y = yll;
  vbox[1].x = xur;
  vbox[1].y = yll;
  vbox[2].x = xur;
  vbox[2].y = yur;
  vbox[3].x = xll;
  vbox[3].y = yur;
  vbox[4].x = xll;
  vbox[4].y = yll;

  pl.num_points = 5;
  pl.points = vbox;

  gset_fill_colr_ind(cindex);
  
  gfill_area(&pl);
}

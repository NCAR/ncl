#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include "wrapper.h"

float armn(int, float *);
float armx(int, float *);
int cpclrs(int WKID);

/*
 * Procedure drwsrfc uses the NCAR Graphics function c_srface to
 * draw a surface plot of the data values in z.
 *
 * The point of observation is calculated from the 3D coordinate 
 * (s1, s2, s3); the point looked at is the center of the surface.
 *
 *  nx     -  Dimension of the X-axis variable x.
 *  ny     -  Dimension of the Y-axis variable y.
 *  x      -  An array of X-axis values.
 *  y      -  An array of Y-axis values.
 *  z      -  An array dimensioned for nx x ny containing data
 *            values for each (X,Y) coordinate.
 *  s1     -  X value for the eye position.
 *  s2     -  Y value for the eye position.
 *  s3     -  Z value for the eye position.
 *  iwk    -  Work space dimensioned for at least 2*nx*ny.
 *
 */

void drwsrfc (int WKID, int nx, int ny, float *x, float *y, float *z,
              float s1, float s2, float s3, int *iwk)
{
    Gcolr_rep colval;
    float xmn, xmx, ymn, ymx, zmn, zmx, eye[6], *fz;
    int i,j;

	gactivate_ws (WKID);
	colval.rgb.red = 1.;
	colval.rgb.green = 1.;
	colval.rgb.blue = 1.;
	gset_colr_rep(WKID,0,&colval);
	colval.rgb.red = 0.;
	colval.rgb.green = 0.;
	colval.rgb.blue = 0.;
	gset_colr_rep(WKID,1,&colval);
/*
 *  Find the extreme data values.
 */
    xmn = armn(nx, x);
    xmx = armx(nx, x);
    ymn = armn(ny, y);
    ymx = armx(ny, y);
    zmn = armn(nx * ny, z);
    zmx = armx(nx * ny, z);

    if ( (s1 == 0.) &&  (s2 == 0.) &&  (s3 == 0.) ) {
       s1 = -3.;
       s2 = -1.5;
       s3 = 0.75;
    }
    eye[0] = 5. * s1 * (xmx-xmn);
    eye[1] = 5. * s2 * (ymx-ymn);
    eye[2] = 5. * s3 * (zmx-zmn);
    eye[3] = 0.5 * (xmx-xmn);
    eye[4] = 0.5 * (ymx-ymn);
    eye[5] = 0.5 * (zmx-zmn);


/*
 *  Rearrange the array, since c_srface expects an array ordered as 
 *  per Fortran.
 */
    fz = (float *) calloc(nx*ny,sizeof(float));

    for (i = 0; i < nx; i++) {
       for (j = 0; j < ny; j++) {
           
          fz[j*nx+i] = z[i*ny+j];
       }
    }

/*
 *  Plot the surface.
 */
    c_srface (x,y,fz,iwk,nx,nx,ny,eye,0.);
    free(fz);
/*
 *  Deactivate and close workstation, close GKS.
 */
	gdeactivate_ws(WKID);
}

float armn(int num, float *x)
{
   int i;
   float amin;
   amin = x[0];
   for (i = 1 ; i < num ; i++)
      if (x[i] < amin) amin = x[i];
   return(amin);
}
float armx(int num, float *x)
{
   int i;
   float amax;
   amax = x[0];
   for (i = 1 ; i < num ; i++)
      if (x[i] > amax) amax = x[i];
   return(amax);
}
/*
 * Procedure drwvctc uses the NCAR Graphics functions c_vvinit and
 * c_vvectr to draw a vector plot.
 *
 * Where u and v are 2D arrays, this procedure draws a vector plot
 * of the vectors (u[i,j],v[i,j]) for i=1,lx AND j=1,ly.
 *
 */

void drwvctc (int WKID, int lx, int ly, float *u, float *v)
{
    Gcolr_rep colval;
    float *fu, *fv;
    float p,wrk;
    int i,j,iam;

/*
 *  If GKS is not open, open GKS, open workstation and activate a
 *  workstation.  Set the background color to white and the
 *  foreground color to balck.
 */
	gactivate_ws (WKID);
	colval.rgb.red = 1.;
	colval.rgb.green = 1.;
	colval.rgb.blue = 1.;
	gset_colr_rep(WKID,0,&colval);
	colval.rgb.red = 0.;
	colval.rgb.green = 0.;
	colval.rgb.blue = 0.;
	gset_colr_rep(WKID,1,&colval);
/*
 *  Rearrange the arrays, since c_vvinit expects an array ordered as 
 *  per Fortran.
 */
    fu = (float *) calloc(lx*ly,sizeof(float));
    fv = (float *) calloc(lx*ly,sizeof(float));

    for (i = 0; i < lx; i++) {
       for (j = 0; j < ly; j++) {
          fu[j*lx+i] = u[i*ly+j];
          fv[j*lx+i] = v[i*ly+j];
       }
    }
    c_vvinit(fu, lx, fv, ly, &p, 1, lx, ly, &wrk, 1);
    c_vvsetc("mnt"," ");
    c_vvsetc("mxt"," ");
    c_vvectr(fu, fv, &p, &iam, 0, &wrk);
    c_frame();
    free(fu);
    free(fv);

/*
 *  Deactivate and close workstation, close GKS.
 */
	gdeactivate_ws(WKID);
}

void drwconc (int WKID, int lx, int ly, float *zdat)
{
    Gcolr_rep colval;

    float xcra[1000],ycra[1000],rwrk[2000],*fz;
    int   iwrk[1000],iama[20000],iaia[10],igia[10],i,j;
    extern int cpcolr(float *, float *, int *, int *, int *, int *);
    extern int mask(float *, float *, int *, int *, int *, int *);

/*
 *  If GKS is not open, open GKS, open workstation and activate a
 *  workstation.  Set the background color to white and the
 *  foreground color to balck.
 */
	gactivate_ws (WKID);
	colval.rgb.red = 1.;
	colval.rgb.green = 1.;
	colval.rgb.blue = 1.;
	gset_colr_rep(WKID,0,&colval);
	colval.rgb.red = 0.;
	colval.rgb.green = 0.;
	colval.rgb.blue = 0.;
	gset_colr_rep(WKID,1,&colval);
/*
 *  Rearrange the array, since c_cprect expects an array ordered as
 *  per Fortran.
 */
    fz = (float *) calloc(lx*ly,sizeof(float));

    for (i = 0; i < lx; i++) {
       for (j = 0; j < ly; j++) {
          fz[j*lx+i] = zdat[i*ly+j];
       }
    }

    cpclrs(WKID);
    c_cpseti("CLS - contour level selector", 0);
    c_cpseti("NCL - number of contour levels", 7);

    for (i = 1 ; i < 8 ; i++) {
      c_cpseti("PAI - parameter array index", i);
      c_cpsetr("CLV - contour level", 10. * (float) i );
      c_cpseti("CLU - contour level use", 3);
      c_cpseti("LLC - contour label color", 1);
    }

    c_cpsetr("VPL - viewport left",0.05);
    c_cpsetr("VPR - viewport right",0.95);
    c_cpsetr("VPB - viewport bottom",0.05);
    c_cpsetr("VPT - viewport top",0.95);
    c_pcseti("FN  - font number (Helvetica bold)" ,22);
    c_pcseti("CC  - font color",1);
    c_cpsetr("T2D - tension of 2D splines",4.);
    c_cpseti("LLP - line label positioning, penalty scheme",3);
    c_cpseti("LLO - line label orientation",1);
    c_cpsetc("LOT - low labels off"," ");
    c_cpsetr("CWM - character width multiplier",2.5);
    c_cpsetc("ILT - informational label off"," ");

    c_cprect(fz,lx,lx,ly,rwrk,2000,iwrk,1000);
    c_arinam(iama,20000);
    c_cpclam(fz,rwrk,iwrk,iama);
    c_cplbam(fz,rwrk,iwrk,iama);

    c_arscam(iama,xcra,ycra,1000,iaia,igia,7,cpcolr);

    gset_line_colr_ind(1);

    c_cpcldm(fz,rwrk,iwrk,iama,mask);

    c_cplbdr(fz,rwrk,iwrk);
    gset_line_colr_ind(1);
    c_perim(1,0,1,0);
    c_frame();

    free(fz);

/*
 *  Deactivate and close workstation, close GKS.
 */
	gdeactivate_ws(WKID);
}

int cpclrs(int WKID)
{
    Gcolr_rep rgbv[9];
    int i;
/*
 * Define colors.
 */
    rgbv[0].rgb.red   = 1.00;
    rgbv[0].rgb.green = 1.00;
    rgbv[0].rgb.blue  = 1.00;
    rgbv[1].rgb.red   = 0.00;
    rgbv[1].rgb.green = 0.00;
    rgbv[1].rgb.blue  = 0.00;
    rgbv[2].rgb.red   = 0.00;
    rgbv[2].rgb.green = 1.00;
    rgbv[2].rgb.blue  = 1.00;
    rgbv[3].rgb.red   = 0.00;
    rgbv[3].rgb.green = 1.00;
    rgbv[3].rgb.blue  = 0.00;
    rgbv[4].rgb.red   = 0.70;
    rgbv[4].rgb.green = 1.00;
    rgbv[4].rgb.blue  = 0.00;
    rgbv[5].rgb.red   = 1.00;
    rgbv[5].rgb.green = 1.00;
    rgbv[5].rgb.blue  = 0.00;
    rgbv[6].rgb.red   = 1.00;
    rgbv[6].rgb.green = 0.75;
    rgbv[6].rgb.blue  = 0.00;
    rgbv[7].rgb.red   = 1.00;
    rgbv[7].rgb.green = 0.50;
    rgbv[7].rgb.blue  = 0.50;
    rgbv[8].rgb.red   = 1.00;
    rgbv[8].rgb.green = 0.00;
    rgbv[8].rgb.blue  = 0.00;

    for( i = 0; i < 9 ; i++ ) {
        gset_colr_rep(WKID,i,&rgbv[i]);
    }
    return(1);
}

int cpcolr(float *xcra, float *ycra, int *ncra, int *iaia,
           int *igia, int *naia)
{
    int i, ifll = 0;
    Gpoint_list fill_area;

    for( i = 0; i <= *naia; i++ ) {
        if (igia[i] == 3) ifll=iaia[i];
    }
    if (ifll >= 1 && ifll <= 8) {
        gset_fill_colr_ind (ifll+1);
        fill_area.num_points = *ncra-1;
        fill_area.points =
           (Gpoint *) malloc(fill_area.num_points*sizeof(Gpoint));

        if( !fill_area.points ) {
           fprintf( stderr,
             "colram: Not enough memory to create fill area structure\n" );
           gemergency_close_gks();
           exit(1);
        }
        for( i = 0; i < *ncra-1; i++ ) {
          fill_area.points[i].x = xcra[i];
          fill_area.points[i].y = ycra[i];
        }
        gfill_area (&fill_area);
        free(fill_area.points);
    }

    return(1);
}

int mask(float *xwrk, float *ywrk, int *n, int *iarea, int *igrp,
         int *ngrps)
{
  int i,idr;

  idr = 1;

  for ( i = 0 ; i < *ngrps ; i++ ) {
    if (iarea[i] < 0) idr = 0;
  }

  if (idr != 0) c_curve(xwrk, ywrk, *n);

  return(1);
}


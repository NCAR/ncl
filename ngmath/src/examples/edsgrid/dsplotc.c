#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>

#define WKTP 8
#define WKID 1
#define MTRI 40000
#define DTOR .017453292519943

#ifndef MAX
#define MAX(A,B)        (((A) > (B)) ? (A) : (B))
#endif
 
#ifndef MIN
#define MIN(A,B)        (((A) < (B)) ? (A) : (B))
#endif

float rtri[MTRI][10], rtwk[2][MTRI];
int   itwk[MTRI], ntri;

void c_drwtd2(int nx, int ny, float *x, float *y, float *z, 
              float s1, float s2, float s3, int style)
{
/*
 * Procedure c_drwtd2 uses the NCAR Graphics functions in Tdpack to
 * draw a surface plot of the data values in z.
 *
 * The point of observation is specified by (s1, s2, s3); the point 
 * looked at is the center of the surface.  If s1 = s2 = s3 = 0., 
 * then the observation point is calculated automatically.
 *
 *  nx     -  Dimension of the X-axis variable x.
 *  ny     -  Dimension of the Y-axis variable y.
 *  x      -  An array of X-axis values.
 *  y      -  An array of Y-axis values.
 *  z      -  An array dimensioned for nx X ny containing data
 *            values for each (X,Y) coordinate.
 *  s1     -  x value for the eye position.
 *  s2     -  y value for the eye position.
 *  s3     -  z value for the eye position.
 *  style  -  A style index defining the colors used to shade the
 *            surface as per:
 *
 *               1  -  wire frame
 *               2  -  gray shades underneath; gray shades on top.
 *               3  -  gray shades underneath; red shades on top.
 *               4  -  gray shades underneath; green shades on top.
 *               5  -  gray shades underneath; blue shades on top.
 *               6  -  gray shades underneath; cyan shades on top.
 *               7  -  gray shades underneath; magenta shades on top.
 *
 *            If style is positive, then a white backgound is used;
 *            if style is the negative of any of the above values, then
 *            a black background is used.
 */
    float *fz;
    int   i, j;
    float ang1=-35., ang2=25., rmul=2.9, shde=0.1, shdr=0.8;
    float xmin, xmax, ymin, ymax, zmin, zmax, xrng, yrng, zrng, 
          xmid, ymid, zmid;
    float p, q, r, xsl, ysl, zsl, xeye, yeye, zeye;
    Gop_st op_st;
    Gcolr_rep colval;

    ginq_op_st(&op_st);
    if (op_st == GST_GKCL) {
       gopen_gks ("stdout",0);
       if (WKTP == 1) {
         c_ngsetc("me","srf.ncgm");
       }
       else if ( (WKTP >= 20) && (WKTP <= 31) ) {
         c_ngsetc("me","srf.ps");
       }
       gopen_ws (WKID, 0, WKTP);
       gactivate_ws (WKID);
    }

/*
 *  Set foreground and background colors.
 */
    if (style >= 0) {
       colval.rgb.red = 1.;
       colval.rgb.green = 1.;
       colval.rgb.blue = 1.;
       gset_colr_rep(WKID,0,&colval);
       colval.rgb.red = 0.;
       colval.rgb.green = 0.;
       colval.rgb.blue = 0.;
       gset_colr_rep(WKID,1,&colval);
    }
    else {
       colval.rgb.red = 0.;
       colval.rgb.green = 0.;
       colval.rgb.blue = 0.;
       gset_colr_rep(WKID,0,&colval);
       colval.rgb.red = 1.;
       colval.rgb.green = 1.;
       colval.rgb.blue = 1.;
       gset_colr_rep(WKID,1,&colval);
    }

/*
 *  Find mins and maxs.
 */
    xmin = x[0];
    xmax = x[0];
    for (i = 1; i < nx; i++) {
      xmin = MIN(xmin,x[i]);
      xmax = MAX(xmax,x[i]);
    }
    ymin = y[0];
    ymax = y[0];
    for (j = 1; j < ny; j++) {
      ymin = MIN(ymin,y[j]);
      ymax = MAX(ymax,y[j]);
    }
    zmin = z[0];
    zmax = z[0];
    for (i = 1; i < nx * ny; i++) {
      zmin = MIN(zmin, z[i]);
      zmax = MAX(zmax, z[i]);
    }
    xrng = xmax-xmin;
    yrng = ymax-ymin;
    zrng = zmax-zmin;
    xmid = 0.5*(xmin+xmax);
    ymid = 0.5*(ymin+ymax);
    zmid = 0.5*(zmin+zmax);
 
    colval.rgb.red = 1.;
    colval.rgb.green = 0.;
    colval.rgb.blue = 0.;
    gset_colr_rep(WKID,2,&colval);
    colval.rgb.red = 0.;
    colval.rgb.green = 1.;
    colval.rgb.blue = 0.;
    gset_colr_rep(WKID,3,&colval);
    colval.rgb.red = 0.;
    colval.rgb.green = 0.;
    colval.rgb.blue = 1.;
    gset_colr_rep(WKID,4,&colval);
    colval.rgb.red = 0.;
    colval.rgb.green = 1.;
    colval.rgb.blue = 1.;
    gset_colr_rep(WKID,5,&colval);
    colval.rgb.red = 1.;
    colval.rgb.green = 0.;
    colval.rgb.blue = 1.;
    gset_colr_rep(WKID,6,&colval);
    colval.rgb.red = 1.;
    colval.rgb.green = 1.;
    colval.rgb.blue = 0.;
    gset_colr_rep(WKID,7,&colval);
    colval.rgb.red = 0.7;
    colval.rgb.green = 0.7;
    colval.rgb.blue = 0.7;
    gset_colr_rep(WKID,8,&colval);
 
    for (i = 11; i < 43; i++) {
      p = 1. - (float) (i-11)/31.;
      q = 1. - shdr * (float) (i-11)/31.;
      colval.rgb.red = p;
      colval.rgb.green = p;
      colval.rgb.blue = p;
      gset_colr_rep(WKID,i,&colval);
      colval.rgb.red = q;
      colval.rgb.green = q;
      colval.rgb.blue = q;
      gset_colr_rep(WKID,i+32,&colval);
      colval.rgb.red = q;
      colval.rgb.green = shde*q;
      colval.rgb.blue = shde*q;
      gset_colr_rep(WKID,i+64,&colval);
      colval.rgb.red = shde*q;
      colval.rgb.green = q;
      colval.rgb.blue = shde*q;
      gset_colr_rep(WKID,i+96,&colval);
      colval.rgb.red = shde*q;
      colval.rgb.green = shde*q;
      colval.rgb.blue = q;
      gset_colr_rep(WKID,i+128,&colval);
      colval.rgb.red = shde*q;
      colval.rgb.green = q;
      colval.rgb.blue = q;
      gset_colr_rep(WKID,i+160,&colval);
      colval.rgb.red = q;
      colval.rgb.green = shde*q;
      colval.rgb.blue = q;
      gset_colr_rep(WKID,i+192,&colval);
    }
 
/*
 *  Rearrange the array, since Tdpack expects an array ordered as 
 *  per Fortran.
 */
    fz = (float *) calloc(nx*ny,sizeof(float));
  
    for (i = 0; i < nx; i++) {
      for (j = 0; j < ny; j++) {
         fz[j*nx+i] = z[i*ny+j];
      }
    } 

/*
 *  Define plotting styles.
 */
    xsl = 0.05*xrng;
    ysl = 0.05*yrng;
    zsl = 0.00*zrng;
    c_tdstrs(1, -1, 0, -1, 0, -1, 1, 0, xsl, ysl, zsl);
    c_tdstrs(1, -1, 0, -1, 0, -1, 1, 0, xsl, ysl, zsl);
    c_tdstrs(2, 43,74, 43, 74, 1, 1, 0, xsl, ysl, zsl);
    c_tdstrs(3, 43,74, 75,106, 1, 1, 0, xsl, ysl, zsl);
    c_tdstrs(4, 43,74,107,138, 1, 1, 0, xsl, ysl, zsl);
    c_tdstrs(5, 43,74,139,170, 1, 1, 0, xsl, ysl, zsl);
    c_tdstrs(6, 43,74,171,202, 1, 1, 0, xsl, ysl, zsl);
    c_tdstrs(7, 43,74,203,234, 1, 1, 0, xsl, ysl, zsl);

/*
 *  Create the triangle list representing a surface.
 */
    ntri = 0;
    c_tdstri(x,nx,y,ny,fz,nx,&rtri[0][0],MTRI,&ntri,style);
    if (ntri == MTRI) {
      printf("Triangle list overflow in c_tdstri\n");
      exit(1);
    }
    free(fz);

/*
 *  Determine a default eye position if none is specified.
 */
    if ( (s1==0.) && (s2==0.) && (s3==0.) ) {
      r = rmul * (float) sqrt( (float) (xrng*xrng + yrng*yrng + zrng*zrng));
      xeye = xmid + r * (float) cos( (double) (DTOR*ang1)) * 
                (float) cos( (double) (DTOR*ang2));
      yeye = ymid + r * (float) sin( (double) (DTOR*ang1)) * 
                (float) cos( (double) (DTOR*ang2));
      zeye = zmid + r * (float) sin( (double) (DTOR*ang2));
    }
    else {
      xeye = s1;
      yeye = s2;
      zeye = s3;
    }

/*
 *  Initialize Tdpack.
 */
    c_tdinit (xeye, yeye, zeye, xmid, ymid, zmid,
              xmid, ymid, zmid+0.1*zrng, 0);
  
/*
 *  Order the triangles.
 */
    c_tdotri(&rtri[0][0], MTRI, &ntri, &rtwk[0][0], itwk, 1);
    if (ntri == MTRI) {
      printf("Triangle list overflow in c_tdotri\n");
      exit(1);
    }

/*
 *  Draw the triangles.
 */
    c_tddtri(&rtri[0][0], MTRI, &ntri, itwk);
    c_frame();
   
    if (op_st == GST_GKCL) {
      gdeactivate_ws(WKID);
      gclose_ws(WKID);
      gclose_gks();
    }
}

void c_drwtd3(int nx, int ny, int nz, float *x, float *y, float *z,
              float *u, float value, 
              float s1, float s2, float s3, int style)
{
/*
 *
 * Procedure c_drwtd3 uses the NCAR Graphics functions in Tdpack
 * to draw an isosurface plot of the data values in u.
 *
 * The point of observation is specified by (s1, s2, s3); the point 
 * looked at is the center of the surface.  If s1 = s2 = s3 = 0., 
 * then the observation point is calculated automatically.
 *
 *  nx     -  Dimension of the X-axis variable X.
 *  ny     -  Dimension of the Y-axis variable Y.
 *  nz     -  Dimension of the Z-axis variable Z.
 *  x      -  An array of X-axis values.
 *  y      -  An array of Y-axis values.
 *  z      -  An array of Z-axis values.
 *  u      -  An array values for each (X,Y,Z) coordinate.
 *  s1     -  X value for the eye position.
 *  s2     -  Y value for the eye position.
 *  s3     -  Z value for the eye position.
 *  value  -  The iso value.
 *  style  -  A style index defining the colors used to shade the
 *            surface as per:
 *
 *               1  -  wire frame
 *               2  -  gray shades underneath; gray shades on top.
 *               3  -  gray shades underneath; red shades on top.
 *               4  -  gray shades underneath; green shades on top.
 *               5  -  gray shades underneath; blue shades on top.
 *               6  -  gray shades underneath; cyan shades on top.
 *               7  -  gray shades underneath; magenta shades on top.
 *
 *            If style is positive, then a white backgound is use;
 *            if style is the negative of any of the above values, then
 *            a black background is used.
 *           
 */
    int   i, j, k;
    float ang1=-35., ang2=25., rmul=2.9, shde=0.1, shdr=0.8, *fu;
    float xmin, xmax, ymin, ymax, zmin, zmax, xrng, yrng, zrng,
          xmid, ymid, zmid;
    float p, q, r, xsl, ysl, zsl, xeye, yeye, zeye;
    Gop_st op_st;
    Gcolr_rep colval;
 
    ginq_op_st(&op_st);
    if (op_st == GST_GKCL) {
       gopen_gks ("stdout",0);
       if (WKTP == 1) {
         c_ngsetc("me","srf.ncgm");
       }
       else if ( (WKTP >= 20) && (WKTP <= 31) ) {
         c_ngsetc("me","srf.ps");
       }
       gopen_ws (WKID, 0, WKTP);
       gactivate_ws (WKID);
    }
 
/*
 *  Set foreground and background colors.
 */
    if (style >= 0) {
       colval.rgb.red = 1.;
       colval.rgb.green = 1.;
       colval.rgb.blue = 1.;
       gset_colr_rep(WKID,0,&colval);
       colval.rgb.red = 0.;
       colval.rgb.green = 0.;
       colval.rgb.blue = 0.;
       gset_colr_rep(WKID,1,&colval);
    }
    else {
       colval.rgb.red = 0.;
       colval.rgb.green = 0.;
       colval.rgb.blue = 0.;
       gset_colr_rep(WKID,0,&colval);
       colval.rgb.red = 1.;
       colval.rgb.green = 1.;
       colval.rgb.blue = 1.;
       gset_colr_rep(WKID,1,&colval);
    }

/*
 *  Find mins and maxs.
 */
    xmin = x[0];
    xmax = x[0];
    for (i = 1; i < nx; i++) {
      xmin = MIN(xmin,x[i]);
      xmax = MAX(xmax,x[i]);
    }
    ymin = y[0];
    ymax = y[0];
    for (j = 1; j < ny; j++) {
      ymin = MIN(ymin,y[j]);
      ymax = MAX(ymax,y[j]);
    }
    zmin = z[0];
    zmax = z[0];
    for (i = 1; i < nz; i++) {
      zmin = MIN(zmin, z[i]);
      zmax = MAX(zmax, z[i]);
    }
    xrng = xmax-xmin;
    yrng = ymax-ymin;
    zrng = zmax-zmin;
    xmid = 0.5*(xmin+xmax);
    ymid = 0.5*(ymin+ymax);
    zmid = 0.5*(zmin+zmax);
 
    colval.rgb.red = 1.;
    colval.rgb.green = 0.;
    colval.rgb.blue = 0.;
    gset_colr_rep(WKID,2,&colval);
    colval.rgb.red = 0.;
    colval.rgb.green = 1.;
    colval.rgb.blue = 0.;
    gset_colr_rep(WKID,3,&colval);
    colval.rgb.red = 0.;
    colval.rgb.green = 0.;
    colval.rgb.blue = 1.;
    gset_colr_rep(WKID,4,&colval);
    colval.rgb.red = 0.;
    colval.rgb.green = 1.;
    colval.rgb.blue = 1.;
    gset_colr_rep(WKID,5,&colval);
    colval.rgb.red = 1.;
    colval.rgb.green = 0.;
    colval.rgb.blue = 1.;
    gset_colr_rep(WKID,6,&colval);
    colval.rgb.red = 1.;
    colval.rgb.green = 1.;
    colval.rgb.blue = 0.;
    gset_colr_rep(WKID,7,&colval);
    colval.rgb.red = 0.7;
    colval.rgb.green = 0.7;
    colval.rgb.blue = 0.7;
    gset_colr_rep(WKID,8,&colval);
 
    for (i = 11; i < 43; i++) {
      p = 1. - (float) (i-11)/31.;
      q = 1. - shdr * (float) (i-11)/31.;
      colval.rgb.red = p;
      colval.rgb.green = p;
      colval.rgb.blue = p;
      gset_colr_rep(WKID,i,&colval);
      colval.rgb.red = q;
      colval.rgb.green = q;
      colval.rgb.blue = q;
      gset_colr_rep(WKID,i+32,&colval);
      colval.rgb.red = q;
      colval.rgb.green = shde*q;
      colval.rgb.blue = shde*q;
      gset_colr_rep(WKID,i+64,&colval);
      colval.rgb.red = shde*q;
      colval.rgb.green = q;
      colval.rgb.blue = shde*q;
      gset_colr_rep(WKID,i+96,&colval);
      colval.rgb.red = shde*q;
      colval.rgb.green = shde*q;
      colval.rgb.blue = q;
      gset_colr_rep(WKID,i+128,&colval);
      colval.rgb.red = shde*q;
      colval.rgb.green = q;
      colval.rgb.blue = q;
      gset_colr_rep(WKID,i+160,&colval);
      colval.rgb.red = q;
      colval.rgb.green = shde*q;
      colval.rgb.blue = q;
      gset_colr_rep(WKID,i+192,&colval);
    }
 
/*
 *  Rearrange the array, since Tdpack expects an array ordered as 
 *  per Fortran.
 */
    fu = (float *) calloc(nx*ny*nz, sizeof(float));
  
    for (i = 0 ; i < nx ; i++) {
      for (j = 0 ; j < ny ; j++) {
        for (k = 0 ; k < nz ; k++) {
          fu[ny*nx*k + nx*j + i] = u[nz*ny*i + nz*j + k];
        }
      }
    }

    xsl = 0.05*xrng;
    ysl = 0.05*yrng;
    zsl = 0.00*zrng;
    c_tdstrs(1, -1, 0, -1, 0, -1, 1, 0, xsl, ysl, zsl);
    c_tdstrs(1, -1, 0, -1, 0, -1, 1, 0, xsl, ysl, zsl);
    c_tdstrs(2, 43,74, 43, 74, 1, 1, 0, xsl, ysl, zsl);
    c_tdstrs(3, 43,74, 75,106, 1, 1, 0, xsl, ysl, zsl);
    c_tdstrs(4, 43,74,107,138, 1, 1, 0, xsl, ysl, zsl);
    c_tdstrs(5, 43,74,139,170, 1, 1, 0, xsl, ysl, zsl);
    c_tdstrs(6, 43,74,171,202, 1, 1, 0, xsl, ysl, zsl);
    c_tdstrs(7, 43,74,203,234, 1, 1, 0, xsl, ysl, zsl);

/*
 *  Create the triangle list representing an isosurface.
 */
    ntri = 0;
    c_tditri(x, nx, y, ny, z, nz, fu, nx, ny, value, 
             &rtri[0][0], MTRI, &ntri, style);
    if (ntri == MTRI) {
      printf("Triangle list overflow in c_tdstri\n");
      exit(1);
    }
    free(fu);


/*
 *  Determine a default eye position if none is specified.
 */
    if ( (s1==0.) && (s2==0.) && (s3==0.) ) {
      r = rmul * (float) sqrt( (float) (xrng*xrng + yrng*yrng + zrng*zrng));
      xeye = xmid + r * (float) cos( (double) (DTOR*ang1)) *
                (float) cos( (double) (DTOR*ang2));
      yeye = ymid + r * (float) sin( (double) (DTOR*ang1)) *
                (float) cos( (double) (DTOR*ang2));
      zeye = zmid + r * (float) sin( (double) (DTOR*ang2));
    }
    else {
      xeye = s1;
      yeye = s2;
      zeye = s3;
    }

/*
 *  Initialize Tdpack.
 */
    c_tdinit (xeye, yeye, zeye, xmid, ymid, zmid,
              xmid, ymid, zmid+0.1*zrng, 0);
 
/*
 *  Order the triangles.
 */
    c_tdotri(&rtri[0][0], MTRI, &ntri, &rtwk[0][0], itwk, 1);
    if (ntri == MTRI) {
      printf("Triangle list overflow in c_tdotri\n");
      exit(1);
    }
 
/*
 *  Draw the triangles.
 */
    c_tddtri(&rtri[0][0], MTRI, &ntri, itwk);
    c_frame();
 
    if (op_st == GST_GKCL) {
      gdeactivate_ws(WKID);
      gclose_ws(WKID);
      gclose_gks();
    }
}

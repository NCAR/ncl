/*
 *  $Id: c_csex07.c,v 1.1 1998-12-10 00:09:09 fred Exp $
 */

#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>
#include <ncarg/ngmath.h>

#define NDATA   500
#define NX      29
#define NY      25
#define NO     NX*NY
#define N1      10
#define N2      10
#define XMIN   -1.4
#define XMAX    1.4
#define YMIN   -1.2
#define YMAX    1.2
#define IWTYPE  1
#define WKID    1

float dsrnd1();

/*
 *  Do a 2D approximation using a list of output coordinates.
 */
main () 
{
  float xi[NDATA],yi[NDATA],zi[NDATA],xo[NO],yo[NO],*surface,t1,t2;
  float xp[NX],yp[NY];
  int   i,j,indx=0,knots[2],ier;

/*
 *  Create the data array for the surface.
 */
  for (i = 0; i < NDATA; i++) {
    xi[i] = XMIN+(XMAX-XMIN)*dsrnd1();    
    yi[i] = YMIN+(YMAX-YMIN)*dsrnd1();    
    zi[i] = xi[i] + yi[i];
    t1 = 1.0/(pow(fabs(xi[i]-0.1),2.75) + pow(fabs(yi[i]),2.75) + 0.09);
    t2 = 1.0/(pow(fabs(xi[i]+0.1),2.75) + pow(fabs(yi[i]),2.75) + 0.09);
    zi[i] = 0.3*(zi[i]+t1-t2);
  }
  for (j = 0; j < NY; j++) {
    for (i = 0; i < NX; i++) {
      xo[indx] = XMIN+((float)i/(float)(NX-1))*(XMAX-XMIN);
      yo[indx] = YMIN+((float)j/(float)(NY-1))*(YMAX-YMIN);
      indx++;
    }
  }

  knots[0] = N1;
  knots[1] = N2;
 
  surface = c_csa2ls(NDATA,xi,yi,zi,knots,NO,xo,yo,&ier);
  if (ier != 0) {
    printf("Error return from c_csa2xs: %d\n",ier);
    exit(1);
  }

/*
 *  Draw plot.
 */
/*
 *  Open GKS, open and activate a workstation.
 */
  gopen_gks("stdout",0);
  gopen_ws(WKID, NULL, IWTYPE);
  gactivate_ws(WKID);

/*
 *  Create the output grid for plotting.
 */
  for (i = 0; i < NX; i++) {
    xp[i] = XMIN+((float)(i)/(float)(NX-1))*(XMAX-XMIN);
  }
  for (j = 0; j < NY; j++) {
    yp[j] = YMIN+((float)(j)/(float)(NY-1))*(YMAX-YMIN);
  }

/*
 *  Draw the 2D surface.  Since "surface" is already in column
 *  dominate order, we need not rearrange it for input to c_tdez2d.
 */
  c_tdez2d(NX, NY, xp, yp, surface, 2.5, -154., 80., 6);
  c_frame();

/*
 *  Deactivate and close workstation, close GKS.
 */
  gdeactivate_ws(WKID);
  gclose_ws(WKID);
  gclose_gks();
}

float dsrnd1()
{
  static unsigned int iseed = 1;
  int it,it0,it1=-16,it2=32767;

  iseed = iseed*1103515245 + 12345;
  it0   = NGCALLF(ishift,ISHIFT)(&iseed,&it1);
  it    = NGCALLF(iand,IAND)(&it0,&it2);

  return( (float) it/ 32767.);
}

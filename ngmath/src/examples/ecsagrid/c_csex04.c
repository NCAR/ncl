/*
 *  $Id: c_csex04.c,v 1.2 1998-12-11 22:42:49 fred Exp $
 */

#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>
#include <ncarg/ngmath.h>

#define NDIM    2
#define NX     29
#define NY     25
#define NI    1000
#define N1      4
#define N2      4
#define XMIN   -1.0
#define XMAX    1.0
#define YMIN   -1.0
#define YMAX    1.0
#define IWTYPE 1
#define WKID   1

float dsrnd1();

/*
 *  Do a 2D approximation and find mixed second partial derivatives.
 */
main () 
{
  float xi[NI],yi[NI],zi[NI],xo[NX],yo[NY],*function,*derivs,wts[1];
  float ssmth=0.0,*zor,t1,t2;
  int   i,j,knots[2],nderiv[2]={1,1},ier;

/*
 *  Generate input data using the functiuon f(x,y) = y**2 - 0.5*y*x**2
 */
  for (i = 0; i < NI; i++) {
    xi[i] = XMIN+(XMAX-XMIN)*dsrnd1();    
    yi[i] = YMIN+(YMAX-YMIN)*dsrnd1();    
    zi[i] = yi[i]*yi[i] - 0.5*xi[i]*xi[i]*yi[i];
  }

/*
 *  Create the output grid.
 */
  for (i = 0; i < NX; i++) {
    xo[i] = XMIN+((float)i/(float)(NX-1))*(XMAX-XMIN);
  }
  for (j = 0; j < NY; j++) {
    yo[j] = YMIN+((float)j/(float)(NY-1))*(YMAX-YMIN);
  }

/*
 *  Specify the numbers of knots in each coordinate direction.
 */
  knots[0] = N1;
  knots[1] = N2;
 
/*
 *  Calculate the approximated functuion values.
 */
  function = c_csa2s(NI,xi,yi,zi,knots,NX,NY,xo,yo,&ier);
  if (ier != 0) {
    printf("Error return from c_csa2s: %d\n",ier);
    exit(1);
  }

/*
 *  Calculate the second order mixed partial.
 */
  nderiv[0] = 1;
  nderiv[1] = 1;
  wts[0] = -1.;
  derivs = c_csa2xs(NI,xi,yi,zi,wts,knots,ssmth,nderiv,NX,NY,xo,yo,&ier);
  if (ier != 0) {
    printf("Error return from c_csa2xs: %d\n",ier);
    exit(1);
  }

/*
 *  Open GKS, open and activate a workstation.
 */
  gopen_gks("stdout",0);
  gopen_ws(WKID, NULL, IWTYPE);
  gactivate_ws(WKID);
 
/*
 *  Rearrange the C array "function" for input to c_tdez2d.
 */
  zor = (float *) calloc(NX*NY, sizeof(float));
  if (zor == NULL) {
    printf("Unable to allocate temp space\n");
    exit(1);
  }
  for (i = 0; i < NX; i++) {
    for (j = 0; j < NY; j++) {
      zor[j*NX+i] = function[i*NY+j];
    }
  }

/*
 *  Draw plot of approximated function.
 */
  c_tdez2d(NX, NY, xo, yo, zor, 2.7, 45., 78., 6);
  c_set(0.,1.,0.,1.,0.,1.,0.,1.,1);
  c_plchhq(0.5,0.85,
      ":F25:z = f(x,y) = y:S:2:E:  - -:H-10::S:1:E::B::V-6:2:E:  x:S:2:E:",
      0.04,0.,0.);
  c_frame();

/*
 *  Rearrange the C array "derivs" for input to c_tdez2d.
 */
  for (i = 0; i < NX; i++) {
    for (j = 0; j < NY; j++) {
      zor[j*NX+i] = derivs[i*NY+j];
    }
  }

/*
 *  Draw plot of approximated function.
 */
  c_tdez2d(NX, NY, xo, yo, zor, 2.7, 45., 78., 6);
  c_set(0.,1.,0.,1.,0.,1.,0.,1.,1);
  c_plchhq(0.5,0.85,
      ":F25:z =  :F34::S::H8:6:F25::S:2:E::E::F34:>:B::F34::H-35::"
      "V-6:6:F25:x:F34:6:F25:y:E:  f(x,y) = - x",
      0.04,0.,0.);
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

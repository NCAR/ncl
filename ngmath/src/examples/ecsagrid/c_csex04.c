/*
 *  $Id: c_csex04.c,v 1.4 2002-05-03 20:31:38 fred Exp $
 */

#include <math.h>
#include <stdlib.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>
#include <ncarg/ngmath.h>

/*
 *  The dimensionality of the problem.
 */
#define NDIM    2

/*
 *  The number of output data points in the X coordinate direction.
 */
#define NX     29

/*
 *  The number of output data points in the Y coordinate direction.
 */
#define NY     25

/*
 *  The number of input data points.
 */
#define NI    1000

/*
 *  The number of knots in the X direction.
 */
#define N1      4

/*
 *  The number of knots in the Y direction.
 */
#define N2      4

/*
 *  Data limits.
 */
#define XMIN   -1.0
#define XMAX    1.0
#define YMIN   -1.0
#define YMAX    1.0

/*
 *  The GKS workstation type (NCGM).
 */
#define IWTYPE 1

/*
 *  The GKS workstaton identifier.
 */
#define WKID   1

/*
 *  Function prototype for the random number generator.
 */
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
    xi[i] = XMIN+(XMAX-(XMIN))*dsrnd1();    
    yi[i] = YMIN+(YMAX-(YMIN))*dsrnd1();    
    zi[i] = yi[i]*yi[i] - 0.5*xi[i]*xi[i]*yi[i];
  }

/*
 *  Create the output grid.
 */
  for (i = 0; i < NX; i++) {
    xo[i] = XMIN+((float)i/(float)(NX-1))*(XMAX-(XMIN));
  }
  for (j = 0; j < NY; j++) {
    yo[j] = YMIN+((float)j/(float)(NY-1))*(YMAX-(YMIN));
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
            ":F25:z = f(x,y) = y:S:2"
            ":E:  - -:H-10::S:1:E::B::V-6:2:E:  y:V-6:*:V+6:x:S:2:E:",
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

/*
 *  Random number generator returns float.
 */
  return (((float) rand()/ (float) RAND_MAX));
}

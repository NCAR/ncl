/*
 *  $Id: c_csex06.c,v 1.3 2002-05-03 20:31:39 fred Exp $
 */

#include <math.h>
#include <stdlib.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>
#include <ncarg/ngmath.h>

/*
 *  The number of output data points in the X coordinate direction.
 */
#define NX     21

/*
 *  The number of output data points in the Y coordinate direction.
 */
#define NY     31

/*
 *  The number of output data points in the Z coordinate direction.
 */
#define NZ     41

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
 *  The number of knots in the Z direction.
 */
#define N3      4

/*
 *  Data limits.
 */
#define XMIN   -2.0
#define XMAX    2.0
#define YMIN   -2.0
#define YMAX    2.0
#define ZMIN   -2.0
#define ZMAX    2.0

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
 * Test 3D function c_csa3s.
 */
main () 
{
  float xi[NI],yi[NI],zi[NI],ui[NI],xo[NX],yo[NY],zo[NZ],*surface;
  float *zor,t1,t2;
  int   it1,it2,i,j,k,knots[3],ier;

/*
 *  Generate input data.
 */
  for (i = 0; i < NI; i++) {
    xi[i] = XMIN+(XMAX-(XMIN))*dsrnd1();    
    yi[i] = YMIN+(YMAX-(YMIN))*dsrnd1();    
    zi[i] = ZMIN+(ZMAX-(ZMIN))*dsrnd1();    
    ui[i] = 0.75*xi[i]*xi[i] - 1.6*yi[i]*yi[i] + 2.*zi[i]*zi[i];
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
  for (k = 0; k < NZ; k++) {
    zo[k] = ZMIN+((float)k/(float)(NZ-1))*(ZMAX-(ZMIN));
  }

/*
 *  Specify the numbers of knots in each coordinate direction.
 */
  knots[0] = N1;
  knots[1] = N2;
  knots[2] = N3;
 
/*
 *  Calculate the approximated functuion values.
 */
  surface = c_csa3s(NI,xi,yi,zi,ui,knots,NX,NY,NZ,xo,yo,zo,&ier);
  if (ier != 0) {
    printf("Error return from c_csa3s: %d\n",ier);
    exit(1);
  }

/*
 *  Rearrange the C array "surface" for input to c_tdez3d.
 */
  zor = (float *) calloc(NX*NY*NZ, sizeof(float));
  if (zor == NULL) {
    printf("Unable to allocate temp space\n");
    exit(1);
  }
  for (i = 0; i < NX; i++) {
    for (j = 0; j < NY; j++) {
      for (k = 0; k < NZ; k++) {
        it1 = k*NX*NY + j*NX + i;
        it2 = i*NZ*NY + j*NZ + k;
        zor[k*NX*NY + j*NX + i] = surface[i*NZ*NY + j*NZ + k];
        fflush(stdout);
      }
    }
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
 
  c_tdez3d(NX, NY, NZ, xo, yo, zo, zor, 0.7, 2.3, -13., 75., 6);
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

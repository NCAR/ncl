/*
 *  $Id: c_csex05.c,v 1.2 1998-12-11 22:46:07 fred Exp $
 */

#include <math.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>
#include <ncarg/ngmath.h>

#define NX     21
#define NY     21
#define NZ     21
#define NI    1000
#define N1      4
#define N2      4
#define N3      4
#define XMIN   -2.0
#define XMAX    2.0
#define YMIN   -2.0
#define YMAX    2.0
#define ZMIN   -2.0
#define ZMAX    2.0
#define IWTYPE 1
#define WKID   1

float dsrnd1();

/*
 * A 3D approximation using function c_csa3s.c with a sphere as an isosurface.
 */
main () 
{
  float xi[NI],yi[NI],zi[NI],ui[NI],xo[NX],yo[NY],zo[NZ],*surface;
  float *zor,t1,t2;
  int   it1,it2,i,j,k,knots[3],ier;

  for (i = 0; i < NI; i++) {
    xi[i] = XMIN+(XMAX-XMIN)*dsrnd1();    
    yi[i] = YMIN+(YMAX-YMIN)*dsrnd1();    
    zi[i] = ZMIN+(ZMAX-ZMIN)*dsrnd1();    
    ui[i] = xi[i]*xi[i] + yi[i]*yi[i] + zi[i]*zi[i];
  }

  for (i = 0; i < NX; i++) {
    xo[i] = XMIN+((float)i/(float)(NX-1))*(XMAX-XMIN);
  }
  for (j = 0; j < NY; j++) {
    yo[j] = YMIN+((float)j/(float)(NY-1))*(YMAX-YMIN);
  }
  for (k = 0; k < NZ; k++) {
    zo[k] = ZMIN+((float)k/(float)(NZ-1))*(ZMAX-ZMIN);
  }

  knots[0] = N1;
  knots[1] = N2;
  knots[2] = N3;
 
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
 
  c_tdez3d(NX, NY, NZ, xo, yo, zo, zor, 3., 2., -35., 65., 6);
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

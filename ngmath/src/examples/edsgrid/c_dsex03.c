#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>
#include <ncarg/ngmath.h>

#define NUM  171
#define NX    21
#define NY    21
#define IWTYPE 1
#define WKID   1

extern void    c_drwtd2(int, int, float *, float *, float *,
                     float, float, float, int);

main()
{
  int  i, j, k, ier;
  float xi[NUM], yi[NUM], zi[NUM];
  float xo[NX], yo[NY], *output;
  float xminin = -0.2, yminin = -0.2, xmaxin = 1.2, ymaxin = 1.2;
  float xminot =  0.0, yminot =  0.0, xmaxot = 1.0, ymaxot = 1.0;
  float xeye = 1.3, yeye = -1.8, zeye = 3.6;
 
/*
 *  Create random data in three space and define a function.
 */
  for (i = 0; i < NUM; i++) {
    xi[i] = xminin+(xmaxin-xminin)*((float) rand() / (float) RAND_MAX);
    yi[i] = yminin+(ymaxin-yminin)*((float) rand() / (float) RAND_MAX);
    zi[i] = (xi[i]-0.25)*(xi[i]-0.25) + (yi[i]-0.50)*(yi[i]-0.50);
  }
 
/*
 *  Create the output grid.
 */
  for (i = 0; i < NX; i++) {
    xo[i] = xminot + ( (float) i / (float) (NX-1)) * (xmaxot-xminot);
  }
  for (j = 0; j < NY; j++) {
    yo[j] = yminot + ( (float) j / (float) (NY-1)) * (ymaxot-yminot);
  }

/*
 *  Interpolate.
 */
  output = c_dsgrid2s(NUM, xi, yi, zi, NX, NY, xo, yo, &ier);
  if (ier != 0) {
    printf(" Error %d returned from c_dsgrid2s\n",ier);
    exit(1);
  }
  gopen_gks ("stdout",0);
  gopen_ws (WKID, NULL, IWTYPE);
  gactivate_ws(WKID);
  c_drwtd2(NX, NY, xo, yo, output, xeye, yeye, zeye, -6);
  gdeactivate_ws(WKID);
  gclose_ws(WKID);
  gclose_gks();
}

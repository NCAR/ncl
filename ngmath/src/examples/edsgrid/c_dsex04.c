#include <stdio.h>
#include <stddef.h>
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>
#include <ncarg/ngmath.h>

#define NX    21
#define NY    21
#define IWTYPE 1
#define WKID   1

extern void    c_drwtd2(int, int, float *, float *, float *,
                     float, float, float, int);

main()
{
  int  i, j, k, ier;
  float xi[] = {0.00, 1.00, 0.00, 1.00, 0.30, 0.30, 0.30, 0.69,
                0.71, 0.71, 0.69, 0.70, 0.70, 0.70, 0.69, 0.71};
  float yi[] = {0.00, 0.00, 1.00, 1.00, 0.70, 0.30, 0.70, 0.69,
                0.71, 0.71, 0.69, 0.70, 0.70, 0.70, 0.69, 0.71};
  float zi[] = {0.00, 0.00, 0.00, 0.50, 0.50, 0.50, 0.50, 1.00,
                1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00};
  float xo[NX], yo[NY], xinc, yinc, *output;
  float xeye =  3.3, yeye = -3.3, zeye =  3.3;

/*
 *  Create the output grid.
 */
  xinc = 1./ (float) (NX-1); 
  yinc = 1./ (float) (NY-1); 
  for (i = 0; i < NX; i++) {
    xo[i] = xinc * (float) i;
  }
  for (j = 0; j < NY; j++) {
    yo[j] = yinc * (float) j;
  }

/*
 *  Set the shadowing flag.
 */
  c_dsseti("shd", 1);
  output = c_dsgrid2s(sizeof(xi)/sizeof(xi[0]), xi, yi, zi, 
                                            NX, NY, xo, yo, &ier);
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

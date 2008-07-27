/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

#include <ncarg/ngmath.h>
#include "dstypes.h"
#include "dsproto.h"
#include "dsuhead.h"


/*
 *  Get values for float parameters.
 */
void c_dsgetr(char *pnam, float *dval)
{
   if (!strncmp(pnam,"dmv",3) || !strncmp(pnam,"DMV",3)) {
      *dval = ds_missing_value;
   }
   else if (!strncmp(pnam,"exp",3) || !strncmp(pnam,"EXP",3)) {
      *dval = (float) ds_exponent; 
   }
   else if (!strncmp(pnam,"dmx",3) || !strncmp(pnam,"DMX",3)) {
      *dval = (float) ds_max_dist; 
   }
   else {
      sprintf(ds_emsg,"\n  Parameter name supplied is: %s\n",pnam);
      DSErrorHnd(4, "c_dsgetr", stderr, ds_emsg);
   }
}

/*
 *  Set values for float parameters.
 */
void c_dssetr(char *pnam, float dval)
{
   if (!strncmp(pnam,"dmv",3) || !strncmp(pnam,"DMV",3)) {
      ds_missing_value = dval;
   }
   else if (!strncmp(pnam,"exp",3) || !strncmp(pnam,"EXP",3)) {
      ds_exponent = (double) MAX(dval, 0.); 
   }
   else if (!strncmp(pnam,"dmx",3) || !strncmp(pnam,"DMX",3)) {
      ds_max_dist = (double) MAX(dval, 0.); 
      ds_set_max_dist = 1;
   }
   else {
      sprintf(ds_emsg,"\n  Parameter name supplied is: %s\n",pnam);
      DSErrorHnd(4, "c_dssetr", stderr, ds_emsg);
   }
}

/*
 *  C entries in support of the Fortran interface.
 */
void NGCALLF(dssetr,DSSETR) (char *pnam, float *rval)
{
   c_dssetr(pnam, *rval);
}

void NGCALLF(dsgetr,DSGETR) (char *pnam, float *rval)
{
   c_dsgetr(pnam, rval);
}

void NGCALLF(dsgrid3s,DSGRID3S) (int *n, float *x, float *y, float *z,
                float *u, int *nxg, int *nyg, int *nzg, 
                float *xg, float *yg, float *zg, float *out, int *ier)
{
   float *zar;
   int    i, j, k;

   zar = c_dsgrid3s(*n, x, y, z, u, *nxg, *nyg, *nzg, xg, yg, zg, ier);

   if (*ier) return;

   for (i = 0 ; i < *nxg ; i++) {
     for (j = 0 ; j < *nyg ; j++) {
       for (k = 0 ; k < *nzg ; k++) {
             *(out + (*nyg)*(*nxg)*k + (*nxg)*j + i) = 
                         zar[(*nzg)*(*nyg)*i + (*nzg)*j + k];
       }
     }
   }

   free(zar);
   return;
}

void NGCALLF(dsgrid2s,DSGRID2S) (int *n, float *x, float *y, float *u,
               int *nx, int *ny, float *xo, float *yo, float *out, int *ier)
{
   float *zar;
   int    i, j;

   zar = c_dsgrid2s(*n, x, y, u, *nx, *ny, xo, yo, ier);
   if (*ier) return;

   for (i = 0 ; i < *nx ; i++) {
     for (j = 0 ; j < *ny ; j++) {
       *(out + j * (*nx) + i) = zar[i*(*ny)+j];
     }
   }  

   free(zar);
   return;
}

void NGCALLF(dspnt3s,DSPNT3S) (int *n, float *x, float *y, float *z,
                float *u, int *m,
                float *xo, float *yo, float *zo, float *uo, int *ier)
{
   c_dspnt3s(*n, x, y, z, u, *m, xo, yo, zo, uo, ier);
}

void NGCALLF(dspnt2s,DSPNT2S) (int *n, float *x, float *y, float *z, int *m,
                float *xo, float *yo, float *zo, int *ier)
{
   c_dspnt2s(*n, x, y, z, *m, xo, yo, zo, ier);
}

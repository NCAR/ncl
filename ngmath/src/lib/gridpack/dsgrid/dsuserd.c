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
 *  Get values for double parameters.
 */
void c_dsgetrd(char *pnam, double *dval)
{
   if (!strncmp(pnam,"dmv",3) || !strncmp(pnam,"DMV",3)) {
      *dval = ds_missing_value;
   }
   else if (!strncmp(pnam,"exp",3) || !strncmp(pnam,"EXP",3)) {
      *dval = ds_exponent; 
   }
   else if (!strncmp(pnam,"dmx",3) || !strncmp(pnam,"DMX",3)) {
      *dval = ds_max_dist; 
   }
   else {
      sprintf(ds_emsg,"\n  Parameter name supplied is: %s\n",pnam);
      DSErrorHnd(4, "c_dsgetrd", stderr, ds_emsg);
   }
}

/*
 *  Set values for double parameters.
 */
void c_dssetrd(char *pnam, double dval)
{
   if (!strncmp(pnam,"dmv",3) || !strncmp(pnam,"DMV",3)) {
      ds_missing_value = dval;
   }
   else if (!strncmp(pnam,"exp",3) || !strncmp(pnam,"EXP",3)) {
      ds_exponent = MAX(dval, 0.); 
   }
   else if (!strncmp(pnam,"dmx",3) || !strncmp(pnam,"DMX",3)) {
      ds_max_dist = MAX(dval, 0.); 
      ds_set_max_dist = 1;
   }
   else {
      sprintf(ds_emsg,"\n  Parameter name supplied is: %s\n",pnam);
      DSErrorHnd(4, "c_dssetrd", stderr, ds_emsg);
   }
}

#ifdef UNICOS
void NGCALLF(dssetrd,DSSETRD) (char *pnam, double *rval)
{
   DSErrorHnd(7, "dssetrd", stderr, "\n");
}
#else
void NGCALLF(dssetrd,DSSETRD) (char *pnam, double *rval)
{
   c_dssetrd(pnam, *rval);
}
#endif

#ifdef UNICOS
void NGCALLF(dsgetrd,DSGETRD) (char *pnam, double *rval)
{
   DSErrorHnd(7, "dsgetrd", stderr, "\n");
}
#else
void NGCALLF(dsgetrd,DSGETRD) (char *pnam, double *rval)
{
   c_dsgetrd(pnam, rval);
}
#endif

#ifdef UNICOS
void NGCALLF(dsgrid3d,DSGRID3D) (int *n, double *x, double *y, double *z,
                double *u, int *nxg, int *nyg, int *nzg, 
                double *xg, double *yg, double *zg, double *out, int *ier)
{
   DSErrorHnd(7, "dsgrid3d", stderr, "\n");
   *ier = ds_error_status;   
   return;
}
#else
void NGCALLF(dsgrid3d,DSGRID3D) (int *n, double *x, double *y, double *z,
                double *u, int *nxg, int *nyg, int *nzg, 
                double *xg, double *yg, double *zg, double *out, int *ier)
{
   double *zar;
   int    i, j, k;

   zar = c_dsgrid3d(*n, x, y, z, u, *nxg, *nyg, *nzg, xg, yg, zg, ier);

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
#endif

#ifdef UNICOS
void NGCALLF(dspnt3d,DSPNT3D) (int *n, double *x, double *y, double *z,
                double *u, int *m,
                double *xo, double *yo, double *zo, double *uo, int *ier)
{
   DSErrorHnd(7, "dsgrid3d", stderr, "\n");
   *ier = ds_error_status;
   return;
}
#else
void NGCALLF(dspnt3d,DSPNT3D) (int *n, double *x, double *y, double *z,
                double *u, int *m,
                double *xo, double *yo, double *zo, double *uo, int *ier)
{
   c_dspnt3d(*n, x, y, z, u, *m, xo, yo, zo, uo, ier);
}
#endif

#ifdef UNICOS
void NGCALLF(dspnt2d,DSPNT2D) (int *n, double *x, double *y, double *z, int *m,
                double *xo, double *yo, double *zo, int *ier)
{
   DSErrorHnd(7, "dspnt2d", stderr, "\n");
   *ier = ds_error_status;
   return;
}
#else
void NGCALLF(dspnt2d,DSPNT2D) (int *n, double *x, double *y, double *z, int *m,
                double *xo, double *yo, double *zo, int *ier)
{
   c_dspnt2d(*n, x, y, z, *m, xo, yo, zo, ier);
}
#endif

#ifdef UNICOS
void NGCALLF(dsgrid2d,DSGRID2D) (int *n, double * x, double * y, double * u,
               int *nx, int *ny, double *xo, double *yo, double *out, int *ier)
{
   DSErrorHnd(7, "dsgrid2d", stderr, "\n");
   *ier = ds_error_status;
   return;
}
#else
void NGCALLF(dsgrid2d,DSGRID2D) (int *n, double * x, double * y, double * u,
               int *nx, int *ny, double *xo, double *yo, double *out, int *ier)
{
   double *zar;
   int    i, j;

   zar = c_dsgrid2d(*n, x, y, u, *nx, *ny, xo, yo, ier);
   if (*ier) return;

   for (i = 0 ; i < *nx ; i++) {
     for (j = 0 ; j < *ny ; j++) {
       *(out + j * (*nx) + i) = zar[i*(*ny)+j];
     }
   }   

   free(zar);
   return;
}
#endif

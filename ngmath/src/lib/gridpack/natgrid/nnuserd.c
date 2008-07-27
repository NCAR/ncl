/*
 * $Id: nnuserd.c,v 1.9 2008-07-27 03:10:13 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

#include "nnuheadd.h"
#include "nnuhead.h"

extern   int   error_status;

/*
 *  Get values for double parameters.
 */
void c_nngetrd(char *pnam, double *dval)
{
   if (!strncmp(pnam,"bi",2) OR !strncmp(pnam,"BI",2) OR
            !strncmp(pnam,"bI",2) OR !strncmp(pnam,"Bi",2)) {
      *dval = bI;
   }
   else if (!strncmp(pnam,"bj",2) OR !strncmp(pnam,"BJ",2) OR
            !strncmp(pnam,"bJ",2) OR !strncmp(pnam,"Bj",2)) {
      *dval = bJ;
   }
   else if (!strncmp(pnam,"magx",4) OR !strncmp(pnam,"MAGX",4)) {
      *dval = magx;
   }
   else if (!strncmp(pnam,"magy",4) OR !strncmp(pnam,"MAGY",4)) {
      *dval = magy;
   }
   else if (!strncmp(pnam,"magz",4) OR !strncmp(pnam,"MAGZ",4)) {
      *dval = magz;
   }
   else if (!strncmp(pnam,"hor",3) OR !strncmp(pnam,"HOR",3)) {
      *dval = horilap;
   }
   else if (!strncmp(pnam,"ver",3) OR !strncmp(pnam,"VER",3)) {
      *dval = vertlap;
   }
   else if (!strncmp(pnam,"nul",3) OR !strncmp(pnam,"NUL",3)) {
      *dval = nuldat;
   }
   else if (!strncmp(pnam,"xas",3) OR !strncmp(pnam,"XAS",3)) {
      *dval = magx_auto;
   }
   else if (!strncmp(pnam,"yas",3) OR !strncmp(pnam,"YAS",3)) {
      *dval = magy_auto;
   }
   else if (!strncmp(pnam,"zas",3) OR !strncmp(pnam,"ZAS",3)) {
      *dval = magz_auto;
   }
   else {
      sprintf(emsg,"\n  Parameter name supplied is: %s\n",pnam);
      ErrorHnd(23, "c_nngetrd", stderr, emsg);
   }
}

/*
 *  Set values for double parameters.
 */
void c_nnsetrd(char *pnam, double dval)
{
   if (!strncmp(pnam,"bi",2) OR !strncmp(pnam,"BI",2) OR
            !strncmp(pnam,"bI",2) OR !strncmp(pnam,"Bi",2)) {
      if (dval < 1.) {
        bI = 1.;
      }
      else if (dval > 3.) {
        bI = 3.;
      }
      else {
        bI = dval;
      }
   }
   else if (!strncmp(pnam,"bj",2) OR !strncmp(pnam,"BJ",2) OR
            !strncmp(pnam,"bJ",2) OR !strncmp(pnam,"Bj",2)) {
      if (dval < 3.) {
        bJ = 3.;
      }
      else if (dval > 9.) {
        bJ = 9.;
      }
      else {
        bJ = dval;
      }
   }
   else if (!strncmp(pnam,"magx",4) OR !strncmp(pnam,"MAGX",4)) {
      magx = dval;
   }
   else if (!strncmp(pnam,"magy",4) OR !strncmp(pnam,"MAGY",4)) {
      magy = dval;
   }
   else if (!strncmp(pnam,"magz",4) OR !strncmp(pnam,"MAGZ",4)) {
      magz = dval;
   }
   else if (!strncmp(pnam,"hor",3) OR !strncmp(pnam,"HOR",3)) {
      horilap = dval;
   }
   else if (!strncmp(pnam,"ver",3) OR !strncmp(pnam,"VER",3)) {
      vertlap = dval;
   }
   else if (!strncmp(pnam,"nul",3) OR !strncmp(pnam,"NUL",3)) {
      nuldat = dval;
   }
   else {
      sprintf(emsg,"\n  Parameter name supplied is: %s\n",pnam);
      ErrorHnd(23, "c_nnsetrd", stderr, emsg);
   }
}

/*
 *  C entries in support of the Fortran interface.
 */

#ifdef UNICOS
void NGCALLF(natgridd,NATGRIDD) (int *n, double *x, double *y, double *z, 
                int *nxg, int *nyg, double *xg, double *yg, double *zg,
                int *ier)
{
   ErrorHnd(29, "natgridd", stderr, "\n");
   *ier = error_status;
   return;
}
#else
void NGCALLF(natgridd,NATGRIDD) (int *n, double *x, double *y, double *z, 
                int *nxg, int *nyg, double *xg, double *yg, double *zg,
                int *ier)
{
   double *zar;
   int   nn, mm;

   zar = c_natgridd(*n, x, y, z, *nxg, *nyg, xg, yg, ier);

   if (*ier) return;

   for (mm = 0 ; mm < *nxg ; mm++) {
     for (nn = 0 ; nn < *nyg ; nn++) {
       *(zg + nn * (*nxg) + mm) = zar[mm*(*nyg)+nn];
     }
   }    
   free(zar);

   return;
}
#endif

#ifdef UNICOS
void NGCALLF(nnsetrd,NNSETRD) (char *pnam, double *rval)
{
   ErrorHnd(29, "nnsetrd", stderr, "\n");
}
#else
void NGCALLF(nnsetrd,NNSETRD) (char *pnam, double *rval)
{
   c_nnsetrd(pnam, *rval);
}
#endif

#ifdef UNICOS
void NGCALLF(nngetrd,NNGETRD) (char *pnam, double *rval)
{
   ErrorHnd(29, "nngetrd", stderr, "\n");
}
#else
void NGCALLF(nngetrd,NNGETRD) (char *pnam, double *rval)
{
   c_nngetrd(pnam, rval);
}
#endif

#ifdef UNICOS
void NGCALLF(nngetsloped,NNGETSLOPED) (int *row, int *col, 
             double *slope, int *ier)
{
   ErrorHnd(29, "nngetsloped", stderr, "\n");
   *ier = error_status;
}
#else
void NGCALLF(nngetsloped,NNGETSLOPED) (int *row, int *col, 
             double *slope, int *ier)
{
   c_nngetsloped(*row-1, *col-1, slope, ier);
}
#endif

#ifdef UNICOS
void NGCALLF(nngetaspectd,NNGETASPECTD) (int *row, int *col, 
             double *aspect, int *ier)
{
   ErrorHnd(29, "nngetaspectd", stderr, "\n");
   *ier = error_status;
}
#else
void NGCALLF(nngetaspectd,NNGETASPECTD) (int *row, int *col, 
             double *aspect, int *ier)
{
   c_nngetaspectd(*row-1, *col-1, aspect, ier);
}
#endif

#ifdef UNICOS
void NGCALLF(nnpntinitd,NNPNTINITD) (int *n, double *x, double *y, double *z)
{
   ErrorHnd(29, "nnpntinitd", stderr, "\n");
}
#else
void NGCALLF(nnpntinitd,NNPNTINITD) (int *n, double *x, double *y, double *z)
{
   c_nnpntinitd (*n, x, y, z);
}
#endif

#ifdef UNICOS
void NGCALLF(nnpntd,NNPNTD) (double *x, double *y, double *point)
{
   ErrorHnd(29, "nnpntd", stderr, "\n");
}
#else
void NGCALLF(nnpntd,NNPNTD) (double *x, double *y, double *point)
{
   c_nnpntd (*x, *y, point);
}
#endif

#ifdef UNICOS
void NGCALLF(nnpntendd,NNPNTENDD) ()
{
   ErrorHnd(29, "nnpntendd", stderr, "\n");
}
#else
void NGCALLF(nnpntendd,NNPNTENDD) ()
{
   c_nnpntendd ();
}
#endif

#include "nnuheads.h"
#include "nnuhead.h"


/*
 *  Get values for float parameters.
 */
void c_nngetr(char *pnam, float *dval)
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
      ErrorHnd(23, "c_nngetr", filee, emsg);
   }
}

/*
 *  Set values for float parameters.
 */
void c_nnsetr(char *pnam, float dval)
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
      ErrorHnd(23, "c_nngetc", filee, emsg);
   }
}

/*
 *  C entries in support of the Fortran interface.
 */
void NGCALLF(natgrids,NATGRIDS) (int *n, float *x, float *y, float *z, 
                int *nxg, int *nyg, float *xg, float *yg, 
                float *zg, int *ier)
{
   float *zar;
   int   nn, mm;

   zar = c_natgrids(*n, x, y, z, *nxg, *nyg, xg, yg, ier);

   if (*ier) return;

   for (mm = 0 ; mm < *nxg ; mm++) {
     for (nn = 0 ; nn < *nyg ; nn++) {
       *(zg + nn * (*nxg) + mm) = zar[mm*(*nyg)+nn];
     }
   }    
   free(zar);

   return;
}
void NGCALLF(nnsetr,NNSETR) (char *pnam, float *rval)
{
   c_nnsetr(pnam, *rval);
}
void NGCALLF(nngetr,NNGETR) (char *pnam, float *rval)
{
   c_nngetr(pnam, rval);
}

void NGCALLF(nngetslopes,NNGETSLOPES) (int *row, int *col, 
             float *slope, int *ier)
{
   c_nngetslopes(*row-1, *col-1, slope, ier);
}
void NGCALLF(nngetaspects,NNGETASPECTS) (int *row, int *col, 
             float *aspect, int *ier)
{
   c_nngetaspects(*row-1, *col-1, aspect, ier);
}
void NGCALLF(nnpntinits,NNPNTINITS) (int *n, float *x, float *y, float *z)
{
   c_nnpntinits (*n, x, y, z);
}
void NGCALLF(nnpnts,NNPNTS) (float *x, float *y, float *point)
{
   c_nnpnts (*x, *y, point);
}
void NGCALLF(nnpntend,NNPNTEND) ()
{
   c_nnpntend ();
}

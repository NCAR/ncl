#include "nnuheads.h"

/*
 *  Get values for character parameters.
 */
void c_nngetc(char *pnam, char *vnam)
{
   char *s;
   if (!strncmp(pnam,"alg",3) OR !strncmp(pnam,"ALG",3)) {
      s = tri_file;
   }
   else if (!strncmp(pnam,"erf",3) OR !strncmp(pnam,"ERF",3)) {
      s = error_file;
   }
   else {
      sprintf(emsg,"\n  Parameter name supplied is: %s\n",pnam);
      ErrorHnd(23, "c_nngetc", filee, emsg);
      return;
   }
   for ( ;  *s != '\0'; ++s, ++vnam) {
      *vnam = *s;
   }
      *vnam = '\0';
}

/*
 *  Get values for integer parameters.
 */
void c_nngeti(char *pnam, int *ival)
{
   if (!strncmp(pnam,"asc",3) OR !strncmp(pnam,"ASC",3)) {
      *ival = auto_scale;
   }
   else if (!strncmp(pnam,"igr",3) OR !strncmp(pnam,"IGR",3)) {
      *ival = igrad;
   }
   else if (!strncmp(pnam,"upd",3) OR !strncmp(pnam,"UPD",3)) {
      *ival = updir;
   }
   else if (!strncmp(pnam,"non",3) OR !strncmp(pnam,"NON",3)) {
      *ival = non_neg;
   }
   else if (!strncmp(pnam,"sdi",3) OR !strncmp(pnam,"SDI",3)) {
      *ival = sdip;
   }
   else if (!strncmp(pnam,"rad",3) OR !strncmp(pnam,"RAD",3)) {
      *ival = rads;
   }
   else if (!strncmp(pnam,"opt",3) OR !strncmp(pnam,"OPT",3)) {
      *ival = optim;
   }
   else if (!strncmp(pnam,"ext",3) OR !strncmp(pnam,"EXT",3)) {
      *ival = extrap;
   }
   else if (!strncmp(pnam,"adf",3) OR !strncmp(pnam,"ADF",3)) {
      *ival = adf;
   }
   else if (!strncmp(pnam,"dup",3) OR !strncmp(pnam,"DUP",3)) {
      *ival = dup;
   }
   else {
      sprintf(emsg,"\n  Parameter name supplied is: %s\n",pnam);
      ErrorHnd(23, "c_nngeti", filee, emsg);
   }
}

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
 *  Set values for character parameters.
 */
void c_nnsetc(char *pnam, char *vnam)
{
   int i;
   char *s;
   if (!strncmp(pnam,"alg",3) OR !strncmp(pnam,"ALG",3)) {
      s = tri_file;
      for ( ;  *vnam != '\0'; ++s, ++vnam) {
         *s = *vnam;
      }
         *s = '\0';
   }
   else if (!strncmp(pnam,"erf",3) OR !strncmp(pnam,"ERF",3)) {
      if (!strncmp(vnam,"stderr",6)) {
         filee = stderr;
         strcpy(error_file,"stderr");
      }
      else if (!strncmp(vnam,"stdout",6)) {
         filee = stdout;
         strcpy(error_file,"stdout");
      }
      else {
         if ((filee = fopen(vnam,"w")) EQ (FILE *) NULL)
         {
            ErrorHnd(24, "c_nnsetc", stderr, "\n");
            return;
         }
         strcpy(error_file,vnam);
      }
   }
   else {
      sprintf(emsg,"\n  Parameter name supplied is: %s\n",pnam);
      ErrorHnd(23, "c_nnsetc", filee, emsg);
   }
}

/*
 *  Set values for integer parameters.
 */
void c_nnseti(char *pnam, int ival)
{
   if (!strncmp(pnam,"asc",3) OR !strncmp(pnam,"ASC",3)) {
      auto_scale = ival;
   }
   else if (!strncmp(pnam,"igr",3) OR !strncmp(pnam,"IGR",3)) {
      igrad = ival;
   }
   else if (!strncmp(pnam,"upd",3) OR !strncmp(pnam,"UPD",3)) {
      updir = ival;
   }
   else if (!strncmp(pnam,"non",3) OR !strncmp(pnam,"NON",3)) {
      non_neg = ival;
   }
   else if (!strncmp(pnam,"sdi",3) OR !strncmp(pnam,"SDI",3)) {
      sdip = ival;
   }
   else if (!strncmp(pnam,"rad",3) OR !strncmp(pnam,"RAD",3)) {
      rads = ival;
   }
   else if (!strncmp(pnam,"opt",3) OR !strncmp(pnam,"OPT",3)) {
      optim = ival;
   }
   else if (!strncmp(pnam,"ext",3) OR !strncmp(pnam,"EXT",3)) {
      extrap = ival;
   }
   else if (!strncmp(pnam,"adf",3) OR !strncmp(pnam,"ADF",3)) {
      adf = ival;
   }
   else if (!strncmp(pnam,"dup",3) OR !strncmp(pnam,"DUP",3)) {
      dup = ival;
   }
   else {
      sprintf(emsg,"\n  Parameter name supplied is: %s\n",pnam);
      ErrorHnd(23, "c_nnseti", filee, emsg);
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
   float **zar;
   int   nn, mm;

   zar = c_natgrids(*n, x, y, z, *nxg, *nyg, xg, yg, ier);

   if (*ier) return;

   for (mm = 0 ; mm < *nxg ; mm++) {
     for (nn = 0 ; nn < *nyg ; nn++) {
       *(zg + nn * (*nxg) + mm) = zar[mm][nn];
     }
   }    

   return;
}
void NGCALLF(nnseti,NNSETI) (char *pnam, int *ival)
{
   c_nnseti(pnam, *ival);
}
void NGCALLF(nngeti,NNGETI) (char *pnam, int *ival)
{
   c_nngeti(pnam, ival);
}
void NGCALLF(nnsetr,NNSETR) (char *pnam, float *rval)
{
   c_nnsetr(pnam, *rval);
}
void NGCALLF(nngetr,NNGETR) (char *pnam, float *rval)
{
   c_nngetr(pnam, rval);
}
void NGCALLF(f_nnsetc,F_NNSETC) (char *pnam, char *cval, int *clen)
{
   char cdum[256];
   int i;

   for (i = 0 ; i < *clen ; i++) {
     cdum[i] = cval[i];
   }
   i = *clen;
   cdum[i] = '\0';
   c_nnsetc(pnam, cdum);
}
void NGCALLF(f_nngetc,F_NNGETC) (char *pnam, char *cval, int *clen)
{
   char cdum[256] = {" "};
   int i,jf;

   c_nngetc(pnam, cdum);
   jf = 0;
   for (i = 0 ; i < *clen ; i++) {
      if ((cdum[i] != '\0') && (jf == 0)) {
         cval[i] = cdum[i];
      }
      else
      {
         jf = 1;
         cval[i] = ' ';
      }
   }
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

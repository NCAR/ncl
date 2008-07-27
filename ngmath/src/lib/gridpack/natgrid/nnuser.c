/*
 * $Id: nnuser.c,v 1.9 2008-07-27 03:10:13 haley Exp $
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

#include "nnuheads.h"
#include "nnuhead.h"

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
      ErrorHnd(23, "c_nngetc", stderr, emsg);
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
   else if (!strncmp(pnam,"mdm",3) OR !strncmp(pnam,"MDM",3)) {
      *ival = maxmsg;
   }
   else if (!strncmp(pnam,"dup",3) OR !strncmp(pnam,"DUP",3)) {
      printf("\n  Natgrid info: parameter 'dup' is no longer supported; duplicate\n                points are now culled automatically\n");
   }
   else {
      sprintf(emsg,"\n  Parameter name supplied is: %s\n",pnam);
      ErrorHnd(23, "c_nngeti", stderr, emsg);
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
   else {
      sprintf(emsg,"\n  Parameter name supplied is: %s\n",pnam);
      ErrorHnd(23, "c_nnsetc", stderr, emsg);
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
   else if (!strncmp(pnam,"mdm",3) OR !strncmp(pnam,"MDM",3)) {
      maxmsg = ival;
   }
   else if (!strncmp(pnam,"dup",3) OR !strncmp(pnam,"DUP",3)) {
      nndup = ival;
      printf("\n  Natgrid info: parameter 'dup' is no longer supported; duplicate\n                points are now culled automatically\n");
   }
   else {
      sprintf(emsg,"\n  Parameter name supplied is: %s\n",pnam);
      ErrorHnd(23, "c_nnseti", stderr, emsg);
   }
}

void NGCALLF(nnseti,NNSETI) (char *pnam, int *ival)
{
   c_nnseti(pnam, *ival);
}
void NGCALLF(nngeti,NNGETI) (char *pnam, int *ival)
{
   c_nngeti(pnam, ival);
}

void NGCALLF(fnnsetc,FNNSETC) (char *pnam, char *cval, int *clen)
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
void NGCALLF(fnngetc,FNNGETC) (char *pnam, char *cval, int *clen)
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

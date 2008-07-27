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
 *  Get values for character parameters.
 */
void c_dsgetc(char *pnam, char *vnam)
{
   char *s;
   if (!strncmp(pnam,"erf",3) || !strncmp(pnam,"ERF",3)) {
      s = ds_error_file;
   }
   else {
      sprintf(ds_emsg,"\n  Parameter name supplied is: %s\n",pnam);
      DSErrorHnd(4, "c_dsgetc", stderr, ds_emsg);
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
void c_dsgeti(char *pnam, int *ival)
{
   if (!strncmp(pnam,"shd",3) || !strncmp(pnam,"SHD",3)) {
      *ival = ds_shadowing;
   }
   else {
      sprintf(ds_emsg,"\n  Parameter name supplied is: %s\n",pnam);
      DSErrorHnd(4, "c_dsgeti", stderr, ds_emsg);
   }
}


/*
 *  Set values for character parameters.
 */
void c_dssetc(char *pnam, char *vnam)
{
   int i;
   char *s;
   if (!strncmp(pnam,"xxx",3) || !strncmp(pnam,"XXX",3)) {
      for ( ;  *vnam != '\0'; ++s, ++vnam) {
         *s = *vnam;
      }
         *s = '\0';
   }
   else {
      sprintf(ds_emsg,"\n  Parameter name supplied is: %s\n",pnam);
      DSErrorHnd(4, "c_dssetc", stderr, ds_emsg);
   }
}

/*
 *  Set values for integer parameters.
 */
void c_dsseti(char *pnam, int ival)
{
   int i_tmp;

   if (!strncmp(pnam,"shd",3) || !strncmp(pnam,"SHD",3)) {
      ds_shadowing = ival;
   }
   else {
      sprintf(ds_emsg,"\n  Parameter name supplied is: %s\n",pnam);
      DSErrorHnd(4, "c_dsseti", stderr, ds_emsg);
   }
}

void NGCALLF(dsseti,DSSETI) (char *pnam, int *ival)
{
   c_dsseti(pnam, *ival);
}
void NGCALLF(dsgeti,DSGETI) (char *pnam, int *ival)
{
   c_dsgeti(pnam, ival);
}

void NGCALLF(fdssetc,FDSSETC) (char *pnam, char *cval, int *clen)
{
   char cdum[256];
   int i;

   for (i = 0 ; i < *clen ; i++) {
     cdum[i] = cval[i];
   }
   i = *clen;
   cdum[i] = '\0';
   c_dssetc(pnam, cdum);
}
void NGCALLF(fdsgetc,FDSGETC) (char *pnam, char *cval, int *clen)
{
   char cdum[256] = {" "};
   int i,jf;

   c_dsgetc(pnam, cdum);
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

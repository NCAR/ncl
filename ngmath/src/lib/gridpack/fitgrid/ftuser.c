/*
 * $Id: ftuser.c,v 1.5 2003-08-11 22:44:01 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU General Public License as published     *
* by the Free Software Foundation; either version 2 of the License, or  *
* (at your option) any later version.                                   *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* General Public License for more details.                              *
*                                                                       *
* You should have received a copy of the GNU General Public License     *
* along with this software; if not, write to the Free Software         *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

#include "ftuvars.h"

/*
 * Copy double data to float array, using a void array. 
 */
void *copy_dtof(void *dval, int size)
{
   int i;
   void *fval;

   fval = (float *) calloc(size, sizeof(float));
   for( i = 0; i < size; i++ ) {
      ((float*)fval)[i]  = (float)((double*)dval)[i];
   }
   return(fval);
}

/*
 * Copy float data to double array, using a void array. 
 */
void *copy_ftod(void *fval, int size)
{
   int i;
   void *dval;

   dval = (double *) calloc(size, sizeof(double));
   for( i = 0; i < size; i++ ) {
       ((double*)dval)[i]  = (double)((float*)fval)[i];
   }
   return(dval);
}

/*
 *  Set values for int parameters.
 */
int c_ftseti(char *pnam, int ival)
{
   if (!strncmp(pnam,"sf1",3) || !strncmp(pnam,"SF1",3)) {
      ft_islp = ival;
   }
   else if (!strncmp(pnam,"df1",3) || !strncmp(pnam,"DF1",3)) {
      ft_df1 = ival;
   }
   else if (!strncmp(pnam,"df2",3) || !strncmp(pnam,"DF2",3)) {
      ft_df2 = ival;
   }
   else if (!strncmp(pnam,"sf2",3) || !strncmp(pnam,"SF2",3)) {
      ft_sms = ival;
   }
   else if (!strncmp(pnam,"df3",3) || !strncmp(pnam,"DF3",3)) {
      ft_df3 = ival;
   }
   else if (!strncmp(pnam,"df4",3) || !strncmp(pnam,"DF4",3)) {
      ft_df4 = ival;
   }
   else if (!strncmp(pnam,"df5",3) || !strncmp(pnam,"DF5",3)) {
      ft_df5 = ival;
   }
   else if (!strncmp(pnam,"df6",3) || !strncmp(pnam,"DF6",3)) {
      ft_df6 = ival;
   }
   else if (!strncmp(pnam,"df7",3) || !strncmp(pnam,"DF7",3)) {
      ft_df7 = ival;
   }
   else if (!strncmp(pnam,"df8",3) || !strncmp(pnam,"DF8",3)) {
      ft_df8 = ival;
   }
   else {
      return(1);
   }
   return(0);
}

/*
 *  Set values for float parameters.
 */
int c_ftsetr(char *pnam, float fval)
{
   if (!strncmp(pnam,"sl1",3) || !strncmp(pnam,"SL1",3)) {
      ft_slp1 = (double) fval;
   }
   else if (!strncmp(pnam,"sln",3) || !strncmp(pnam,"SLN",3)) {
      ft_slpn = (double) fval;
   }
   else if (!strncmp(pnam,"sig",3) || !strncmp(pnam,"SIG",3)) {
      ft_sigma = (double) fval; 
   }
   else if (!strncmp(pnam,"smt",3) || !strncmp(pnam,"SMT",3)) {
      ft_s = (double) fval; 
   }
   else if (!strncmp(pnam,"eps",3) || !strncmp(pnam,"EPS",3)) {
      ft_eps = (double) fval; 
   }
   else if (!strncmp(pnam,"z11",3) || !strncmp(pnam,"Z11",3)) {
      ft_z11 = (double) fval; 
   }
   else if (!strncmp(pnam,"zm1",3) || !strncmp(pnam,"ZM1",3)) {
      ft_zm1 = (double) fval; 
   }
   else if (!strncmp(pnam,"z1n",3) || !strncmp(pnam,"Z1N",3)) {
      ft_z1n = (double) fval; 
   }
   else if (!strncmp(pnam,"zmn",3) || !strncmp(pnam,"ZMN",3)) {
      ft_zmn = (double) fval; 
   }
   else {
      return(1);
   }
   return(0);
}

/*
 *  Set values for double parameters.
 */
int c_ftsetrd(char *pnam, double dval)
{
   if (!strncmp(pnam,"sl1",3) || !strncmp(pnam,"SL1",3)) {
      ft_slp1 = dval;
   }
   else if (!strncmp(pnam,"sln",3) || !strncmp(pnam,"SLN",3)) {
      ft_slpn = dval;
   }
   else if (!strncmp(pnam,"sig",3) || !strncmp(pnam,"SIG",3)) {
      ft_sigma = dval; 
   }
   else if (!strncmp(pnam,"smt",3) || !strncmp(pnam,"SMT",3)) {
      ft_s = dval; 
   }
   else if (!strncmp(pnam,"eps",3) || !strncmp(pnam,"EPS",3)) {
      ft_eps = dval; 
   }
   else if (!strncmp(pnam,"z11",3) || !strncmp(pnam,"Z11",3)) {
      ft_z11 = dval; 
   }
   else if (!strncmp(pnam,"zm1",3) || !strncmp(pnam,"ZM1",3)) {
      ft_zm1 = dval; 
   }
   else if (!strncmp(pnam,"z1n",3) || !strncmp(pnam,"Z1N",3)) {
      ft_z1n = dval; 
   }
   else if (!strncmp(pnam,"zmn",3) || !strncmp(pnam,"ZMN",3)) {
      ft_zmn = dval; 
   }
   else {
      return(1);
   }
   return(0);
}

/*
 *  Set values for character parameters.
 */
int c_ftsetc(char *pnam, char *vnam)
{
   int i;
   char *s;
   if (!strncmp(pnam,"dum",3) || !strncmp(pnam,"DUM",3)) {
      s = ft_cdum;
      for ( ;  *vnam != '\0'; ++s, ++vnam) {
         *s = *vnam;
      }
         *s = '\0';
   }
   else {
      return(1);
   }
   return(0);
}

/*
 *  Get values for int parameters.
 */
int c_ftgeti(char *pnam, int *ival)
{
   if (!strncmp(pnam,"sf1",3) || !strncmp(pnam,"SF1",3)) {
      *ival = ft_islp;
   }
   else if (!strncmp(pnam,"sf2",3) || !strncmp(pnam,"SF2",3)) {
      *ival = ft_sms;
   }
   else if (!strncmp(pnam,"df1",3) || !strncmp(pnam,"DF1",3)) {
      *ival = ft_df1;
   }
   else if (!strncmp(pnam,"df2",3) || !strncmp(pnam,"DF2",3)) {
      *ival = ft_df2;
   }
   else if (!strncmp(pnam,"df3",3) || !strncmp(pnam,"DF3",3)) {
      *ival = ft_df3;
   }
   else if (!strncmp(pnam,"df4",3) || !strncmp(pnam,"DF4",3)) {
      *ival = ft_df4;
   }
   else if (!strncmp(pnam,"df5",3) || !strncmp(pnam,"DF5",3)) {
      *ival = ft_df5;
   }
   else if (!strncmp(pnam,"df6",3) || !strncmp(pnam,"DF6",3)) {
      *ival = ft_df6;
   }
   else if (!strncmp(pnam,"df7",3) || !strncmp(pnam,"DF7",3)) {
      *ival = ft_df7;
   }
   else if (!strncmp(pnam,"df8",3) || !strncmp(pnam,"DF8",3)) {
      *ival = ft_df8;
   }
   else {
      return(1);
   }
   return(0);
}

/*
 *  Get values for float parameters.
 */
int c_ftgetr(char *pnam, float *fval)
{
   if (!strncmp(pnam,"sl1",3) || !strncmp(pnam,"SL1",3)) {
      *fval = (float) ft_slp1;
   }
   else if (!strncmp(pnam,"sln",3) || !strncmp(pnam,"SLN",3)) {
      *fval = (float) ft_slpn;
   }
   else if (!strncmp(pnam,"sig",3) || !strncmp(pnam,"SIG",3)) {
      *fval = (float) ft_sigma; 
   }
   else if (!strncmp(pnam,"smt",3) || !strncmp(pnam,"SMT",3)) {
      *fval = (float) ft_s; 
   }
   else if (!strncmp(pnam,"eps",3) || !strncmp(pnam,"EPS",3)) {
      *fval = (float) ft_eps; 
   }
   else if (!strncmp(pnam,"z11",3) || !strncmp(pnam,"Z11",3)) {
      *fval = (float) ft_z11; 
   }
   else if (!strncmp(pnam,"zm1",3) || !strncmp(pnam,"ZM1",3)) {
      *fval = (float) ft_zm1; 
   }
   else if (!strncmp(pnam,"z1n",3) || !strncmp(pnam,"Z1N",3)) {
      *fval = (float) ft_z1n; 
   }
   else if (!strncmp(pnam,"zmn",3) || !strncmp(pnam,"ZMN",3)) {
      *fval = (float) ft_zmn; 
   }
   else {
      return(1);
   }
   return(0);
}

/*
 *  Get values for double parameters.
 */
int c_ftgetrd(char *pnam, double *dval)
{
   if (!strncmp(pnam,"sl1",3) || !strncmp(pnam,"SL1",3)) {
      *dval = ft_slp1;
   }
   else if (!strncmp(pnam,"sln",3) || !strncmp(pnam,"SLN",3)) {
      *dval = ft_slpn;
   }
   else if (!strncmp(pnam,"sig",3) || !strncmp(pnam,"SIG",3)) {
      *dval = ft_sigma; 
   }
   else if (!strncmp(pnam,"smt",3) || !strncmp(pnam,"SMT",3)) {
      *dval = ft_s; 
   }
   else if (!strncmp(pnam,"eps",3) || !strncmp(pnam,"EPS",3)) {
      *dval = ft_eps; 
   }
   else if (!strncmp(pnam,"z11",3) || !strncmp(pnam,"Z11",3)) {
      *dval = ft_z11; 
   }
   else if (!strncmp(pnam,"zm1",3) || !strncmp(pnam,"ZM1",3)) {
      *dval = ft_zm1; 
   }
   else if (!strncmp(pnam,"z1n",3) || !strncmp(pnam,"Z1N",3)) {
      *dval = ft_z1n; 
   }
   else if (!strncmp(pnam,"zmn",3) || !strncmp(pnam,"ZMN",3)) {
      *dval = ft_zmn; 
   }
   else {
      return(1);
   }
   return(0);
}

/*
 *  Get values for character parameters.
 */
int c_ftgetc(char *pnam, char *vnam)
{
   char *s;
   if (!strncmp(pnam,"dum",3) || !strncmp(pnam,"DUM",3)) {
      s = ft_cdum;
   }
   else {
      return(1);
   }
   for ( ;  *s != '\0'; ++s, ++vnam) {
      *vnam = *s;
   }
   *vnam = '\0';
   return(0);
}

/*
 *  Set floating point arrays.
 */
int c_ftsetfa(char *pnam, int n, float *far)
{
   if (!strncmp(pnam,"zx1",3) || !strncmp(pnam,"ZX1",3)) {
      ft_zx1.size = n;
	  ft_zx1.data = (float *) far;
      ft_zx1.type = ft_float;
   }
   else if (!strncmp(pnam,"zxm",3) || !strncmp(pnam,"ZXM",3)) {
      ft_zxm.size = n;
      ft_zxm.type = ft_float;
	  ft_zxm.data = (float *) far;
   }
   else if (!strncmp(pnam,"zy1",3) || !strncmp(pnam,"ZY1",3)) {
      ft_zy1.size = n;
      ft_zy1.type = ft_float;
	  ft_zy1.data = (float *) far;
   }
   else if (!strncmp(pnam,"zyn",3) || !strncmp(pnam,"ZYN",3)) {
      ft_zyn.size = n;
      ft_zyn.type = ft_float;
	  ft_zyn.data = (float *) far;
   }
   else {
      return(1);
   }
   return(0);
}
/*
 *  Set double arrays.
 */
int c_ftsetda(char *pnam, int n, double *dar)
{
   if (!strncmp(pnam,"zx1",3) || !strncmp(pnam,"ZX1",3)) {
      ft_zx1.size = n;
	  ft_zx1.data = (double *) dar;
      ft_zx1.type = ft_double;
   }
   else if (!strncmp(pnam,"zxm",3) || !strncmp(pnam,"ZXM",3)) {
      ft_zxm.size = n;
	  ft_zxm.data = (double *) dar;
      ft_zxm.type = ft_double;
   }
   else if (!strncmp(pnam,"zy1",3) || !strncmp(pnam,"ZY1",3)) {
      ft_zy1.size = n;
	  ft_zy1.data = (double *) dar;
      ft_zy1.type = ft_double;
   }
   else if (!strncmp(pnam,"zyn",3) || !strncmp(pnam,"ZYN",3)) {
      ft_zyn.size = n;
	  ft_zyn.data = (double *) dar;
      ft_zyn.type = ft_double;
   }
   else {
      return(1);
   }
   return(0);
}

/*
 *  Retrieve array sizes.
 */
int c_ftgetfa_size(char *pnam)
{
   if (!strncmp(pnam,"zx1",3) || !strncmp(pnam,"ZX1",3)) {
      return(ft_zx1.size);
   }
   else if (!strncmp(pnam,"zxm",3) || !strncmp(pnam,"ZXM",3)) {
      return(ft_zxm.size);
   }
   else if (!strncmp(pnam,"zy1",3) || !strncmp(pnam,"ZY1",3)) {
      return(ft_zy1.size);
   }
   else if (!strncmp(pnam,"zyn",3) || !strncmp(pnam,"ZYN",3)) {
      return(ft_zyn.size);
   }
   else {
      return(-1);
   }
}


/*
 *  Retreive floating array data.
 */
float *c_ftgetfa_data(char *pnam)
{
   void *fval;
   if (!strncmp(pnam,"zx1",3) || !strncmp(pnam,"ZX1",3)) {
      if(ft_zx1.type == ft_double) {
         ft_zx1.type = ft_float;
         fval        = copy_dtof(ft_zx1.data,ft_zx1.size);
         free(ft_zx1.data);
		 ft_zx1.data = fval;
      }
      return((float *)ft_zx1.data);
   }
   else if (!strncmp(pnam,"zxm",3) || !strncmp(pnam,"ZXM",3)) {
      if(ft_zxm.type == ft_double) {
         ft_zxm.type = ft_float;
         fval =      copy_dtof(ft_zxm.data,ft_zxm.size);
         free(ft_zxm.data);
         ft_zxm.data = fval;
      }
      return((float *)ft_zxm.data);
   }
   else if (!strncmp(pnam,"zy1",3) || !strncmp(pnam,"ZY1",3)) {
      if(ft_zy1.type == ft_double) {
         ft_zy1.type = ft_float;
         fval =      copy_dtof(ft_zy1.data,ft_zy1.size);
         free(ft_zy1.data);
         ft_zy1.data = fval;
      }
      return((float *)ft_zy1.data);
   }
   else if (!strncmp(pnam,"zyn",3) || !strncmp(pnam,"ZYN",3)) {
      if(ft_zyn.type == ft_double) {
         ft_zyn.type = ft_float;
         fval =      copy_dtof(ft_zyn.data,ft_zyn.size);
         free(ft_zyn.data);
         ft_zyn.data = fval;
      }
      return((float *)ft_zyn.data);
   }
   else {
      return(NULL);
   }
}

/*
 *  Retreive double array data.
 */
double *c_ftgetda_data(char *pnam)
{
   void *dval;
   if (!strncmp(pnam,"zx1",3) || !strncmp(pnam,"ZX1",3)) {
      if(ft_zx1.type == ft_float) {
         ft_zx1.type = ft_double;
         dval        = copy_ftod(ft_zx1.data,ft_zx1.size);
         free(ft_zx1.data);
		 ft_zx1.data = dval;
      }
      return((double *)ft_zx1.data);
   }
   else if (!strncmp(pnam,"zxm",3) || !strncmp(pnam,"ZXM",3)) {
	 if(ft_zxm.type == ft_float) {
        ft_zxm.type = ft_double;
	    dval        = copy_ftod(ft_zxm.data,ft_zxm.size);
	    free(ft_zxm.data);
	    ft_zxm.data = dval;
	  }
      return((double *)ft_zxm.data);
   }
   else if (!strncmp(pnam,"zy1",3) || !strncmp(pnam,"ZY1",3)) {
	  if(ft_zy1.type == ft_float) {
 	     ft_zy1.type = ft_double;
	     dval        = copy_ftod(ft_zy1.data,ft_zy1.size);
	     free(ft_zy1.data);
	     ft_zy1.data = dval;
 	  }
      return((double *)ft_zy1.data);
   }
   else if (!strncmp(pnam,"zyn",3) || !strncmp(pnam,"ZYN",3)) {
	  if(ft_zyn.type == ft_float) {
	     ft_zyn.type = ft_double;
	     dval      = copy_ftod(ft_zyn.data,ft_zyn.size);
	     free(ft_zyn.data);
	     ft_zyn.data = dval;
  	  }
      return((double *)ft_zyn.data);
   }
   else {
      return(NULL);
   }
}


#include "ftuvars.h"

/*
 *  Set values for int parameters.
 */
int c_ftseti(char *pnam, int ival)
{
   if (!strncmp(pnam,"sf1",3) || !strncmp(pnam,"SF1",3)) {
      ft_islp = ival;
   }
   else if (!strncmp(pnam,"sf2",3) || !strncmp(pnam,"SF2",3)) {
      ft_sms = ival;
   }
   else if (!strncmp(pnam,"df1",3) || !strncmp(pnam,"df1",3)) {
      ft_df1 = ival;
   }
   else if (!strncmp(pnam,"df2",3) || !strncmp(pnam,"df2",3)) {
      ft_df2 = ival;
   }
   else if (!strncmp(pnam,"df3",3) || !strncmp(pnam,"df3",3)) {
      ft_df3 = ival;
   }
   else if (!strncmp(pnam,"df4",3) || !strncmp(pnam,"df4",3)) {
      ft_df4 = ival;
   }
   else if (!strncmp(pnam,"df5",3) || !strncmp(pnam,"df5",3)) {
      ft_df5 = ival;
   }
   else if (!strncmp(pnam,"df6",3) || !strncmp(pnam,"df6",3)) {
      ft_df6 = ival;
   }
   else if (!strncmp(pnam,"df7",3) || !strncmp(pnam,"df7",3)) {
      ft_df7 = ival;
   }
   else if (!strncmp(pnam,"df8",3) || !strncmp(pnam,"df8",3)) {
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
      ft_slp1 = fval;
   }
   else if (!strncmp(pnam,"sln",3) || !strncmp(pnam,"SLN",3)) {
      ft_slpn = fval;
   }
   else if (!strncmp(pnam,"sig",3) || !strncmp(pnam,"SIG",3)) {
      ft_sigma = fval; 
   }
   else if (!strncmp(pnam,"smt",3) || !strncmp(pnam,"SMT",3)) {
      ft_s = fval; 
   }
   else if (!strncmp(pnam,"eps",3) || !strncmp(pnam,"EPS",3)) {
      ft_eps = fval; 
   }
   else if (!strncmp(pnam,"z11",3) || !strncmp(pnam,"Z11",3)) {
      ft_z11 = fval; 
   }
   else if (!strncmp(pnam,"zm1",3) || !strncmp(pnam,"ZM1",3)) {
      ft_zm1 = fval; 
   }
   else if (!strncmp(pnam,"z1n",3) || !strncmp(pnam,"Z1N",3)) {
      ft_z1n = fval; 
   }
   else if (!strncmp(pnam,"zmn",3) || !strncmp(pnam,"ZMN",3)) {
      ft_zmn = fval; 
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
      *fval = ft_slp1;
   }
   else if (!strncmp(pnam,"sln",3) || !strncmp(pnam,"SLN",3)) {
      *fval = ft_slpn;
   }
   else if (!strncmp(pnam,"sig",3) || !strncmp(pnam,"SIG",3)) {
      *fval = ft_sigma; 
   }
   else if (!strncmp(pnam,"smt",3) || !strncmp(pnam,"SMT",3)) {
      *fval = ft_s; 
   }
   else if (!strncmp(pnam,"eps",3) || !strncmp(pnam,"EPS",3)) {
      *fval = ft_eps; 
   }
   else if (!strncmp(pnam,"z11",3) || !strncmp(pnam,"Z11",3)) {
      *fval = ft_z11; 
   }
   else if (!strncmp(pnam,"zm1",3) || !strncmp(pnam,"ZM1",3)) {
      *fval = ft_zm1; 
   }
   else if (!strncmp(pnam,"z1n",3) || !strncmp(pnam,"Z1N",3)) {
      *fval = ft_z1n; 
   }
   else if (!strncmp(pnam,"zmn",3) || !strncmp(pnam,"ZMN",3)) {
      *fval = ft_zmn; 
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
 *  Set floating arrays.
 */
int c_ftsetfa(char *pnam, int n, float *far)
{
   if (!strncmp(pnam,"zx1",3) || !strncmp(pnam,"ZX1",3)) {
      ft_zx1.size = n;
      ft_zx1.data = far;
   }
   else if (!strncmp(pnam,"zxm",3) || !strncmp(pnam,"ZXM",3)) {
      ft_zxm.size = n;
      ft_zxm.data = far;
   }
   else if (!strncmp(pnam,"zy1",3) || !strncmp(pnam,"ZY1",3)) {
      ft_zy1.size = n;
      ft_zy1.data = far;
   }
   else if (!strncmp(pnam,"zyn",3) || !strncmp(pnam,"ZYN",3)) {
      ft_zyn.size = n;
      ft_zyn.data = far;
   }
   else {
      return(1);
   }
   return(0);
}

/*
 *  Retreive floating array sizes.
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
   if (!strncmp(pnam,"zx1",3) || !strncmp(pnam,"ZX1",3)) {
      return(ft_zx1.data);
   }
   else if (!strncmp(pnam,"zxm",3) || !strncmp(pnam,"ZXM",3)) {
      return(ft_zxm.data);
   }
   else if (!strncmp(pnam,"zy1",3) || !strncmp(pnam,"ZY1",3)) {
      return(ft_zy1.data);
   }
   else if (!strncmp(pnam,"zyn",3) || !strncmp(pnam,"ZYN",3)) {
      return(ft_zyn.data);
   }
   else {
      return(NULL);
   }
}

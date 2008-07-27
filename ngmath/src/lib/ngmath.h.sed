/* 
 * $Id: ngmath.h.sed,v 1.3 2008-07-27 04:02:35 haley Exp $
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


/*
 *  This file contains some system includes used by Ngmath functions,
 *  a source for the NGCALLF macro, the function prototypes for all 
 *  user entry points in the Ngmath library, and some specific defines.
 */
#include <stdio.h>
#include <stddef.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

/*
 *  Fortran function macro.  This macro is used to provide the appropriate
 *  system-specific C function name for it to be Fortran callable.
 */
#ifndef NGCALLF
 
#define NGCALLF(reg,caps)   SED_NGCALLF

#endif  /* NGCALLF */

/*
 *  Function prototypes for the shgrid package.
 */
int c_shgetnp(float, float, float, int, float *, float *, float *,
              int, int *);
float *c_shgrid(int, float [], float [], float [], float [],
                int, int, int, float [], float [], float [], int*);
void c_shseti(char *, int);
int c_shgeti(char *);

void NGCALLF(shgrid,SHGRID)(int *, float *, float *, float *, float *,
                            int *, int *, int *, float *, float *,
                            float *, float *, int *, float *, int *);
void NGCALLF(shgetnp,SHGETNP)(float *, float *, float *, int *, float *,
                              float *, float *, int *, int *, float *,
                              int *, int *);
void NGCALLF(shseti,SHSETI)(char *, int *, size_t);
void NGCALLF(shgeti,SHGETI)(char *, int *, size_t);

/*
 *  Function prototypes for the cssgrid package.
 */
int   *c_csstri(int, float [], float [], int *, int *);
float *c_cssgrid(int, float [], float [], float [],
                 int, int, float [], float [], int *);
void   c_csvoro(int, float [], float [], int, int,
                float [], float [], float [], int *,
                int *, int [], int *);
void   c_cstrans(int, float *, float *, float *, float *, float *);
void   c_csscoord(float, float, float, float *, float *, float *);
void   c_cssetr(char *, float);
void   c_csgetr(char *, float *);
void   c_csseti(char *, int);
void   c_csgeti(char *, int *);
void   c_css2c(int, float *, float *, float *, float *, float *);
void   c_csc2s(int, float *, float *, float *, float *, float *);

int   *c_csstrid(int, double [], double [], int *, int *);
double *c_cssgridd(int, double [], double [], double [],
                 int, int, double [], double [], int *);
void   c_csvorod(int, double [], double [], int, int,
                double [], double [], double [], int *,
                int *, int [], int *);
void   c_cstransd(int, double *, double *, double *, double *, double *);
void   c_csscoordd(double, double, double, double *, double *, double *);
void   c_cssetd(char *, double);
void   c_csgetd(char *, double *);
void   c_css2cd(int, double *, double *, double *, double *, double *);
void   c_csc2sd(int, double *, double *, double *, double *, double *);

void NGCALLF(csstri,CSSTRI)(int *, float *, float *, int *, int *, 
             int *, double *, int *);
void NGCALLF(cssgrid,CSSGRID)(int *, float *, float *, float *, int *, int *, 
             float *, float *, float *, int *, double *, int *);
void NGCALLF(csscoord,CSSCOORD)(float *, float *, float *, float *, 
             float *, float *);
void NGCALLF(csvoro,CSVORO)(int *, float *, float *, int *, int *, int *, 
             double *, int *, float *, float *, float *, int *, int *, 
             int *, int *);
void NGCALLF(cssetr,CSSETR)(char *,float *,size_t);
void NGCALLF(csgetr,CSGETR)(char *,float *,size_t);
void NGCALLF(csseti,CSSETI)(char *,int *,size_t);
void NGCALLF(csgeti,CSSETI)(char *,int *,size_t);

void NGCALLF(csstrid,CSSTRID)(int *, double *, double *, int *, int *, 
             int *, double *, int *);
void NGCALLF(cssgridd,CSSGRIDD)(int *, double *, double *, double *, 
             int *, int *, double *, double *, double *, int *, 
             double *, int *);
void NGCALLF(csscoordd,CSSCOORDD)(double *, double *, double *, 
             double *, double *, double *);
void NGCALLF(csvorod,CSVOROD)(int *, double *, double *, int *, int *, 
             int *, double *, int *, double *, double *, double *, int *, 
             int *, int *, int *);
void NGCALLF(cssetd,CSSETD)(char *,double *,size_t);
void NGCALLF(csgetd,CSGETD)(char *,double *,size_t);
void NGCALLF(css2c,CSS2C)(int *, float *, float *, float *, float *, float *);
void NGCALLF(css2cd,CSS2CD)(int *, double *, double *, double *, 
             double *, double *);
void NGCALLF(csc2s,CSC2S)(int *, float *, float *, float *, float *, float *);
void NGCALLF(csc2sd,CSC2SD)(int *, double *, double *, double *, 
             double *, double *);
 
/*
 *  Function prototypes for the csagrid package.
 */
float *c_csa1s(int, float [], float [], int, int, float [], int *);
float *c_csa1xs(int, float [], float [], float [], int,
             float, int, int, float [], int *);
float *c_csa2s(int, float [], float [], float [], int [],
               int, int, float [], float [], int *);
float *c_csa2xs(int, float [], float [], float [], float [], int [], float,
                int [], int, int, float [], float [], int *);
float *c_csa2ls(int, float [], float [], float [], int [],
                int, float [], float [], int *);
float *c_csa2lxs(int, float [], float [], float [], float [], int [],
                 float, int [], int, float [], float [], int *);
float *c_csa3s(int, float [], float [], float [], float [], int [], int, int,
               int, float [], float [], float [], int *);
float *c_csa3xs(int, float [], float [], float [], float [], float [],
                int [], float, int [], int, int, int, float [],
                float [], float [], int *);
float *c_csa3ls(int, float [], float [], float [], float [],
                int [], int, float [], float [], float[], int *);
float *c_csa3lxs(int, float [], float [], float [], float [],
                 float [], int [], float, int [],
                 int, float [], float [], float [], int *);

double *c_csa1d(int, double [], double [], int, int, double [], int *);
double *c_csa1xd(int, double [], double [], double [], int,
                 double, int, int, double [], int *);
double *c_csa2d(int, double [], double [], double [], int [],
                int, int, double [], double [], int *);
double *c_csa2xd(int, double [], double [], double [], double [], int [], 
                 double, int [], int, int, double [], double [], int *);
double *c_csa2ld(int, double [], double [], double [], int [],
                 int, double [], double [], int *);
double *c_csa2lxd(int, double [], double [], double [], double [], int [],
                  double, int [], int, double [], double [], int *);
double *c_csa3d(int, double [], double [], double [], double [], int [], 
                int, int, int, double [], double [], double [], int *);
double *c_csa3xd(int, double [], double [], double [], double [], double [],
                 int [], double, int [], int, int, int, double [],
                 double [], double [], int *);
double *c_csa3ld(int, double [], double [], double [], double [],
                 int [], int, double [], double [], double[], int *);
double *c_csa3lxd(int, double [], double [], double [], double [],
                  double [], int [], double, int [],
                  int, double [], double [], double [], int *);

void NGCALLF(csa1s,CSA1S)(int *, float *, float *, int *, int *, float *, 
             float *, int *, float *, int *);
void NGCALLF(csa1xs,CSA1XS)(int *, float *, float *, float *, int *, 
             float *, int *, int *, float *, float *, int *, float *, int *);
void NGCALLF(csa2xs,CSA2XS)(int *, float *, float *, float *, int *, float *,
             int *, int *, int *, float *, float *, float *, int *, float *, 
             int *);
void NGCALLF(csa2lxs,CSA2LXS)(int *, float *, float *, float *, int *, float *,
             int *, int *, float *, float *, float *, int *, float *, int *);
void NGCALLF(csa3xs,CSA3XS)(int *, float *, float *, float *, int *, float *,
             int *, int *, int *, int *, float *, float *, float *, float *, 
             int *, float *, int *);
void NGCALLF(csa3lxs,CSA3LXS)(int *, float *, float *, float *, int *, float *,
             int *, int *, float *, float *, float *, float *, int *, 
             float *, int *);

void NGCALLF(csa1d,CSA1D)(int *, double *, double *, int *, int *, double *, 
             double *, int *, double *, int *);
void NGCALLF(csa1xd,CSA1XD)(int *, double *, double *, double *, int *, 
             double *, int *, int *, double *, double *, int *, double *,
             int *);
void NGCALLF(csa2xd,CSA2XD)(int *, double *, double *, double *, int *, 
             double *,
             int *, int *, int *, double *, double *, double *, int *, 
             double *, int *);
void NGCALLF(csa2lxd,CSA2LXD)(int *, double *, double *, double *, int *, 
             double *, int *, int *, double *, double *, double *, int *, 
             double *, int *);
void NGCALLF(csa3xd,CSA3XD)(int *, double *, double *, double *, int *, 
             double *, int *, int *, int *, int *, double *, double *, 
             double *, double *, int *, double *, int *);
void NGCALLF(csa3lxd,CSA3LXD)(int *, double *, double *, double *, int *, 
             double *, int *, int *, double *, double *, double *, double *,
             int *, double *, int *);

/*
 *  Function prototypes for the fitgrid package.
 */
int c_ftseti(char *, int);
int c_ftsetr(char *, float);
int c_ftsetrd(char *, double);
int c_ftsetc(char *, char *);
int c_ftgeti(char *, int *);
int c_ftgetr(char *, float *);
int c_ftgetrd(char *, double *);
int c_ftgetc(char *, char *);
int c_ftsetfa(char *, int, float *);
int c_ftsetda(char *, int, double *);
int c_ftgetfa_size(char *);
float *c_ftgetfa_data(char *);
double *c_ftgetda_data(char *);
int c_ftcurv (int, float [], float [], int, float [], float []);
int c_ftcurvd(int, float [], float [], int, float [], float []);
int c_ftcurvi(float, float, int, float [], float [], float *);
int c_ftcurvp(int, float [], float [], float, int, float [], float yo[]);
int c_ftcurvpi(float, float, float, int, float [], float [], float *);
int c_ftcurvs(int, float [], float [], int, float [], int, float [], float []);
int c_ftcurvs1(int, float [], float [], int, float [],
               int, float, float, float [], float []);
int c_ftcurvps(int, float [], float [], float, int, float [],
               int, float [], float []);
int c_ftkurv(int, float [], float [], int, float [], float [], float []);
int c_ftkurvp(int, float [], float [], int, float [], float [], float []);
int c_ftkurvd(int, float [], float [], int, float [], float [], float [],
              float [], float [], float [], float []);
int c_ftkurvpd(int, float [], float [], int, float [], float [], float [],
               float [], float [], float [], float []);
float *c_ftsurf(int, int, float *, float *, float *,
                int, int, float *, float *, int *);
int c_ftcurvdp(int, double [], double [], int, double [], double []);
int c_ftcurvddp(int, double [], double [], int, double [], double []);
int c_ftcurvidp(double, double, int, double [], double [], double *);
int c_ftcurvpdp(int, double [], double [], double, int, double [], double []);
int c_ftcurvpidp(double, double, double, int, double [], double [], 
                 double *);
int c_ftcurvsdp(int, double [], double [], int, double [], int, double [],
                double []);
int c_ftcurvs1dp(int, double [], double [], int, double [],
                 int, double, double, double [], double []);
int c_ftcurvpsdp(int, double [], double [], double, int, double [],
                 int, double [], double []);
int c_ftkurvdp(int, double [], double [], int, double [], double [], 
               double []);
int c_ftkurvpdp(int, double [], double [], int, double [], double [],
                double []);
int c_ftkurvddp(int, double [], double [], int, double [], double [],
                double [], double [], double [], double [], double []);
int c_ftkurvpddp(int, double [], double [], int, double [], double [],
                 double [], double [], double [], double [], double []);
double *c_ftsurfdp(int, int, double *, double *, double *,
                   int, int, double *, double *, int *);

void NGCALLF(curv1,CURV1)(int *, float *, float *, float *, float *,
                          int *, float *, float *, float *, int *);
void NGCALLF(fcurv2,FCURV2)(int *, float *, float *, float *, float *, 
                            int *, float *, float *);
void NGCALLF(fcurvd,FCURVD)(int *, float *, float *, float *, float *, 
                            int *, float *, float *);
void NGCALLF(fcurvi,FCURVI)(float *, float *, int *, float *, float *, 
                            float *, float *, float *);
void NGCALLF(curvp1,CURVP1)(int *, float *, float *, float *, float *, 
                            float *, float *, int *);
void NGCALLF(fcurvp2,FCURVP2)(int *, float *, float *, float *, float *, 
                              float *, int *, float *, float *);
void NGCALLF(fcurvpi,FCURVPI)(float *, float *, float *, int *, float *, 
                              float *, float *, float *, float *);
void NGCALLF(curvs,CURVS)(int *, float *, float *, float *, int *, 
                          float *, float *, float *, float *, float *, 
                          float *, int *);
void NGCALLF(curvps,CURVPS)(int *, float *, float *, float *, float *, int *,
                          float *, float *, float *, float *, float *, 
                          float *, int *);
void NGCALLF(kurv1,KURV1)(int *, float *, float *, float *, float *, int *,
                          float *, float *, float *, float *, float *, 
                          int *);
void NGCALLF(fkurv2,FKURV2)(int *, float *, float *, int *, float *, 
                            float *, float *, float *, float *, 
                            float *, float *);
void NGCALLF(kurvp1,KURVP1)(int *, float *, float *, float *, float *, 
                            float *, float *, float *, int *);
void NGCALLF(fkurvp2,FKURVP2)(int *, float *, float *, int *, float *, 
                              float *, float *, float *, float *, float *,
                              float *);
void NGCALLF(fkurvd,FKURVD)(int *, float *, float *, int *, float *, 
                            float *, float *, float *, float *, float *, 
                            float *, float *, float *, float *, float *);
void NGCALLF(fkurvpd,FKURVPD)(int *, float *, float *, int *, float *, 
                              float *, float *, float *, float *, 
                              float *, float *, float *, float *, 
                              float *, float *);
void NGCALLF(surf1,SURF1)(int *, int *, float *, float *, float *, int *, 
                          float *, float *, float *, float *, float *, 
                          float *, float *, float *, int *, float *, 
                          float *, float *, int *);
void NGCALLF(fsurf2,FSURF2)(float *, float *, float *, int *, int *, 
                            float *, float *, float *, int *, float *, 
                            float *);
void NGCALLF(curv1dp,CURV1DP)(int *, double *, double *, double *, double *,
                              int *, double *, double *, double *, int *);
void NGCALLF(fcurv2dp,FCURV2DP)(int *, double *, double *, double *,
                                double *, int *, double *, double *);
void NGCALLF(fcurvddp,FCURVDDP)(int *, double *, double *, double *,
                                double *, int *, double *, double *);
void NGCALLF(fcurvidp,FCURVIDP)(double *, double *, int *, double *,
                                double *, double *, double *, double *);
void NGCALLF(curvp1dp,CURVP1DP)(int *, double *, double *, double *,
                                double *, double *, double *, int *);
void NGCALLF(fcurvp2dp,FCURVP2DP)(int *, double *, double *, double *,
                                  double *, double *, int *, double *,
                                  double *);
void NGCALLF(fcurvpidp,FCURVPIDP)(double *, double *, double *, int *,
                                  double *, double *, double *,
                                  double *, double *);
void NGCALLF(curvsdp,CURVSDP)(int *, double *, double *, double *,
                              int *, double *, double *, double *,
                              double *, double *, double *, int *);
void NGCALLF(curvpsdp,CURVPSDP)(int *, double *, double *, double *,
                                double *, int *, double *, double *, 
                                double *, double *, double *, double *,
                                int *);
void NGCALLF(kurv1dp,KURV1DP)(int *, double *, double *, double *, double *,
                              int *, double *, double *, double *, double *,
                              double *, int *);
void NGCALLF(fkurv2dp,FKURV2DP)(int *, double *, double *, int *,
                                double *, double *, double *, double *,
                                double *, double *, double *);
void NGCALLF(kurvp1dp,KURVP1DP)(int *, double *, double *, double *,
                                double *, double *, double *, double *,
                                int *);
void NGCALLF(fkurvp2dp,FKURVP2DP)(int *, double *, double *, int *,
                                  double *, double *, double *, double *, 
                                  double *, double *, double *);
void NGCALLF(fkurvddp,FKURVDDP)(int *, double *, double *, int *,
                                double *, double *, double *, double *,
                                double *, double *, double *, double *,
                                double *, double *, double *);
void NGCALLF(fkurvpddp,FKURVPDDP)(int *, double *, double *, int *,
                                  double *, double *, double *, double *,
                                  double *, double *, double *, double *,
                                  double *, double *, double *);
void NGCALLF(surf1dp,SURF1DP)(int *, int *, double *, double *, double *,
                              int *, double *, double *, double *,
                              double *, double *, double *, double *,
                              double *, int *, double *, double *,
                              double *, int *);
void NGCALLF(fsurf2dp,FSURF2DP)(double *, double *, double *, int *,
                                int *, double *, double *, double *,
                                int *, double *, double *);

/*
 *  Function prototypes for the dsgrid package.
 */
void     c_dssetc(char *, char *);
void     c_dsgetc(char *, char *);
void     c_dsseti(char *, int);
void     c_dsgeti(char *, int *);
void     c_dsgetr(char *, float *);
void     c_dssetr(char *, float);
void     c_dssetrd(char *, double);
void     c_dsgetrd(char *, double *);
float    *c_dsgrid2s(int, float [], float [], float [],
                     int, int, float [], float [], int *);
double   *c_dsgrid2d(int, double [], double [], double [],
                     int, int, double [], double [], int *);
float    *c_dsgrid3s(int, float [], float [], float [], float [],
                     int, int, int, float [], float [], float [], int *);
double   *c_dsgrid3d(int, double [], double [], double [], double [],
                     int, int, int, double [], double [], double [], int *);
void     c_dspnt3d(int, double [], double [], double [], double [],
                   int, double [], double [], double [], double [], int *);
void     c_dspnt2d(int, double [], double [], double [],
                   int, double [], double [], double [], int *);
void     c_dspnt3s(int, float [], float [], float [], float [],
                   int, float [], float [], float [], float [], int *);
void     c_dspnt2s(int, float [], float [], float [],
                   int, float [], float [], float [], int *);
void NGCALLF(dsgrid3d,DSGRID3D) (int *, double *, double *, double *,
                double *, int *, int *, int *,
                double *, double *, double *, double *, int *ier);
void NGCALLF(dsgrid2d,DSGRID2D) (int *, double *, double *, double *,
                int *, int *, double *, double *, double *, int *);

void NGCALLF(dspnt3d,DSPNT3D) (int *, double *, double *, double *,
                double *, int *, double *, double *, double *, double *, 
                int *);
void NGCALLF(dspnt2d,DSPNT2D) (int *, double *, double *, double *, int *,
                double *, double *, double *, int *);
void NGCALLF(dsseti,DSSETI) (char *, int *);
void NGCALLF(dsgeti,DSGETI) (char *, int *);
void NGCALLF(dssetrd,DSSETRD) (char *, double *);
void NGCALLF(dsgetrd,DSGETRD) (char *, double *);
void NGCALLF(fdssetc,FDSSETC) (char *, char *, int *);
void NGCALLF(fdsgetc,FDSGETC) (char *, char *, int *);
void NGCALLF(dsgrid3s,DSGRID3S) (int *, float *, float *, float *, float *, 
                int *, int *, int *, float *, float *, float *, float *, 
                int *);
void NGCALLF(dsgrid2s,DSGRID2S) (int *, float *, float *, float *,
               int *, int *, float *, float *, float *, int *);
void NGCALLF(dssetr,DSSETR) (char *, float *);
void NGCALLF(dsgetr,DSGETR) (char *, float *);
void NGCALLF(dspnt3s,DSPNT3S) (int *, float *, float *, float *, float *, 
                int *, float *, float *, float *, float *, int *);
void NGCALLF(dspnt2s,DSPNT2S) (int *, float *, float *, float *, int *,
                float *, float *, float *, int *);

/*
 *  Function prototypes for the natgrid package.
 */

/* was duplicated in nnuhead.h */
void    c_nnseti(char *, int);
void    c_nngeti(char *, int *);
void    c_nnsetr(char *, float);
void    c_nngetr(char *, float *);

/* was duplicated in nnuheads.h */
void    c_nnsetc(char *, char *);
void    c_nngetc(char *, char *);
float   *c_natgrids(int, float [], float [], float [],
                     int, int, float [], float [], int *);

/* was duplicated in nncheads.h */
void    c_nngetslopes(int, int, float *, int *);
void    c_nngetaspects(int, int, float *, int *);
void    c_nnpntinits(int, float [], float [], float []);
void    c_nnpnts(float, float, float *);
void    c_nnpntend();
void    c_nngetwts(int *, int *, float *, float *, float *, float *);

/* was duplicated in nnuheadd.h */
void    c_nnsetrd(char *, double);
void    c_nngetrd(char *, double *);
double  *c_natgridd(int, double [], double [], double [],
                     int, int, double [], double [], int *);

/* was duplicated in nncheadd.h */
void    c_nngetsloped(int, int, double *, int *);
void    c_nngetaspectd(int, int, double *, int *);
void    c_nnpntinitd(int, double [], double [], double []);
void    c_nnpntd(double, double, double *);
void    c_nnpntendd();
void    c_nngetwtsd(int *, int *, double *, double *, double *, double *);

#ifdef  UNICOS
#include <fortran.h>
#define NGstring            _fcd
#define NGCstrToFstr(cstr,len) ((cstr)?_cptofcd((char *)cstr,len):_cptofcd("",0)
)
#define NGFstrToCstr(fstr) (_fcdtocp(fstr))
#define NGFlgclToClgcl(flog)  (_ltob(&flog))
#define NGClgclToFlgcl(clog)  (_btol(clog))
float   *c_natgrids(int, float [], float [], float [],
float   *c_natgrids(int, float [], float [], float [],
float   *c_natgrids(int, float [], float [], float [],
float   *c_natgrids(int, float [], float [], float [],
                     int, int, float [], float [], int *);
                     int, int, float [], float [], int *);
                     int, int, float [], float [], int *);
                     int, int, float [], float [], int *);
#else
#define NGstring            char *
#define NGCstrToFstr(cstr,len) (char *)cstr
#define NGFstrToCstr(fstr) fstr
#define NGFlgclToClgcl(flog)  flog
#define NGClgclToFlgcl(clog)  clog
#endif

#define NGSTRLEN(cstr)      ((cstr)?strlen(cstr):0)
 

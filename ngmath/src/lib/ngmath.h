/* 
 * $Id: ngmath.h,v 1.17 2003-11-25 22:14:32 haley Exp $
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
* along with this software; if not, write to the Free Software          *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

#include <stdio.h>

/*
 *  This file contains the function prototypes for all
 *  user entry points in the ngmath library.
 */

/*
 *  Function prototypes for the shgrid package.
 */
int c_shgetnp(float, float, float, int, float *, float *, float *,
              int, int *); 
float *c_shgrid(int, float [], float [], float [], float [], 
                int, int, int, float [], float [], float [], int*);

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

/*
 *  Function prototypes for the natgrid package.
 */
void    c_nnseti(char *, int);
void    c_nngeti(char *, int *);
void    c_nnsetr(char *, float);
void    c_nngetr(char *, float *);
void    c_nnsetc(char *, char *);
void    c_nngetc(char *, char *);
void    c_nngetslopes(int, int, float *, int *);
void    c_nngetaspects(int, int, float *, int *);
void    c_nngetwts(int *, int *, float *, float *, float *, float *);
void    c_nnpntinits(int, float [], float [], float []);
void    c_nnpnts(float, float, float *);
void    c_nnpntend();
float   *c_natgrids(int, float [], float [], float [],
                     int, int, float [], float [], int *);

void    c_nnsetrd(char *, double);
void    c_nngetrd(char *, double *);
void    c_nngetsloped(int, int, double *, int *);
void    c_nngetaspectd(int, int, double *, int *);
void    c_nngetwtsd(int *, int *, double *, double *, double *, double *);
void    c_nnpntinitd(int, double [], double [], double []);
void    c_nnpntd(double, double, double *);
void    c_nnpntendd();
double  *c_natgridd(int, double [], double [], double [],
                     int, int, double [], double [], int *);

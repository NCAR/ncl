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
 *  Specify all of the function prototypes.
 *
 *  Single-precision C routines.
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

/*
 *  Double-precision C routines.
 */
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
 *  Fortran function macro.  This macro is used to provide the appropriate
 *  system-specific C function name for it to be Fortran callable.
 */
#ifndef NGCALLF
 
#if defined(UNICOS) || defined(NGCAPS)
#define NGCALLF(reg,caps)       caps
 
#elif   defined(RS6000) || defined(__hpux)
#define NGCALLF(reg,caps)       reg
 
#else
#ifdef  __STDC__
#define NGCALLF(reg,caps)       reg##_
#else
#define NGCALLF(reg,caps)       reg/**/_
 
#endif  /* __STDC__ */
#endif  /* UNICOS else ... */
#endif  /* NGCALLF */

/*
 *  Prototypes for single-precision Fortran function calls.
 */
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

/*
 *  Prototypes for double-precision Fortran function calls.
 */
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

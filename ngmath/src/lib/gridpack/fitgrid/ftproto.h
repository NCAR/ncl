/*
 * $Id: ftproto.h,v 1.4 2000-09-19 23:54:46 fred Exp $
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
 *  Specify all of the function prototypes.
 */
int c_ftseti(char *, int);
int c_ftsetr(char *, float);
int c_ftsetc(char *, char *);
int c_ftgeti(char *, int *);
int c_ftgetr(char *, float *);
int c_ftgetc(char *, char *);
int c_ftsetfa(char *, int, float *);
int c_ftgetfa_size(char *);
float *c_ftgetfa_data(char *);
int c_ftcurv (int, float [], float [], int, float [], float []);
int c_ftcurvd(int, float [], float [], int, float [], float []);
int c_ftcurvi(float, float, int, float [], float [], float *);
int c_ftcurvp(int, float [], float [], float, int, float [], float yo[]);
int c_ftcurvpi(float, float, float, int, float [], float [], float *);
int c_ftcurvs(int, float [], float [], int, float [], int, float [], float []);
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

/*
 *  Fortran function macro.  This macro is used to provide the appropriate
 *  system-specific C function name for it to be Fortran callable.
 */
#ifndef NGCALLF
 
#ifdef  UNICOS
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
 *  Prototypes for Fortran function calls.
 */
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
void NGCALLF(fkurvp2,FKURVP2)(int *, float *, float *, int *, float *, float *, float *, float *, float *, float *, float *);
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

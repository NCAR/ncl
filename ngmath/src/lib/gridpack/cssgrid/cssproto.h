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

#include <stddef.h>
#include <string.h>
#include <stdio.h>
 
/*
 *  Specify all of the function prototypes.
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
 *  Fortran function macro.  This macro is used to provide the appropriate
 *  system-specific C function name for it to be Fortran callable.
 */
#ifndef NGCALLF

#if defined(F_UPPERCASE)

#define NGCALLF(reg,caps)       caps

#elif defined(F_NO_UNDERSCORES)

#define NGCALLF(reg,caps)       reg
 
#else
#ifdef  __STDC__
#define NGCALLF(reg,caps)       reg##_
#else
#define NGCALLF(reg,caps)       reg/**/_
 
#endif  /* __STDC__ */
#endif	/* F_UPPERCASE */
#endif  /* NGCALLF */

#ifdef  UNICOS
#include <fortran.h>
#define NGstring            _fcd
#define NGCstrToFstr(cstr,len) ((cstr)?_cptofcd((char *)cstr,len):_cptofcd("",0)
)
#define NGFstrToCstr(fstr) (_fcdtocp(fstr))
#define NGFlgclToClgcl(flog)  (_ltob(&flog))
#define NGClgclToFlgcl(clog)  (_btol(clog))
#else
#define NGstring            char *
#define NGCstrToFstr(cstr,len) (char *)cstr
#define NGFstrToCstr(fstr) fstr
#define NGFlgclToClgcl(flog)  flog
#define NGClgclToFlgcl(clog)  clog
#endif

#define NGSTRLEN(cstr)      ((cstr)?strlen(cstr):0)
 
/*
 *  Prototypes for Fortran function calls.
 */
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

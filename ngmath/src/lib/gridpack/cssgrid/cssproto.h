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

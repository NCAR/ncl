/*
 * $Id: nnuheadd.h,v 1.4 2000-07-13 02:49:27 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU Lesser General Public License as        *
* published by the Free Software Foundation; either version 2.1 of the  *
* License, or (at your option) any later version.                       *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* Lesser General Public License for more details.                       *
*                                                                       *
* You should have received a copy of the GNU Lesser General Public      *
* License along with this software; if not, write to the Free Software  *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

void   c_nnsetrd(char *, double);
void   c_nngetrd(char *, double *);

extern void   c_nngetsloped(int, int, double *, int *);
extern void   c_nngetaspectd(int, int, double *, int *);
extern void   c_nnpntinitd(int, double *, double *, double *);
extern void   c_nnpntd(double, double, double *);
extern void   c_nnpntendd();

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
 *  Fortran entry points.
 */
void  NGCALLF(natgridd,NATGRIDD) (int *, double *, double *, double *,
              int *, int *, double *, double *, double *, int *);
void  NGCALLF(nnsetrd,NNSETRD) (char *, double *);
void  NGCALLF(nngetrd,NNGETRD) (char *, double *);
void  NGCALLF(nngetsloped,NNGETSLOPED) (int *, int *, double *, int *);
void  NGCALLF(nngetaspectd,NNGETASPECTD) (int *, int *, double *, int *);
void  NGCALLF(nnpntinitd,NNPNTINITD) (int *, double *, double *, double *);
void  NGCALLF(nnpntd,NNPNTD) (double *, double *, double *);
void  NGCALLF(nnpntend,NNPNTEND) ();

double  *c_natgridd(int, double [], double [], double [],
                    int, int, double [], double [], int *);

/*
 * $Id: nnuhead.h,v 1.4 2000-08-22 03:34:55 haley Exp $
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
#include <string.h>

#define EQ             ==
#define NE             !=
#define AND            &&
#define OR             ||

extern int       igrad,  densi,   non_neg,    sdip,        rads,
                 optim,  extrap,  southhemi,  updir, auto_scale,
                 adf,    nndup;

extern double    bI,        bJ,         magx,       magy,
                 magz,      horilap,    vertlap,    nuldat,
                 magx_auto, magy_auto,  magz_auto,  horilap_save,
                 vertlap_save;

extern char      tri_file[], error_file[], emsg[];
extern FILE      *filee;

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

void   c_nnsetc(char *, char *);
void   c_nngetc(char *, char *);
void   c_nnseti(char *, int);
void   c_nngeti(char *, int *);

extern void   ErrorHnd(int, char *, FILE *, char *);

void  NGCALLF(nnseti,NNSETI) (char *, int *);
void  NGCALLF(nngeti,NNGETI) (char *, int *);

/*
 * $Id: nnuheads.h,v 1.4 2000-07-13 02:49:27 haley Exp $
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

void   c_nnsetr(char *, float);
void   c_nngetr(char *, float *);

extern void   c_nngetslopes(int, int, float *, int *);
extern void   c_nngetaspects(int, int, float *, int *);
extern void   c_nnpntinits(int, float *, float *, float *);
extern void   c_nnpnts(float, float, float *);
extern void   c_nnpntend();

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
void  NGCALLF(natgrids,NATGRIDS) (int *, float *, float *, float *,
              int *, int *, float *, float *, float *, int *);
void  NGCALLF(nnsetr,NNSETR) (char *, float *);
void  NGCALLF(nngetr,NNGETR) (char *, float *);
void  NGCALLF(nngetslopes,NNGETSLOPES) (int *, int *, float *, int *);
void  NGCALLF(nngetaspects,NNGETASPECTS) (int *, int *, float *, int *);
void  NGCALLF(nnpntinits,NNPNTINITS) (int *, float *, float *, float *);
void  NGCALLF(nnpnts,NNPNTS) (float *, float *, float *);
void  NGCALLF(nnpntend,NNPNTEND) ();

float  *c_natgrids(int, float [], float [], float [],
                   int, int, float [], float [], int *);

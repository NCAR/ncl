/*
 * $Id: nnuheadd.h,v 1.9 2005-05-04 02:02:49 fred Exp $
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

#include <ncarg/ngmath.h>

extern void   c_nngetsloped(int, int, double *, int *);
extern void   c_nngetaspectd(int, int, double *, int *);
extern void   c_nnpntinitd(int, double *, double *, double *);
extern void   c_nnpntd(double, double, double *);
extern void   c_nnpntendd();

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
void  NGCALLF(nnpntendd,NNPNTENDD) ();

/*
 * $Id: nncheadd.h,v 1.3 2000-07-13 02:49:25 haley Exp $
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

struct asinfod
{  int          crows;
   int          ccols;
   double       **aspect_outd;
   double       **slope_outd;
};
struct asinfod  curasd;

extern double   armind(int, double *);
extern double   armaxd(int, double *);

extern void     Initialized(int, double [], double [], int, int,
                            double [], double []);

int             ReadDatad(int, double *, double *, double *);
double          **MakeGridd(int, int, double *, double *);

void            c_nngetsloped(int, int, double *, int *);
void            c_nngetaspectd(int, int, double *, int *);
void            c_nnpntinitd(int, double [], double [], double []);
extern void     c_nnpntd(double, double, double *);
void            c_nnpntendd();

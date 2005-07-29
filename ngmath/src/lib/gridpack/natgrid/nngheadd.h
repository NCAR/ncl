/*
 * $Id: nngheadd.h,v 1.5 2005-07-29 23:19:56 fred Exp $
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

void    Initialized(int, double [], double [], int, int,
                    double [], double []);

double  armind(int, double *);
double  armaxd(int, double *);

extern int     ReadDatad();
extern double  **MakeGridd(int, int, double *, double *);

extern void   c_nnsetrd(char *, double);
extern void   c_nngetrd(char *, double *);

extern void   Terminate();

extern int    cull_dtriples(int, double *);
extern int    comp_dtriples(const void *, const void *);

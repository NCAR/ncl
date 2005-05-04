/*
 * $Id: nnuheads.h,v 1.11 2005-05-04 18:29:45 fred Exp $
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

extern void   c_nngetslopes(int, int, float *, int *);
extern void   c_nngetaspects(int, int, float *, int *);
extern void   c_nnpntinits(int, float *, float *, float *);
extern void   c_nnpnts(float, float, float *);
extern void   c_nnpntend();
extern void   c_nngetwts(int *, int *, float *, float *, float *, float *);
extern void   c_nngetwtsd(int *, int *, double *, double *, double *, double *);

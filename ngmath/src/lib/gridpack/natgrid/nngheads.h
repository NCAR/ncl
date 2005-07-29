/*
 * $Id: nngheads.h,v 1.5 2005-07-29 23:19:56 fred Exp $
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

void    Initialize(int, float [], float [], int, int, 
                                  float [], float []);

double  armin(int, float *);
double  armax(int, float *);

extern int    ReadData();
extern float  **MakeGrid(int, int, float *, float *);

extern void   c_nnsetr(char *, float);
extern void   c_nngetr(char *, float *);

extern void   Terminate();

extern int    cull_striples(int, float *);
extern int    comp_striples(const void *, const void *);

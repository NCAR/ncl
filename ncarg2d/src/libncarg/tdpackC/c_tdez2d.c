/*
 *      $Id: c_tdez2d.c,v 1.3 2000-07-12 16:26:38 haley Exp $
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

#include <ncarg/ncargC.h>

void c_tdez2d
#ifdef NeedFuncProto
(
    int nx,
    int ny,
    float *x,
    float *y,
    float *z,
    float rmult,
    float theta,
    float phi,
    int ist
)
#else
 (nx,ny,x,y,z,rmult,theta,phi,ist)
    int nx,
    int ny,
    float *x,
    float *y,
    float *z,
    float rmult,
    float theta,
    float phi,
    int ist
#endif
{
    int nx2;
    int ny2;
    float rmult2;
    float theta2;
    float phi2;
    int ist2;

    nx2 = nx;
    ny2 = ny;
    rmult2 = rmult;
    theta2 = theta;
    phi2 = phi;
    ist2 = ist;

    NGCALLF(tdez2d,TDEZ2D)(&nx2,&ny2,x,y,z,&rmult2,&theta2,&phi2,&ist2);
}

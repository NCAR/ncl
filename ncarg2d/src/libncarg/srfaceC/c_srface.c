/*
 *	$Id: c_srface.c,v 1.4 2000-08-22 15:06:30 haley Exp $
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
* along with this software; if not, write to the Free Software         *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

#include <ncarg/ncargC.h>

extern void NGCALLF(srface,SRFACE)(float*,float*,float*,int*,int*,int*,int*,
                                   float*,float*);

void c_srface
#ifdef NeedFuncProto
(
    float *x,
    float *y,
    float *z,
    int *m,
    int mx,
    int nx,
    int ny,
    float s[6],
    float stereo
)
#else
(x,y,z,m,mx,nx,ny,s,stereo)
    float *x;
    float *y;
    float *z;
    int *m;
    int mx;
    int nx;
    int ny;
    float s[6];
    float stereo;
#endif
{
    NGCALLF(srface,SRFACE)(x,y,z,m,&mx,&nx,&ny,s,&stereo);
}

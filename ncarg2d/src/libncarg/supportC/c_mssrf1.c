/*
 *	$Id: c_mssrf1.c,v 1.3 2000-07-31 20:12:04 haley Exp $
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

extern void NGCALLF(mssrf1,MSSRF1)(int*,int*,float*,float*,float*,int*,
                                   float*,float*,float*,float*,float*,
                                   float*,float*,float*,int*,float*,float*,
                                   float*,int*);


void c_mssrf1
#ifdef NeedFuncProto
(
    int m,
    int n,
    float *x,
    float *y,
    float *z,
    int iz,
    float *zx1,
    float *zxm,
    float *zy1,
    float *zyn,
    float zxy11,
    float zxym1,
    float zxy1n,
    float zxymn,
    int islpsw,
    float *zp,
    float *temp,
    float sigma,
    int *ierr
)
#else
(m,n,x,y,z,iz,zx1,zxm,zy1,zyn,zxy11,zxym1,zxy1n,zxymn,islpsw,zp,temp,sigma,ierr)
    int m;
    int n;
    float *x;
    float *y;
    float *z;
    int iz;
    float *zx1;
    float *zxm;
    float *zy1;
    float *zyn;
    float zxy11;
    float zxym1;
    float zxy1n;
    float zxymn;
    int islpsw;
    float *zp;
    float *temp;
    float sigma;
    int *ierr;
#endif
{
    NGCALLF(mssrf1,MSSRF1)(&m,&n,x,y,z,&iz,zx1,zxm,zy1,zyn,&zxy11,&zxym1,
                           &zxy1n,&zxymn,&islpsw,zp,temp,&sigma,ierr);
}

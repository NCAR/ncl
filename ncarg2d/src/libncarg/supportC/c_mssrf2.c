/*
 *	$Id: c_mssrf2.c,v 1.2 2000-07-12 16:26:27 haley Exp $
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

extern float NGCALLF(mssrf2,MSSRF2)(float*,float*,int*,int*,float*,float*,
									float*,int*,float*,float*);

float c_mssrf2 
#ifdef NeedFuncProto
(
    float xx,
    float yy,
    int m,
    int n,
    float *x,
    float *y,
    float *z,
    int iz,
    float *zp,
    float sigma
)
#else
(xx,yy,m,n,x,y,z,iz,zp,sigma)
    float xx;
    float yy;
    int m;
    int n;
    float *x;
    float *y;
    float *z;
    int iz;
    float *zp;
    float sigma;
#endif
{
    float xmssrf2;

    xmssrf2 = (float)NGCALLF(mssrf2,MSSRF2)(&xx,&yy,&m,&n,x,y,z,&iz,zp,&sigma);
    return(xmssrf2);
}

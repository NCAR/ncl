/*
 *	$Id: c_msbsf2.c,v 1.2 2000-07-12 16:26:26 haley Exp $
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

void c_msbsf2
#ifdef NeedFuncProto
(
    float dxmin,
    float dxmax,
    int md,
    float dymin,
    float dymax,
    int nd,
    float *dz,
    int idz,
    int m,
    int n,
    float xmin,
    float xmax,
    float ymin,
    float ymax,
    float *z,
    int iz,
    float *zp,
    float *work,
    float sigma
)
#else
(dxmin,dxmax,md,dymin,dymax,nd,dz,idz,m,n,xmin,xmax,ymin,ymax,z,iz,zp,work,sigma)
    float dxmin;
    float dxmax;
    int md;
    float dymin;
    float dymax;
    int nd;
    float *dz;
    int idz;
    int m;
    int n;
    float xmin;
    float xmax;
    float ymin;
    float ymax;
    float *z;
    int iz;
    float *zp;
    float *work;
    float sigma;
#endif
{
    float *dz2,*z2,*zp2,*work2;
    float dxmin2,dxmax2,dymin2,dymax2,xmin2,xmax2,ymin2,ymax2,sigma2;
    int i, j, k, l, l2;

    dxmin2 = dxmin;
    dxmax2 = dxmax;
    dymin2 = dymin;
    dymax2 = dymax;
    xmin2 = xmin;
    xmax2 = xmax;
    ymin2 = ymin;
    ymax2 = ymax;
    sigma2 = sigma;

    NGCALLF(msbsf2,MSBSF2)(&dxmin2,&dxmax,&md,&dymin2,&dymax2,&nd,dz,&idz,&m,&n,&xmin2,&xmax2,&ymin2,&ymax2,z,&iz,zp,work,&sigma2);
}

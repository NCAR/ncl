/*
 *	$Id: c_msbsf1.c,v 1.3 2000-07-31 20:12:03 haley Exp $
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

extern void NGCALLF(msbsf1,MSBSF1)(int*,int*,float*,float*,float*,float*,
                                   float*,int*,float*,float*,float*);


void c_msbsf1 
#ifdef NeedFuncProto
(
    int m,
    int n,
    float xmin,
    float xmax,
    float ymin,
    float ymax,
    float *z,
    int iz,
    float *zp,
    float *temp,
    float sigma
)
#else
(m,n,xmin,xmax,ymin,ymax,z,iz,zp,temp,sigma)
    int m;
    int n;
    float xmin;
    float xmax;
    float ymin;
    float ymax;
    float *z;
    int iz;
    float *zp;
    float *temp;
    float sigma;
#endif
{
    NGCALLF(msbsf1,MSBSF1)(&m,&n,&xmin,&xmax,&ymin,&ymax,z,&iz,zp,temp,
                           &sigma);
}

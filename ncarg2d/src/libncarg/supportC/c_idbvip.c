/*
 *	$Id: c_idbvip.c,v 1.2 2000-07-12 16:26:26 haley Exp $
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

extern void NGCALLF(idbvip,IDBVIP)(int*,int*,float*,float*,float*,int*,
                                   float*,float*,float*,int*,float*);

void c_idbvip
#ifdef NeedFuncProto
(
    int md,
    int ndp,
    float *xd,
    float *yd,
    float *zd,
    int nip,
    float *xi,
    float *yi,
    float *zi,
    int *iwk,
    float *wk
)
#else
(md,ndp,xd,yd,zd,nip,xi,yi,zi,iwk,wk)
    int md;
    int ndp;
    float *xd;
    float *yd;
    float *zd;
    int nip;
    float *xi;
    float *yi;
    float *zi;
    int *iwk;
    float *wk;
#endif
{
    NGCALLF(idbvip,IDBVIP)(&md,&ndp,xd,yd,zd,&nip,xi,yi,zi,iwk,wk);
}

/*
 *      $Id: c_tdstri.c,v 1.2 2000-07-12 16:26:42 haley Exp $
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

void c_tdstri
#ifdef NeedFuncProto
(
    float *u,
    int    nu,
    float *v,
    int    nv,
    float *w,
    int    lw1d,
    float *rtri,
    int    mtri,
    int   *ntri,
    int    irst
)
#else
(u,nu,v,nv,w,lw1d,rtri,mtri,ntri,irst)
    float *u;
    int    nu;
    float *v;
    int    nv;
    float *w;
    int    lw1d;
    float *rtri;
    int    mtri;
    int   *ntri;
    int    irst;
#endif
{
    int nu2,nv2,lw1d2,mtri2,irst2;
    nu2=nu;
    nv2=nv;
    lw1d2=lw1d;
    mtri2=mtri;
    irst2=irst;
    NGCALLF(tdstri,TDSTRI)(u,&nu2,v,&nv2,w,&lw1d2,rtri,&mtri2,ntri,&irst2);
}

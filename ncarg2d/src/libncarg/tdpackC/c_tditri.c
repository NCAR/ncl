/*
 *      $Id: c_tditri.c,v 1.4 2000-08-22 15:07:17 haley Exp $
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

extern void NGCALLF(tditri,TDITRI)(float*,int*,float*,int*,float*,int*,
                                   float*,int*,int*,float*,float*,int*,
                                   int*,int*);

void c_tditri
#ifdef NeedFuncProto
(
    float *u,
    int    nu,
    float *v,
    int    nv,
    float *w,
    int    nw,
    float *f,
    int    lf1d,
    int    lf2d,
    float  fiso,
    float *rtri,
    int    mtri,
    int   *ntri,
    int    irst
)
#else
(u,nu,v,nv,w,nw,f,lf1d,lf2d,fiso,rtri,mtri,ntri,irst)
    float *u;
    int    nu;
    float *v;
    int    nv;
    float *w;
    int    nw;
    float *f;
    int    lf1d;
    int    lf2d;
    float  fiso;
    float *rtri;
    int    mtri;
    int   *ntri;
    int    irst;
#endif
{
    NGCALLF(tditri,TDITRI)(u,&nu,v,&nv,w,&nw,f,&lf1d,&lf2d,&fiso,
                                           rtri,&mtri,ntri,&irst);
}

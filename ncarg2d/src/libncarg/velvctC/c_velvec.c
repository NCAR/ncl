/*
 *	$Id: c_velvec.c,v 1.3 2000-07-31 20:12:16 haley Exp $
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

extern void NGCALLF(velvec,VELVEC)(float*,int*,float*,int*,int*,int*,float*,
                                   float*,int*,int*,float*);

void c_velvec
#ifdef NeedFuncProto
(
    float *u,
    int lu,
    float *v,
    int lv,
    int m,
    int n,
    float flo,
    float hi,
    int nset,
    int ispv,
    float *spv
)
#else
(u,lu,v,lv,m,n,flo,hi,nset,ispv,spv)
    float *u;
    int lu;
    float *v;
    int lv;
    int m;
    int n;
    float flo;
    float hi;
    int nset;
    int ispv;
    float *spv;
#endif
{
    NGCALLF(velvec,VELVEC)(u,&lu,v,&lv,&m,&n,&flo,&hi,&nset,&ispv,spv);
}

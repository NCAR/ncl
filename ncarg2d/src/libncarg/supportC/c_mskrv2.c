/*
 *	$Id: c_mskrv2.c,v 1.3 2000-07-31 20:12:04 haley Exp $
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

extern void NGCALLF(mskrv2,MSKRV2)(float*,float*,float*,int*,float*,float*,
                                   float*,float*,float*,float*,int*,float*);

void c_mskrv2 
#ifdef NeedFuncProto
(
    float t,
    float *xs,
    float *ys,
    int n,
    float *x,
    float *y,
    float *xp,
    float *yp,
    float *s,
    float sigma,
    int ics,
    float *slp
)
#else
(t,xs,ys,n,x,y,xp,yp,s,sigma,ics,slp)
    float t;
    float *xs;
    float *ys;
    int n;
    float *x;
    float *y;
    float *xp;
    float *yp;
    float *s;
    float sigma;
    int ics;
    float *slp;
#endif
{
    NGCALLF(mskrv2,MSKRV2)(&t,xs,ys,&n,x,y,xp,yp,s,&sigma,&ics,slp);
}

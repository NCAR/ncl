/*
 *	$Id: c_mskrv1.c,v 1.2 2000-07-12 16:26:27 haley Exp $
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

void c_mskrv1 
#ifdef NeedFuncProto
(
    int n,
    float *x,
    float *y,
    float slp1,
    float slpn,
    float *xp,
    float *yp,
    float *temp,
    float *s,
    float sigma,
    int islpsw
)
#else
(n,x,y,slp1,slpn,xp,yp,temp,s,sigma,islpsw)
    int n;
    float *x;
    float *y;
    float slp1;
    float slpn;
    float *xp;
    float *yp;
    float *temp;
    float *s;
    float sigma;
    int islpsw;
#endif
{
    float slp12, slpn2, sigma2;

    slp12 = slp1;
    slpn2 = slpn;
    sigma2 = sigma;
    NGCALLF(mskrv1,MSKRV1)(&n,x,y,&slp12,&slpn2,xp,yp,temp,s,&sigma2,&islpsw);
}

/*
 *	$Id: c_strmln.c,v 1.3 2000-07-31 20:12:00 haley Exp $
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

extern void NGCALLF(strmln,STRMLN)(float*,float*,float*,int*,int*,int*,int*,
                                   int*);

void c_strmln
#ifdef NeedFuncProto
(
    float *u,
    float *v,
    float *work,
    int imax,
    int iptsx,
    int jptsy,
    int nset,
    int *ier
)
#else
(u,v,work,imax,iptsx,jptsy,nset,ier)
    float *u;
    float *v;
    float *work;
    int imax;
    int iptsx;
    int jptsy;
    int nset;
    int *ier;
#endif
{
    NGCALLF(strmln,STRMLN)(u,v,work,&imax,&iptsx,&jptsy,&nset,ier);
}

/*
 *      $Id: c_tdcudp.c,v 1.1 2003-11-24 21:03:42 kennison Exp $
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

extern void NGCALLF(tdcudp,TDCUDP)(float*,float*,float*,int*,int*,
				   float*,float*);

void c_tdcudp
#ifdef NeedFuncProto
(
    float *ucrv,
    float *vcrv,
    float *wcrv,
    int    ncrv,
    int    iarh,
    float  arhl,
    float  arhw
)
#else
(ucrv,vcrv,wcrv,ncrv,iarh,arhl,arhw)
    float *ucrv;
    float *vcrv;
    float *wcrv;
    int    ncrv;
    int    iarh;
    float  arhl;
    float  arhw;
#endif
{
    NGCALLF(tdcudp,TDCUDP)(ucrv,vcrv,wcrv,&ncrv,&iarh,&arhl,&arhw);
}

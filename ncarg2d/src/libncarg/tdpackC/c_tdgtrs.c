/*
 *      $Id: c_tdgtrs.c,v 1.4 2000-07-31 20:12:08 haley Exp $
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

extern void NGCALLF(tdgtrs,TDGTRS)(int*,int*,int*,int*,int*,int*,
                                   int*,int*,float*,float*,float*);

void c_tdgtrs
#ifdef NeedFuncProto
(
    int    irst,
    int   *ifc1,
    int   *ifc2,
    int   *ifc3,
    int   *ifc4,
    int   *ilc1,
    int   *ilc2,
    int   *iltd,
    float *ustp,
    float *vstp,
    float *wstp
)
#else
(irst,ifc1,ifc2,ifc3,ifc4,ilc1,ilc2,iltd,ustp,vstp,wstp)
    int    irst;
    int   *ifc1;
    int   *ifc2;
    int   *ifc3;
    int   *ifc4;
    int   *ilc1;
    int   *ilc2;
    int   *iltd;
    float *ustp;
    float *vstp;
    float *wstp;
#endif
{
    NGCALLF(tdgtrs,TDGTRS)(&irst,ifc1,ifc2,ifc3,ifc4,ilc1,
                                 ilc2,iltd,ustp,vstp,wstp);
}

/*
 *	$Id: c_trn32s.c,v 1.2 2000-07-12 16:25:55 haley Exp $
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

void c_trn32s
#ifdef NeedFuncProto
(
    float x,
    float y,
    float z,
    float xt,
    float yt,
    float zt,
    int iflag
)
#else
(x,y,z,xt,yt,zt,iflag)
    float x;
    float y;
    float z;
    float xt;
    float yt;
    float zt;
    int iflag;
#endif
{
    float x2, y2, z2, xt2, yt2, zt2;
    x2 = x;
    y2 = y;
    z2 = z;
    xt2 = xt;
    yt2 = yt;
    zt2 = zt;
    NGCALLF(trn32s,TRN32S)(&x2,&y2,&z2,&xt2,&yt2,&zt2,&iflag);
}

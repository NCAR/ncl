/*
 *	$Id: c_trn32s.c,v 1.3 2000-07-31 20:11:55 haley Exp $
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

extern void NGCALLF(trn32s,TRN32S)(float*,float*,float*,float*,float*,
                                   float*,int*);

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
    NGCALLF(trn32s,TRN32S)(&x,&y,&z,&xt,&yt,&zt,&iflag);
}

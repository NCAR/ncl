/*
 *      $Id: c_tdpara.c,v 1.3 2000-07-12 16:26:41 haley Exp $
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

void c_tdpara
#ifdef NeedFuncProto
(
    float ua00,
    float va00,
    float wa00,
    float uv10,
    float vv10,
    float wv10,
    float uv01,
    float vv01,
    float wv01
)
#else
(ua00,va00,wa00,uv10,vv10,wv10,uv01,vv01,wv01)
    float ua00;
    float va00;
    float wa00;
    float uv10;
    float vv10;
    float wv10;
    float uv01;
    float vv01;
    float wv01;
#endif
{
    float ua002,va002,wa002,uv102,vv102,wv102,uv012,vv012,wv012;
    ua002=ua00;
    va002=va00;
    wa002=wa00;
    uv102=uv10;
    vv102=vv10;
    wv102=wv10;
    uv012=uv01;
    vv012=vv01;
    wv012=wv01;
    NGCALLF(tdpara,TDPARA)(&ua002,&va002,&wa002,&uv102,&vv102,&wv102,
                                                &uv012,&vv012,&wv012);
}

/*
 *      $Id: c_tdpara.c,v 1.4 2000-07-31 20:12:10 haley Exp $
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

extern void NGCALLF(tdpara,TDPARA)(float*,float*,float*,float*,float*,float*,
                                                        float*,float*,float*);

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
    NGCALLF(tdpara,TDPARA)(&ua00,&va00,&wa00,&uv10,&vv10,&wv10,
                                             &uv01,&vv01,&wv01);
}

/*
 *	$Id: c_pwrit.c,v 1.2 2000-07-12 16:25:49 haley Exp $
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

void c_pwrit
#ifdef NeedFuncProto
(
    float px,
    float py,
    char *ch,
    int nc,
    int is,
    int io,
    int ic
)
#else
(px,py,ch,nc,is,io,ic)
    float px;
    float py;
    char *ch;
    int nc;
    int is;
    int io;
    int ic;
#endif
{
    float px2,py2;

    NGstring ch2;
    int len;

    px2 = px;
    py2 = py;

    if( !ch ) nc = 0;

    len = NGSTRLEN(ch);
    ch2 = NGCstrToFstr(ch,len);
    NGCALLF(pwrit,PWRIT)(&px2,&py2,ch2,&nc,&is,&io,&ic,len);
}

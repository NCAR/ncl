/*
 *	$Id: c_pwrzs.c,v 1.2 2000-07-12 16:25:55 haley Exp $
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

void c_pwrzs
#ifdef NeedFuncProto
(
    float x,
    float y,
    float z,
    char *id,
    int n,
    int isize,
    int lin3,
    int itop,
    int icnt
)
#else
(x,y,z,id,n,isize,lin3,itop,icnt)
    float x;
    float y;
    float z;
    char *id;
    int n;
    int isize;
    int lin3;
    int itop;
    int icnt;
#endif
{
    float x2, y2, z2;
    NGstring id2;
    int len;
    x2 = x;
    y2 = y;
    z2 = z;
    len = NGSTRLEN(id);
    id2 = NGCstrToFstr(id,len);
    NGCALLF(pwrzs,PWRZS)(&x2,&y2,&z2,id2,&n,&isize,&lin3,&itop,&icnt,len);
}

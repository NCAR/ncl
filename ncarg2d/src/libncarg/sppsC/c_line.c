/*
 *	$Id: c_line.c,v 1.2 2000-07-12 16:25:48 haley Exp $
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

void c_line
#ifdef NeedFuncProto
(
    float x1,
    float y1,
    float x2,
    float y2
)
#else
(x1,y1,x2,y2)
    float x1;
    float y1;
    float x2;
    float y2;
#endif
{
    float u1,v1,u2,v2;
    u1 = x1;
    v1 = y1;
    u2 = x2;
    v2 = y2;
    NGCALLF(line,LINE)(&u1,&v1,&u2,&v2);
}

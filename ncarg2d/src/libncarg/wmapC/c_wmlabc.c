/*
 *	$Id: c_wmlabc.c,v 1.2 2000-07-12 16:27:10 haley Exp $
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

void c_wmlabc
#ifdef NeedFuncProto
(
    float x,
    float y,
    char *city,
    char *temps
)
#else
(x,y,city,temps)
    float x;
    float y;
    char *city;
    char *temps;
)
#endif
{
    NGstring city2;
    NGstring temps2;
	int len1, len2;
    float x2, y2;
    x2 = x;
    y2 = y;
	len1 = NGSTRLEN(city);
	len2 = NGSTRLEN(temps);
	city2 = NGCstrToFstr(city,len1);
	temps2 = NGCstrToFstr(temps,len2);
    NGCALLF(wmlabc,WMLABC)(&x2,&y2,city2,temps2,len1,len2);
}

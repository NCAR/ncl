/*
 *	$Id: c_mappos.c,v 1.1 2001-10-10 02:51:21 haley Exp $
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

extern void NGCALLF(mappos,MAPPOS)(float*,float*,float*,float*);

void c_mappos
#ifdef NeedFuncProto
(
    float arg1,
    float arg2,
    float arg3,
    float arg4
)
#else
(arg1,arg2,arg3,arg4)
    float arg1;
    float arg2;
    float arg3;
    float arg4;
#endif
{
    NGCALLF(mappos,MAPPOS)(&arg1,&arg2,&arg3,&arg4);
}

/*
 *	$Id: c_fl2int.c,v 1.3 2000-07-31 20:11:49 haley Exp $
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

extern void NGCALLF(fl2int,FL2INT)(float*,float*,int*,int*);

void c_fl2int
#ifdef NeedFuncProto
(
    float px,
    float py,
    int *ix,
    int *iy
)
#else
(px,py,ix,iy)
    float px;
    float py;
    int *ix;
    int *iy;
#endif
{
    NGCALLF(fl2int,FL2INT)(&px,&py,ix,iy);
}

/*
 *      $Id: c_tdprpt.c,v 1.3 2000-07-12 16:26:41 haley Exp $
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

void c_tdprpt
#ifdef NeedFuncProto
(
    float  ui3d,
    float  vi3d,
    float  wi3d,
    float *xi2d,
    float *yi2d
)
#else
(ui3d,vi3d,wi3d,xi2d,yi2d)
    float  ui3d;
    float  vi3d;
    float  wi3d;
    float *xi2d;
    float *yi2d;
#endif
{
    float ui3d2,vi3d2,wi3d2;
    ui3d2=ui3d;
    vi3d2=vi3d;
    wi3d2=wi3d;
    NGCALLF(tdprpt,TDPRPT)(&ui3d2,&vi3d2,&wi3d2,xi2d,yi2d);
}

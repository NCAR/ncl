/*
 *      $Id: c_tdclrs.c,v 1.2 2000-07-12 16:26:38 haley Exp $
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

void c_tdclrs
#ifdef NeedFuncProto
(
    int   iwid,
    int   ibow,
    float shde,
    float shdr,
    int   iofc,
    int   iolc,
    int   ilmt
)
#else
(iwid,ibow,shde,shdr,iofc,iolc,ilmt)
    int   iwid;
    int   ibow;
    float shde;
    float shdr;
    int   iofc;
    int   iolc;
    int   ilmt;
#endif
{
    int iwid2,ibow2;
    float shde2,shdr2;
    int iofc2,iolc2,ilmt2;
    iwid2=iwid;
    ibow2=ibow;
    shde2=shde;
    shdr2=shdr;
    iofc2=iofc;
    iolc2=iolc;
    ilmt2=ilmt;
    NGCALLF(tdclrs,TDCLRS)(&iwid2,&ibow2,&shde2,&shdr2,&iofc2,&iolc2,&ilmt2);
}

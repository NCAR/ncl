/*
 *	$Id: c_trn32i.c,v 1.2 2000-07-12 16:24:34 haley Exp $
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

void c_trn32i
#ifdef NeedFuncProto
(
    float ut,
    float vt,
    float wt,
    float xt,
    float yt,
    float zt,
    int ient
)
#else
(ut,vt,wt,xt,yt,zt,ient)
    float ut;
    float vt;
    float wt;
    float xt;
    float yt;
    float zt;
    int ient;
#endif
{
    float ut2,vt2,wt2,xt2,yt2,zt2;

    ut2 = ut;
    vt2 = vt;
    wt2 = wt;
    xt2 = xt;
    yt2 = yt;
    zt2 = zt;
    NGCALLF(trn32i,TRN32I)(&ut2,&vt2,&wt2,&xt2,&yt2,&zt2,&ient);
}

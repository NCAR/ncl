/*
 *      $Id: c_tdline.c,v 1.2 2000-07-12 16:26:40 haley Exp $
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

void c_tdline
#ifdef NeedFuncProto
(
    float ucp1,
    float vcp1,
    float wcp1,
    float ucp2,
    float vcp2,
    float wcp2
)
#else
(ucp1,vcp1,wcp1,ucp2,vcp2,wcp2)
    float ucp1;
    float vcp1;
    float wcp1;
    float ucp2;
    float vcp2;
    float wcp2;
#endif
{
    float ucp12,vcp12,wcp12,ucp22,vcp22,wcp22;
    ucp12=ucp1;
    vcp12=vcp1;
    wcp12=wcp1;
    ucp22=ucp2;
    vcp22=vcp2;
    wcp22=wcp2;
    NGCALLF(tdline,TDLINE)(&ucp12,&vcp12,&wcp12,&ucp22,&vcp22,&wcp22);
}

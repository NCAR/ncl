/*
 *	$Id: c_set3.c,v 1.2 2000-07-12 16:26:49 haley Exp $
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

void c_set3 
#ifdef NeedFuncProto
(
    float xa,
    float xb,
    float ya,
    float yb,
    float ulo,
    float uhi,
    float vlo,
    float vhi,
    float wlo,
    float whi,
    float eye[3]
)
#else
 (xa,xb,ya,yb,ulo,uhi,vlo,vhi,wlo,whi,eye)
    float xa;
    float xb;
    float ya;
    float yb;
    float ulo;
    float uhi;
    float vlo;
    float vhi;
    float wlo;
    float whi;
    float eye[3];
#endif
{
    float xa2, xb2, ya2, yb2, ulo2, uhi2, vlo2, vhi2, wlo2, whi2;
    xa2 = xa;
    xb2 = xb;
    ya2 = ya;
    yb2 = yb;
    ulo2 = ulo;
    uhi2 = uhi;
    vlo2 = vlo;
    vhi2 = vhi;
    wlo2 = wlo;
    whi2 = whi;
    NGCALLF(set3,SET3)(&xa2,&xb2,&ya2,&yb2,&ulo2,&uhi2,&vlo2,&vhi2,&wlo2,&whi2,eye);
}

/*
 *      $Id: c_tdinit.c,v 1.3 2000-07-12 16:26:40 haley Exp $
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

void c_tdinit
#ifdef NeedFuncProto
(
    float umid,
    float vmid,
    float wmid,
    float uori,
    float vori,
    float wori,
    float uthi,
    float vthi,
    float wthi,
    float otep
)
#else
(umid,vmid,wmid,uori,vori,wori,uthi,vthi,wthi,otep)
    float umid;
    float vmid;
    float wmid;
    float uori;
    float vori;
    float wori;
    float uthi;
    float vthi;
    float wthi;
    float otep;
#endif
{
    float umid2,vmid2,wmid2,uori2,vori2,wori2,uthi2,vthi2,wthi2,otep2;
    umid2=umid;
    vmid2=vmid;
    wmid2=wmid;
    uori2=uori;
    vori2=vori;
    wori2=wori;
    uthi2=uthi;
    vthi2=vthi;
    wthi2=wthi;
    otep2=otep;
    NGCALLF(tdinit,TDINIT)(&umid2,&vmid2,&wmid2,&uori2,&vori2,&wori2,
                                                &uthi2,&vthi2,&wthi2,&otep2);
}

/*
 *      $Id: c_tdstrs.c,v 1.3 2000-07-12 16:26:42 haley Exp $
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

void c_tdstrs
#ifdef NeedFuncProto
(
    int   irst,
    int   ifc1,
    int   ifc2,
    int   ifc3,
    int   ifc4,
    int   ilc1,
    int   ilc2,
    int   iltd,
    float ustp,
    float vstp,
    float wstp
)
#else
(irst,ifc1,ifc2,ifc3,ifc4,ilc1,ilc2,iltd,ustp,vstp,wstp)
    int   irst;
    int   ifc1;
    int   ifc2;
    int   ifc3;
    int   ifc4;
    int   ilc1;
    int   ilc2;
    int   iltd;
    float ustp;
    float vstp;
    float wstp;
#endif
{
    int irst2,ifc12,ifc22,ifc32,ifc42,ilc12,ilc22,iltd2;
    float ustp2,vstp2,wstp2;
    irst2=irst;
    ifc12=ifc1;
    ifc22=ifc2;
    ifc32=ifc3;
    ifc42=ifc4;
    ilc12=ilc1;
    ilc22=ilc2;
    iltd2=iltd;
    ustp2=ustp;
    vstp2=vstp;
    wstp2=wstp;
    NGCALLF(tdstrs,TDSTRS)(&irst2,&ifc12,&ifc22,&ifc32,&ifc42,&ilc12,&ilc22,
                                                &iltd2,&ustp2,&vstp2,&wstp2);
}

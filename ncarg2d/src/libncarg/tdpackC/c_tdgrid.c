/*
 *      $Id: c_tdgrid.c,v 1.2 2000-07-12 16:26:39 haley Exp $
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

void c_tdgrid
#ifdef NeedFuncProto
(
    float xbeg,
    float xstp,
    int   noxs,
    float ybeg,
    float ystp,
    int   noys,
    int   igrd
)
#else
(xbeg,xstp,noxs,ybeg,ystp,noys,igrd)
    float xbeg;
    float xstp;
    int   noxs;
    float ybeg;
    float ystp;
    int   noys;
    int   igrd;
#endif
{
    float xbeg2,xstp2;
    int   noxs2;
    float ybeg2,ystp2;
    int   noys2,igrd2;
    xbeg2=xbeg;
    xstp2=xstp;
    noxs2=noxs;
    ybeg2=ybeg;
    ystp2=ystp;
    noys2=noys;
    igrd2=igrd;
    NGCALLF(tdgrid,TDGRID)(&xbeg2,&xstp2,&noxs2,&ybeg2,&ystp2,&noys2,&igrd2);
}

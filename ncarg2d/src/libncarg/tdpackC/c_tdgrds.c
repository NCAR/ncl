/*
 *      $Id: c_tdgrds.c,v 1.2 2000-07-12 16:26:39 haley Exp $
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

void c_tdgrds
#ifdef NeedFuncProto
(
    float umin,
    float vmin,
    float wmin,
    float umax,
    float vmax,
    float wmax,
    float ustp,
    float vstp,
    float wstp,
    int   igrt,
    int   ihid
)
#else
(umin,vmin,wmin,umax,vmax,wmax,ustp,vstp,wstp,igrt,ihid)
    float umin;
    float vmin;
    float wmin;
    float umax;
    float vmax;
    float wmax;
    float ustp;
    float vstp;
    float wstp;
    int   igrt;
    int   ihid;
#endif
{
    float umin2,vmin2,wmin2,umax2,vmax2,wmax2,ustp2,vstp2,wstp2;
    int   igrt2,ihid2;
    umin2=umin;
    vmin2=vmin;
    wmin2=wmin;
    umax2=umax;
    vmax2=vmax;
    wmax2=wmax;
    ustp2=ustp;
    vstp2=vstp;
    wstp2=wstp;
    igrt2=igrt;
    ihid2=ihid;
    NGCALLF(tdgrds,TDGRDS)(&umin2,&vmin2,&wmin2,&umax2,&vmax2,&wmax2,
                                  &ustp2,&vstp2,&wstp2,&igrt2,&ihid2);
}

/*
 *      $Id: c_tdmtri.c,v 1.2 2000-07-12 16:26:41 haley Exp $
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

void c_tdmtri
#ifdef NeedFuncProto
(
    int    imrk,
    float  umrk,
    float  vmrk,
    float  wmrk,
    float  smrk,
    float *rtri,
    int    mtri,
    int   *ntri,
    int    irst,
    float  umin,
    float  vmin,
    float  wmin,
    float  umax,
    float  vmax,
    float  wmax
)
#else
(imrk,umrk,vmrk,wmrk,smrk,rtri,mtri,ntri,irst,umin,vmin,wmin,umax,vmax,wmax)
    int    imrk;
    float  umrk;
    float  vmrk;
    float  wmrk;
    float  smrk;
    float *rtri;
    int    mtri;
    int   *ntri;
    int    irst;
    float  umin;
    float  vmin;
    float  wmin;
    float  umax;
    float  vmax;
    float  wmax;
#endif
{
    int   imrk2;
    float umrk2;
    float vmrk2;
    float wmrk2;
    float smrk2;
    int   mtri2;
    int   irst2;
    float umin2;
    float vmin2;
    float wmin2;
    float umax2;
    float vmax2;
    float wmax2;
    imrk2=imrk;
    umrk2=umrk;
    vmrk2=vmrk;
    wmrk2=wmrk;
    smrk2=smrk;
    mtri2=mtri;
    irst2=irst;
    umin2=umin;
    vmin2=vmin;
    wmin2=wmin;
    umax2=umax;
    vmax2=vmax;
    wmax2=wmax;
    NGCALLF(tdmtri,TDMTRI)(&imrk2,&umrk2,&vmrk2,&wmrk2,&smrk2,rtri,&mtri2,
                   ntri,&irst2,&umin2,&vmin2,&wmin2,&umax2,&vmax2,&wmax2);
}

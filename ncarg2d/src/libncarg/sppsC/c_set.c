/*
 *	$Id: c_set.c,v 1.2 2000-07-12 16:25:49 haley Exp $
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

void c_set
#ifdef NeedFuncProto
(
    float vl,
    float vr,
    float vb,
    float vt,
    float wl,
    float wr,
    float wb,
    float wt,
    int lf
)
#else
(vl,vr,vb,vt,wl,wr,wb,wt,lf)
    float vl;
    float vr;
    float vb;
    float vt;
    float wl;
    float wr;
    float wb;
    float wt;
    int lf;
#endif
{

    float vl2,vr2,vb2,vt2,wl2,wr2,wb2,wt2;
    vl2 = vl;
    vb2 = vb;
    wl2 = wl;
    wb2 = wb;
    vr2 = vr;
    vt2 = vt;
    wr2 = wr;
    wt2 = wt;
    NGCALLF(set,SET)(&vl2,&vr2,&vb2,&vt2,&wl2,&wr2,&wb2,&wt2,&lf);
}

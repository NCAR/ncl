/*
 *      $Id: c_tdlbls.c,v 1.2 2000-07-12 16:26:40 haley Exp $
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

void c_tdlbls
#ifdef NeedFuncProto
(
    float umin,
    float vmin,
    float wmin,
    float umax,
    float vmax,
    float wmax,
    char* unlb,
    char* vnlb,
    char* wnlb,
    char* uilb,
    char* vilb,
    char* wilb,
    int   ipck
)
#else
(umin,vmin,wmin,umax,vmax,wmax,unlb,vnlb,wnlb,uilb,vilb,wilb,ipck)
    float umin;
    float vmin;
    float wmin;
    float umax;
    float vmax;
    float wmax;
    char* unlb;
    char* vnlb;
    char* wnlb;
    char* uilb;
    char* vilb;
    char* wilb;
    int   ipck;
#endif
{
    float umin2,vmin2,wmin2,umax2,vmax2,wmax2;
    int lounlb;
    NGstring unlb2;
    int lovnlb;
    NGstring vnlb2;
    int lownlb;
    NGstring wnlb2;
    int louilb;
    NGstring uilb2;
    int lovilb;
    NGstring vilb2;
    int lowilb;
    NGstring wilb2;
    int   ipck2;
    umin2=umin;
    vmin2=vmin;
    wmin2=wmin;
    umax2=umax;
    vmax2=vmax;
    wmax2=wmax;
    lounlb=NGSTRLEN(unlb);
    unlb2=NGCstrToFstr(unlb,lounlb);
    lovnlb=NGSTRLEN(vnlb);
    vnlb2=NGCstrToFstr(vnlb,lovnlb);
    lownlb=NGSTRLEN(wnlb);
    wnlb2=NGCstrToFstr(wnlb,lownlb);
    louilb=NGSTRLEN(uilb);
    uilb2=NGCstrToFstr(uilb,louilb);
    lovilb=NGSTRLEN(vilb);
    vilb2=NGCstrToFstr(vilb,lovilb);
    lowilb=NGSTRLEN(wilb);
    wilb2=NGCstrToFstr(wilb,lowilb);
    ipck2=ipck;
    NGCALLF(tdlbls,TDLBLS)(&umin2,&vmin2,&wmin2,&umax2,&vmax2,&wmax2,
                          unlb2,vnlb2,wnlb2,uilb2,vilb2,wilb2,&ipck2,
                           lounlb,lovnlb,lownlb,louilb,lovilb,lowilb);
}

/*
 *      $Id: c_tdlbls.c,v 1.4 2000-08-22 15:07:17 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU General Public License as published     *
* by the Free Software Foundation; either version 2 of the License, or  *
* (at your option) any later version.                                   *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* General Public License for more details.                              *
*                                                                       *
* You should have received a copy of the GNU General Public License     *
* along with this software; if not, write to the Free Software         *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

#include <ncarg/ncargC.h>

extern void NGCALLF(tdlbls,TDLBLS)(float*,float*,float*,float*,float*,float*,
                                   NGstring,NGstring,NGstring,NGstring,
                                   NGstring,NGstring,int*,
                                   int,int,int,int,int,int);

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
    NGCALLF(tdlbls,TDLBLS)(&umin,&vmin,&wmin,&umax,&vmax,&wmax,
                            unlb2,vnlb2,wnlb2,uilb2,vilb2,wilb2,&ipck,
                           lounlb,lovnlb,lownlb,louilb,lovilb,lowilb);
}
